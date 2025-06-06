# Function to generate a dictionary list of file names, local paths,
# and mirrored S3 location URIs from file_dict.csv
model_file_dict <- function(run_id = NULL, year = NULL) {
  env <- environment()
  wd <- here::here()
  suppressPackageStartupMessages(library(magrittr))

  if (!is.null(run_id)) {
    if (run_id == "") {
      stop("run_id cannot be an empty string")
    } else if (!stringr::str_detect(run_id, "^[a-z0-9]+(?:[-][a-z0-9]+)*$")) {
      stop("run_id must contain only alphanumeric characters and hyphens")
    } else if (!stringr::str_detect(run_id, "^[0-9]{4}-[0-9]{2}-[0-9]{2}-[a-z]*-[a-z]*")) { # nolint
      stop("run_id must be in the format YYYY-MM-DD-<adjective>-<person>")
    }
  }

  if (!is.null(year)) {
    if (year == "") {
      stop("year cannot be an empty string")
    } else if (!stringr::str_detect(year, "^[0-9]{4}$")) {
      stop("year must be a four-digit number")
    } else if (is.numeric(year)) {
      stop("year must be a string")
    }
  }

  # Convert flat dictionary file to nested list
  dict <- read.csv(
    here::here("misc", "file_dict.csv"),
    colClasses = c("character", "character", "numeric", rep("character", 9)),
    na.strings = ""
  ) %>%
    dplyr::mutate(
      s3 = as.character(purrr::map_if(
        path_s3, ~ !is.na(.x), glue::glue,
        .envir = env, .na = NULL, .null = NA_character_
      )),
      s3 = ifelse(!is.na(s3), file.path(paste0("s3://", s3_bucket), s3), NA),
      local = ifelse(!is.na(path_local), file.path(wd, path_local), NA)
    ) %>%
    dplyr::select(type, name, s3, local) %>%
    split(., .$type) %>%
    purrr::map(., ~ split(.x, .x$name, drop = TRUE)) %>%
    purrr::map(., ~ purrr::map(.x, function(x) {
      as.list(x)[!is.na(x) & names(x) %in% c("s3", "local")]
    }))

  return(dict)
}

# Used to fetch a run's output from S3 and populate it locally. Useful for
# running reports and performing local troubleshooting
# nolint start: cyclocomp_linter
model_fetch_run <- function(run_id, year) {
  tictoc::tic(paste0("Fetched run: ", run_id))

  paths <- model_file_dict(run_id, year)
  s3_objs <- grep("s3://", unlist(paths), value = TRUE)
  bucket <- strsplit(s3_objs[1], "/")[[1]][3]

  for (path in paths$output) {
    is_directory <- endsWith(path$s3, "/")
    if (is_directory) {
      partitioned_by_run <- endsWith(path$s3, paste0("run_id=", run_id, "/"))
      if (partitioned_by_run) {
        dir_path <- path$s3
      } else {
        dir_path <- paste0(path$s3, "year=", year, "/run_id=", run_id, "/")
      }

      message("Now fetching: ", dir_path)
      objs_prefix <- sub(paste0("s3://", bucket, "/"), "", dir_path)
      objs <- aws.s3::get_bucket_df(bucket, objs_prefix)
      objs <- dplyr::filter(objs, Size > 0)
      if (nrow(objs) > 0 && all(endsWith(objs$Key, ".parquet"))) {
        df <- dplyr::collect(arrow::open_dataset(dir_path))
        arrow::write_parquet(df, path$local)
      } else if (nrow(objs) > 0) {
        for (key in objs$Key) {
          message("Now fetching: ", key)
          local_path <- file.path(path$local, basename(key))
          local_path <- unname(gsub("//", "/", local_path))
          aws.s3::save_object(key, bucket = bucket, file = local_path)
        }
      } else {
        warning(path$local, " does not exist for this run")
      }
    } else {
      message("Now fetching: ", path$s3)
      if (aws.s3::object_exists(path$s3, bucket = bucket)) {
        aws.s3::save_object(path$s3, bucket = bucket, file = path$local)
      } else {
        warning(path$local, " does not exist for this run")
      }
    }
  }
  tictoc::toc()
}
# nolint end: cyclocomp_linter

# Extract the number of iterations that occurred before early stopping during
# cross-validation. See the tune::tune_bayes() argument `extract`
extract_num_iterations <- function(x) {
  fit <- workflows::extract_fit_engine(x)
  evals <- purrr::pluck(fit, "record_evals", "validation", 1, "eval")
  length(evals)
}

# Given the result of a CV search, get the number of iterations from the
# result set with the best performing hyperparameters
select_iterations <- function(tune_results, metric, type = "mean") {
  stopifnot(type %in% c("mean", "median", "max"))
  func <- switch(type,
    mean = mean,
    median = median,
    max = max
  )

  tune_results %>%
    dplyr::select(id, .metrics, .extracts) %>%
    tidyr::unnest(cols = .metrics) %>%
    dplyr::filter(.metric == params$cv$best_metric) %>%
    dplyr::select(-.extracts) %>%
    dplyr::left_join(
      tune_results %>%
        tidyr::unnest(cols = .extracts) %>%
        tidyr::unnest(cols = .extracts) %>%
        dplyr::select(!dplyr::where(is.list), -.config, -.iter)
    ) %>%
    dplyr::inner_join(tune::select_best(tune_results, metric = metric)) %>%
    suppressMessages() %>%
    dplyr::summarize(num_iterations = ceiling(func(.extracts)))
}

# Silly copy of ccao::vars_recode to convert text versions of categoricals back
# to numbers
var_encode <- function(data,
                       cols = dplyr::everything(),
                       dictionary = ccao::vars_dict) {
  var <- "var_code"

  dict_long <- dictionary %>%
    dplyr::filter(
      .data$var_type == "char" &
        .data$var_data_type == "categorical"
    ) %>%
    dplyr::select(
      dplyr::starts_with("var_name_"),
      .data$var_code:.data$var_value_short
    ) %>%
    tidyr::pivot_longer(
      dplyr::starts_with("var_name_"),
      names_to = "var_type",
      values_to = "var_name"
    ) %>%
    dplyr::distinct(
      .data$var_code,
      .data$var_value, .data$var_value_short, .data$var_name
    )

  dplyr::mutate(
    data,
    dplyr::across(dplyr::all_of(cols), function(x, y = dplyr::cur_column()) {
      if (y %in% dict_long$var_name) {
        var_rows <- which(dict_long$var_name == y)
        idx <- match(x, dict_long$var_value[var_rows])
        out <- dict_long[[var]][var_rows][idx]
        return(out)
      } else {
        return(x)
      }
    })
  )
}

# Yardstick doesn't currently include MdAPE, so we'll add it here
mdape_vec <- function(truth, estimate, case_weights = NULL, na_rm = TRUE) {
  yardstick::check_numeric_metric(truth, estimate, case_weights)

  if (na_rm) {
    result <- yardstick::yardstick_remove_missing(truth, estimate, case_weights)

    truth <- result$truth
    estimate <- result$estimate
  } else if (yardstick::yardstick_any_missing(truth, estimate, case_weights)) {
    return(NA_real_)
  }

  errors <- abs((truth - estimate) / truth)
  out <- median(errors)
  out <- out * 100
  out
}

# Modified rolling origin forecast split function. Splits the training data into
# N separate time windows, each of which can overlap by N months. Also splits
# out a validation set from each windows for Tidymodels / LightGBM.
#
# WARNING: If train_includes_val is TRUE, then each training window contains
# the training data AND the validation data (they overlap! see GitLab issue #82
# and the README for more information)
create_rolling_origin_splits <- function(data,
                                         v = 5,
                                         overlap_months = 0,
                                         date_col,
                                         val_prop,
                                         train_includes_val = FALSE,
                                         cumulative = FALSE) {
  stopifnot(
    v >= 2 && v <= 20,
    overlap_months >= 0,
    val_prop >= 0 && val_prop < 1,
    is.logical(train_includes_val),
    is.logical(cumulative)
  )

  data <- dplyr::arrange(data, {{ date_col }})

  # Find the duration (in months) that splits the date range of the training
  # data. There may be some remainder
  duration_per_fold <- data %>%
    dplyr::summarise(
      min_date = min({{ date_col }}),
      max_date = max({{ date_col }})
    ) %>%
    dplyr::mutate(
      dur_per_fold = lubridate::as.duration(
        ceiling((max_date - min_date) / v)
      )
    ) %>%
    dplyr::pull(dur_per_fold) %/% lubridate::dmonths(1)

  if (overlap_months > duration_per_fold) {
    stop(
      "Overlap period must be less than the duration of each fold. ",
      "Please reduce overlap_months or decrease the number of folds."
    )
  }

  # If an overlap period is provided, shift and expand the periods/dates
  # such that windows overlap by each overlap amount, but still cover the
  # entire date period
  start_dates <- seq.Date(
    from = min(data$meta_sale_date),
    by = paste0(duration_per_fold + overlap_months, " months"),
    length.out = v
  )
  start_offset <- c(
    lubridate::dmonths(0),
    lubridate::as.duration(
      cumsum(rep(lubridate::dmonths(overlap_months), v - 1))
    )
  )
  start_dates <- as.Date(start_dates - start_offset)

  # Split the training data into N separate time windows, overlapping based on
  # the overlap argument, then recombine into a single dataframe
  data_split <- purrr::imap_dfr(start_dates, function(x, i) {
    if (x == max(start_dates)) {
      end_date <- as.Date(Inf)
    } else {
      end_date <- x + lubridate::dmonths(duration_per_fold + overlap_months)
      end_date <- as.Date(end_date)
    }
    data_sub <- data %>%
      dplyr::mutate(split_id = i, idx = dplyr::row_number()) %>%
      dplyr::filter({{ date_col }} >= x & {{ date_col }} <= end_date) %>%
      dplyr::group_by(split_id) %>%
      dplyr::summarize(min_idx = min(idx), max_idx = max(idx))
  })

  # If no overlap period set, force the indices to be non-overlapping
  if (overlap_months == 0) {
    data_split <- data_split %>%
      dplyr::mutate(
        min_idx = dplyr::lag(max_idx),
        min_idx = ifelse(is.na(min_idx), 1, min_idx + 1)
      )
  }

  # Create indices to split the data into training and validation sets
  if (cumulative) {
    starts <- rep(1, length(data_split$max_idx))
  } else {
    starts <- data_split$min_idx
  }

  in_idx <- mapply(seq, starts, data_split$max_idx, SIMPLIFY = FALSE)
  out_idx <- lapply(in_idx, function(x) {
    n <- length(x)
    m <- min(n - floor(n * val_prop), n - 1) + 1
    add_to_idx <- ifelse(x[1] == 1, 0, x[1] - 1)
    seq(add_to_idx + max(m, 3), add_to_idx + n)
  })

  if (!train_includes_val) {
    in_idx <- mapply(setdiff, in_idx, out_idx, SIMPLIFY = FALSE)
  }

  # Create the final rsample object from indices
  indices <- mapply(
    rsample:::merge_lists, in_idx, out_idx,
    SIMPLIFY = FALSE
  )
  split_objs <- purrr::map(
    indices, rsample::make_splits,
    data = data, class = "rof_split"
  )
  split_objs <- list(
    splits = split_objs,
    id = recipes::names0(length(split_objs), "Slice")
  )
  rset <- rsample::new_rset(
    splits = split_objs$splits,
    ids = split_objs$id,
    subclass = c("rolling_origin", "rset")
  )
  return(rset)
}
