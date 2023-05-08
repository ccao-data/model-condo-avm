#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# 1. Setup ---------------------------------------------------------------------
#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Start the stage timer
tictoc::tic.clearlog()
tictoc::tic("Ingest")

# Pre-allocate memory for java JDBC driver
options(java.parameters = "-Xmx10g")

# Load R libraries
suppressPackageStartupMessages({
  library(arrow)
  library(ccao)
  library(DBI)
  library(data.table)
  library(dplyr)
  library(glue)
  library(here)
  library(igraph)
  library(lubridate)
  library(purrr)
  library(reticulate)
  library(RJDBC)
  library(s2)
  library(sf)
  library(tictoc)
  library(tidyr)
  library(yaml)
})
source(here("R", "helpers.R"))

# Load Python packages and functions with reticulate
use_virtualenv("pipenv/")
source_python("py/flagging.py")

# Initialize a dictionary of file paths. See misc/file_dict.csv for details
paths <- model_file_dict()

# Load the parameters file containing the run settings
params <- read_yaml("params.yaml")

# Setup the Athena JDBC driver
aws_athena_jdbc_driver <- RJDBC::JDBC(
  driverClass = "com.simba.athena.jdbc.Driver",
  classPath = list.files("~/drivers", "^Athena.*jar$", full.names = TRUE),
  identifier.quote = "'"
)

# Establish Athena connection
AWS_ATHENA_CONN_JDBC <- dbConnect(
  aws_athena_jdbc_driver,
  url = Sys.getenv("AWS_ATHENA_JDBC_URL"),
  aws_credentials_provider_class = Sys.getenv("AWS_CREDENTIALS_PROVIDER_CLASS"),
  Schema = "Default",
  WorkGroup = "read-only-with-scan-limit"
)




#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# 2. Pull Data -----------------------------------------------------------------
#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
message("Pulling data from Athena")

# Pull the training data, which contains actual sales + attached characteristics
# from the condominium input view. We want to get sales spanning multiple
# parcels only for sales that sell with deeded parking spots
tictoc::tic("Training data pulled")
training_data <- dbGetQuery(
  conn = AWS_ATHENA_CONN_JDBC, glue("
  SELECT
      sale.sale_price AS meta_sale_price,
      sale.sale_date AS meta_sale_date,
      sale.doc_no AS meta_sale_document_num,
      sale.deed_type AS meta_sale_deed_type,
      sale.seller_name AS meta_sale_seller_name,
      sale.buyer_name AS meta_sale_buyer_name,
      sale.sale_filter_is_outlier AS sv_is_ptax203_outlier,
      condo.*
  FROM model.vw_pin_condo_input condo
  INNER JOIN default.vw_pin_sale sale
      ON sale.pin = condo.meta_pin
      AND sale.year = condo.meta_year
  WHERE (condo.meta_year 
      BETWEEN '{params$input$min_sale_year}' 
      AND '{params$input$max_sale_year}')
  AND sale.num_parcels_sale <= 2
  ")
)

# Heuristic for handling multi-PIN sales. We want to keep sales with a deeded
# parking spot, but only the sale for the unit, not the parking. Drop other 
# multi-unit sale types since we don't have a way to disaggregate each
# unit's value
training_data <- training_data %>%
  group_by(meta_sale_document_num) %>%
  arrange(meta_tieback_proration_rate) %>%
  mutate(keep_unit_sale =
    meta_tieback_proration_rate >= (lag(meta_tieback_proration_rate) * 3) 
  ) %>%
  filter(n() == 1 | (n() == 2 & keep_unit_sale)) %>%
  ungroup() %>%
  filter(!as.logical(as.numeric(ind_pin_is_multilline))) %>%
  select(-keep_unit_sale)

tictoc::toc()

# Pull all condo input data for the assessment year. This will be the
# data we actually run the model on
tictoc::tic("Assessment data pulled")

# 2022/2023 data is currently desynchronized between office collected
# characteristics and the newest data available in the system of record.
# Here we attach all of the data department's 2022 non-iasWorld data to
# iasWorld's 2023 universe of condos and their associated iasWorld data to make
# sure all condo data is as up-to-date as possible, and the assessment universe
# is complete. THIS STEP SHOULD BE TEMPORARY AND ADDRESSED WITH A PERMANENT
# SOLUTION ASAP.
assessment_data <- dbGetQuery(
  conn = AWS_ATHENA_CONN_JDBC, glue("
  SELECT *
  FROM model.vw_pin_condo_input
  WHERE meta_year IN (
    '{params$assessment$data_year}',
    '{params$assessment$shift_year}'
  )
  ")
)
tictoc::toc()

# These meta columns need to be brought forward one year along with other
# data department-sourced columns.
shifted_meta_columns <- c(
  "meta_year",
  "meta_mailed_bldg",
  "meta_mailed_land",
  "meta_mailed_tot",
  "meta_certified_bldg",
  "meta_certified_land",
  "meta_certified_tot",
  "meta_board_bldg",
  "meta_board_land",
  "meta_board_tot",
  "meta_1yr_pri_board_bldg",
  "meta_1yr_pri_board_land",
  "meta_1yr_pri_board_tot",
  "meta_2yr_pri_board_bldg",
  "meta_2yr_pri_board_land",
  "meta_2yr_pri_board_tot"
)

assessment_data_shifted <- left_join(
  assessment_data %>%
    filter(meta_year == params$assessment$shift_year) %>%
    select(starts_with("meta") & !shifted_meta_columns),
  assessment_data %>%
    filter(meta_year == params$assessment$data_year) %>%
    select(
      c(
        "meta_pin", "meta_card_num",
        shifted_meta_columns,
        !starts_with("meta")
        )
      ),
  by = join_by(meta_pin, meta_card_num)
) %>%
  # We can't join on lline due to PINs that were switched from 399s to 299s b/w
  # 2022 and 2023, which means we need to remove some duplicates.
  distinct()

# Pull  neighborhood-level land rates per sqft, as calculated by Valuations
tictoc::tic("Land rate data pulled")
land_nbhd_rate_data <- dbGetQuery(
  conn = AWS_ATHENA_CONN_JDBC, glue("
  SELECT *
  FROM ccao.land_nbhd_rate
  WHERE year = '{params$assessment$year}'
  ")
)
tictoc::toc()

# Close connection to Athena
dbDisconnect(AWS_ATHENA_CONN_JDBC)
rm(AWS_ATHENA_CONN_JDBC, aws_athena_jdbc_driver)




#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# 3. Define Functions ----------------------------------------------------------
#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Ingest-specific helper functions for data cleaning, spatial lags, etc.

# Create a dictionary of column types, as specified in ccao::vars_dict
col_type_dict <- ccao::vars_dict %>%
  distinct(var_name = var_name_model, var_type = var_data_type)

# Mini-function to ensure that columns are the correct type
recode_column_type <- function(col, col_name, dict = col_type_dict) {
  col_type <- dict %>%
    filter(var_name == col_name) %>%
    pull(var_type)
  
  switch(
    col_type,
    numeric = as.numeric(col),
    character = as.character(col),
    logical = as.logical(as.numeric(col)),
    categorical = as.factor(col),
    date = lubridate::as_date(col)
  )
}


# Create quantiles with unbounded top and bottom bins. Used to bin
# condo building sales prices into strata
val_create_ntiles <- function(x, probs, na.rm = TRUE) {
  stopifnot(
    is.numeric(x),
    is.numeric(probs),
    is.logical(na.rm)
  )
  
  output <- list(c(
    -Inf,
    unique(stats::quantile(x, probs = probs, na.rm = na.rm, names = FALSE)),
    Inf
  ))
  output <- ifelse(all(is.na(x)), list(NA_real_), output)
  
  return(output)
}


# Given a sale price x, assign the sale price to a pre-made strata bin
val_assign_ntile <- function(x, ntiles) {

  output <- as.character(ifelse(
    !is.na(x),
    purrr::pmap(
      list(x, ntiles),
      ~ cut(.x, breaks = .y, labels = FALSE)
    ),
    NA_character_
  ))
  
  return(output)
}


# Given a set of k-means centers and a sale price, find the nearest center
val_assign_center <- function(x, centers) {

  output <- as.character(ifelse(
    !is.na(x) & !is.na(centers),
    purrr::pmap(
      list(x, centers), 
      ~ which.min(mapply(function(z, y) sum(z - y) ^ 2, .x, .y))
    ),
    NA_character_
  ))
  
  return(output)
}


# Rescaling function to normalize a continuous range to be between a min and max
rescale <- function(x, min = 0, max = 1) {
  output <- (x - min(x, na.rm = TRUE)) /
    (max(x, na.rm = TRUE) - min(x, na.rm = TRUE)) *
    (max - min) + min
  
  return(output)
}




#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# 4. Validate Sales ------------------------------------------------------------
#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
message("Validating training data sales")

# Create an outlier sale flag using a variety of heuristics. See flagging.py for
# the full list. Also exclude any sales that have a flag on Q10 of the PTAX-203
# form AND are large statistical outliers
training_data_w_sv <- training_data %>%
  mutate(
    meta_sale_price = as.numeric(meta_sale_price),
    sv_is_ptax203_outlier = as.logical(as.numeric(sv_is_ptax203_outlier))
  ) %>%
  # Run Python-based automatic sales validation to identify outliers
  create_stats(as.list(params$input$sale_validation$stat_groups)) %>%
  string_processing() %>%
  iso_forest(
    as.list(params$input$sale_validation$stat_groups),
    params$input$sale_validation$iso_forest
  ) %>%
  outlier_taxonomy(
    as.list(params$input$sale_validation$dev_bounds),
    as.list(params$input$sale_validation$stat_groups)
  ) %>%
  # Combine outliers identified via PTAX-203 with the heuristic-based outliers
  rename(sv_is_autoval_outlier = sv_is_outlier) %>%
  mutate(
    sv_is_autoval_outlier = sv_is_autoval_outlier == "Outlier",
    sv_is_autoval_outlier = replace_na(sv_is_autoval_outlier, FALSE),
    sv_is_outlier = sv_is_autoval_outlier | sv_is_ptax203_outlier,
    sv_outlier_type = ifelse(
      sv_outlier_type == "Not outlier" & sv_is_ptax203_outlier,
      "PTAX-203 flag",
      sv_outlier_type
    ),
    sv_outlier_type = replace_na(sv_outlier_type, "Not outlier"),
    sv_is_outlier = sv_outlier_type != "Not outlier"
  ) %>%
  select(
    meta_pin, meta_sale_date, meta_sale_document_num,
    sv_is_ptax203_outlier, sv_is_autoval_outlier, sv_is_outlier, sv_outlier_type
  ) %>%
  # Rejoin validation output to the original training data. CAUTION: converting
  # data to pandas and back WILL alter certain R data types. For example,
  # missing character values are replaced with "NA"
  right_join(
    training_data %>% select(-sv_is_ptax203_outlier),
    by = c(
      "meta_pin",
      "meta_sale_date", "meta_sale_document_num"
    )
  )




#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# 5. Add Features and Clean ----------------------------------------------------
#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
message("Adding time features and cleaning")

## 5.1. Training Data ----------------------------------------------------------

# Clean up the training data. Goal is to get it into a publishable format.
# Final featurization, missingness, etc. is handled via Tidymodels recipes
training_data_clean <- training_data_w_sv %>%
  # Recode factor variables using the definitions stored in ccao::vars_dict
  # This will remove any categories not stored in the dictionary and convert
  # them to NA (useful since there are a lot of misrecorded variables)
  ccao::vars_recode(cols = starts_with("char_"), type = "code") %>%
  # Coerce columns to the data types recorded in the dictionary. Necessary
  # because the SQL drivers will often coerce types on pull (boolean becomes
  # character)
  mutate(across(
    !starts_with("sv_"),
    ~ recode_column_type(.x, cur_column())
  )) %>%
  # Create time/date features using lubridate
  dplyr::mutate(
    # Calculate interval periods and time since the start of the sales sample
    time_interval = interval(
      make_date(params$input$min_sale_year, 1, 1),
      ymd(meta_sale_date)
    ),
    time_sale_year = as.numeric(year(meta_sale_date)),
    time_sale_day = as.numeric(time_interval %/% days(1)) + 1,
    # Get components of dates to correct for seasonality and other factors
    time_sale_quarter_of_year = paste0("Q", quarter(meta_sale_date)),
    time_sale_month_of_year = as.integer(month(meta_sale_date)),
    time_sale_day_of_year = as.integer(yday(meta_sale_date)),
    time_sale_day_of_month = as.integer(day(meta_sale_date)),
    time_sale_day_of_week = as.integer(wday(meta_sale_date)),
    time_sale_post_covid = meta_sale_date >= make_date(2020, 3, 15),
    # Time window to use for cross-validation during training. The last X% of
    # each window is held out as a validation set
    time_split = time_interval %/% months(params$input$time_split),
    # Collapse the last 2 splits into their earlier neighbor. This is done
    # because part of the final time window will be held out for the test set,
    # which will shrink the last split to the point of being too small for CV
    time_split = ifelse(
      time_split > max(time_split) - 2,
      max(time_split) - 2,
      time_split
    ),
    time_split = as.character(time_split + 1),
    time_split = factor(time_split, levels = sort(unique(time_split)))
  ) %>%
  select(-time_interval) %>%
  relocate(starts_with("sv_"), .after = everything())


## 5.2. Assessment Data --------------------------------------------------------

# Clean the assessment data. This is the target data that the trained model is
# used on. The cleaning steps are the same as above, with the exception of the
# time variables
assessment_data_clean <- assessment_data_shifted %>%
  ccao::vars_recode(cols = starts_with("char_"), type = "code") %>%
  mutate(across(everything(), ~ recode_column_type(.x, cur_column()))) %>%
  # Create sale date features BASED ON THE ASSESSMENT DATE. The model predicts
  # the sale price of properties on the date of assessment. Not the date of an
  # actual sale
  dplyr::mutate(
    meta_sale_date = as_date(params$assessment$date),
    time_interval = interval(
      make_date(params$input$min_sale_year, 1, 1),
      ymd(meta_sale_date)
    ),
    time_sale_year = as.numeric(year(meta_sale_date)),
    time_sale_day = as.numeric(time_interval %/% days(1)) + 1,
    time_sale_quarter_of_year = paste0("Q", quarter(meta_sale_date)),
    time_sale_month_of_year = as.integer(month(meta_sale_date)),
    time_sale_day_of_year = as.integer(yday(meta_sale_date)),
    time_sale_day_of_month = as.integer(day(meta_sale_date)),
    time_sale_day_of_week = as.integer(wday(meta_sale_date)),
    time_sale_post_covid = meta_sale_date >= make_date(2020, 3, 15)
  ) %>%
  select(-time_interval)


## 5.3. Land Rates -------------------------------------------------------------
message("Saving land rates")

# Write land data directly to file, since it's already mostly clean
land_nbhd_rate_data %>%
  select(meta_nbhd = town_nbhd, land_rate_per_sqft) %>%
  write_parquet(paths$input$land_nbhd_rate$local)




#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# 6. Condo Strata --------------------------------------------------------------
#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
message("Calculating condo strata")

## 6.1. Calculate Strata -------------------------------------------------------

# Condominiums' unit characteristics (such as square footage, # of bedrooms,
# etc.) are not tracked by the CCAO. As such, e need to rely on other
# information to determine the value of unsold condos. Fortunately, condos are
# more homogeneous than single-family homes and are pre-grouped into like units
# (buildings)

# As such, we can use the historic sale price of other sold units in the same
# building to determine an unsold condo's value. To do so, we construct condo
# "strata", which are bins of the 5-year average sale price of the building.
# Units and buildings in the same strata should ultimately have very similar
# assessed values

# The first step here is to get the average sale price of condos in each
# building. The first 10 digits of a given PIN are the building (the last 4 are
# the unit)

# Get the the recency-weighted mean log10 sale price of each building
bldg_5yr_sales_avg <- training_data_clean %>%
  filter(
    meta_sale_date > make_date(as.numeric(params$input$max_sale_year) - 4),
    !sv_is_outlier
  ) %>%
  select(
    meta_pin10, meta_sale_price, meta_sale_date,
    all_of(params$input$strata$group_var)
  ) %>%
  mutate(
    meta_sale_date_norm = rescale(
      as.numeric(meta_sale_date),
      params$input$strata$weight_min,
      params$input$strata$weight_max
    )
  ) %>%
  group_by(meta_pin10, across(any_of(params$input$strata$group_var))) %>%
  summarise(
    mean_log10_sale_price = weighted.mean(
      log10(meta_sale_price),
      meta_sale_date_norm,
      na.rm = TRUE
    ),
    meta_pin10_5yr_num_sale = n()
  ) %>%
  ungroup()

# Use either k-means clustering or simple quantiles to construct a condominium
# building strata model. This model can be used to assign strata to buildings
if (params$input$strata$type == "kmeans") {
  
  # Set seed for k-means reproducibility
  set.seed(params$input$strata$seed)

  # For k-means, construct strata as a 1-dimensional cluster of the average
  # sale price of the building
  bldg_strata_model <- bldg_5yr_sales_avg %>%
    group_by(across(all_of(params$input$strata$group_var))) %>%
    summarize(
      meta_strata_model_1 = list(kmeans(
        mean_log10_sale_price,
        centers = params$input$strata$k_1,
        iter.max = 200,
        nstart = 50,
        algorithm = "MacQueen"
      )$centers),
      meta_strata_model_2 = list(kmeans(
        mean_log10_sale_price,
        centers = params$input$strata$k_2,
        iter.max = 200,
        nstart = 25,
        algorithm = "MacQueen"
      )$centers)
    ) %>%
    ungroup()
  
} else if (params$input$strata$type == "ntile") {
  
  # Construct strata as quantile bins of the average sale price of the building
  bldg_strata_model <- bldg_5yr_sales_avg %>%
    group_by(across(all_of(params$input$strata$group_var))) %>%
    summarize(
      meta_strata_model_1 = val_create_ntiles(
        x = mean_log10_sale_price,
        probs = seq(0, 1, 1 / params$input$strata$k_1)[
          c(-1, -(params$input$strata$k_1 + 1))
        ]
      ),
      meta_strata_model_2 = val_create_ntiles(
        x = mean_log10_sale_price,
        probs = seq(0, 1, 1 / params$input$strata$k_2)[
          c(-1, -(params$input$strata$k_2 + 1))
        ]
      )
    ) %>%
    ungroup()
}

# Save strata model to file in case we need to use it later
bldg_strata_model %>%
  write_parquet(paths$input$condo_strata$local)


## 6.2. Assign Strata ----------------------------------------------------------

# Use strata models to create strata of building-level, previous-5-year sale
# prices. These strata are used as categorical variables in the model
bldg_strata <- bldg_5yr_sales_avg %>%
  left_join(bldg_strata_model, by = params$input$strata$group_var) %>%
  mutate(
    meta_strata_1 = switch(
      params$input$strata$type,
      kmeans = val_assign_center(mean_log10_sale_price, meta_strata_model_1),
      ntile = val_assign_ntile(mean_log10_sale_price, meta_strata_model_1)
    ),
    meta_strata_2 = switch(
      params$input$strata$type,
      kmeans = val_assign_center(mean_log10_sale_price, meta_strata_model_2),
      ntile = val_assign_ntile(mean_log10_sale_price, meta_strata_model_2)
    )
  ) %>%
  group_by(across(params$input$strata$group_var), meta_strata_1) %>%
  mutate(meta_strata_1_5yr_num_sale = sum(meta_pin10_5yr_num_sale)) %>%
  group_by(across(params$input$strata$group_var), meta_strata_2) %>%
  mutate(meta_strata_2_5yr_num_sale = sum(meta_pin10_5yr_num_sale)) %>%
  ungroup() %>%
  select(
    -c(mean_log10_sale_price, meta_strata_model_1, meta_strata_model_2),
    -all_of(params$input$strata$group_var)
  )

# Attach the strata and sale counts for both assessment and training data
training_data_w_strata <- training_data_clean %>%
  left_join(bldg_strata, by = "meta_pin10") %>%
  mutate(meta_pin10_5yr_num_sale = replace_na(meta_pin10_5yr_num_sale, 0)) %>%
  write_parquet(paths$input$training$local)

assessment_data_w_strata <- assessment_data_clean %>%
  left_join(bldg_strata, by = "meta_pin10") %>%
  mutate(meta_pin10_5yr_num_sale = replace_na(meta_pin10_5yr_num_sale, 0)) %>%
  write_parquet(paths$input$assessment$local)


## 6.3. Missing Strata ---------------------------------------------------------

# Condo buildings that don't have any recent sales will be missing strata.
# We use KNN to assign strata for those buildings based on longitude, latitude,
# year built, and number of livable building units.

# This step is now performed via the Tidymodels recipes package. See R/recipes.R

# Reminder to upload to DVC store
message(
  "Be sure to add updated input data to DVC and finalized data to git LFS!\n",
  "See https://dvc.org/doc/start/data-management/data-versioning ",
  "for more information"
)

# End the stage timer
tictoc::toc(log = FALSE)
