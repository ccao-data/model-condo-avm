#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# 1. Setup ---------------------------------------------------------------------
#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Pre-allocate memory for java JDBC driver
options(java.parameters = "-Xmx10g")

# Load R libraries
library(arrow)
library(ccao)
library(DBI)
library(data.table)
library(dplyr)
library(factoextra)
library(fastDummies)
library(glue)
library(here)
library(igraph)
library(lubridate)
library(purrr)
library(RJDBC)
library(s2)
library(sf)
library(tibble)
library(tictoc)
library(tidyr)
library(yaml)
source(here("R", "helpers.R"))

# Initialize a dictionary of file paths. See R/file_dict.csv for details
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

# Pull the training data, which contains actual sales + attached characteristics
# from the condominium input view
tictoc::tic()
training_data <- dbGetQuery(
  conn = AWS_ATHENA_CONN_JDBC, glue("
  SELECT
      sale.sale_price AS meta_sale_price,
      sale.sale_date AS meta_sale_date,
      sale.doc_no AS meta_sale_document_num,
      condo.*
  FROM model.vw_pin_condo_input condo
  INNER JOIN default.vw_pin_sale sale
      ON sale.pin = condo.meta_pin
      AND sale.year = condo.meta_year
  WHERE (condo.meta_year 
      BETWEEN '{params$input$min_sale_year}' 
      AND '{params$input$max_sale_year}')
  AND ((sale.sale_price_log10
      BETWEEN sale.sale_filter_lower_limit
      AND sale.sale_filter_upper_limit)
      AND sale.sale_filter_count >= 10)
  ")
)
tictoc::toc()

# Pull all condo input data for the assessment year. This will be the
# data we actually run the model on
tictoc::tic()
assessment_data <- dbGetQuery(
  conn = AWS_ATHENA_CONN_JDBC, glue("
  SELECT *
  FROM model.vw_pin_condo_input
  WHERE meta_year = '{params$assessment$data_year}'
  ")
)
tictoc::toc()

# Pull  neighborhood-level land rates per sqft, as calculated by Valuations
tictoc::tic()
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


# Normalize data so that knn isn't biased by scale of any dimension (i.e. one
# dimension having a range of 1000 vs another having a range of 100). This can
# create issues since knn depends on the distance formula
normalize <- function(x, min = 0, max = 1) {
  output <- (x - min(x, na.rm = T)) /
    (max(x, na.rm = T) - min(x, na.rm = T)) *
    (max - min) + min
  
  return(output)
}


# Find the K-nearest neighbors within a group to calculate a spatial lag
st_knn <- function(x, y = NULL, k = 1) {
  s2x <- sf::st_as_s2(x)
  if (is.null(y)) {
    z <- s2::s2_closest_edges(s2x, s2x, k = (k + 1))
    # Drop the starting observation/point
    z <- lapply(z, sort)
    z <- lapply(seq_along(z), function(i) setdiff(z[[i]], i))
    return(z)
  } else {
    s2y <- sf::st_as_s2(y)
    z <- s2::s2_closest_edges(s2x, s2y, k = k)
  }
}




#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# 4. Add Features and Clean ----------------------------------------------------
#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

## 4.1. Training Data ----------------------------------------------------------

# Clean up the training data. Goal is to get it into a publishable format.
# Final featurization, filling, etc. is handled via Tidymodels recipes
training_data_clean <- training_data %>%
  # Recode factor variables using the definitions stored in ccao::vars_dict
  # This will remove any categories not stored in the dictionary and convert
  # them to NA (useful since there are a lot of misrecorded variables)
  ccao::vars_recode(cols = starts_with("char_"), type = "code") %>%
  # Coerce columns to the data types recorded in the dictionary. Necessary
  # because the SQL drivers will often coerce types on pull (boolean becomes
  # character)
  mutate(across(everything(), ~ recode_column_type(.x, cur_column()))) %>%
  # Create sale date features using lubridate
  dplyr::mutate(
    # Calculate interval periods and times since Jan 01, 1997
    time_interval = interval(ymd("1997-01-01"), ymd(.data$meta_sale_date)),
    time_sale_year = year(meta_sale_date),
    time_sale_day = time_interval %/% days(1),
    # Get components of dates for fixed effects to correct seasonality
    time_sale_quarter_of_year = paste0("Q", quarter(meta_sale_date)),
    time_sale_day_of_year = yday(meta_sale_date),
    # Time window to use for cross-validation and calculating spatial lags
    time_split = time_interval %/% months(params$input$time_split)
  )

# Calculate KNN spatial lags for each N month period used in CV. The N month
# partitioning ensures that no training data leaks into the validation set
# during CV
training_data_lagged <- training_data_clean %>%
  # Convert coords to geometry used to calculate weights
  filter(!is.na(loc_longitude), !is.na(loc_latitude)) %>%
  st_as_sf(
    coords = c("loc_longitude", "loc_latitude"),
    crs = 4326,
    remove = FALSE
  ) %>%
  # Divide training data into N-month periods and calculate spatial lags
  # for each period
  group_by(time_split) %>%
  mutate(
    nb = st_knn(geometry, k = params$input$spatial_lag$k),
    across(
      all_of(params$input$spatial_lag$predictor),
      function(x) purrr::map_dbl(nb, function(idx, var = x) mean(var[idx])),
      .names = "lag_{.col}"
    )
  ) %>%
  # Clean up output, bind rows that were missing lat/lon
  ungroup() %>%
  st_drop_geometry() %>%
  bind_rows(
    training_data_clean %>%
      filter(is.na(loc_x_3435) | is.na(loc_y_3435))
  ) %>%
  select(-c(nb, time_interval))


## 4.2. Assessment Data --------------------------------------------------------

# Clean the assessment data. This the target data that the trained model is 
# used on. The cleaning steps are the same as above, with the exception of the
# time vars and identifying complexes
assessment_data_clean <- assessment_data %>%
  ccao::vars_recode(cols = starts_with("char_"), type = "code") %>%
  mutate(across(everything(), ~ recode_column_type(.x, cur_column()))) %>%
  # Create sale date features BASED ON THE ASSESSMENT DATE. The model predicts
  # the sale price of properties on the date of assessment. Not the date of an
  # actual sale
  dplyr::mutate(
    meta_sale_date = as_date(params$assessment$date),
    time_interval = interval(ymd("1997-01-01"), ymd(.data$meta_sale_date)),
    time_sale_year = year(meta_sale_date),
    time_sale_day = time_interval %/% days(1),
    time_sale_quarter_of_year = paste0("Q", quarter(meta_sale_date)),
    time_sale_day_of_year = yday(meta_sale_date),
    time_split = time_interval %/% months(params$input$time_split)
  )

# Grab the most recent sale within the last 3 split periods for each PIN. This
# will be the search space for the spatial lag for assessment data
sales_data <- training_data_clean %>%
  filter(
    meta_sale_date >= as_date(params$assessment$date) -
      months(params$input$time_split * 3),
    !is.na(loc_longitude)
  ) %>%
  group_by(meta_pin) %>%
  filter(max(meta_sale_date) == meta_sale_date) %>%
  ungroup() %>%
  st_as_sf(
    coords = c("loc_longitude", "loc_latitude"),
    crs = 4326,
    remove = TRUE
  ) %>%
  select(
    meta_pin, meta_year, meta_sale_price, 
    all_of(params$input$spatial_lag$predictor),
  )

# Join the sales data to the assessment data. Create lagged spatial predictors
# using the K-nearest neighboring sales. This replicates a spatial Durbin model
# with a time component
assessment_data_lagged <- assessment_data_clean %>%
  filter(!is.na(loc_longitude), !is.na(loc_latitude)) %>%
  st_as_sf(
    coords = c("loc_longitude", "loc_latitude"),
    crs = 4326,
    remove = FALSE
  ) %>%
  mutate(
    nb = st_knn(geometry, sales_data$geometry, k = params$input$spatial_lag$k),
    meta_sale_price = NA_real_,
    across(
      all_of(params$input$spatial_lag$predictor),
      function(x) purrr::map_dbl(nb, function(idx, var = dplyr::cur_column()) {
        mean(sales_data[[var]][idx])
      }),
      .names = "lag_{.col}"
    )
  ) %>%
  # Clean up output
  ungroup() %>%
  st_drop_geometry() %>%
  bind_rows(
    assessment_data_clean %>%
      filter(is.na(loc_x_3435) | is.na(loc_y_3435))
  ) %>%
  select(-c(nb, meta_sale_price, time_interval))


## 4.3. Land Rates -------------------------------------------------------------

# Write land data directly to file, since it's already mostly clean
land_nbhd_rate_data %>%
  select(meta_nbhd = town_nbhd, land_rate_per_sqft) %>%
  write_parquet(paths$input$land_nbhd_rate$local)




#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# 5. Condo Strata --------------------------------------------------------------
#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

## 5.1. Calculate Strata -------------------------------------------------------

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
bldg_5yr_sales_avg <- training_data_lagged %>%
  filter(
    meta_sale_date > make_date(as.numeric(params$input$max_sale_year) - 4)
  ) %>%
  select(
    meta_pin10, meta_sale_price, meta_sale_date,
    all_of(params$input$strata$group_var)
  ) %>%
  mutate(
    meta_sale_date_norm = normalize(
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
    )
  ) %>%
  ungroup()

# Use either k-means clustering or simple quantiles to construct a condominium
# building strata model. This model can be used to assign strata to buildings
if (params$input$strata$type == "kmeans") {
  
  # Set seed for k-means reproducibility
  set.seed(params$input$strata$seed)

  # For k-means, construct strata as a 1-dimensional cluster of the average
  # sale price of the building
  bldg_strata <- bldg_5yr_sales_avg %>%
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
  
} else {
  
  # Construct strata as quantile bins of the average sale price of the building
  bldg_strata <- bldg_5yr_sales_avg %>%
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


## 5.2. Assign Strata ----------------------------------------------------------

# Use strata models to create strata of building-level, previous-5-year sale
# prices. These strata are used as categorical variables in the model

# First, attach the 5-year weighted average sale price for each building to the
# training and assessment data, then attach the strata models themselves,
# finally use the assignment functions to assign each property. For k-means,
# assign to the nearest center, for quantiles, assign within bucket
training_data_w_strata <- training_data_lagged %>%
  left_join(
    bldg_5yr_sales_avg %>% select(-all_of(params$input$strata$group_var)),
    by = "meta_pin10"
  ) %>%
  left_join(bldg_strata, by = params$input$strata$group_var) %>%
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
  )

# Do the same for the assessment data. There will be some data leakage here, but
# it's nearly unavoidable (see README for details)
assessment_data_w_strata <- assessment_data_lagged %>%
  left_join(
    bldg_5yr_sales_avg %>% select(-all_of(params$input$strata$group_var)),
    by = "meta_pin10"
  ) %>%
  left_join(bldg_strata, by = params$input$strata$group_var) %>%
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
  )
      

## 5.3. Missing Strata ---------------------------------------------------------

# Next we address condo buildings that don't have any recent sales are are thus
# missing strata. We'll use KNN to assign strata for those buildings based on
# longitude, latitude, year built, and number of livable building units.

# Create training and test sets (training set will be buildings not missing
# strata, test set will be those that are).
strata_columns <- grep("strata", names(training_data_lagged), value = TRUE)

strata_normal <- assessment_data_lagged %>%
  
  # There is ONE 2021 299 that exists in iasworld.pardat but niether
  # iasworld.oby nor iasworld.comdat. Why? We may never know. We need to make
  # sure this PIN with missing characteristics doesn't ruin KNN for the other
  # PINs.
  filter(
    if_all(
      c(loc_longitude, loc_latitude, char_yrblt, char_building_units),
      ~ !is.na(.)
    )
  ) %>%
  
  select(
    meta_pin10, contains("strata"),
    loc_longitude, loc_latitude,
    char_yrblt, char_building_units
  ) %>%
  distinct() %>%
  
  # Apply the normalization function we created earlier across all numeric
  # dimension of the data
  mutate(across(where(is.numeric), normalize))

strata_train <- strata_normal %>% filter(!is.na(meta_strata_10))
strata_test  <- anti_join(strata_normal, strata_train, by = 'meta_pin10')

# Apply knn function from class package
for (i in strata_columns) {
  
  strata_test[, i] <- class::knn(
    train = strata_train %>% select(-c(meta_pin10, contains("strata"))),
    test = strata_test %>% select(-c(meta_pin10, contains("strata"))),
    cl = strata_train %>% pull(i),
    k = params$input$strata_k
  )
  
}

# Fill in missing strata in assessment data using KNN generated strata
assessment_data_lagged %>%
  left_join(
    strata_test %>%
      select(meta_pin10, contains("strata")),
    by = "meta_pin10"
  ) %>%
  mutate(
    !!paste0(strata_columns[1]) := coalesce(
      (!!as.name(paste0(strata_columns[1], ".x"))),
      (!!as.name(paste0(strata_columns[1], ".y")))
    ),
    !!paste0(strata_columns[2]) := coalesce(
      (!!as.name(paste0(strata_columns[2], ".x"))),
      (!!as.name(paste0(strata_columns[2], ".y")))
    )
  ) %>%
  select(-ends_with(c(".x", ".y"))) %>%
  # Write to file
  write_parquet(paths$input$assessment$local)


# Reminder to upload to DVC store
message(
  "Be sure to add updated input data to DVC and finalized data to git LFS!\n",
  "See https://dvc.org/doc/start/data-and-model-versioning for more information"
)