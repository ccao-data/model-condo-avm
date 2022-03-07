#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# 1. Setup ---------------------------------------------------------------------
#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Pre-allocate memory for java JDBC driver
options(java.parameters = "-Xmx10g")
Sys.setenv(R_INSTALL_STAGED = FALSE)

# Load R libraries
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
library(RJDBC)
library(s2)
library(sf)
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
  Schema = "Default"
)




#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# 2. Pull Data -----------------------------------------------------------------
#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Pull the training data, which contains actual sales + attached characteristics
# from the residential input view
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
AND NOT is_multisale
  ")
)
tictoc::toc()

# Pull all residential PIN input data for the assessment year. This will be the
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

# Pull site-specific (pre-determined) land values and neighborhood-level land
# rates per sqft, as calculated by Valuations
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
    time_sale_week = time_interval %/% weeks(1),
    
    # Get components of dates for fixed effects to correct seasonality
    time_sale_quarter_of_year = quarter(meta_sale_date),
    time_sale_week_of_year = week(meta_sale_date),
    
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
  # Clean up output, bind rows that were missing lat/lon, and write to file
  ungroup() %>%
  st_drop_geometry() %>%
  bind_rows(
    training_data_clean %>%
      filter(is.na(loc_x_3435) | is.na(loc_y_3435))
  ) %>%
  select(-c(nb, time_interval)) %>%
  write_parquet(paths$input$training$local)


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
    time_sale_day_of_year = day(meta_sale_date),
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
  # Clean up output and write to file
  ungroup() %>%
  st_drop_geometry() %>%
  bind_rows(
    assessment_data_clean %>%
      filter(is.na(loc_x_3435) | is.na(loc_y_3435))
  ) %>%
  select(-c(nb, meta_sale_price, time_interval)) %>%
  write_parquet(paths$input$assessment$local)

## 3.3. Land Rates -------------------------------------------------------------

# Write land data directly to file, since it's already mostly clean
land_nbhd_rate_data %>%
  select(meta_nbhd = town_nbhd, land_rate_per_sqft) %>%
  write_parquet(paths$input$land_nbhd_rate$local)

# Reminder to upload to DVC store
message(
  "Be sure to add updated input data to DVC and finalized data to git LFS!\n",
  "See https://dvc.org/doc/start/data-and-model-versioning for more information"
)
