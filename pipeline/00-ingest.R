#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# 1. Setup ---------------------------------------------------------------------
#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# NOTE: See DESCRIPTION for library dependencies and R/setup.R for
# variables used in each pipeline stage

# Start the stage timer
tictoc::tic.clearlog()
tictoc::tic("Ingest")

# Load libraries, helpers, and recipes from files
purrr::walk(list.files("R/", "\\.R$", full.names = TRUE), source)

# Load additional dev R libraries (see README#managing-r-dependencies)
suppressPackageStartupMessages({
  library(DBI)
  library(igraph)
  library(noctua)
})

# Adds arrow support to speed up ingest process
noctua_options(unload = TRUE)

# Establish Athena connection
AWS_ATHENA_CONN_NOCTUA <- dbConnect(
  noctua::athena(),
  rstudio_conn_tab = FALSE
)



#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# 2. Define Functions ----------------------------------------------------------
#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Ingest-specific helper functions for data cleaning, etc.

# Create a dictionary of column types, as specified in ccao::vars_dict
col_type_dict <- ccao::vars_dict %>%
  distinct(var_name = var_name_model, var_type = var_data_type) %>%
  drop_na(var_name)

# Mini-function to ensure that columns are the correct type
recode_column_type <- function(col, col_name, dictionary = col_type_dict) {
  col_type <- dictionary %>%
    filter(var_name == col_name) %>%
    pull(var_type)

  switch(col_type,
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
      ~ which.min(mapply(function(z, y) sum(z - y)^2, .x, .y))
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


# Mini function to deal with arrays
# Some Athena columns are stored as arrays but are converted to string on
# ingest. In such cases, we either keep the contents of the cell (if 1 unit),
# collapse the array into a comma-separated string (if more than 1 unit),
# or replace with NA if the array is empty
process_array_column <- function(x) {
  purrr::map_chr(x, function(cell) {
    if (length(cell) > 1) {
      paste(cell, collapse = ", ")
    } else if (length(cell) == 1) {
      as.character(cell) # Convert the single element to character
    } else {
      NA # Handle cases where the array is empty
    }
  })
}



#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# 3. Pull Data -----------------------------------------------------------------
#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
message("Pulling data from Athena")

# Pull the training data, which contains actual sales + attached characteristics
# from the condominium input view. We want to get sales spanning multiple
# parcels only for sales that sell with deeded parking spots
tictoc::tic("Training data pulled")
training_data <- dbGetQuery(
  conn = AWS_ATHENA_CONN_NOCTUA, glue("
  SELECT
      sale.sale_price AS meta_sale_price,
      sale.sale_date AS meta_sale_date,
      sale.doc_no AS meta_sale_document_num,
      sale.deed_type AS meta_sale_deed_type,
      sale.seller_name AS meta_sale_seller_name,
      sale.buyer_name AS meta_sale_buyer_name,
      sale.num_parcels_sale AS meta_sale_num_parcels,
      sale.sv_is_outlier,
      sale.sv_outlier_reason1,
      sale.sv_outlier_reason2,
      sale.sv_outlier_reason3,
      condo.*
  FROM model.vw_pin_condo_input condo
  INNER JOIN default.vw_pin_sale sale
      ON sale.pin = condo.meta_pin
      AND sale.year = condo.year
  WHERE condo.year
      BETWEEN '{params$input$min_sale_year}'
      AND '{params$input$max_sale_year}'
  AND sale.deed_type IN ('01', '02', '05')
  AND NOT sale.sale_filter_same_sale_within_365
  AND NOT sale.sale_filter_less_than_10k
  AND NOT sale.sale_filter_deed_type
  AND Year(sale.sale_date) >= {params$input$min_sale_year}
  AND sale.num_parcels_sale <= 2
  ")
)
tictoc::toc()

# Raw sales document number data used to identify some sales accidentally
# excluded from the original training runs. See
# https://github.com/ccao-data/data-architecture/pull/334 for more info
tictoc::tic("Sales data pulled")
sales_data <- dbGetQuery(
  conn = AWS_ATHENA_CONN_NOCTUA, glue("
  SELECT DISTINCT
      substr(saledt, 1, 4) AS year,
      instruno AS doc_no_old,
      NULLIF(REPLACE(instruno, 'D', ''), '') AS doc_no_new
  FROM iasworld.sales
  WHERE substr(saledt, 1, 4) >= '{params$input$min_sale_year}'
  ")
)
tictoc::toc()

# Pull all condo PIN input data for the assessment and prior year. We will only
# use the assessment year to run the model, but the prior year can be used for
# report generation
tictoc::tic("Assessment data pulled")
assessment_data <- dbGetQuery(
  conn = AWS_ATHENA_CONN_NOCTUA, glue("
  SELECT *
  FROM model.vw_pin_condo_input
  WHERE year IN (
    '{as.numeric(params$assessment$data_year) - 1}',
    '{params$assessment$data_year}'
  )
  ")
)
tictoc::toc()



##### START TEMPORARY FIX FOR MISSING DATA. REMOVE ONCE 2024 DATA IS AVAILABLE
library(data.table)
conflict_prefer_all("dplyr", "data.table", quiet = TRUE)
conflict_prefer_all("lubridate", "data.table", quiet = TRUE)
fill_cols <- assessment_data %>%
  select(
    starts_with("loc_"),
    starts_with("prox_"),
    starts_with("acs5_"),
    starts_with("other_"),
    starts_with("shp_")
  ) %>%
  names()
assessment_data_temp <- as.data.table(assessment_data) %>%
  mutate(across(starts_with("loc_tax_"), process_array_column))
assessment_data_temp_2024 <- assessment_data_temp[
  meta_year == "2024",
][
  assessment_data_temp[meta_year == "2023"],
  (fill_cols) := mget(paste0("i.", fill_cols)),
  on = .(meta_pin, meta_card_num)
]
assessment_data <- rbind(
  assessment_data_temp[meta_year != "2024"],
  assessment_data_temp_2024
) %>%
  as_tibble()

training_data_temp <- as.data.table(training_data) %>%
  mutate(across(starts_with("loc_tax_"), process_array_column))
training_data_temp_2024 <- training_data_temp[
  meta_year == "2024",
][
  assessment_data_temp[meta_year == "2023"],
  (fill_cols) := mget(paste0("i.", fill_cols)),
  on = .(meta_pin, meta_card_num)
]
training_data <- rbind(
  training_data_temp[meta_year != "2024"],
  training_data_temp_2024
) %>%
  as_tibble()
rm(
  assessment_data_temp, assessment_data_temp_2024,
  training_data_temp, training_data_temp_2024
)
##### END TEMPORARY FIX



units <- dbGetQuery(
  conn = AWS_ATHENA_CONN_NOCTUA, glue("select unitno, parid, adrno
 from iasworld.pardat
 where taxyr = '2025'
 and class = '299'"
  ))

assessment_data_working <- assessment_data %>%
  filter(year == '2023',
         meta_modeling_group == 'CONDO') %>%
  select(meta_pin10, char_building_units, meta_pin)

units_working <- assessment_data_working %>%
  left_join(units, by = c("meta_pin" = "parid")) %>%
  group_by(meta_pin10) %>%
  mutate(distinct_addresses = n_distinct(adrno)) %>%
  rename(pin = meta_pin,
         pin10 = meta_pin10) %>%
  filter(!is.na(unitno),
         distinct_addresses <= 2) %>%
  mutate(
    unitno_cleaned = gsub("[^A-Za-z0-9/-]", "", unitno), # Remove non-alphanumeric characters except `/` and `-`
    unitno_cleaned = sub("^[A-Za-z]+(?=[0-9])", "", unitno_cleaned, perl = TRUE), # Remove leading letters if followed by a number,
    unit_length = nchar(unitno_cleaned),
    numeric_unit_length = nchar(gsub("[A-Za-z]", "", unitno_cleaned)),
    part_before_dash = sub("-.*", "", unitno),

    # Remove the part before the `-` if it matches `adrno`
    unitno_cleaned = ifelse(
      part_before_dash == adrno,
      sub(".*?-", "", unitno_cleaned), # Remove everything before and including the first `-`
      unitno_cleaned # Otherwise, keep the original `unitno`
    ),
    # Always keep the part before `/` or `-`
    unitno_cleaned = sub("[/-].*", "", unitno_cleaned)
  ) %>%
  select(-part_before_dash) %>%
  group_by(pin10) %>%
  mutate(
    max_numeric_length = ifelse(char_building_units >= 2, max(nchar(gsub("[A-Za-z]", "", unitno_cleaned))), NA),
    min_numeric_length = ifelse(char_building_units >= 2, min(nchar(gsub("[A-Za-z]", "", unitno_cleaned))), NA),
    numeric_length_all_3 = ifelse(char_building_units >= 2, all(nchar(gsub("[A-Za-z]", "", unitno_cleaned)) == 3), NA)
  ) %>%
  ungroup() %>% # Ungroup to ensure calculations are complete
  mutate(
    is_ground = grepl("(?i)gr|ground|gf", unitno), # Check for "ground" (case-insensitive)
    is_basement = grepl("(?i)bsmt|basement!grdn", unitno), # Check for "bsmt" or "basement" (case-insensitive)
    floor = case_when(
      is_ground ~ "0", # Assign "0" for ground
      is_basement ~ "-1", # Assign "-1" for basement
      max_numeric_length == 3 & numeric_unit_length == 3 ~ substr(gsub("[^0-9]", "", unitno_cleaned), 1, 1),
      unitno_cleaned == "1" | unitno_cleaned == "A" | unitno_cleaned == "1A" ~ "1",
      grepl("^[0-9][A-Za-z]$", unitno_cleaned) ~ substr(unitno_cleaned, 1, 1), # Single digit followed by a letter
      grepl("^0[0-9][0-9]+$", unitno_cleaned) ~ substr(unitno_cleaned, 2, 2), # Leading 0, take the second digit
      grepl("^[1-9][0-9][A-Za-z]$", unitno_cleaned) ~ substr(unitno_cleaned, 1, 2), # Two digits followed by a letter, take the first two digits
      max_numeric_length == 4 & numeric_unit_length == 4 ~ substr(gsub("[^0-9]", "", unitno_cleaned), 1, 2),
      max_numeric_length == 4 & numeric_unit_length == 3 ~ substr(gsub("[^0-9]", "", unitno_cleaned), 1, 1),
      TRUE ~ NA_character_ # Default to NA for other cases
    ),
    reason = case_when(
      is_ground ~ "Unit is ground",
      is_basement ~ "Unit is 'basement",
      max_numeric_length == 3 & numeric_unit_length == 3 ~ "Numeric length is 3, using the first digit",
      unitno_cleaned == "1" | unitno_cleaned == "A" | unitno_cleaned == "1A" ~ "Default case for unitno_cleaned being 1, A, or 1A",
      grepl("^[0-9][A-Za-z]$", unitno_cleaned) ~ "Single digit followed by a letter",
      grepl("^0[0-9][0-9]+$", unitno_cleaned) ~ "Leading 0, using the second digit",
      grepl("^[1-9][0-9][A-Za-z]$", unitno_cleaned) ~ "Two digits followed by a letter, taking the first two digits",
      max_numeric_length == 4 & numeric_unit_length == 4 ~ "Numeric length is 4, using the first two digits",
      max_numeric_length == 4 & numeric_unit_length == 3 ~ "Numeric length is 3 out of 4, using the first digit",
      TRUE ~ "No matching condition"
    )
  ) %>%
  select(pin, floor)

assessment_data <- assessment_data %>%
  left_join(units_working, by = c("meta_pin" = "pin"))

assessment_data <- training_data %>%
  left_join(units_working, by = c("meta_pin" = "pin"))

# Save both years for report generation using the characteristics
assessment_data %>%
  write_parquet(paths$input$char$local)

# Save only the assessment year data to use for assessing values
assessment_data <- assessment_data %>%
  filter(year == params$assessment$data_year)

# Pull  neighborhood-level land rates per sqft, as calculated by Valuations
tictoc::tic("Land rate data pulled")
land_nbhd_rate_data <- dbGetQuery(
  conn = AWS_ATHENA_CONN_NOCTUA, glue("
  SELECT *
  FROM ccao.land_nbhd_rate
  WHERE year = '{params$assessment$year}'
  ")
)
tictoc::toc()

# Close connection to Athena
dbDisconnect(AWS_ATHENA_CONN_NOCTUA)
rm(AWS_ATHENA_CONN_NOCTUA)




#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# 4. Add Features and Clean ----------------------------------------------------
#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
message("Adding time features and cleaning")

## 4.1. Training Data ----------------------------------------------------------

# Heuristic for handling multi-PIN sales. We want to keep sales with a deeded
# parking spot, but only the sale for the unit, not the parking. Drop other
# multi-unit sale types since we don't have a way to disaggregate each
# unit's value
training_data_ms <- training_data %>%
  group_by(meta_sale_document_num) %>%
  arrange(meta_sale_document_num, meta_tieback_proration_rate) %>%
  mutate(
    # Attach sale to the condo UNIT if one of the PINs in the sale is a garage
    # and the unit % of ownership is greater than 3x the garage % of ownership.
    # The sum() call here ensures that one (and only one) PIN of the multi-sale
    # is a garage unit
    keep_unit_sale =
      meta_tieback_proration_rate >= (lag(meta_tieback_proration_rate) * 3) &
      sum(meta_cdu == "GR", na.rm = TRUE) == 1, # nolint
    # If there are multiple PINs associated with a sale, take only the
    # proportion of the sale value that is attributable to the main unit (based
    # on percentage of ownership)
    total_proration_rate = sum(meta_tieback_proration_rate, na.rm = TRUE),
    meta_sale_price = as.numeric(meta_sale_price),
    meta_sale_price = ifelse(
      n() == 2 & keep_unit_sale,
      meta_sale_price * (meta_tieback_proration_rate / total_proration_rate),
      meta_sale_price
    ),
    meta_sale_price = round(meta_sale_price, 0)
  ) %>%
  filter(n() == 1 | (n() == 2 & keep_unit_sale)) %>%
  ungroup() %>%
  filter(!as.logical(as.numeric(ind_pin_is_multilline))) %>%
  select(-keep_unit_sale, -total_proration_rate)

# Multi-sale outlier detection / sales validation kludge. The main sales
# validation logic cannot yet handle multi-sale properties, but they're a
# significant minority of the total sales sample. We can borrow some
# conservative thresholds from the main sales validation output to identify
# likely non-arms-length sales. ONLY APPLIES to multi-sale properties
training_data_fil <- training_data_ms %>%
  mutate(
    sv_outlier_reason1 = case_when(
      meta_sale_price < 50000 & meta_sale_num_parcels == 2 ~
        "Low price (multi)",
      meta_sale_price > 1700000 & meta_sale_num_parcels == 2 ~
        "High price (multi)",
      TRUE ~ sv_outlier_reason1
    ),
    sv_is_outlier = ifelse(
      (meta_sale_price < 50000 & meta_sale_num_parcels == 2) |
        (meta_sale_price > 1700000 & meta_sale_num_parcels == 2),
      TRUE,
      sv_is_outlier
    )
  )

# Clean up the training data. Goal is to get it into a publishable format.
# Final featurization, missingness, etc. is handled via Tidymodels recipes
training_data_clean <- training_data_fil %>%
  # Recode factor variables using the definitions stored in ccao::vars_dict
  # This will remove any categories not stored in the dictionary and convert
  # them to NA (useful since there are a lot of misrecorded variables)
  ccao::vars_recode(cols = starts_with("char_"), code_type = "code") %>%
  # Coerce columns to the data types recorded in the dictionary. Necessary
  # because the SQL drivers will often coerce types on pull (boolean becomes
  # character)
  mutate(across(
    any_of(col_type_dict$var_name),
    ~ recode_column_type(.x, cur_column())
  )) %>%
  mutate(
    # Treat sales for non-livable spaces as outliers. They are included for
    # reference only
    sv_is_outlier = ifelse(
      meta_modeling_group == "NONLIVABLE",
      TRUE,
      sv_is_outlier
    ),
    # Assign 'Non-livable area' to the first outlier reason and
    # set the other two outlier reason columns to NA
    sv_outlier_reason1 = ifelse(
      meta_modeling_group == "NONLIVABLE",
      "Non-livable area",
      sv_outlier_reason1
    ),
    sv_outlier_reason2 = ifelse(
      meta_modeling_group == "NONLIVABLE",
      NA_character_,
      sv_outlier_reason2
    ),
    sv_outlier_reason3 = ifelse(
      meta_modeling_group == "NONLIVABLE",
      NA_character_,
      sv_outlier_reason3
    )
  ) %>%
  # Only exclude explicit outliers from training. Sales with missing validation
  # outcomes will be considered non-outliers
  mutate(
    sv_is_outlier = replace_na(sv_is_outlier, FALSE)
  ) %>%
  # Some Athena columns are stored as arrays but are converted to string on
  # ingest. In such cases, take the first element and clean the string
  # Apply the helper function to process array columns
  mutate(
    across(starts_with("loc_tax_"), process_array_column),
    loc_tax_municipality_name =
      replace_na(loc_tax_municipality_name, "UNINCORPORATED")
  ) %>%
  mutate(
    # Miscellanous column-level cleanup
    ccao_is_corner_lot = replace_na(ccao_is_corner_lot, FALSE),
    ccao_is_active_exe_homeowner = replace_na(ccao_is_active_exe_homeowner, 0L),
    ccao_n_years_exe_homeowner = replace_na(ccao_n_years_exe_homeowner, 0L),
    across(where(is.character), \(x) na_if(x, "")),
    across(where(bit64::is.integer64), \(x) as.numeric(x))
  ) %>%
  # Create time/date features using lubridate
  mutate(
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
    time_sale_post_covid = meta_sale_date >= make_date(2020, 3, 15)
  ) %>%
  select(-any_of(c("time_interval"))) %>%
  relocate(starts_with("sv_"), .after = everything()) %>%
  relocate("year", .after = everything()) %>%
  relocate(
    (starts_with("meta_") & !meta_pin:meta_2yr_pri_board_tot),
    .after = meta_2yr_pri_board_tot
  ) %>%
  relocate(starts_with("ind_"), .after = starts_with("meta_")) %>%
  relocate(starts_with("char_"), .after = starts_with("ind_")) %>%
  filter(
    between(
      meta_sale_date,
      make_date(params$input$min_sale_year, 1, 1),
      make_date(params$input$max_sale_year, 12, 31)
    )
  ) %>%
  as_tibble()


## 4.2. Assessment Data --------------------------------------------------------

# Clean the assessment data. This is the target data that the trained model is
# used on. The cleaning steps are the same as above, with the exception of the
# time variables
assessment_data_clean <- assessment_data %>%
  ccao::vars_recode(cols = starts_with("char_"), code_type = "code") %>%
  # Apply the helper function to process array columns
  mutate(
    across(starts_with("loc_tax_"), process_array_column),
    loc_tax_municipality_name =
      replace_na(loc_tax_municipality_name, "UNINCORPORATED")
  ) %>%
  mutate(across(
    any_of(col_type_dict$var_name),
    ~ recode_column_type(.x, cur_column())
  )) %>%
  # Same Athena string cleaning and feature cleanup as the training data
  mutate(
    ccao_is_active_exe_homeowner = replace_na(ccao_is_active_exe_homeowner, 0L),
    ccao_n_years_exe_homeowner = replace_na(ccao_n_years_exe_homeowner, 0L),
    across(where(is.character), \(x) na_if(x, "")),
    across(where(bit64::is.integer64), \(x) as.numeric(x))
  ) %>%
  # Create sale date features BASED ON THE ASSESSMENT DATE. The model predicts
  # the sale price of properties on the date of assessment. Not the date of an
  # actual sale
  mutate(
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
  select(-any_of(c("time_interval"))) %>%
  relocate(starts_with("sv_"), .after = everything()) %>%
  relocate("year", .after = everything()) %>%
  relocate(
    (starts_with("meta_") & !meta_pin:meta_2yr_pri_board_tot),
    .after = meta_2yr_pri_board_tot
  ) %>%
  relocate(starts_with("ind_"), .after = starts_with("meta_")) %>%
  relocate(starts_with("char_"), .after = starts_with("ind_")) %>%
  as_tibble()


## 4.3. Land Rates -------------------------------------------------------------
message("Saving land rates")

# Write land data directly to file, since it's already mostly clean
land_nbhd_rate_data %>%
  select(meta_nbhd = town_nbhd, meta_class = class, land_rate_per_sqft) %>%
  write_parquet(paths$input$land_nbhd_rate$local)




#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# 5. Condo Strata --------------------------------------------------------------
#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
message("Calculating condo strata")

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


## 5.2. Assign Strata ----------------------------------------------------------

# Use strata models to create strata of building-level, previous-5-year sale
# prices. These strata are used as categorical variables in the model
bldg_strata <- bldg_5yr_sales_avg %>%
  left_join(bldg_strata_model, by = params$input$strata$group_var) %>%
  mutate(
    meta_strata_1 = switch(params$input$strata$type,
                           kmeans = val_assign_center(mean_log10_sale_price, meta_strata_model_1),
                           ntile = val_assign_ntile(mean_log10_sale_price, meta_strata_model_1)
    ),
    meta_strata_2 = switch(params$input$strata$type,
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
  relocate(
    c(starts_with("meta_strata"), meta_pin10_5yr_num_sale),
    .before = starts_with("ind_")
  ) %>%
  write_parquet(paths$input$training$local)

assessment_data_w_strata <- assessment_data_clean %>%
  left_join(bldg_strata, by = "meta_pin10") %>%
  mutate(meta_pin10_5yr_num_sale = replace_na(meta_pin10_5yr_num_sale, 0)) %>%
  relocate(
    c(starts_with("meta_strata"), meta_pin10_5yr_num_sale),
    .before = starts_with("ind_")
  ) %>%
  write_parquet(paths$input$assessment$local)


## 5.3. Missing Strata ---------------------------------------------------------

# Condo buildings that don't have any recent sales will be missing strata.
# We use KNN to assign strata for those buildings based on longitude, latitude,
# year built, and number of livable building units.

# This step is now performed via the Tidymodels recipes package. See R/recipes.R

# Reminder to upload to DVC store
message(
  "Be sure to add updated input data to DVC and finalized data to S3\n",
  "See https://dvc.org/doc/start/data-management/data-versioning ",
  "for more information"
)

# End the stage timer
tictoc::toc(log = FALSE)
