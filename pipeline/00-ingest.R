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
  library(data.table)
  library(DBI)
  library(igraph)
  library(noctua)
})

# Load data.table without breaking everything else
conflict_prefer_all("dplyr", "data.table", quiet = TRUE)
conflict_prefer_all("lubridate", "data.table", quiet = TRUE)

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
  WHERE CAST(condo.year AS int)
      BETWEEN CAST({params$input$min_sale_year} AS int) -
        {params$input$n_years_prior}
      AND CAST({params$input$max_sale_year} AS int)
  AND sale.deed_type IN ('01', '02', '05')
  AND NOT sale.sale_filter_same_sale_within_365
  AND NOT sale.sale_filter_less_than_10k
  AND NOT sale.sale_filter_deed_type
  AND sale.num_parcels_sale <= 2
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
  mutate(sv_is_outlier = replace_na(sv_is_outlier, FALSE)) %>%
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
  relocate(starts_with("char_"), .after = starts_with("ind_"))



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
# 5. Building Means ------------------------------------------------------------
#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
message("Calculating building rolling means")

# Condominiums' unit characteristics (such as square footage, # of bedrooms,
# etc.) are not tracked well by the CCAO. As such, we need to rely on other
# information to determine the value of unsold condos. Fortunately, condos are
# more homogeneous than single-family homes and are pre-grouped into like units
# (buildings)

# We can use the historic sale price of other sold units in the same
# building to determine an unsold condo's value. To do so, we construct a
# time-weighted, leave-one-out, rolling mean of sale prices for each building.
# In other words, we get the average of sales in the building in the past
# N years, excluding the sale we're trying to predict.

## 5.1. Construct Rolling Means ------------------------------------------------

# This offset is the size of the rolling window
offset <- years(params$input$n_years_prior)

# Mush together the training and assessment data and sort by PIN and sale date.
# Note that "sales" from the assessment data (i.e. the constructed sale on the
# lien date) will always be the last sale in the building, since they occur
# after all the training data sales. We exploit this property to also calculate
# the rolling means for the assessment data by simply taking the N year rolling
# average of sales prior to the lien date
bldg_rolling_means_dt <- training_data_clean %>%
  mutate(data_source = "training") %>%
  select(
    meta_pin10, meta_pin, meta_tieback_proration_rate,
    meta_sale_date, meta_sale_price, meta_sale_document_num, sv_is_outlier,
    meta_modeling_group, data_source
  ) %>%
  bind_rows(
    assessment_data_clean %>%
      mutate(data_source = "assessment") %>%
      select(
        meta_pin10, meta_pin, meta_tieback_proration_rate,
        meta_sale_date,
        meta_modeling_group, data_source
      )
  ) %>%
  as.data.table() %>%
  setkey(meta_pin10, meta_sale_date)

# Construct the time-weighted, leave-one-out rolling mean of building sale
# prices. We use data.table here since it's MUCH faster than dplyr for this
# task. The code here is a bit dense. View the output dataframe for debugging
# (it helps a lot).
bldg_rolling_means_dt[
  ,
  # Create initial time weights for the range of sales across the whole training
  # date range. This is a logistic curve centered 3 years before the lien
  # date and bounded between the min and max weights. The parameters here were
  # discovered with some rough grid search
  sale_wt := params$input$building$weight_max / (
    params$input$building$weight_max +
      exp(
        -(0.002 * as.integer(meta_sale_date - (max(meta_sale_date) - years(3))))
      )
  ) * (1 - params$input$building$weight_min) + params$input$building$weight_min
][
  ,
  # Scale weights so the max weight is 1
  sale_wt := sale_wt / max(sale_wt)
][
  # Calculate the adaptive rolling window size using some tricky interval logic.
  # To demo what's actually going on here with `findInterval()`:
  #
  # Given the following sales Y:
  # 2015-12-01 2018-01-01 2022-06-15 2025-01-01
  #
  # And their 5-year offset X:
  # 2010-12-01 2013-01-01 2017-06-15 2020-01-01
  #
  # For each element of X, find the _index position_ of the breaks in Y that
  # contains that element e.g. for the first element of X:
  #  2015-12-01 2018-01-01 2022-06-15 2025-01-01
  # └── 2010-12-01 is outside any of the cuts, so the index is 0
  #
  # Or for the fourth element of X:
  # 2015-12-01 2018-01-01 2022-06-15 2025-01-01
  #                      └── 2020-01-01 is between these two, so the index is 2
  #
  # Using this technique, we can determine how many index positions we need to
  # move before the offset current date (sale date - N years) finds prior sales
  # that are within the N year time window. We then subtract that number of
  # index positions from the window size, effectively shrinking the front of the
  # window by N positions and excluding sales outside the N year window.
  #
  # In the case of the 4th element of Y, we end up with a window size of
  # 4 - 2 == 2. This means that our rolling mean will include the last two sales
  # in Y, the sales at positions 3 and 4. Since position 4 is the target sale,
  # we will avoid data leakage later on in the pipeline by subtracting the
  # target sale price from the mean (or subtracting 0 in the case of the
  # assessment set, which does not have a sale price).
  !sv_is_outlier & meta_modeling_group == "CONDO" | data_source == "assessment",
  `:=`(
    index_in_group = seq_len(.N),
    shrink_win_by_n_positions = findInterval(
      meta_sale_date %m-% offset, meta_sale_date
    )
  ),
  by = .(meta_pin10)
][
  # This is the size of the rolling window relative to EACH sale i.e. for any
  # given sale, how many index positions back do we need to go to get only sales
  # from the past N years
  !sv_is_outlier & meta_modeling_group == "CONDO" | data_source == "assessment",
  window_size := index_in_group - shrink_win_by_n_positions,
  by = .(meta_pin10)
][
  !sv_is_outlier & meta_modeling_group == "CONDO" | data_source == "assessment",
  # Calculate the numerator and denominator of the weighted rolling mean, but
  # EXCLUDE the current sale. This is the leave-one-out part. Note that we need
  # to replace NA values with 0 in the denominator for cases where there's no
  # current sale but we still want to create a mean of prior sales e.g. for
  # the assessment data
  `:=`(
    cnt = data.table::frollsum(
      as.numeric(!is.na(meta_sale_price)),
      n = window_size,
      align = "right",
      adaptive = TRUE,
      na.rm = TRUE,
      hasNA = TRUE
    ) - as.numeric(!is.na(meta_sale_price)),
    wtd_valsum = data.table::frollsum(
      meta_sale_price * sale_wt,
      n = window_size,
      align = "right",
      adaptive = TRUE,
      na.rm = TRUE,
      hasNA = TRUE
    ) - replace(meta_sale_price, is.na(meta_sale_price), 0) * sale_wt,
    wtd_cnt = data.table::frollsum(
      as.numeric(!is.na(meta_sale_price)) * sale_wt,
      n = window_size,
      align = "right",
      adaptive = TRUE,
      na.rm = TRUE,
      hasNA = TRUE
    ) - as.numeric(!is.na(meta_sale_price)) * sale_wt
  ),
  by = .(meta_pin10)
][, wtd_mean := wtd_valsum / wtd_cnt][
  ,
  `:=`(
    wtd_mean =
      fifelse(is.nan(wtd_mean) | is.infinite(wtd_mean), NA_real_, wtd_mean),
    cnt = fifelse(is.nan(cnt) | is.infinite(cnt), NA_real_, cnt)
  )
]


## 5.2. Re-attach to Original Data ---------------------------------------------

# Extract the constructed building means from the dedicated dataframe and
# re-attach them to their respective datasets. Note that some PINs will
# not have a mean (no sales in the building or no sales in the window). These
# missing values get imputed during the training stage
training_data_clean <- training_data_clean %>%
  left_join(
    bldg_rolling_means_dt %>%
      filter(data_source == "training") %>%
      select(
        meta_pin10, meta_sale_document_num,
        meta_pin10_bldg_roll_mean = wtd_mean,
        meta_pin10_bldg_roll_count = cnt
      ),
    by = c("meta_pin10", "meta_sale_document_num")
  ) %>%
  mutate(
    # Also construct a "percentage of units sold in the building" feature
    meta_pin10_bldg_roll_pct_sold =
      meta_pin10_bldg_roll_count / char_building_units,
    meta_pin10_bldg_roll_pct_sold = ifelse(
      is.na(meta_pin10_bldg_roll_pct_sold) |
        is.nan(meta_pin10_bldg_roll_pct_sold) |
        is.infinite(meta_pin10_bldg_roll_pct_sold),
      NA_real_,
      meta_pin10_bldg_roll_pct_sold
    )
  ) %>%
  filter(
    between(
      meta_sale_date,
      make_date(params$input$min_sale_year, 1, 1),
      make_date(params$input$max_sale_year, 12, 31)
    )
  ) %>%
  as_tibble() %>%
  write_parquet(paths$input$training$local)

assessment_data_clean <- assessment_data_clean %>%
  left_join(
    bldg_rolling_means_dt %>%
      filter(data_source == "assessment") %>%
      select(
        meta_pin,
        meta_pin10_bldg_roll_mean = wtd_mean,
        meta_pin10_bldg_roll_count = cnt
      ),
    by = c("meta_pin")
  ) %>%
  mutate(
    meta_pin10_bldg_roll_pct_sold =
      meta_pin10_bldg_roll_count / char_building_units,
    meta_pin10_bldg_roll_pct_sold = ifelse(
      is.na(meta_pin10_bldg_roll_pct_sold) |
        is.nan(meta_pin10_bldg_roll_pct_sold) |
        is.infinite(meta_pin10_bldg_roll_pct_sold),
      NA_real_,
      meta_pin10_bldg_roll_pct_sold
    )
  ) %>%
  as_tibble() %>%
  write_parquet(paths$input$assessment$local)

# Throw errors if any of the constructed mean features are negative
if (any(training_data_clean$meta_pin10_bldg_roll_mean < 0)) {
  stop("Negative building rolling mean detected in training data")
} else if (any(assessment_data_clean$meta_pin10_bldg_roll_mean < 0)) {
  stop("Negative building rolling mean detected in assessment data")
}

# Reminder to upload to DVC store
message(
  "Be sure to add updated input data to DVC and finalized data to S3\n",
  "See https://dvc.org/doc/start/data-management/data-versioning ",
  "for more information"
)

# End the stage timer
tictoc::toc(log = FALSE)
