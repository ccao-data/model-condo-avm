#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# 1. Setup ---------------------------------------------------------------------
#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Start the stage timer and clear logs from prior stage
tictoc::tic.clearlog()
tictoc::tic("Assess")

# Load libraries and scripts
options(dplyr.summarise.inform = FALSE)
library(arrow)
library(assessr)
library(ccao)
library(dplyr)
library(here)
library(lightsnip)
library(purrr)
library(recipes)
library(stringr)
library(tictoc)
library(tidyr)
library(yaml)

# Load helpers and recipes from files
walk(list.files("R/", "\\.R$", full.names = TRUE), source)

# Initialize a dictionary of file paths. See R/file_dict.csv for details
paths <- model_file_dict()

# Load the parameters file containing the run settings
params <- read_yaml("params.yaml")

# Columns to use for ratio study comparison (by prefix)
rsf_prefix <- gsub("_tot", "", params$ratio_study$far_column)
rsn_prefix <- gsub("_tot", "", params$ratio_study$near_column)

# Load the training data to use as a source of sales. These will be attached to
# PIN-level output (for comparison) and used as the basis for a sales ratio
# analysis on the assessment data
sales_data <- read_parquet(paths$input$training$local)




#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# 2. Predict Values ------------------------------------------------------------
#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Load the final lightgbm model object and recipe from file
lgbm_final_full_fit <- lightsnip::lgbm_load(paths$output$workflow_fit$local)
lgbm_final_full_recipe <- readRDS(paths$output$workflow_recipe$local)

# Load the data for assessment. This is the universe of condo units 
# that need values. Use the trained lightgbm model to estimate a single
# fair-market value for each unit
assessment_data_pred <- read_parquet(paths$input$assessment$local) %>%
  as_tibble() %>%
  mutate(
    pred_card_initial_fmv = predict(
      lgbm_final_full_fit,
      new_data = bake(
        lgbm_final_full_recipe,
        new_data = .,
        all_predictors()
      )
    )$.pred
  )




#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# 3. Post-Modeling Adjustments -------------------------------------------------
#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

## 3.1. Value Non-Livable Units ------------------------------------------------

# Many 14-digit PINs are non-livable units such as parking spaces, common
# areas, or storage areas. These units are valued by the model but tend to be
# overvalued (since most of a unit's value comes from location). As a result,
# these units are set to a fixed fair market value
assessment_data_nl <- assessment_data_pred %>%
  mutate(
    pred_pin_final_fmv = case_when(
      meta_modeling_group == "NONLIVABLE" &
        (meta_mailed_tot * 10) <= params$pv$nonlivable_threshold ~
        meta_mailed_tot * 10,
      meta_modeling_group == "NONLIVABLE" &
        (meta_mailed_tot * 10) > params$pv$nonlivable_threshold~
        as.numeric(params$pv$nonlivable_fixed_fmv),
      TRUE ~ pred_card_initial_fmv
    )
  )


## 3.2. Prorate/Round ----------------------------------------------------------

# Round PIN-level predictions using the breaks and amounts specified in params
assessment_data_final <- assessment_data_nl %>%
  mutate(
    pred_pin_final_fmv_round = ccao::val_round_fmv(
      pred_pin_final_fmv,
      breaks = params$pv$round_break,
      round_to = params$pv$round_to_nearest,
      type = params$pv$round_type
    )
  ) 

# Merge the finalized unit-level data back to the main tibble of predictions
assessment_data_merged <- assessment_data_pred %>%
  left_join(
    assessment_data_final %>%
      select(
        meta_pin, meta_card_num, 
        pred_pin_final_fmv, pred_pin_final_fmv_round
      ),
    by = c("meta_pin", "meta_card_num")
  ) %>%
  mutate(
    township_code = meta_township_code,
    meta_year = as.character(meta_year)
  )




#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# 4. Card-Level Data -----------------------------------------------------------
#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Keep only card-level variables of interest, including: ID variables (run_id,
# pin, card), characteristics, and predictions. For condos card and PIN are the
# same
assessment_data_merged %>%
  select(
    meta_year, meta_pin, meta_class, meta_card_num, meta_modeling_group,
    ends_with("_num_sale"), pred_card_initial_fmv,
    all_of(params$model$predictor$all), township_code
  ) %>%
  ccao::vars_recode(
    starts_with("char_"),
    type = "long",
    as_factor = FALSE
  ) %>%
  write_parquet(paths$output$assessment_card$local)




#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# 5. PIN-Level Data ------------------------------------------------------------
#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Generate PIN-level stats for each run. These are used for desktop review,
# looking at YoY changes, comparing to sales, etc.

## 5.1. Load Sales/Land --------------------------------------------------------

# Load the MOST RECENT sale per PIN from the year prior to the assessment year.
# These are the sales that will be used for ratio studies in the evaluate stage.
# We want our assessed value to be as close as possible to this sale
sales_data_ratio_study <- sales_data %>%
  filter(meta_year == params$assessment$data_year) %>%
  group_by(meta_pin) %>%
  filter(meta_sale_date == max(meta_sale_date)) %>%
  distinct(
    meta_pin, meta_year,
    sale_ratio_study_price = meta_sale_price,
    sale_ratio_study_date = meta_sale_date,
    sale_ratio_study_document_num = meta_sale_document_num
  ) %>%
  ungroup()

# Keep the two most recent sales for each PIN from any year. These are just for
# review, not for ratio studies
sales_data_two_most_recent <- sales_data %>%
  group_by(meta_pin) %>%
  slice_max(meta_sale_date, n = 2) %>%
  distinct(
    meta_pin, meta_year,
    meta_sale_price, meta_sale_date, meta_sale_document_num
  ) %>%
  mutate(mr = paste0("sale_recent_", row_number())) %>%
  tidyr::pivot_wider(
    id_cols = meta_pin,
    names_from = mr,
    values_from = c(meta_sale_date, meta_sale_price, meta_sale_document_num),
    names_glue = "{mr}_{gsub('meta_sale_', '', .value)}"
  ) %>%
  select(meta_pin, contains("1"), contains("2")) %>%
  ungroup()

# Get the % change in the average sale price within building between the last
# assessment year and this year
sales_data_bldg_change_pct <- sales_data %>%
  filter(
    meta_year == params$assessment$data_year |
    meta_year == params$ratio_study$far_year
  ) %>%
  group_by(meta_year, meta_pin10) %>%
  filter(n() >= 5) %>%
  mutate(meta_year = ifelse(
    meta_year == params$assessment$data_year, "cur", "pri")
  ) %>%
  summarize(mean_sale_price = mean(meta_sale_price)) %>%
  pivot_wider(names_from = meta_year, values_from = mean_sale_price) %>%
  mutate(flag_prior_far_yoy_bldg_change_pct = (cur - pri) / pri) %>%
  select(meta_pin10, flag_prior_far_yoy_bldg_change_pct)

# Load land rates from file
land_nbhd_rate <- read_parquet(paths$input$land_nbhd_rate$local)


## 5.2. Keep PIN Level --------------------------------------------------

# Clean PIN level data, keeping only columns in common with the SF/MF model
assessment_data_pin <- assessment_data_merged %>%
  rename_with(
    .fn = ~ gsub(paste0(rsn_prefix, "_"), "prior_near_", .x),
    .cols = starts_with(rsn_prefix)
  ) %>%
  rename_with(
    .fn = ~ gsub(paste0(rsf_prefix, "_"), "prior_far_", .x),
    .cols = starts_with(rsf_prefix)
  ) %>%
  select(
    # Keep ID and meta variables
    meta_year, meta_pin, meta_pin10, meta_triad_code, meta_township_code,
    meta_nbhd_code, meta_tax_code, meta_class, meta_tieback_key_pin,
    meta_tieback_proration_rate, meta_cdu, meta_modeling_group,
    meta_pin_num_landlines, char_yrblt,
    
    # Keep overall building square footage
    char_total_bldg_sf = char_building_sf,
    char_unit_sf, char_land_sf,

    # Keep locations, prior year values, and indicators
    loc_longitude, loc_latitude,
    starts_with(c(
      "loc_property_", "loc_cook_", "loc_chicago_",
      "loc_census", "loc_school_", "prior_", "ind_"
    )),
    meta_pin10_5yr_num_sale,

    # Keep PIN-level predicted values
    pred_pin_final_fmv, pred_pin_final_fmv_round, township_code
  ) %>%
  ungroup()


## 5.3. Value Land -------------------------------------------------------------

# Attach land and sales data to the PIN-level data, then calculate land and
# building values for each PIN
assessment_data_pin_2 <- assessment_data_pin %>%
  left_join(land_nbhd_rate, by = c("meta_nbhd_code" = "meta_nbhd")) %>%
  left_join(sales_data_two_most_recent, by = "meta_pin") %>%
  left_join(sales_data_ratio_study, by = c("meta_year", "meta_pin")) %>%
  left_join(sales_data_bldg_change_pct, by = "meta_pin10") %>%
  # Land values are provided by Valuations and are capped at a percentage of the
  # total FMV for the PIN. For condos, all units in a building share the same
  # land square footage, but the value is assigned by the % of ownership
  mutate(
    pred_pin_final_fmv_land = case_when(
      char_land_sf * land_rate_per_sqft * meta_tieback_proration_rate >=
        pred_pin_final_fmv_round * params$pv$land_pct_of_total_cap ~
      pred_pin_final_fmv_round * params$pv$land_pct_of_total_cap,
      TRUE ~ char_land_sf * land_rate_per_sqft * meta_tieback_proration_rate
    ),
    pred_pin_uncapped_fmv_land =
      char_land_sf * land_rate_per_sqft * meta_tieback_proration_rate,
    pred_pin_final_fmv_bldg =
      pred_pin_final_fmv_round - pred_pin_final_fmv_land
  ) %>%
  # Calculate effective rates (rate with 50% cap) + the % of the PIN value
  # dedicated to the building
  mutate(
    pred_pin_land_rate_effective = pred_pin_final_fmv_land /
      (char_land_sf * meta_tieback_proration_rate),
    pred_pin_land_pct_total = pred_pin_final_fmv_land / pred_pin_final_fmv_round
  ) %>%
  # Convert prior values to FMV from AV, then calculate year-over-year
  # percent and nominal changes
  mutate(
    across(starts_with("prior_"), ~ .x * 10),
    prior_far_yoy_change_nom = pred_pin_final_fmv_round - prior_far_tot,
    prior_far_yoy_change_pct = prior_far_yoy_change_nom / prior_far_tot,
    prior_near_yoy_change_nom = pred_pin_final_fmv_round - prior_near_tot,
    prior_near_yoy_change_pct = prior_near_yoy_change_nom / prior_near_tot
  )


## 5.4. Add Flags --------------------------------------------------------------

# Flags are used for identifying PINs for potential desktop review
assessment_data_pin_final <- assessment_data_pin_2 %>%
  # Rename existing indicators to flags
  rename_with(~ gsub("ind_", "flag_", .x), starts_with("ind_")) %>%
  # Add flag for potential proration issues (rates don't sum to 1)
  group_by(meta_tieback_key_pin) %>%
  mutate(flag_proration_sum_not_1 = ifelse(
    !is.na(meta_tieback_key_pin),
    sum(meta_tieback_proration_rate) != 1,
    FALSE
  )) %>%
  ungroup() %>%
  # Flag for capped land value
  mutate(
    flag_land_value_capped = pred_pin_final_fmv_round *
      params$pv$land_pct_of_total_cap == pred_pin_final_fmv_land
  ) %>%
  # Flags for changes in values
  mutate(
    flag_prior_near_to_pred_unchanged =
      prior_near_tot >= pred_pin_final_fmv_round - 100 &
        prior_near_tot <= pred_pin_final_fmv_round + 100,
    flag_prior_near_yoy_inc_gt_50_pct = prior_near_yoy_change_pct > 0.5,
    flag_prior_near_yoy_dec_gt_5_pct = prior_near_yoy_change_pct < -0.05,
  ) %>%
  # Flag high-value properties from prior years
  group_by(meta_township_code) %>%
  mutate(flag_prior_near_fmv_top_decile = ntile(prior_near_tot, 10) == 10) %>%
  ungroup() %>%
  mutate(
    meta_pin_num_landlines = tidyr::replace_na(meta_pin_num_landlines, 1),
    flag_pin_is_multiland = tidyr::replace_na(flag_pin_is_multiland, FALSE),
    flag_nonlivable_space = meta_modeling_group == "NONLIVABLE",
    flag_pin10_5yr_num_sale = meta_pin10_5yr_num_sale
  ) %>%
  select(-meta_modeling_group, -meta_pin10_5yr_num_sale) %>%
  relocate(flag_prior_far_yoy_bldg_change_pct, .after = starts_with("flag_"))


## 5.5. Clean/Reorder/Save -----------------------------------------------------

# Recode characteristics from numeric encodings to human-readable strings
temp <- assessment_data_pin_final %>%
  ccao::vars_recode(
    cols = starts_with("char_"),
    type = "short",
    as_factor = FALSE
  ) %>%
  select(-meta_pin10) %>%
  # Reorder columns into groups by prefix
  select(
    starts_with(c("meta_", "loc_")),
    char_yrblt, char_total_bldg_sf, char_unit_sf, char_land_sf, 
    starts_with(c("land", "prior_far_", "prior_near_")),
    pred_pin_final_fmv, pred_pin_final_fmv_bldg,
    pred_pin_final_fmv_land, pred_pin_final_fmv_round,
    pred_pin_land_rate_effective, pred_pin_land_pct_total,
    starts_with(c("sale_", "flag_")), township_code
  ) %>%
  as_tibble() %>%
  write_parquet(paths$output$assessment_pin$local)

# End the stage timer and write the time elapsed to a temporary file
tictoc::toc(log = TRUE)
bind_rows(tictoc::tic.log(format = FALSE)) %>%
  arrow::write_parquet(gsub("//*", "/", file.path(
    paths$intermediate$timing$local,
    "model_timing_assess.parquet"
  )))
