#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# 1. Setup ---------------------------------------------------------------------
#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# NOTE: See DESCRIPTION for library dependencies and R/setup.R for
# variables used in each pipeline stage

# Start the stage timer and clear logs from prior stage
tictoc::tic.clearlog()
tictoc::tic("Assess")

# Load libraries, helpers, and recipes from files
purrr::walk(list.files("R/", "\\.R$", full.names = TRUE), source)

# Columns to use for ratio study comparison (by prefix)
rsf_prefix <- gsub("_tot", "", params$ratio_study$far_column)
rsn_prefix <- gsub("_tot", "", params$ratio_study$near_column)

# Load the training data to use as a source of sales. These will be attached to
# PIN-level output (for comparison) and used as the basis for a sales ratio
# analysis on the assessment data
sales_data <- read_parquet(paths$input$training$local)

# Load land rates from file
land_nbhd_rate <- read_parquet(
  paths$input$land_nbhd_rate$local
)




#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# 2. Predict Values ------------------------------------------------------------
#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
message("Predicting off-market values with trained model")

# Load the final lightgbm model object and recipe from file
lgbm_final_full_fit <- lightsnip::lgbm_load(paths$output$workflow_fit$local)
lgbm_final_full_recipe <- readRDS(paths$output$workflow_recipe$local)

# Load the data for assessment. This is the universe of condo units
# that need values. Use the trained lightgbm model to estimate a single
# FMV per unit
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
message("Performing post-modeling adjustments")

## 3.1. Value Non-Livable Units ------------------------------------------------
message("Fixing non-livable unit values")

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
        (meta_mailed_tot * 10) > params$pv$nonlivable_threshold ~
        as.numeric(params$pv$nonlivable_fixed_fmv),
      meta_modeling_group == "NONLIVABLE" &
        is.na(meta_mailed_tot) ~ as.numeric(params$pv$nonlivable_fixed_fmv),
      TRUE ~ pred_card_initial_fmv
    )
  )


## 3.2. Peg to % Ownership -----------------------------------------------------
message("Aggregating to building level")

# For condominiums, we need to aggregate values to the building level, then
# multiply by proration rate/percent ownership to get the final unit value. For
# example, if you have a 3 unit building, with percentages .25, .25, .5,
# and the model-predicted values for all 3 units are the same, you want to
# divide the value of the building proportionally across units.
# Suppose each unit is valued at $100. The whole building is worth $300.
# Unit 1 valued at 25% of $300, or $75.

# Note that this valuation method is essentially required by statute
assessment_data_bldg <- assessment_data_nl %>%
  # In cases where a PIN has multiple llines, count only the value of the first
  # line when getting the building total value
  arrange(meta_pin, meta_card_num, meta_lline_num) %>%
  group_by(meta_pin) %>%
  mutate(first_lline = first(meta_lline_num)) %>%
  ungroup() %>%
  filter(meta_lline_num == first_lline | is.na(first_lline)) %>%
  select(-first_lline) %>%
  group_by(meta_pin10) %>%
  mutate(
    bldg_total_value = sum(pred_pin_final_fmv, na.rm = TRUE),
    bldg_total_proration_rate = sum(
      meta_tieback_proration_rate,
      na.rm = TRUE
    ),
    pred_pin_final_fmv = bldg_total_value *
      (meta_tieback_proration_rate / bldg_total_proration_rate),

    # For certain units (common areas), we want to have a consistent low value
    # across time (usually $10)
    pred_pin_final_fmv = case_when(
      meta_modeling_group == "NONLIVABLE" &
        (meta_mailed_tot * 10) <= params$pv$nonlivable_threshold ~
        meta_mailed_tot * 10,
      meta_modeling_group == "NONLIVABLE" &
        is.na(meta_mailed_tot) ~ 10,
      TRUE ~ pred_pin_final_fmv
    )
  ) %>%
  ungroup()


## 3.3. Round and Finalize -----------------------------------------------------
message("Rounding and finalizing")

# Round PIN-level predictions using the breaks and amounts specified in params
assessment_data_final <- assessment_data_bldg %>%
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
        meta_pin, meta_card_num, meta_lline_num,
        pred_pin_final_fmv, pred_pin_final_fmv_round
      ),
    by = c("meta_pin", "meta_card_num", "meta_lline_num")
  ) %>%
  mutate(
    township_code = meta_township_code,
    meta_year = as.character(meta_year)
  )




#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# 4. Card-Level Data -----------------------------------------------------------
#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
message("Saving card-level data")

# Keep only card-level variables of interest, including: ID variables (run_id,
# pin, card), characteristics, and predictions. For condos card and PIN are the
# same
assessment_data_merged %>%
  select(
    meta_year, meta_pin, meta_class, meta_card_num, meta_lline_num,
    meta_modeling_group, ends_with("_num_sale"), pred_card_initial_fmv,
    all_of(params$model$predictor$all), township_code
  ) %>%
  mutate(
    ccao_n_years_exe_homeowner = as.integer(ccao_n_years_exe_homeowner)
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
message("Attaching recent sales to PIN-level data")

# Load the MOST RECENT sale per PIN from the year prior to the assessment year.
# These are the sales that will be used for ratio studies in the evaluate stage.
# We want our assessed value to be as close as possible to this sale
sales_data_ratio_study <- sales_data %>%
  # For ratio studies, we don't want to include outliers
  filter(!sv_is_outlier) %>%
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
  distinct(
    meta_pin, meta_year,
    meta_sale_price, meta_sale_date, meta_sale_document_num,
    sv_outlier_type
  ) %>%
  # Include outliers, since these data are used for desk review and
  # not for modeling
  rename(meta_sale_outlier_type = sv_outlier_type) %>%
  mutate(
    meta_sale_outlier_type = ifelse(
      meta_sale_outlier_type == "Not outlier", NA, meta_sale_outlier_type
    )
  ) %>%
  group_by(meta_pin) %>%
  slice_max(meta_sale_date, n = 2) %>%
  mutate(mr = paste0("sale_recent_", row_number())) %>%
  tidyr::pivot_wider(
    id_cols = meta_pin,
    names_from = mr,
    values_from = c(
      meta_sale_date,
      meta_sale_price,
      meta_sale_document_num,
      meta_sale_outlier_type
    ),
    names_glue = "{mr}_{gsub('meta_sale_', '', .value)}"
  ) %>%
  select(meta_pin, contains("1"), contains("2")) %>%
  ungroup()

# Get the % change in the average sale price within building between the last
# assessment year and this year
sales_data_bldg_change_pct <- sales_data %>%
  filter(!sv_is_outlier) %>%
  filter(
    meta_year == params$assessment$data_year |
      meta_year == params$ratio_study$far_year
  ) %>%
  group_by(meta_year, meta_pin10) %>%
  filter(n() >= 5) %>%
  mutate(meta_year = ifelse(
    meta_year == params$assessment$data_year, "cur", "pri"
  )) %>%
  summarize(mean_sale_price = mean(meta_sale_price)) %>%
  pivot_wider(names_from = meta_year, values_from = mean_sale_price) %>%
  mutate(flag_prior_far_yoy_bldg_change_pct = (cur - pri) / pri) %>%
  select(meta_pin10, flag_prior_far_yoy_bldg_change_pct)


## 5.2. Keep PIN-Level Data ----------------------------------------------------
message("Saving PIN-level data")

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
      "loc_property_", "loc_ward_", "loc_chicago_",
      "loc_census", "loc_school_", "loc_tax_", "prior_", "ind_"
    )),
    meta_pin10_5yr_num_sale,

    # Keep PIN-level predicted values
    pred_pin_final_fmv, pred_pin_final_fmv_round, township_code
  ) %>%
  ungroup() %>%
  # Overwrite missing land values (only a few PINs)
  mutate(char_land_sf = replace_na(char_land_sf, 0))


## 5.3. Value Land -------------------------------------------------------------
message("Attaching and parsing land values")

# Attach land and sales data to the PIN-level data, then calculate land and
# building values for each PIN
assessment_data_pin_2 <- assessment_data_pin %>%
  left_join(
    land_nbhd_rate,
    by = c("meta_nbhd_code" = "meta_nbhd", "meta_class")
  ) %>%
  left_join(sales_data_two_most_recent, by = "meta_pin") %>%
  left_join(sales_data_ratio_study, by = c("meta_year", "meta_pin")) %>%
  left_join(sales_data_bldg_change_pct, by = "meta_pin10") %>%
  # Land values are provided by Valuations and are capped at a percentage of the
  # total FMV for the PIN. For condos, all units in a building share the same
  # land square footage, but the value is assigned by the % of ownership
  mutate(
    pred_pin_final_fmv_land = ceiling(case_when(
      char_land_sf * land_rate_per_sqft * meta_tieback_proration_rate >=
        pred_pin_final_fmv_round * params$pv$land_pct_of_total_cap ~
        pred_pin_final_fmv_round * params$pv$land_pct_of_total_cap,
      TRUE ~ char_land_sf * land_rate_per_sqft * meta_tieback_proration_rate
    )),
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
message("Adding Desk Review flags")

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
    # nolint start
    flag_prior_near_to_pred_unchanged =
      prior_near_tot >= pred_pin_final_fmv_round - 100 &
        prior_near_tot <= pred_pin_final_fmv_round + 100,
    # nolint end
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
message("Saving final PIN-level data")

# Recode characteristics from numeric encodings to human-readable strings
assessment_data_pin_final %>%
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
