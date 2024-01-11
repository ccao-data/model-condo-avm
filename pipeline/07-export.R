#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# 1. Setup ---------------------------------------------------------------------
#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# NOTE: See DESCRIPTION for library dependencies and R/setup.R for
# variables used in each pipeline stage

# Load libraries, helpers, and recipes from files
purrr::walk(list.files("R/", "\\.R$", full.names = TRUE), source)

# Load additional dev R libraries (see README#managing-r-dependencies)
suppressPackageStartupMessages({
  library(DBI)
  library(openxlsx)
  library(noctua)
})

# Establish Athena connection
AWS_ATHENA_CONN_NOCTUA <- dbConnect(noctua::athena())




#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# 2. Pull Model Data -----------------------------------------------------------
#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
message("Pulling model data from Athena")

# Pull the PIN-level assessment data, which contains all the fields needed to
# create the review spreadsheets
assessment_pin <- dbGetQuery(
  conn = AWS_ATHENA_CONN_NOCTUA, glue("
  SELECT *
  FROM model.assessment_pin
  WHERE run_id = '{params$export$run_id}'
  AND meta_triad_code = '{params$export$triad_code}'
  ")
)

# Pull card-level data only for all PINs. Needed for upload, since values are
# tracked by card, even though they're presented by PIN
assessment_card <- dbGetQuery(
  conn = AWS_ATHENA_CONN_NOCTUA, glue("
  SELECT c.*
  FROM model.assessment_card c
  INNER JOIN (
      SELECT *
      FROM model.assessment_pin
      WHERE run_id = '{params$export$run_id}'
      AND meta_triad_code = '{params$export$triad_code}'
  ) p
  ON c.year = p.year
      AND c.run_id = p.run_id
      AND c.meta_pin = p.meta_pin
  ")
)

# Pull land for condos with multiple land lines (very rare)
land <- dbGetQuery(
  conn = AWS_ATHENA_CONN_NOCTUA, glue("
  SELECT
      taxyr AS meta_year,
      parid AS meta_pin,
      lline AS meta_line_num,
      sf AS meta_line_sf
  FROM iasworld.land
  WHERE taxyr = '{params$assessment$data_year}'
  ")
)




#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# 3. Prep Desk Review ----------------------------------------------------------
#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
message("Preparing data for Desk Review export")

# Prep data with a few additional columns + put everything in the right
# order for DR sheets
assessment_pin_prepped <- assessment_pin %>%
  mutate(
    prior_near_land_rate = round(
      prior_near_land / (char_land_sf * meta_tieback_proration_rate),
      2
    ),
    prior_near_bldg_rate = round(prior_near_bldg / char_unit_sf, 2),
    pred_pin_bldg_rate_effective = round(
      pred_pin_final_fmv_round / char_unit_sf,
      2
    ),
    prior_near_land_pct_total = round(prior_near_land / prior_near_tot, 4),
    property_full_address = paste0(
      loc_property_address,
      ", ", loc_property_city, " ", loc_property_state,
      ", ", loc_property_zip
    ),
    meta_pin10 = str_sub(meta_pin, 1, 10),
    flag_common_area = replace_na(
      as.logical(as.numeric(flag_prior_near_to_pred_unchanged)) &
        prior_near_tot <= params$pv$nonlivable_threshold,
      0
    ),
    char_type_resd = NA
  ) %>%
  select(
    township_code, meta_pin, meta_class, meta_nbhd_code,
    property_full_address, loc_cook_municipality_name, meta_pin10,
    meta_tieback_key_pin, meta_tieback_proration_rate,
    prior_near_land, prior_near_bldg, prior_near_tot,
    prior_near_land_rate, prior_near_bldg_rate, prior_near_land_pct_total,
    pred_pin_final_fmv, pred_pin_final_fmv_land, pred_pin_final_fmv_bldg,
    pred_pin_final_fmv_round, land_rate_per_sqft, pred_pin_land_rate_effective,
    pred_pin_bldg_rate_effective, pred_pin_land_pct_total,
    prior_near_yoy_change_nom, prior_near_yoy_change_pct,
    sale_recent_1_date, sale_recent_1_price, sale_recent_1_document_num,
    sale_recent_2_date, sale_recent_2_price, sale_recent_2_document_num,
    char_yrblt, char_total_bldg_sf, char_type_resd, char_land_sf,
    char_unit_sf, flag_nonlivable_space, flag_pin10_5yr_num_sale,
    flag_common_area, flag_proration_sum_not_1, flag_pin_is_multiland,
    flag_land_gte_95_percentile, flag_bldg_gte_95_percentile,
    flag_land_value_capped, flag_prior_near_to_pred_unchanged,
    flag_pred_initial_to_final_changed,
    flag_prior_near_yoy_inc_gt_50_pct, flag_prior_near_yoy_dec_gt_5_pct
  ) %>%
  mutate(
    across(starts_with("flag_"), as.numeric),
    across(where(is.numeric), ~ na_if(.x, Inf))
  ) %>%
  arrange(township_code, meta_pin) %>%
  mutate(
    meta_pin = glue(
      '=HYPERLINK("https://www.cookcountyassessor.com/pin/{meta_pin}",
      "{meta_pin}")'
    ),
    property_full_address = str_remove_all(
      property_full_address,
      "[^[:alnum:]|' ',.-]"
    )
  )

# Prep building-level (PIN10) data
assessment_pin10_prepped <- assessment_pin_prepped %>%
  group_by(township_code, meta_pin10, meta_nbhd_code) %>%
  summarize(
    property_full_address = first(property_full_address),
    loc_cook_municipality_name = first(loc_cook_municipality_name),
    num_pin_livable = sum(!flag_nonlivable_space),
    num_pin_nonlivable = sum(flag_nonlivable_space),
    total_tieback_proration_rate = sum(meta_tieback_proration_rate),
    prior_near_bldg_total = sum(prior_near_tot),
    pred_pin_final_fmv_bldg_total = sum(pred_pin_final_fmv_round),
    prior_near_yoy_change_nom_total =
      pred_pin_final_fmv_bldg_total - prior_near_bldg_total,
    prior_near_yoy_change_pct =
      (pred_pin_final_fmv_bldg_total - prior_near_bldg_total) /
        prior_near_bldg_total,
    char_yrblt = first(char_yrblt),
    char_total_bldg_sf = first(char_total_bldg_sf)
  ) %>%
  ungroup() %>%
  arrange(meta_pin10)




#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# 4. Export Desk Review --------------------------------------------------------
#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Write raw data to sheets for parcel details
for (town in unique(assessment_pin_prepped$township_code)) {
  message("Now processing: ", town_convert(town))


  ## 4.1. PIN-Level ------------------------------------------------------------

  # Filter overall data to specific township
  assessment_pin_filtered <- assessment_pin_prepped %>%
    filter(township_code == town) %>%
    select(-township_code)

  # Generate sheet and column headers
  model_header <- str_to_title(paste(
    params$assessment$year, "Model"
  ))
  comp_header <- str_to_title(paste(
    params$ratio_study$near_year, params$ratio_study$near_stage
  ))
  sheet_header <- str_to_title(glue(
    comp_header, "Values vs.", model_header, "Values - Parcel-Level Results",
    .sep = " "
  ))

  pin_sheet_name <- "PIN Detail"
  class(assessment_pin_filtered$meta_pin) <- c(
    class(assessment_pin_filtered$meta_pin), "formula"
  )

  # Get range of rows in the PIN data + number of header rows
  pin_row_range <- 7:(nrow(assessment_pin_filtered) + 9)

  # Load the excel workbook template from file
  wb <- loadWorkbook(here("misc", "desk_review_template.xlsx"))

  # Create formatting styles
  style_price <- createStyle(numFmt = "$#,##0")
  style_2digit <- createStyle(numFmt = "$#,##0.00")
  style_pct <- createStyle(numFmt = "PERCENTAGE")
  style_comma <- createStyle(numFmt = "COMMA")

  # Add styles to PIN sheet
  addStyle(
    wb, pin_sheet_name,
    style = style_price,
    rows = pin_row_range, cols = c(9:11, 15:18, 23, 26, 29), gridExpand = TRUE
  )
  addStyle(
    wb, pin_sheet_name,
    style = style_2digit,
    rows = pin_row_range, cols = c(12:13, 19:21), gridExpand = TRUE
  )
  addStyle(
    wb, pin_sheet_name,
    style = style_pct,
    rows = pin_row_range, cols = c(8, 14, 22, 24), gridExpand = TRUE
  )
  addStyle(
    wb, pin_sheet_name,
    style = style_comma,
    rows = pin_row_range, cols = c(32, 34, 35, 37), gridExpand = TRUE
  )
  addFilter(wb, pin_sheet_name, 6, 1:44)

  # Write PIN-level data to workbook
  writeData(
    wb, pin_sheet_name, assessment_pin_filtered,
    startCol = 1, startRow = 7, colNames = FALSE
  )

  # Write formulas and headers to workbook
  writeFormula(
    wb, pin_sheet_name,
    assessment_pin_filtered$meta_pin,
    startRow = 7
  )
  writeData(
    wb, pin_sheet_name, tibble(sheet_header),
    startCol = 2, startRow = 1, colNames = FALSE
  )
  writeData(
    wb, pin_sheet_name, tibble(params$export$run_id),
    startCol = 3, startRow = 3, colNames = FALSE
  )
  writeData(
    wb, pin_sheet_name, tibble(comp_header),
    startCol = 9, startRow = 5, colNames = FALSE
  )
  writeData(
    wb, pin_sheet_name, tibble(model_header),
    startCol = 15, startRow = 5, colNames = FALSE
  )


  # 4.2. Building-Level --------------------------------------------------------

  # Filter building data to specific township
  assessment_pin10_filtered <- assessment_pin10_prepped %>%
    filter(township_code == town) %>%
    select(-township_code)

  bldg_sheet_name <- "Building (PIN10)"

  # Get range of rows in the building data + number of header rows
  bldg_row_range <- 5:(nrow(assessment_pin10_filtered) + 6)

  # Add styles to bldg sheet
  addStyle(
    wb, bldg_sheet_name,
    style = style_price,
    rows = bldg_row_range, cols = c(8:10), gridExpand = TRUE
  )
  addStyle(
    wb, bldg_sheet_name,
    style = style_pct,
    rows = bldg_row_range, cols = c(7, 11), gridExpand = TRUE
  )
  addStyle(
    wb, bldg_sheet_name,
    style = style_comma,
    rows = bldg_row_range, cols = c(5:6, 13), gridExpand = TRUE
  )
  addFilter(wb, bldg_sheet_name, 4, 1:13)

  # Write bldg-level data to workbook
  writeData(
    wb, bldg_sheet_name, assessment_pin10_filtered,
    startCol = 1, startRow = 5, colNames = FALSE
  )

  # Write formulas and headers to workbook
  writeData(
    wb, bldg_sheet_name, tibble(comp_header),
    startCol = 8, startRow = 3, colNames = FALSE
  )
  writeData(
    wb, bldg_sheet_name, tibble(model_header),
    startCol = 9, startRow = 3, colNames = FALSE
  )

  # Save workbook to file based on town name
  saveWorkbook(
    wb,
    here(
      "output", "desk_review",
      glue(
        params$assessment$year,
        str_replace(town_convert(town), " ", "_"),
        "Initial_Model_Values_Condo.xlsx",
        .sep = "_"
      )
    ),
    overwrite = TRUE
  )
  rm(wb)
}




#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# 5. Prep iasWorld Upload ------------------------------------------------------
#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Prepare data for iasWorld upload
upload_data_prepped <- assessment_pin %>%
  left_join(
    assessment_card %>%
      select(meta_year, meta_pin, meta_card_num, meta_lline_num),
    by = c("meta_year", "meta_pin")
  ) %>%
  mutate(meta_pin10 = str_sub(meta_pin, 1, 10)) %>%
  group_by(meta_pin10, meta_tieback_proration_rate) %>%
  # For PINs missing an individual building value, fill with the average of
  # PINs with the same proration rate in the building. This is super rare,
  # maybe 1 PIN out of every 100K. It happens mostly because of mis-coded nbhds
  mutate(
    pred_pin_final_fmv_bldg = ifelse(
      is.na(pred_pin_final_fmv_bldg),
      mean(pred_pin_final_fmv_bldg, na.rm = TRUE),
      pred_pin_final_fmv_bldg
    )
  ) %>%
  group_by(meta_pin10) %>%
  mutate(
    # Sum the building value of each PIN to the building total value
    pred_pin10_final_fmv_bldg = sum(pred_pin_final_fmv_bldg, na.rm = TRUE),

    # Hotfix for adjusting the total building value such that bldg_total *
    # proration_rate = unit_value. Only applies to buildings where rates don't
    # sum to 100%
    pred_pin10_final_fmv_bldg = round(pred_pin10_final_fmv_bldg *
      (1 / sum(meta_tieback_proration_rate, na.rm = TRUE))),

    # For any missing LLINE values, simply fill with 1
    meta_lline_num = replace_na(meta_lline_num, 1)
  ) %>%
  ungroup() %>%
  select(
    township_code = meta_township_code,
    PARID = meta_pin,
    CARD = meta_card_num,
    LLINE = meta_lline_num,
    USER18 = pred_pin10_final_fmv_bldg,
    USER20 = meta_tieback_proration_rate,
    OVRRCNLD = pred_pin_final_fmv_bldg
  ) %>%
  arrange(township_code, PARID)




#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# 6. Export iasWorld Upload ----------------------------------------------------
#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Write each town to a headerless CSV for mass upload
for (town in unique(upload_data_prepped$township_code)) {
  message("Now processing: ", town_convert(town))

  upload_data_fil <- upload_data_prepped %>%
    filter(township_code == town) %>%
    select(-township_code)

  write_csv(
    x = upload_data_fil,
    file = here(
      "output", "iasworld",
      glue(
        params$assessment$year,
        str_replace(town_convert(town), " ", "_"),
        "iasworld_upload.csv",
        .sep = "_"
      )
    ),
    na = "",
    col_names = TRUE
  )
}
