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
AWS_ATHENA_CONN_NOCTUA <- dbConnect(noctua::athena(), rstudio_conn_tab = FALSE)


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

# Pull assessable permit flag
flag_assessable_permits <- dbGetQuery(
  conn = AWS_ATHENA_CONN_NOCTUA, glue("
  SELECT pin, has_recent_assessable_permit
  FROM default.vw_pin_status
  WHERE year = '{params$assessment$data_year}'
  ")
)


#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# 3. Define Workbook Schemas ---------------------------------------------------
#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Each entry in the schema specifies:
#   display_name  - column header text (must match the template header row)
#   style         - named style from wb_styles (price, 2digit_price, 2digit_num,
#                   pct, comma, link)
#   formula       - TRUE if this column is written via writeFormula
#   cond          - named conditional-formatting group (color_scale,
#                   sale_outlier_1, sale_outlier_2)
#   hidden        - TRUE if the column is hidden in Excel (formula-only columns
#                   not present in the source data frame)

pin_detail_schema <- list(
  meta_pin = list(formula = TRUE, display_name = "PIN"),
  meta_class = list(display_name = "Class"),
  meta_nbhd_code = list(display_name = "Nbhd."),
  property_full_address = list(display_name = "Street Address"),
  loc_tax_municipality_name = list(display_name = "Municipality"),
  meta_pin10 = list(formula = TRUE, display_name = "Condo Building ID (PIN10)"),
  meta_tieback_key_pin = list(display_name = "Tieback Key PIN"),
  meta_tieback_proration_rate = list(
    style = "pct", display_name = "Tieback Percent Ownership"
  ),
  prior_near_land = list(style = "price", display_name = "Land"),
  prior_near_bldg = list(style = "price", display_name = "Building"),
  prior_near_tot = list(style = "price", display_name = "Total"),
  prior_near_land_rate = list(
    style = "2digit_price", display_name = "Lnd. Rate S. F."
  ),
  prior_near_bldg_rate = list(
    style = "2digit_price", display_name = "Unit Rate S. F."
  ),
  prior_near_land_pct_total = list(
    style = "pct", display_name = "Lnd. % of Total"
  ),
  pred_pin_final_fmv = list(style = "price", display_name = "Before Rounding"),
  pred_pin_final_fmv_land = list(style = "price", display_name = "Land"),
  pred_pin_final_fmv_bldg = list(style = "price", display_name = "Building"),
  pred_pin_final_fmv_round = list(style = "price", display_name = "Total"),
  land_rate_per_sqft = list(
    style = "2digit_price", display_name = "Lnd. Rate Original"
  ),
  pred_pin_land_rate_effective = list(
    style = "2digit_price", display_name = "Lnd. Rate Effective"
  ),
  pred_pin_bldg_rate_effective = list(
    style = "2digit_price", display_name = "Unit Rate S. F."
  ),
  pred_pin_land_pct_total = list(
    style = "pct", display_name = "Lnd. % of Tot."
  ),
  prior_near_yoy_change_nom = list(
    style = "price", display_name = "YoY ∆ $"
  ),
  prior_near_yoy_change_pct = list(
    style = "pct", cond = "color_scale", display_name = "YoY ∆ %"
  ),
  sale_ratio = list(
    formula = TRUE, style = "2digit_num", display_name = "Sale Ratio"
  ),
  valuations_note = list(display_name = "Valuations Notes"),
  sale_recent_1_date = list(
    cond = "sale_outlier_1", display_name = "Sale Date 1"
  ),
  sale_recent_1_price = list(
    style = "price", cond = "sale_outlier_1", display_name = "Sale Amount 1"
  ),
  sale_recent_1_outlier_reason = list(
    cond = "sale_outlier_1",
    display_name = "Non-Arm's-Length Sale Flag 1"
  ),
  sale_recent_1_document_num = list(
    cond = "sale_outlier_1", display_name = "Sale Doc. 1"
  ),
  sale_recent_1_num_parcels = list(
    cond = "sale_outlier_1", display_name = "Sale Num. Parcels 1"
  ),
  sale_recent_2_date = list(
    cond = "sale_outlier_2", display_name = "Sale Date 2"
  ),
  sale_recent_2_price = list(
    style = "price", cond = "sale_outlier_2", display_name = "Sale Amount 2"
  ),
  sale_recent_2_outlier_reason = list(
    cond = "sale_outlier_2",
    display_name = "Non-Arm's-Length Sale Flag 2"
  ),
  sale_recent_2_document_num = list(
    cond = "sale_outlier_2", display_name = "Sale Doc. 2"
  ),
  sale_recent_2_num_parcels = list(
    cond = "sale_outlier_2", display_name = "Sale Num. Parcels 2"
  ),
  char_yrblt = list(display_name = "Year Built"),
  char_total_bldg_sf = list(
    style = "comma", display_name = "Total Bld. S. F."
  ),
  char_land_sf = list(style = "comma", display_name = "Lnd. S. F."),
  char_unit_sf = list(style = "comma", display_name = "Condo Unit S. F."),
  meta_pin10_bldg_roll_mean = list(
    style = "price", display_name = "5-Year Bldg. Sale Price Avg."
  ),
  meta_pin10_bldg_roll_count = list(
    style = "comma", display_name = "5-Year Bldg. Sale Count"
  ),
  flag_pin10_bldg_roll_mean_imputed = list(
    display_name = "Bldg. Sale Avg. was Imputed"
  ),
  flag_nonlivable_space = list(display_name = "Condo Non-Livable Space"),
  flag_proration_sum_not_1 = list(
    display_name = "% Ownership Sum Not Equal to 1"
  ),
  flag_pin_is_multiland = list(display_name = "Multi-Land"),
  flag_land_gte_95_percentile = list(
    display_name = "Lnd. >= 95% in Town"
  ),
  flag_land_value_capped = list(display_name = "Land Value Capped"),
  flag_prior_near_to_pred_unchanged = list(display_name = "Value Unchanged"),
  flag_prior_near_yoy_inc_gt_50_pct = list(
    display_name = "YoY Change >= 50%"
  ),
  flag_prior_near_yoy_dec_gt_5_pct = list(
    display_name = "YoY Change <= -5%"
  ),
  flag_has_recent_assessable_permit = list(
    display_name = "Recent Assessable Permit"
  ),
  total_mv = list(
    formula = TRUE, style = "price", hidden = TRUE, display_name = "Total MV"
  ),
  mv_difference = list(
    formula = TRUE, style = "price", hidden = TRUE,
    display_name = "MV Difference"
  )
)

bldg_schema <- list(
  meta_pin10 = list(display_name = "PIN10"),
  meta_nbhd_code = list(display_name = "Nbhd."),
  property_full_address = list(display_name = "Street Address"),
  loc_tax_municipality_name = list(display_name = "Municipality"),
  num_pin_livable = list(style = "comma", display_name = "Num. Livable PINs"),
  num_pin_nonlivable = list(
    style = "comma", display_name = "Num. Non-Livable PINs"
  ),
  total_tieback_proration_rate = list(
    style = "pct", display_name = "Total Res. % Ownership"
  ),
  prior_near_bldg_total = list(
    style = "price", display_name = "Building Total FMV"
  ),
  pred_pin_final_fmv_bldg_total = list(
    style = "price", display_name = "Building Total FMV"
  ),
  prior_near_yoy_change_nom_total = list(
    style = "price", display_name = "YoY ∆ $"
  ),
  prior_near_yoy_change_pct = list(
    style = "pct", cond = "color_scale", display_name = "YoY ∆ %"
  ),
  char_yrblt = list(display_name = "Year Built"),
  char_total_bldg_sf = list(style = "comma", display_name = "Bld. Total S. F.")
)

# Validate that the schemas match the template column headers
validate_schema_vs_template(
  pin_detail_schema,
  here("misc", "desk_review_template.xlsx"),
  "PIN Detail",
  header_row = 4
)
validate_schema_vs_template(
  bldg_schema,
  here("misc", "desk_review_template.xlsx"),
  "Buildings",
  header_row = 4
)

# Precompute Excel column letters used in formula strings and cond. formatting
fmv_round_col <- int2col(col_pos(pin_detail_schema, "pred_pin_final_fmv_round"))
prior_near_tot_col <- int2col(col_pos(pin_detail_schema, "prior_near_tot"))
sale1_price_col <- int2col(col_pos(pin_detail_schema, "sale_recent_1_price"))
sale1_outlier_col <- int2col(
  col_pos(pin_detail_schema, "sale_recent_1_outlier_reason")
)
sale2_outlier_col <- int2col(
  col_pos(pin_detail_schema, "sale_recent_2_outlier_reason")
)
sale1_num_parcels_col <- int2col(
  col_pos(pin_detail_schema, "sale_recent_1_num_parcels")
)
sale2_num_parcels_col <- int2col(
  col_pos(pin_detail_schema, "sale_recent_2_num_parcels")
)


#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# 4. Prep Desk Review ----------------------------------------------------------
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
    across(
      ends_with("added_later") & where(is.logical),
      ~ as.numeric(.x)
    ),
    # Empty fields to be filled out via other means
    valuations_note = NA,
    sale_ratio = NA
  ) %>%
  # Add assessable permit flag
  left_join(flag_assessable_permits, by = c("meta_pin" = "pin")) %>%
  mutate(
    flag_has_recent_assessable_permit =
      as.numeric(has_recent_assessable_permit),
    sale_recent_1_outlier_reason =
      if_else(sale_recent_1_is_outlier, sale_recent_1_outlier_reason, ""),
    sale_recent_2_outlier_reason =
      if_else(sale_recent_2_is_outlier, sale_recent_2_outlier_reason, "")
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
  ) %>%
  # Replace values for buildings with ONLY parking with NA, as they do not
  # receive correct values from this pipeline and are very rare
  group_by(meta_pin10) %>%
  mutate(
    across(
      starts_with("pred_pin_"),
      ~ ifelse(is.nan(pred_pin_final_fmv), NA, .x)
    )
  ) %>%
  ungroup()


# Prep building-level (PIN10) data
assessment_pin10_prepped <- assessment_pin_prepped %>%
  group_by(township_code, meta_pin10, meta_nbhd_code) %>%
  summarize(
    property_full_address = first(property_full_address),
    loc_tax_municipality_name = first(loc_tax_municipality_name),
    num_pin_livable = sum(!flag_nonlivable_space),
    num_pin_nonlivable = sum(flag_nonlivable_space),
    total_tieback_proration_rate = sum(meta_tieback_proration_rate),
    prior_near_bldg_total = sum(prior_near_tot),
    pred_pin_final_fmv_bldg_total = sum(pred_pin_final_fmv_round),
    prior_near_yoy_change_nom_total =
      pred_pin_final_fmv_bldg_total - prior_near_bldg_total,
    # nolint start
    prior_near_yoy_change_pct =
      (pred_pin_final_fmv_bldg_total - prior_near_bldg_total) /
        prior_near_bldg_total,
    # nolint end
    char_yrblt = first(char_yrblt),
    char_total_bldg_sf = first(char_total_bldg_sf)
  ) %>%
  ungroup() %>%
  arrange(meta_pin10)


#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# 5. Export Desk Review --------------------------------------------------------
#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Indices of all schema columns whose `cond` field equals `cond_name`
cols_with_cond <- function(schema, cond_name) {
  which(vapply(schema, function(x) identical(x$cond, cond_name), logical(1)))
}

# Indices of all schema columns marked hidden = TRUE
cols_hidden <- function(schema) {
  which(vapply(schema, function(x) isTRUE(x$hidden), logical(1)))
}

# Write raw data to sheets for parcel details
for (town in unique(assessment_pin_prepped$township_code)) {
  message("Now processing: ", town_convert(town))

  ## 5.1. PIN-Level ------------------------------------------------------------

  # Filter building data to specific township
  assessment_pin10_filtered <- assessment_pin10_prepped %>%
    filter(township_code == town) %>%
    select(-township_code) %>%
    select(all_of(names(bldg_schema)))

  building_coords <- assessment_pin10_filtered %>%
    # Handle a rare edge case where neighborhoods span multiple townships
    # and so introduce duplicate pin10s; more details here:
    # https://github.com/ccao-data/data-architecture/issues/275
    distinct(meta_pin10) %>%
    tibble::rowid_to_column("building_coord") %>%
    select(meta_pin10, building_coord)

  pin_sheet_name <- "PIN Detail"
  bldg_sheet_name <- "Buildings"

  # Filter overall data to specific township
  assessment_pin_filtered <- assessment_pin_prepped %>%
    filter(township_code == town) %>%
    select(-township_code) %>%
    left_join(building_coords, by = "meta_pin10") %>%
    mutate(
      building_coord = ifelse(
        is.na(building_coord),
        NA,
        getCellRefs(data.frame(row = building_coord + 4, column = 1))
      )
    ) %>%
    mutate(
      meta_pin10 = ifelse(
        is.na(building_coord),
        NA,
        glue::glue(
          '=HYPERLINK(@CELL("address",{bldg_sheet_name}!{building_coord}),',
          '"{meta_pin10}")'
        )
      )
    ) %>%
    # This select statement aligns the pin dataframe with the pin detail schema
    select(all_of(setdiff(names(pin_detail_schema), c("total_mv", "mv_difference"))))

  # Get range of rows in the PIN data + number of header rows
  num_head <- 6
  pin_row_range <- (num_head + 1):(nrow(assessment_pin_filtered) + num_head)
  pin_row_range_w_header <- c(num_head, pin_row_range)
  pin_col_range <- seq_along(pin_detail_schema)

  assessment_pin_w_row_ids <- assessment_pin_filtered %>%
    tibble::rowid_to_column("row_id") %>%
    mutate(row_id = row_id + num_head)

  # Calculate MVs so we can store them as separate, hidden columns for use
  # in the neighborhood breakouts pivot table
  assessment_pin_mvs <- assessment_pin_w_row_ids %>%
    mutate(
      total_mv = glue::glue("={fmv_round_col}{row_id}"),
      mv_difference = glue::glue(
        "=({fmv_round_col}{row_id}) - ({prior_near_tot_col}{row_id})"
      )
    ) %>%
    select(total_mv, mv_difference)

  # Calculate sales ratios, and use a formula so that they update dynamically
  # if the spreadsheet user updates the FMV
  assessment_pin_sale_ratios <- assessment_pin_w_row_ids %>%
    mutate(
      sale_ratio = glue::glue(
        '=IF(ISBLANK({sale1_price_col}{row_id}), "",',
        " {fmv_round_col}{row_id} / {sale1_price_col}{row_id})"
      )
    )

  # Mark AV fields and sales ratio fields as formulas, since these fields
  # compute values based on other fields
  class(assessment_pin_mvs$total_mv) <- c(
    class(assessment_pin_mvs$total_mv), "formula"
  )
  class(assessment_pin_mvs$mv_difference) <- c(
    class(assessment_pin_mvs$mv_difference), "formula"
  )
  class(assessment_pin_sale_ratios$sale_ratio) <- c(
    class(assessment_pin_sale_ratios$sale_ratio), "formula"
  )

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

  class(assessment_pin_filtered$meta_pin10) <- c(
    class(assessment_pin_filtered$meta_pin10), "formula"
  )
  class(assessment_pin_filtered$meta_pin) <- c(
    class(assessment_pin_filtered$meta_pin), "formula"
  )

  # Load the excel workbook template from file
  wb <- loadWorkbook(here("misc", "desk_review_template.xlsx"))

  # Create formatting styles
  wb_styles <- list(
    price = createStyle(numFmt = "$#,##0"),
    "2digit_price" = createStyle(numFmt = "$#,##0.00"),
    "2digit_num" = createStyle(numFmt = "0.00"),
    pct = createStyle(numFmt = "PERCENTAGE"),
    comma = createStyle(numFmt = "COMMA"),
    link = createStyle(fontColour = "blue", textDecoration = "underline")
  )

  # Add styles to PIN sheet using schema
  for (style_name in names(wb_styles)) {
    style_cols <- cols_with_style(pin_detail_schema, style_name)
    if (length(style_cols) == 0) next
    addStyle(
      wb, pin_sheet_name,
      style = wb_styles[[style_name]],
      rows = pin_row_range, cols = style_cols, gridExpand = TRUE
    )
  }
  addFilter(wb, pin_sheet_name, num_head, pin_col_range)

  # Format YoY % change column with a range of colors from low to high
  walk(
    cols_with_cond(pin_detail_schema, "color_scale"),
    ~ conditionalFormatting(
      wb, pin_sheet_name,
      cols = .x,
      rows = pin_row_range,
      style = c("#F8696B", "#FFFFFF", "#00B0F0"),
      rule = c(-1, 0, 1),
      type = "colourScale"
    )
  )
  # Format sale such that they are orange for adjusted multi-PIN sales
  conditionalFormatting(
    wb, pin_sheet_name,
    cols = cols_with_cond(pin_detail_schema, "sale_outlier_1"),
    rows = pin_row_range,
    style = createStyle(bgFill = "#FFCC99"),
    rule = paste0("$", sale1_num_parcels_col, num_head + 1, "=2"),
    type = "expression"
  )
  conditionalFormatting(
    wb, pin_sheet_name,
    cols = cols_with_cond(pin_detail_schema, "sale_outlier_2"),
    rows = pin_row_range,
    style = createStyle(bgFill = "#FFCC99"),
    rule = paste0("$", sale2_num_parcels_col, num_head + 1, "=2"),
    type = "expression"
  )

  # Format sale columns such that they are red if the sale has an outlier flag
  conditionalFormatting(
    wb, pin_sheet_name,
    cols = cols_with_cond(pin_detail_schema, "sale_outlier_1"),
    rows = pin_row_range,
    style = createStyle(bgFill = "#FF9999"),
    rule = paste0("$", sale1_outlier_col, num_head + 1, '!=""'),
    type = "expression"
  )
  # For some reason vector cols don't work with expressions, so we have
  # to duplicate the conditional formatting for the sale outlier flag above
  # to apply it to the second range of columns
  conditionalFormatting(
    wb, pin_sheet_name,
    cols = cols_with_cond(pin_detail_schema, "sale_outlier_2"),
    rows = pin_row_range,
    style = createStyle(bgFill = "#FF9999"),
    rule = paste0("$", sale2_outlier_col, num_head + 1, '!=""'),
    type = "expression"
  )

  # Write PIN-level data to workbook
  writeData(
    wb, pin_sheet_name, assessment_pin_filtered,
    startCol = 1, startRow = num_head + 1, colNames = FALSE
  )

  # Write formulas and headers to workbook
  writeFormula(
    wb, pin_sheet_name,
    assessment_pin_filtered$meta_pin,
    startCol = col_pos(pin_detail_schema, "meta_pin"),
    startRow = num_head + 1
  )
  writeFormula(
    wb, pin_sheet_name,
    assessment_pin_filtered$meta_pin10,
    startCol = col_pos(pin_detail_schema, "meta_pin10"),
    startRow = num_head + 1
  )
  writeFormula(
    wb, pin_sheet_name,
    assessment_pin_sale_ratios$sale_ratio,
    startCol = col_pos(pin_detail_schema, "sale_ratio"),
    startRow = num_head + 1
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
    startCol = col_pos(pin_detail_schema, "prior_near_land"),
    startRow = num_head - 1, colNames = FALSE
  )
  writeData(
    wb, pin_sheet_name, tibble(model_header),
    startCol = col_pos(pin_detail_schema, "pred_pin_final_fmv"),
    startRow = num_head - 1, colNames = FALSE
  )

  # Write hidden formulas
  writeFormula(
    wb, pin_sheet_name,
    assessment_pin_mvs$total_mv,
    startCol = col_pos(pin_detail_schema, "total_mv"),
    startRow = num_head + 1
  )
  writeFormula(
    wb, pin_sheet_name,
    assessment_pin_mvs$mv_difference,
    startCol = col_pos(pin_detail_schema, "mv_difference"),
    startRow = num_head + 1
  )
  hidden_cols <- cols_hidden(pin_detail_schema)
  setColWidths(
    wb, pin_sheet_name,
    hidden_cols,
    widths = rep(1, length(hidden_cols)),
    hidden = rep(TRUE, length(hidden_cols)), ignoreMergedCells = FALSE
  )

  # Add a named range for the PIN-level data, which the template will use
  # to populate the Neighborhood Breakouts pivot table
  createNamedRegion(
    wb, pin_sheet_name,
    cols = pin_col_range, rows = pin_row_range_w_header,
    name = "pin_detail_range", overwrite = TRUE
  )

  # 5.2. Building-Level --------------------------------------------------------

  # Get range of rows in the building data + number of header rows
  num_head_bldg <- 4
  bldg_row_range <- (num_head_bldg + 1):(nrow(assessment_pin10_filtered) + num_head_bldg)

  # Add styles to bldg sheet using schema
  for (style_name in names(wb_styles)) {
    style_cols <- cols_with_style(bldg_schema, style_name)
    if (length(style_cols) == 0) next
    addStyle(
      wb, bldg_sheet_name,
      style = wb_styles[[style_name]],
      rows = bldg_row_range, cols = style_cols, gridExpand = TRUE
    )
  }
  addFilter(wb, bldg_sheet_name, num_head_bldg, seq_along(bldg_schema))

  # Format YoY % change column with a range of colors from low to high
  walk(
    cols_with_cond(bldg_schema, "color_scale"),
    ~ conditionalFormatting(
      wb, bldg_sheet_name,
      cols = .x,
      rows = bldg_row_range,
      style = c("#F8696B", "#FFFFFF", "#00B0F0"),
      rule = c(-1, 0, 1),
      type = "colourScale"
    )
  )

  # Write bldg-level data to workbook
  writeData(
    wb, bldg_sheet_name, assessment_pin10_filtered,
    startCol = 1, startRow = num_head_bldg + 1, colNames = FALSE
  )

  # Write formulas and headers to workbook
  writeData(
    wb, bldg_sheet_name, tibble(comp_header),
    startCol = col_pos(bldg_schema, "prior_near_bldg_total"),
    startRow = num_head_bldg - 1, colNames = FALSE
  )
  writeData(
    wb, bldg_sheet_name, tibble(model_header),
    startCol = col_pos(bldg_schema, "pred_pin_final_fmv_bldg_total"),
    startRow = num_head_bldg - 1, colNames = FALSE
  )

  # Save workbook to file based on town name
  workbook_name <- glue(
    params$assessment$year,
    str_replace(town_convert(town), " ", "_"),
    "Initial_Model_Values_Condo.xlsx",
    .sep = "_"
  )
  saveWorkbook(
    wb, here("output", "desk_review", workbook_name),
    overwrite = TRUE
  )
  rm(wb)
}

### NOTE ###
# OpenXLSX is not perfect and messes up the macros and formatting on saved
# workbooks. To finish each workbook, you must manually:

# 1. Open the Neighborhood Breakout sheet and ensure that the values are
#    all formatted correctly in the pivot table; if not (e.g. if
#    `Average of YoY ∆ %` is formatted as a date when it should be a percentage)
#    then manually update the formatting by selecting
#    PivotTable Fields > Values > {fieldname} > Value Field Settings... >
#    Number Format.


#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# 6. Prep iasWorld Upload ------------------------------------------------------
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
  mutate(
    # For PINs missing an individual building value, fill with the average of
    # PINs with the same proration rate in the building. This is super rare,
    # maybe 1 PIN out of every 100K. It happens mostly because of mis-coded
    # nbhds
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
    pred_pin10_final_fmv_bldg = round(
      pred_pin10_final_fmv_bldg * (1 / sum(
        meta_tieback_proration_rate,
        na.rm = TRUE
      ))
    ),
    # For any missing LLINE values, simply fill with 1
    meta_lline_num = replace_na(meta_lline_num, 1)
  ) %>%
  ungroup() %>%
  select(
    township_code = meta_township_code,
    PARID = meta_pin,
    CARD = meta_card_num,
    LLINE = meta_lline_num,
    MV = pred_pin10_final_fmv_bldg
  ) %>%
  arrange(township_code, PARID)


#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# 7. Export iasWorld Upload ----------------------------------------------------
#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Write each town to a headerless CSV for mass upload
for (town in unique(upload_data_prepped$township_code)) {
  message("Now processing: ", town_convert(town))

  upload_data_fil <- upload_data_prepped %>%
    filter(township_code == town, MV > 0, !is.na(MV)) %>%
    select(-township_code)

  readr::write_csv(
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
