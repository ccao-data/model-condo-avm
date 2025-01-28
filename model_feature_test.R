
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
  )



pins_to_track <- c("1722304059", "2003305043")


