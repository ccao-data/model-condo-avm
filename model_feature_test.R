
units <- dbGetQuery(
  conn = AWS_ATHENA_CONN_NOCTUA, glue("select unitno, parid, adrno
 from iasworld.pardat
 where taxyr = '2025'
 and class = '299'"
))

second_highest <- function(x) {
  unique_vals <- sort(unique(x), decreasing = TRUE) # Sort unique values in descending order
  if (length(unique_vals) >= 2) {
    return(unique_vals[2]) # Return the second value if it exists
  } else {
    return(NA) # Return NA if there's no second-highest value
  }
}

assessment_data_working <- assessment_data %>%
  filter(year == '2023',
         meta_modeling_group == 'CONDO') %>%
  select(meta_pin10, char_building_units, meta_pin)


units_working <- assessment_data_working %>%
  left_join(units, by = c("meta_pin" = "parid")) %>%
  group_by(meta_pin10) %>%
  # There are condos which are distinct plots (single family units) which have
  # unit numbers which relate to a plot number. We don't have a clean way of doing this
  # so we filter to two addresses. An example is Pin10 0324102014
  mutate(distinct_addresses = n_distinct(adrno)) %>%
  rename(pin = meta_pin, pin10 = meta_pin10) %>%
  filter(!is.na(unitno), distinct_addresses <= 2) %>%
  mutate(
    # Remove non-alphanumeric characters except `/` and `-`
    unitno_cleaned = gsub("[^A-Za-z0-9/-]", "", unitno),
    # # Remove leading letters if followed by a number
    # unitno_cleaned = sub("^[A-Za-z]+(?=[0-9])", "", unitno_cleaned, perl = TRUE),
    part_before_dash = sub("-.*", "", unitno),
    # Remove the part before the `-` if it matches `adrno`
    # For example addresses may be 100 W Ashland Unit 100-11
    unitno_cleaned = ifelse(
      part_before_dash == adrno,
      sub(".*?-", "", unitno_cleaned),
      unitno_cleaned
    ),
    # Always keep the part before `/` or `-`
    # Addresses are often coded as Unit 101 - 102
    unitno_cleaned = sub("[/-].*", "", unitno_cleaned),
    unitno_cleaned = ifelse(unitno_cleaned == adrno, NA, unitno_cleaned)
  ) %>%
  select(-part_before_dash) %>%
  # Define unit lengths for if-else statements
  mutate(unit_length = nchar(unitno_cleaned),
         numeric_unit_length = nchar(gsub("[A-Za-z]", "", unitno_cleaned))) %>%
  group_by(pin10) %>%
  mutate(
    max_numeric_length = ifelse(char_building_units >= 2, max(nchar(gsub("[A-Za-z]", "", unitno_cleaned))), NA),
    min_numeric_length = ifelse(char_building_units >= 2, min(nchar(gsub("[A-Za-z]", "", unitno_cleaned))), NA),
    numeric_length_all_3 = ifelse(char_building_units >= 2, all(nchar(gsub("[A-Za-z]", "", unitno_cleaned)) == 3), NA),
    # Compute the 95th percentile for numeric lengths within each pin10 group
    # This helps clear out addresses where they may be 4 digits, where the vast majority
    # are 3 digits
    numeric_length_95th = ifelse(char_building_units >= 2, quantile(numeric_unit_length, 0.95, na.rm = TRUE), NA)
  ) %>%
  ungroup() %>%
  mutate(
    is_ground = grepl("(?i)gr|ground|gf", unitno), # Check for "ground"
    is_basement = grepl("(?i)bsmt|basement!grdn", unitno), # Check for "bsmt" or "basement"
    floor = case_when(
      is_ground ~ "0", # Assign "0" for ground
      is_basement ~ "-1", # Assign "-1" for basement
      # If all units have 3 digits, then we create an indicator based on the first digit
      # 101 is 1, 405 would be 4.
      max_numeric_length == 3 & numeric_unit_length == 3 ~ substr(gsub("[^0-9]", "", unitno_cleaned), 1, 1),
      # Indicator for straightforward assumptions
      unitno_cleaned == "1" | unitno_cleaned == "A" | unitno_cleaned == "1A" ~ "1",
      # Single digit followed by a letter 1_A should be floor 1
      grepl("^[0-9][A-Za-z]$", unitno_cleaned) ~ substr(unitno_cleaned, 1, 1),
      # If there's a leading 0, then we assume the floors can roll-over to 10.
      # 0934 would be 9, while 1034 would be coded with a different technique.
      grepl("^0[0-9][0-9]+$", unitno_cleaned) ~ substr(unitno_cleaned, 2, 2),
      # Two digits followed by a letter, take the first two digits
      grepl("^[1-9][0-9][A-Za-z]$", unitno_cleaned) ~ substr(unitno_cleaned, 1, 2),
      # 4 Digit numbers 1204 should have a floor of 12
      max_numeric_length == 4 & numeric_unit_length == 4 ~ substr(gsub("[^0-9]", "", unitno_cleaned), 1, 2),
      # When the max numeric length is 4 and the particular unit is a length of 3, we assume that it should be
      # the 1st digit (913) is 9. This matches with the previous indicator.
      max_numeric_length == 4 & numeric_unit_length == 3 ~ substr(gsub("[^0-9]", "", unitno_cleaned), 1, 1),
      # Max length of 3, and there is only one digit then we assume that the floor is 0
      max_numeric_length == 3 & numeric_unit_length == 1 ~ "0",
      # If the length is 2, then we assume that the first digit is the floor (94 → 9)
      max_numeric_length == 3 & numeric_unit_length == 2 ~ substr(gsub("[^0-9]", "", unitno_cleaned), 1, 1),
      # If the length is 3, take the first two digits as the floor (e.g., 123 → 12)
      max_numeric_length == 3 & numeric_unit_length == 3 ~ substr(gsub("[^0-9]", "", unitno_cleaned), 1, 2),
      # When the max numeric length is 2 and the particular unit is a length of 2, we assume that it should be
      # the 1st digit (32 → 3).
      max_numeric_length == 2 & numeric_unit_length == 2 ~ substr(gsub("[^0-9]", "", unitno_cleaned), 1, 1),
      # When the max numeric length is 2 and the particular unit is a length of 1, we assume that it should be
      # 0 since the floor is empty (9 → 0), 19 → 1.
      max_numeric_length == 2 & numeric_unit_length == 1 ~ "0",
      TRUE ~ NA_character_ # Default to NA for other cases
    ),
    reason = case_when(
      is_ground ~ "Unit is on the ground floor",
      is_basement ~ "Unit is in the basement",
      max_numeric_length == 3 & numeric_unit_length == 3 ~ "Unit number has three digits, using the first digit as the floor",
      unitno_cleaned == "1" | unitno_cleaned == "A" | unitno_cleaned == "1A" ~ "Unit number is a known default for floor 1",
      grepl("^[0-9][A-Za-z]$", unitno_cleaned) ~ "Unit number consists of a digit followed by a letter, using the first digit as the floor",
      grepl("^0[0-9][0-9]+$", unitno_cleaned) ~ "Unit number has a leading zero, using the second digit as the floor",
      grepl("^[1-9][0-9][A-Za-z]$", unitno_cleaned) ~ "Unit number consists of two digits followed by a letter, using the first two digits as the floor",
      max_numeric_length == 4 & numeric_unit_length == 4 ~ "Unit number has four digits, using the first two digits as the floor",
      max_numeric_length == 4 & numeric_unit_length == 3 ~ "Unit number has three digits out of a four-digit max, using the first digit as the floor",
      max_numeric_length == 3 & numeric_unit_length == 1 ~ "Unit number is a single digit, assigning floor 0",
      max_numeric_length == 3 & numeric_unit_length == 2 ~ "Unit number has two digits, using the first digit as the floor",
      max_numeric_length == 3 & numeric_unit_length == 3 ~ "Unit number has three digits, using the first two digits as the floor",
      max_numeric_length == 2 & numeric_unit_length == 2 ~ "Unit number has two digits, using the first digit as the floor",
      max_numeric_length == 2 & numeric_unit_length == 1 ~ "Unit number has a single digit with max length 2, assuming floor 0",
      TRUE ~ "No matching condition"
    )
  ) %>%
  group_by(pin10) %>%
  # Test to see if there are any values which are 'clearly' incorrect
  mutate(top_floor_difference = max(as.numeric(floor), na.rm = TRUE) - second_highest(as.numeric(floor)),
         floor = as.numeric(floor))



pins_to_track <- c("1722304059",
                   "2003305043 - address followed by dashes",
                   "17221040381056 - 91st floor, no idea why",
                   "14283130541067 - correct unitno with same address",
                   "0917419041, building number before unit number",
                   "14171010631001 - Incorrect address before unit_no",
                   "03024100831140 - same address with unit relating to plot",
                   "0224104048 - don't know why it's not recoding")

Questions_to_answer <- c("What to do with 1 / 2 digit buildings",
                         "What to do with Letters followed by numbers A-1, G-8, these go quite hight",
                         "filter units by floor by pin10 and if it's less than 5 we don't assign")

