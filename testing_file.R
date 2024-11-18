assessment_data_pred <- read_parquet(paths$input$assessment$local) %>%
  as_tibble() %>%
  # Bake the data first and extract meta strata columns
  {
    baked_data <- bake(lgbm_final_full_recipe, new_data = ., all_predictors())
    mutate(
      .,
      pred_card_initial_fmv = as.numeric(predict(
        lgbm_final_full_fit,
        new_data = baked_data
      )$.pred),
      # Some strata are imputed during the baking process
      # so we extract all values.
      temp_strata_1 = baked_data$meta_strata_1,
      temp_strata_2 = baked_data$meta_strata_2
    )
  }

# For the lightgbm model, values are recoded to a 0 based scale.
# This means that these values are a 1:1 match with values of a
# different scale. Because of this, we map values to our original
# calculations for continuity.
strata_mapping_1 <- assessment_data_pred %>%
  filter(!is.na(meta_strata_1)) %>%
  distinct(temp_strata_1, meta_strata_1) %>%
  with(setNames(meta_strata_1, temp_strata_1))

strata_mapping_2 <- assessment_data_pred %>%
  filter(!is.na(meta_strata_2)) %>%
  distinct(temp_strata_2, meta_strata_2) %>%
  with(setNames(meta_strata_2, temp_strata_2))

# Apply mappings
assessment_data_pred <- assessment_data_pred %>%
  mutate(
    # Binary variable to identify condos which have imputed strata
    flag_strata_is_imputed = ifelse(is.na(meta_strata_1), 1, 0),
    # Use mappings to replace meta_strata_1 and meta_strata_2 directly
    # Unname removes the previously encoded information for clarity
    meta_strata_1 = unname(strata_mapping_1[as.character(temp_strata_1)]),
    meta_strata_2 = unname(strata_mapping_2[as.character(temp_strata_2)])
  ) %>%
  # Remove duplicated columns
  select(-temp_strata_1, -temp_strata_2)


assessment_data_pred_old <- read_parquet(paths$input$assessment$local) %>%
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

# Perform the comparison
comparison_result <- assessment_data_pred %>%
  inner_join(assessment_data_pred_old, by = "meta_pin", suffix = c("_new", "_old")) %>%
  mutate(
    match_pred_card_initial_fmv = pred_card_initial_fmv_new == pred_card_initial_fmv_old,
    match_meta_strata_1 = ifelse(
      !is.na(meta_strata_1_new) & !is.na(meta_strata_1_old),
      meta_strata_1_new == meta_strata_1_old,
      NA
    ),
    match_meta_strata_2 = ifelse(
      !is.na(meta_strata_2_new) & !is.na(meta_strata_2_old),
      meta_strata_2_new == meta_strata_2_old,
      NA
    )
  ) %>%
  select(
    meta_pin, meta_strata_1_new, meta_strata_1_old,
    meta_strata_2_new, meta_strata_2_old,
    pred_card_initial_fmv_new, pred_card_initial_fmv_old,
    match_pred_card_initial_fmv, match_meta_strata_1, match_meta_strata_2
  )

# Print the result
print(comparison_result)

