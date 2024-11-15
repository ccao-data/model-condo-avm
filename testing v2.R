# These are pulled directly from the existing model pipeline. Aside from being
# renamed with v2, they *should* produce identical results.

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

assessment_data_pred_v2 <- read_parquet(paths$input$assessment$local) %>%
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

# Test to make sure join by meta_pin is distinct
meta_pin_counts <- assessment_data_pred_v2 %>%
  group_by(meta_pin) %>%
  summarise(count = n()) %>%
  filter(count > 1)

# This should produce an outcome with 0 values, i.e. all fmvs matching
comparison_result <- assessment_data_pred %>%
  inner_join(assessment_data_pred_v2, by = "meta_pin", suffix = c("_v2", "_v1")) %>%
  mutate(
    # It predicts on card level but based on distinct meta_pin? Am I missing something here?
    match_pred_card_initial_fmv = pred_card_initial_fmv_v2 == pred_card_initial_fmv_v1
  ) %>%
  select(meta_pin, pred_card_initial_fmv_v2, pred_card_initial_fmv_v1, match_pred_card_initial_fmv) %>%
  mutate(difference = pred_card_initial_fmv_v2 - pred_card_initial_fmv_v1) %>%
  filter(difference != 0)

# My output has 2,460 rows which don't match. Out of a dataset of ~450,000 it's not a huge number, but
# is confusing.
