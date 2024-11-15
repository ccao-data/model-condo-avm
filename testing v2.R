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
toc()


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

comparison_result <- assessment_data_pred %>%
  inner_join(assessment_data_pred_v2, by = "meta_pin", suffix = c("_v2", "_v1")) %>%
  mutate(
    match_pred_card_initial_fmv = pred_card_initial_fmv_v2 == pred_card_initial_fmv_v1
  ) %>%
  select(meta_pin, pred_card_initial_fmv_v2, pred_card_initial_fmv_v1, match_pred_card_initial_fmv) %>%
  mutate(difference = pred_card_initial_fmv_v2 - pred_card_initial_fmv_v1)
