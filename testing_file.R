assessment_data_pred <- read_parquet(paths$input$assessment$local) %>%
  as_tibble() %>%
  mutate(test1 = meta_strata_1,
         test2 = meta_strata_2)

baked_predictors <- bake(
  lgbm_final_full_recipe,
  new_data = assessment_data_pred,
  everything()
)

assessment_data_pred <- assessment_data_pred %>%
  select(-c("meta_strata_1", "meta_strata_2"))

test_join <- baked_predictors %>%
  inner_join(assessment_data_pred, by = "meta_pin") %>%
  select(meta_pin, meta_strata_1, meta_strata_2, test1, test2)

