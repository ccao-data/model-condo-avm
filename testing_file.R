## I have been running this in the Assess stage of the pipeline

assessment_data_pred <- read_parquet(paths$input$assessment$local) %>%
  as_tibble() %>%
  # Create a column which will not be modified by the pipeline
  mutate(test1 = meta_strata_1,
         test2 = meta_strata_2)

# Bake the recipe from the data and return all columns
baked_predictors <- bake(
  lgbm_final_full_recipe,
  new_data = assessment_data_pred,
  everything()
)

# There will be two meta_strata_1 columns, so we remove it from the first
# dataset
assessment_data_pred <- assessment_data_pred %>%
  select(-c("meta_strata_1", "meta_strata_2"))

# Join the two datasets together.
test_join <- baked_predictors %>%
  # It seems as though a lot of meta_pin values are lost in the process
  # even though the observations stay the same
  inner_join(assessment_data_pred, by = "meta_pin") %>%
  select(meta_pin, meta_strata_1, meta_strata_2, test1, test2)

# Ideally all kept values of meta_strata_1 and test_1 should match
