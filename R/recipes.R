# Main data prep recipe, removes unnecessary variables, cleans up factors,
# and logs certain skewed numeric variables
mod_recp_prep <- function(data, keep_vars, id_vars) {
  recipe(meta_sale_price ~ ., data = data) %>%
    
    # Convert PIN to numeric so it's not set to NA by missing factor levels
    # Set PIN role to "id" so it's not included in the actual fit
    update_role(any_of(id_vars), new_role = "id") %>%
    step_mutate_at(has_role("id"), fn = ~ as.numeric(as.character(.))) %>%
    step_rm(-any_of(keep_vars), -all_outcomes(), -has_role("id")) %>%
    
    # Replace NA in factors with "Unknown"
    step_unknown(all_nominal(), -has_role("id")) %>%
    
    # Drop any remaining rows with missing values in their predictors. Note that
    # this will be skipped when baking on new data, so the # of predicted values
    # will always be equal to the number of rows in the input data, regardless
    # of whether the input data has missing values
    step_naomit(
      all_predictors(), all_outcomes(),
      skip = TRUE
    ) %>%
    
    # Log transform price and income variables (these are all extremely
    # positively skewed). Likely not necessary for lightgbm but helps for linear
    # model
    step_log(
      all_outcomes(),
      ends_with("_sf"),
      contains("income"),
      offset = 1
    ) 
}


# Extra recipe step for converting categoricals to one-hot encoding. Only used
# in cases where categoricals are not handled natively such as linear models
dummy_recp_prep <- function(recipe) {
  recipe %>%
    step_mutate_at(starts_with("ind_"), fn = ~ as.numeric(.)) %>%
    step_dummy(all_nominal(), -all_outcomes(), -has_role("id"))
}
