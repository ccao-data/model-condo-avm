# Main data prep recipe, removes unnecessary variables, cleans up factors,
# and logs certain skewed numeric variables
mod_recp_prep <- function(data, keep_vars, id_vars) {
  recipe(meta_sale_price ~ ., data = data) %>%
    
    # Convert PIN to numeric so it's not set to NA by missing factor levels
    # Set PIN role to "id" so it's not included in the actual fit
    update_role(any_of(id_vars), new_role = "id") %>%
    step_mutate_at(has_role("id"), fn = ~ as.numeric(as.character(.))) %>%
    step_rm(-any_of(keep_vars), -all_outcomes(), -has_role("id")) %>%
    
    # Preprocessing for all predictors and outcome
    step_unknown(all_nominal(), -has_role("id")) %>%
    step_log(
      all_outcomes(), ends_with("_sf"),
      contains("income"), contains("meta_nbhd_"), contains("meta_bldg_"),
      offset = 1
    ) 
}
