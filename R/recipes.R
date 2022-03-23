#' Create the main data cleaning Tidymodels recipe
#'
#' @description Main data pre-processing recipe. These steps are applied to the
#' training set and any new data fed to the model (using bake()). Goal of this
#' recipe is to create a clean matrix with few/no missing values, well-behaved
#' distributions, and cleaned up categorical variables
#'
#' @param data A data frame containing the input data. Can contain extraneous
#'   columns and missing values.
#' @param pred_vars Character vector of column names to keep for modeling. These
#'   will be the right-hand side of the regression AKA predictors.
#' @param cat_vars Character vector of categorical column names. These will be
#'   integer-encoded (base 0).
#' @param knn_vars Character vector of column names. These columns will have
#'   missing values imputed via KNN.
#' @param knn_imp_vars Character vector of column names. These columns will be
#'   used to impute the columns in knn_vars.
#' @param id_vars Character vector of ID variables. These can be kept in "baked"
#'   data without being treated as predictors.
#'
#' @return A recipe object that can be used to clean model input data.
#'
model_main_recipe <- function(data, pred_vars, cat_vars,
                              knn_vars, knn_imp_vars, id_vars) {
  recipe(data) %>%
    
    # Set the role of each variable in the input data
    update_role(meta_sale_price, new_role = "outcome") %>%
    update_role(all_of(pred_vars), new_role = "predictor") %>%
    update_role(all_of(id_vars), new_role = "ID") %>%
    
    # Remove any variables not an outcome var or in the pred_vars vector
    step_rm(-all_outcomes(), -all_predictors(), -has_role("ID")) %>%
    
    # Replace novel levels with "new"
    step_novel(all_of(cat_vars), -has_role("ID")) %>%
    
    # Impute missing values using KNN. Specific to condo model, usually used to
    # impute missing condo building strata
    step_impute_knn(
      all_of(knn_vars),
      neighbors = 20,
      impute_with = imp_vars(all_of(knn_imp_vars)),
      options = list(
        nthread = parallel::detectCores(logical = FALSE),
        eps = 1e-08
      )
    ) %>%
    
    # Replace NA in factors with "unknown" 
    step_unknown(all_of(cat_vars), -has_role("ID")) %>%
    
    # Convert factors to 0-indexed integers
    step_integer(
      all_of(cat_vars), -has_role("ID"),
      strict = TRUE, zero_based = TRUE
    )
}
