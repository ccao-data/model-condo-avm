# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
##### Setup ####
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Load R libraries
options(tidymodels.dark = TRUE)
library(arrow) 
library(assessr)
library(beepr)
library(butcher)
library(ccao)
library(dplyr)
library(glmnet)
library(here)
library(lightgbm)
library(purrr)
library(sf)
library(stringr)
library(tictoc)
library(tidymodels)
library(treesnip)
library(vctrs)

# Load helper functions from file
source("R/recipes.R")
source("R/model_funs.R")

# Get number of available cores and number of threads to use per core
num_threads <- parallel::detectCores(logical = TRUE) - 1

# Start full script timer
tictoc::tic(msg = "Full Modeling Complete!")

# Set seed for reproducibility
set.seed(27)

# Toggle cross validation and set number of folds to use
cv_enable <- as.logical(model_get_env("R_CV_ENABLE", FALSE))
cv_write_params <- as.logical(model_get_env("R_CV_WRITE_PARAMS", FALSE))
cv_num_folds <- as.numeric(model_get_env("R_CV_NUM_FOLDS", 5))
cv_control <- control_bayes(verbose = TRUE, no_improve = 10, seed = 27)




# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
##### Prepare Data #####
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# List of variables that uniquely identify each structure or unit
mod_id_vars <- c("meta_pin")

# List RHS predictors. Condos don't have characteristics like SF homes do, so
# here we're mostly using meta characteristics like location
mod_predictors <- c(
  "meta_year", "meta_class", "meta_town_code",
  "meta_sale_month", "char_age", "econ_tax_rate", "econ_midincome",
  "meta_nbhd_med", "meta_bldg_med"
)

# Load the full set of training data, in this case NAs are kept in the data 
# since LightGBM can handle them
full_data <- read_parquet(here("input", "modeldata.parquet")) %>%
  arrange(meta_sale_date)

# Create train/test split by time, with most recent observations in the test set
# We want our best model(s) to be predictive of the future, since properties are
# assessed on the basis of past sales
time_split <- initial_time_split(full_data, prop = 0.80)
test <- testing(time_split)
train <- training(time_split)

# Create v-fold CV splits for the main training set
train_folds <- vfold_cv(train, v = cv_num_folds)

# Create a recipe for the training data which removes non-predictor columns,
# normalizes/logs data, and removes/imputes missing values
train_recipe <- mod_recp_prep(
  data = train,
  keep_vars = mod_predictors, 
  id_vars = mod_id_vars
)

# Extract number of cols used in model (P) and categorical column names
juiced_train <- juice(prep(train_recipe)) %>% 
  select(-any_of(c(mod_id_vars, "meta_sale_price")))

train_p <- ncol(juiced_train)
train_cat_vars <- juiced_train %>%
  select(where(is.factor)) %>%
  colnames()

# Remove unnecessary data
rm(time_split, juiced_train); gc()




# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
##### LightGBM Model #####
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

### Step 1 - Model initialization

# Set model params save path
lgbm_params_path <- here("output", "params", "lgbm_params.rds")

# Initialize lightbgm model specification
# Note that categorical vars must be explicitly specified for lightgbm
lgbm_model <- boost_tree(
  trees = tune(), tree_depth = tune(), min_n = tune(),
  loss_reduction = tune(), sample_size = tune(),
  mtry = tune(), learn_rate = tune()
) %>%
  set_engine("lightgbm") %>%
  set_mode("regression") %>%
  set_args(
    num_threads = num_threads,
    categorical_feature = train_cat_vars,
    verbose = -1 
  )

# Initialize lightgbm workflow
lgbm_wflow <- workflow() %>%
  add_model(lgbm_model) %>%
  add_recipe(train_recipe)


### Step 2 - Cross-validation

# Begin CV tuning if enabled
if (cv_enable) {

  # Create param search space for lgbm
  lgbm_params <- lgbm_model %>%
    parameters() %>%
    update(
      trees = trees(range = c(500, 1500)),
      mtry = mtry(c(5L, floor(train_p / 3))),
      min_n = min_n(),
      tree_depth = tree_depth(c(3L, 12L)),
      loss_reduction = loss_reduction(c(-3, 0.5)),
      learn_rate = learn_rate(c(-3, -0.3)),
      sample_size = sample_prop()
    )

  # Use Bayesian tuning to find best performing params
  tictoc::tic(msg = "LightGBM CV model fitting complete!")
  lgbm_search <- tune_bayes(
    object = lgbm_wflow,
    resamples = train_folds,
    initial = 5, iter = 50,
    param_info = lgbm_params,
    metrics = metric_set(rmse, rsq),
    control = cv_control
  )
  tictoc::toc()
  beepr::beep(2)

  # Save tuning results to file
  if (cv_write_params) {
    lgbm_search %>%
      model_axe_tune_data() %>%
      saveRDS(lgbm_params_path)
  }

  # Choose the best model that minimizes RMSE
  lgbm_final_params <- select_best(lgbm_search, metric = "rmse")
  
} else {

  # If no CV, load best params from file if exists, otherwise use defaults
  if (file.exists(lgbm_params_path)) {
    lgbm_final_params <- select_best(readRDS(lgbm_params_path), metric = "rmse")
  } else {
    lgbm_final_params <- list(
      trees = 1500, tree_depth = 5, min_n = 8, loss_reduction = 0.2613,
      mtry = 8, sample_size = 0.66, learn_rate = 0.0175
    )
  }
}


### Step 3 - Finalize model

# Fit the final model using the training data
lgbm_wflow_final_fit <- lgbm_wflow %>%
  finalize_workflow(as.list(lgbm_final_params)) %>%
  fit(data = train)

# Fit the final model using the full data, this is the model used for assessment
lgbm_wflow_final_full_fit <- lgbm_wflow %>%
  finalize_workflow(as.list(lgbm_final_params)) %>%
  fit(data = full_data)

# Remove unnecessary objects
rm_intermediate("lgbm")




# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
##### Finish Up #####
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Get predictions on the test set using the final model then save to file
test %>%
  mutate(
    pred = model_predict(
      lgbm_wflow_final_fit %>% pull_workflow_fit(),
      lgbm_wflow_final_fit %>% pull_workflow_prepped_recipe(),
      test
    )
  ) %>%
  write_parquet(here("output", "data", "testdata.parquet"))

# Save the finalized model object to file so it can be used elsewhere
list(
  "spec" = lgbm_wflow_final_full_fit %>%
    pull_workflow_fit(),
  "recipe" = lgbm_wflow_final_full_fit %>%
    pull_workflow_prepped_recipe() %>%
    model_axe_recipe()
  ) %>%
  model_save(here("output", "models", "stacked_model.zip"))

# Generate modeling diagnostic/performance report
rmarkdown::render(
  input = here("reports", "model_report.Rmd"),
  output_file = here("output", "reports", "model_report.html")
)

# Stop all timers
tictoc::toc()

# BIG BEEP
beepr::beep(8)
