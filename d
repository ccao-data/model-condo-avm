[1mdiff --git a/params.yaml b/params.yaml[m
[1mindex b558ddf..3870218 100644[m
[1m--- a/params.yaml[m
[1m+++ b/params.yaml[m
[36m@@ -93,13 +93,13 @@[m [mcv:[m
 [m
   # Number of initial iterations to create before tuning. Recommend this number[m
   # be greater than the number of hyperparameters being tuned[m
[31m-  initial_set: 20[m
[32m+[m[32m  initial_set: 2[m
   [m
   # Maximum number of search iterations[m
[31m-  max_iterations: 100[m
[32m+[m[32m  max_iterations: 5[m
   [m
   # Max number of search iterations without improvement before stopping search[m
[31m-  no_improve: 30[m
[32m+[m[32m  no_improve: 2[m
   [m
   # Metric used to select the "best" set of parameters from CV iterations. Must[m
   # be manually included the metric_set() passed to tune_bayes()[m
[36m@@ -227,8 +227,8 @@[m [mmodel:[m
   parameter:[m
     # Total number of iterations. Usually changed in tandem with learning_rate[m
     # https://lightgbm.readthedocs.io/en/latest/Parameters.html#num_iterations[m
[31m-    num_iterations: 1500[m
[31m-    learning_rate: 0.075[m
[32m+[m[32m    num_iterations: 15[m
[32m+[m[32m    learning_rate: 0.1[m
     [m
     # For CV only, proportion of the training data to hold out for use in [m
     # early stopping + the metric to evaluate. See R docs for details:[m
[1mdiff --git a/pipeline/00-ingest.R b/pipeline/00-ingest.R[m
[1mindex c9ecea6..fd9b9d6 100644[m
[1m--- a/pipeline/00-ingest.R[m
[1m+++ b/pipeline/00-ingest.R[m
[36m@@ -232,7 +232,7 @@[m [mtraining_data_clean <- training_data %>%[m
     time_sale_day = time_interval %/% days(1),[m
     [m
     # Get components of dates for fixed effects to correct seasonality[m
[31m-    time_sale_quarter_of_year = quarter(meta_sale_date),[m
[32m+[m[32m    time_sale_quarter_of_year = paste0("Q", quarter(meta_sale_date)),[m
     time_sale_day_of_year = day(meta_sale_date),[m
     [m
     # Time window to use for cross-validation and calculating spatial lags[m
