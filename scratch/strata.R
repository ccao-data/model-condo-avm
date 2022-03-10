# Pre-allocate memory for java JDBC driver
options(java.parameters = "-Xmx10g")
Sys.setenv(R_INSTALL_STAGED = FALSE)

# Load R libraries
library(arrow)
library(ccao)
library(DBI)
library(data.table)
library(dplyr)
library(glue)
library(here)
library(igraph)
library(lubridate)
library(purrr)
library(RJDBC)
library(s2)
library(sf)
library(tictoc)
library(tidyr)
library(yaml)
source(here("R", "helpers.R"))

# Initialize a dictionary of file paths. See R/file_dict.csv for details
paths <- model_file_dict()

# Load the parameters file containing the run settings
params <- read_yaml("params.yaml")

# Functions to create and assign strata
val_create_ntiles <- function(x, probs, na.rm = TRUE) { # nolint
  stopifnot(
    is.numeric(x),
    is.numeric(probs),
    is.logical(na.rm)
  )
  
  output <- list(c(
    -Inf,
    unique(stats::quantile(x, probs = probs, na.rm = na.rm, names = FALSE)),
    Inf
  ))
  output <- ifelse(all(is.na(x)), list(NA_real_), output)
  
  return(output)
}

val_assign_ntile <- function(x, ntiles) {
  stopifnot(
    is.numeric(x),
    is.list(ntiles)
  )
  
  output <- ifelse(
    !is.na(x),
    purrr::pmap_chr(
      list(num = x, cuts = ntiles),
      function(num, cuts) as.character(cut(num, breaks = cuts, dig.lab = 10))
    ),
    NA_character_
  )
  
  return(output)
}

# Load data created in 00-ingest.R
training_data_clean <- read_parquet(here("input", "training_data.parquet"))
assessment_data_clean <- read_parquet(here("input", "assessment_data.parquet"))

# Condominiums characteristics (such as square footage, number of bedrooms, etc)
# are not tracked by the CCAO. We need to rely on other information to
# determine the value of unsold condos. Fortunately, condos are more homogenous
# than single-family homes and are pre-grouped into like units (buildings).

# As such, we can use the historic sale price of other sold units in the same
# building to determine an unsold condo's value. The goal of this script is to
# construct building-level summary stats (averages), as well as strata which can
# be applied to unsold buildings

# The first step here is to get the average sale price of condos in each
# building. The first 10 digits of a given PIN are the building (the last 4 are
# the unit). Here, we get the leave-one-out building average sale price for the
# 5 years prior to the assessment year. The leave-one-out average is the
# building average EXCLUDING the current PIN/sale. This is done to prevent data
# leakage between the outcome and predictor variables while training

bldg_avg_wo_target <- training_data_clean %>%
  group_by(meta_pin10) %>%
  
  # We also exclude any properties with less than 1 sale in the building
  filter(n() - 1) > 0, meta_year >= params$input$min_sale_year) %>%
  mutate(
    meta_sale_avg_wo_target = 
      (sum(meta_sale_price) - meta_sale_price) / (n() - 1)
  ) %>%
  ungroup() %>%
  select(
    meta_pin, meta_pin10, meta_township_code,
    meta_sale_date, meta_sale_avg_wo_target
  )

# Including the leave-one-out mean as a model feature is possible, but it tends
# to overfit. As such, we need to discretize the average building sale price
# into bins (called strata internally). 
bldg_strata <- bldg_avg_wo_target %>%  
  summarize(
    meta_strata_groups_10 = val_create_ntiles(
      x = meta_sale_avg_wo_target,
      probs = seq(0.1, 0.9, 0.1)
    ),
    meta_strata_groups_300 = val_create_ntiles(
      x = meta_sale_avg_wo_target,
      probs = seq(0.003333, 0.996666, 0.003333)
    )
  ) 

# Here we get just the normal building-level average sale price. This will be
# used when predicting on the assessment data
bldg_avg_w_target <- training_data_clean %>%
  group_by(meta_pin10) %>%
  filter(meta_year >= params$input$min_sale_year) %>%
  summarise(meta_sale_avg_w_target = mean(meta_sale_price, na.rm = TRUE))

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
##### Gather Data #####
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Create strata of building-level, previous-5-year sale prices. These strata are
# used as categorical variables in the model

# Attach each strata level then save training_data to file
# This file is then loaded by the model.R script contained here:
# https://gitlab.com/ccao-data-science---modeling/models/ccao_condo_avm
training_data_clean <- training_data_clean %>%
  left_join(bldg_avg_w_target) %>%
  bind_cols(bldg_strata) %>% 
  mutate(
    meta_strata_10 = val_assign_ntile(
      meta_sale_avg_w_target,
      meta_strata_groups_10
    ),
    meta_strata_300 = val_assign_ntile(
      meta_sale_avg_w_target,
      meta_strata_groups_300
    )
  ) %>%
  select(
    -meta_sale_avg_w_target,
    -meta_strata_groups_10, 
    -meta_strata_groups_300
  )

# Save cleaned assmntdata to parquet file
# This file is then loaded by the valuation.R script contained here:
# https://gitlab.com/ccao-data-science---modeling/models/ccao_condo_avm
assessment_data_clean <- assessment_data_clean %>%
  left_join(bldg_avg_w_target) %>%
  bind_cols(bldg_strata) %>% 
  mutate(
    meta_strata_10 = val_assign_ntile(
      meta_sale_avg_w_target,
      meta_strata_groups_10
    ),
    meta_strata_300 = val_assign_ntile(
      meta_sale_avg_w_target,
      meta_strata_groups_300
    )
  ) %>%
  select(
    -meta_sale_avg_w_target,
    -meta_strata_groups_10, 
    -meta_strata_groups_300
  )