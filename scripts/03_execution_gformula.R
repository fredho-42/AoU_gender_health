# Setup and Libraries ----
library(arrow)
library(tidyverse)
library(furrr)

## Multithreading ----
# FORCE single-threaded math for each worker
# This prevents the "Thread Explosion"
Sys.setenv(OMP_NUM_THREADS = "1")
Sys.setenv(MKL_NUM_THREADS = "1")
Sys.setenv(OPENBLAS_NUM_THREADS = "1")
Sys.setenv(VECLIB_MAXIMUM_THREADS = "1")
Sys.setenv(NUMEXPR_NUM_THREADS = "1")

# Load custom regression engines
source('./scripts/02_functions.R')

# Set parallel threads
n_threads <- 40

## Load Data
dat <- read_parquet('./data/cohort_cleaned_20260226.parquet')

# Input definition ----

## Outcomes ----

outcomes_numeric <- c(
  "general_health", "social_roles", "gph_t", "gmh_t", 
  "drinks_per_week",
  "bmi", "waist_cm", "whtr", "whr", "sbp", "dbp"
)

outcomes_binary <- c(
  'smoking_b', 'vaping_b', 
  "bmi_b", "whtr_b", "whr_b", "hyt_b", 
  
  "hypertension", "chd",
  "atrial_fibrillation", "heart_failure", "stroke_tia", 
  "diabetes", "ckd", "asthma", "copd", 
  "anxiety_depression", "psychosis_bipolar",
  "dementia", "epilepsy", 
  "cancer", "chronic_pain", "ibs", "constipation", 
  "hearing_loss", "alcohol_disorder", "connective_tissue", 
  "mm", 
  
  'hyt_uncontrolled', 'hyt_undetected'
)

# Combine into a single tracking dataframe
outcome_grid <- bind_rows(
  tibble(outcome = outcomes_numeric, type = "numeric"),
  tibble(outcome = outcomes_binary, type = "binary")
)

## Target Contrasts ----
target_contrasts <- c(
  # Comparisons against Cis man
  "Trans woman - Cis man",
  "Trans man - Cis man",
  "Diverse, female at birth - Cis man",
  "Diverse, male at birth - Cis man",
  
  # Comparisons against Cis woman
  "Trans woman - Cis woman",
  "Trans man - Cis woman",
  "Diverse, female at birth - Cis woman",
  "Diverse, male at birth - Cis woman"
)

## Covariates ----
base_covariates <- c("age_c", "race_ethnicity", "census_division")
sens_covariates <- c('education_b', 'employment_status', 'deprivation_quintile')


# Primary Analysis Execution ----
plan(multisession, workers = n_threads)

results_list <- future_map2(
  .x = outcome_grid$outcome,
  .y = outcome_grid$type,
  .f = ~run_regression(
    target_outcome = .x, 
    model_type = .y, 
    data = dat, 
    covars = base_covariates, 
    target_pairs = target_contrasts
  ),
  .progress = T,  
  .options = furrr_options(seed = TRUE) 
)

plan(sequential); gc()

## Extract and Save ----
df_raw_means <- map_dfr(results_list, "raw_means")
df_contrasts <- map_dfr(results_list, "contrasts") 

write_csv(df_raw_means, './results/table_gform_raw_means.csv')
write_csv(df_contrasts, './results/table_gform_contrasts.csv')


# Sensitivity Analysis Execution ----
plan(multisession, workers = n_threads)

results_list_sens <- future_map2(
  .x = outcome_grid$outcome,
  .y = outcome_grid$type,
  .f = ~run_regression(
    target_outcome = .x, 
    model_type = .y, 
    data = dat, 
    covars = c(base_covariates, sens_covariates), 
    target_pairs = target_contrasts
  ),
  .progress = T,  
  .options = furrr_options(seed = TRUE) 
)

plan(sequential); gc()

## Extract and Save ----
df_raw_means_sens <- map_dfr(results_list_sens, "raw_means")
df_contrasts_sens <- map_dfr(results_list_sens, "contrasts") 

write_csv(df_raw_means_sens, './results/table_gform_raw_means_sens.csv')
write_csv(df_contrasts_sens, './results/table_gform_contrasts_sens.csv')