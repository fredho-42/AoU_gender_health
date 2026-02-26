run_regression_fast <- function(target_outcome, model_type, data, covars, target_pairs) {
  require(mgcv)
  require(broom)
  require(dplyr)
  require(stringr)
  require(tidyr)
  
  # 1. Extract Variables & Clean Data
  safe_covars <- setdiff(covars, target_outcome)
  model_vars <- c(target_outcome, "gender_c", safe_covars)
  
  data_clean <- data %>%
    select(all_of(model_vars)) %>%
    drop_na() %>%
    mutate(across(all_of(safe_covars) & where(is.factor), fct_drop))
  
  # 2. SPLINE SETUP: Auto-detect numeric covariates with enough unique values
  is_splinable <- sapply(safe_covars, function(v) {
    is.numeric(data_clean[[v]]) && length(unique(data_clean[[v]])) > 5
  })
  
  num_covars <- safe_covars[is_splinable]
  cat_covars <- safe_covars[!is_splinable]
  
  # Wrap numeric variables in penalized splines
  if (length(num_covars) > 0) {
    spline_terms <- paste0("s(", num_covars, ", bs='ps', k=4)")
  } else {
    spline_terms <- character(0)
  }
  
  # Build Formula
  rhs_terms <- c("gender_c", cat_covars, spline_terms)
  rhs <- paste(rhs_terms, collapse = " + ")
  f <- as.formula(paste(target_outcome, "~", rhs))
  
## HELPER FUNCTION: Fit the model and extract coefficients ----
  fit_and_tidy <- function(ref_group) {
    
    # Relevel the data so the target reference group is the base (intercept)
    df_mod <- data_clean %>%
      mutate(gender_c = relevel(factor(gender_c), ref = ref_group))
    
    # Fit using mgcv::gam
    mod <- tryCatch({
      if (model_type == "numeric") {
        gam(f, data = df_mod, method = "REML")
      } else if (model_type == "binary") {
        gam(f, family = quasipoisson(link = "log"), data = df_mod, method = "REML")
      }
    }, error = function(e) { return(NULL) }) # Return NULL if it crashes
    
    if (is.null(mod)) return(NULL)
    
    # Extract coefficients cleanly (parametric = TRUE is required for GAMs)
    res <- broom::tidy(mod, parametric = TRUE, conf.int = TRUE) %>%
      filter(str_detect(term, "^gender_c")) %>%
      mutate(
        exposure = str_remove(term, "^gender_c"),
        reference = ref_group,
        contrast_clean = paste(exposure, "-", reference) # Matches your target_pairs format
      )
    
    return(res)
  }
  
  # 3. Run the model TWICE to get both sets of reference contrasts
  res_man <- fit_and_tidy("Cis man")
  res_woman <- fit_and_tidy("Cis woman")
  
  # If both models completely failed, return a neat error row
  if (is.null(res_man) && is.null(res_woman)) {
    error_df <- tibble(outcome = target_outcome, model_type = model_type, 
                       converged = FALSE, note = "Models failed to fit")
    return(list(raw_means = tibble(), contrasts = error_df))
  }
  
  # 4. Stack the two results together
  res_combined <- bind_rows(res_man, res_woman) %>%
    mutate(
      outcome = target_outcome,
      model_type = model_type,
      converged = TRUE
    )
  
  # 5. Transform the Math based on Model Type
  if (model_type == "binary") {
    # Modified Poisson: Exponentiate log-odds to get Prevalence Ratios
    res_combined <- res_combined %>%
      mutate(
        metric = "ratio",
        estimate = exp(estimate),
        ci_lower = exp(conf.low),
        ci_upper = exp(conf.high)
      )
  } else if (model_type == "numeric") {
    # Linear: Keep as Absolute Mean Differences
    res_combined <- res_combined %>%
      mutate(
        metric = "difference",
        ci_lower = conf.low,
        ci_upper = conf.high
      )
  }
  
  # 6. Tidy, filter to exact target pairs, and remove any duplicates
  contrasts_df <- res_combined %>%
    filter(contrast_clean %in% target_pairs) %>%
    select(outcome, model_type, converged, metric, exposure, reference, 
           estimate, ci_lower, ci_upper, p.value) %>%
    distinct() 
  
  raw_df <- tibble() 
  
  # Aggressive memory wipe
  rm(data_clean, res_man, res_woman, res_combined)
  gc(verbose = FALSE)
  
  return(list(raw_means = raw_df, contrasts = contrasts_df))
}


run_regression <- function(target_outcome, model_type, data, covars, target_pairs) {
  require(mgcv)
  require(marginaleffects)
  require(broom)
  require(dplyr)
  require(stringr)
  require(tidyr)
  
  safe_covars <- setdiff(covars, target_outcome)
  model_vars <- c(target_outcome, "gender_c", safe_covars)
  
  data_clean <- data %>%
    select(all_of(model_vars)) %>%
    drop_na() %>%
    mutate(across(all_of(safe_covars) & where(is.factor), fct_drop))  
  
  # 1. SPLINE SETUP: Auto-detect numeric covariates
  is_splinable <- sapply(safe_covars, function(v) {
    is.numeric(data_clean[[v]]) && length(unique(data_clean[[v]])) > 5
  })
  
  num_covars <- safe_covars[is_splinable]
  cat_covars <- safe_covars[!is_splinable]
  
  if (length(num_covars) > 0) {
    spline_terms <- paste0("s(", num_covars, ", bs='ps', k=4)")
  } else {
    spline_terms <- character(0)
  }
  
  # Primary Spline Formula (for mgcv::gam)
  rhs_terms <- c("gender_c", cat_covars, spline_terms)
  rhs <- paste(rhs_terms, collapse = " + ")
  f <- as.formula(paste(target_outcome, "~", rhs))
  
  # 2. SHOCK ABSORBER: Catch fatal errors and fit GAMs
  mod <- tryCatch({
    if (model_type == "numeric") {
      gam(f, data = data_clean, method = "REML")
    } else if (model_type == "binary") {
      gam(f, family = quasipoisson(link = "log"), data = data_clean, method = "REML")
    }
  }, error = function(e) {
    return(list(fatal_error = e$message)) 
  })
  
  if ("fatal_error" %in% names(mod)) {
    error_df <- tibble(outcome = target_outcome, model_type = model_type, 
                       converged = FALSE, note = mod$fatal_error)
    return(list(raw_means = error_df, contrasts = error_df))
  }
  
  # 3. CHECK CONVERGENCE STATUS
  is_converged <- mod$converged
  
  # 4. G-FORMULA: Calculate Marginal Means
  raw_means <- avg_predictions(mod, variables = "gender_c")
  
  raw_df <- as_tibble(raw_means) %>%
    mutate(
      outcome = target_outcome, 
      model_type = model_type,
      converged = is_converged 
    ) %>%
    select(outcome, model_type, converged, gender_c, estimate, conf.low, conf.high)
  
  # 5. G-FORMULA: Calculate BOTH Differences and Ratios
  comp_diff <- avg_comparisons(mod, variables = list(gender_c = "pairwise"), 
                               comparison = "difference") %>%
    as_tibble() %>%
    mutate(metric = "difference")
  
  comp_ratio <- avg_comparisons(mod, variables = list(gender_c = "pairwise"), 
                                comparison = "ratio") %>%
    as_tibble() %>%
    mutate(metric = "ratio")
  
  contrasts_raw <- bind_rows(comp_diff, comp_ratio)
  
  # 6. Tidy the output
  contrasts_df <- contrasts_raw %>%
    mutate(
      outcome = target_outcome,
      model_type = model_type,
      converged = is_converged,
      contrast_clean = str_remove_all(contrast, "mean\\(|\\)"),
      contrast_clean = str_replace_all(contrast_clean, "\\s*[/\\-]\\s*", " - "),      
      exposure = str_trim(str_split_i(contrast_clean, " - ", 1)),
      reference = str_trim(str_split_i(contrast_clean, " - ", 2))
    ) %>%
    filter(contrast_clean %in% target_pairs) %>%
    select(outcome, model_type, converged, metric, exposure, reference, 
           estimate, ci_lower = conf.low, ci_upper = conf.high, p.value)
  
  rm(data_clean, mod, raw_means, comp_diff, comp_ratio, contrasts_raw)
  gc(verbose = FALSE)
  
  return(list(raw_means = raw_df, contrasts = contrasts_df))
}


