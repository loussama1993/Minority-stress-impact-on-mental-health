# Load required libraries
library(lavaan)
library(semTools)
library(parallel)

# Function definitions for sensitivity analysis of self-esteem mediation
test_sensitivity_selfesteem <- function(dhe_var, full_covariates = FALSE) {
  if(full_covariates) {
    # Full model
    model <- paste0('
      # Direct effect
      phq9_score ~ c*', dhe_var, ' + age + gender_binary + region + 
                  what_is_your_sexual_orientation_ + 
                  what_sex_were_you_assigned_at_birth_ +
                  what_is_your_employment_status_ +
                  have_you_ever_been_consulted_by_a_mental_health_professional__psychiatrist___psychologist___ +
                  have_you_ever_taken_an_antidepressant_or_anxiolytic_treatment__
      
      # Mediator
      i_have_high_self_esteem ~ a*', dhe_var, ' + age + gender_binary + region + 
                  what_is_your_sexual_orientation_ + 
                  what_sex_were_you_assigned_at_birth_ +
                  what_is_your_employment_status_ +
                  have_you_ever_been_consulted_by_a_mental_health_professional__psychiatrist___psychologist___ +
                  have_you_ever_taken_an_antidepressant_or_anxiolytic_treatment__
      
      phq9_score ~ b*i_have_high_self_esteem
      
      # Indirect effect (a*b)
      indirect := a*b
      total := c + (a*b)
    ')
  } else {
    # Current model
    model <- paste0('
      # Direct effect
      phq9_score ~ c*', dhe_var, ' + cv2*region + 
                  cv3*what_is_your_employment_status_ + 
                  cv4*what_is_your_sexual_orientation_
      
      # Mediator
      i_have_high_self_esteem ~ a*', dhe_var, ' + cv2*region + 
                  cv3*what_is_your_employment_status_ + 
                  cv4*what_is_your_sexual_orientation_
      
      phq9_score ~ b*i_have_high_self_esteem
      
      # Indirect effect (a*b)
      indirect := a*b
      total := c + (a*b)
    ')
  }
  
  # Specify categorical variables
  categorical <- c("region", "gender_binary", "what_is_your_sexual_orientation_",
                   "what_sex_were_you_assigned_at_birth_",
                   "what_is_your_employment_status_",
                   "have_you_ever_been_consulted_by_a_mental_health_professional__psychiatrist___psychologist___",
                   "have_you_ever_taken_an_antidepressant_or_anxiolytic_treatment__")
  
  fit <- sem(model, data = data_new,
             ordered = categorical,
             estimator = "DWLS")
  
  # Model fit checks
  fit_indices <- fitMeasures(fit, c("chisq", "df", "pvalue", "cfi", "tli", "rmsea", "srmr"))
  mod_indices <- modificationIndices(fit)
  
  # Monte Carlo CI for indirect effects
  mc_ci <- monteCarloCI(fit, nRep=1000)
  
  params <- parameterEstimates(fit, standardized = TRUE)
  key_params <- params[params$label %in% c("a", "b", "c", "indirect", "total"),
                       c("label", "est", "std.all", "pvalue", "ci.lower", "ci.upper")]
  
  return(list(
    parameters = key_params,
    fit_indices = fit_indices,
    modification_indices = mod_indices,
    monte_carlo_ci = mc_ci
  ))
}

# Function for sensitivity analysis of depression-suicide mediation
test_sensitivity_depression <- function(dhe_var, full_covariates = FALSE) {
  if(full_covariates) {
    # Full model
    model <- paste0('
      # Direct effect
      suicide_score ~ c*', dhe_var, ' + age + gender_binary + region + 
                  what_is_your_sexual_orientation_ + 
                  what_sex_were_you_assigned_at_birth_ +
                  what_is_your_employment_status_ +
                  have_you_ever_been_consulted_by_a_mental_health_professional__psychiatrist___psychologist___ +
                  have_you_ever_taken_an_antidepressant_or_anxiolytic_treatment__
      
      # Mediator
      phq9_score ~ a*', dhe_var, ' + age + gender_binary + region + 
                  what_is_your_sexual_orientation_ + 
                  what_sex_were_you_assigned_at_birth_ +
                  what_is_your_employment_status_ +
                  have_you_ever_been_consulted_by_a_mental_health_professional__psychiatrist___psychologist___ +
                  have_you_ever_taken_an_antidepressant_or_anxiolytic_treatment__
      
      suicide_score ~ b*phq9_score
      
      # Indirect effect (a*b)
      indirect := a*b
      total := c + (a*b)
    ')
  } else {
    # Current model
    model <- paste0('
      # Direct effect
      suicide_score ~ c*', dhe_var, ' + cv2*region + 
                  cv4*what_is_your_sexual_orientation_
      
      # Mediator
      phq9_score ~ a*', dhe_var, ' + cv2*region + 
                  cv4*what_is_your_sexual_orientation_
      
      suicide_score ~ b*phq9_score
      
      # Indirect effect (a*b)
      indirect := a*b
      total := c + (a*b)
    ')
  }
  
  # Specify categorical variables
  categorical <- c("region", "gender_binary", "what_is_your_sexual_orientation_",
                   "what_sex_were_you_assigned_at_birth_",
                   "what_is_your_employment_status_",
                   "have_you_ever_been_consulted_by_a_mental_health_professional__psychiatrist___psychologist___",
                   "have_you_ever_taken_an_antidepressant_or_anxiolytic_treatment__")
  
  fit <- sem(model, data = data_new,
             ordered = categorical,
             estimator = "DWLS")
  
  # Model fit checks
  fit_indices <- fitMeasures(fit, c("chisq", "df", "pvalue", "cfi", "tli", "rmsea", "srmr"))
  mod_indices <- modificationIndices(fit)
  
  # Monte Carlo CI for indirect effects
  mc_ci <- monteCarloCI(fit, nRep=1000)
  
  params <- parameterEstimates(fit, standardized = TRUE)
  key_params <- params[params$label %in% c("a", "b", "c", "indirect", "total"),
                       c("label", "est", "std.all", "pvalue", "ci.lower", "ci.upper")]
  
  return(list(
    parameters = key_params,
    fit_indices = fit_indices,
    modification_indices = mod_indices,
    monte_carlo_ci = mc_ci
  ))
}

# Function to run sensitivity analyses
run_sensitivity_analyses <- function(dhe_var) {
  # Run current model with checks
  selfesteem_current <- test_sensitivity_selfesteem(dhe_var, FALSE)
  depression_current <- test_sensitivity_depression(dhe_var, FALSE)
  
  # Run full model with checks
  selfesteem_full <- test_sensitivity_selfesteem(dhe_var, TRUE)
  depression_full <- test_sensitivity_depression(dhe_var, TRUE)
  
  list(
    selfesteem = list(current = selfesteem_current, full = selfesteem_full),
    depression = list(current = depression_current, full = depression_full)
  )
}

# Set up parallel processing
cores <- detectCores() - 1
cl <- makeCluster(cores)

# Load required packages in cluster
clusterEvalQ(cl, {
  library(lavaan)
  library(semTools)
})

# Export necessary objects to cluster
clusterExport(cl, c("data_new", "test_sensitivity_selfesteem", 
                    "test_sensitivity_depression", "run_sensitivity_analyses",
                    "dhe_subscales"))

# Run analyses for all DHE subscales
results <- parLapply(cl, dhe_subscales, run_sensitivity_analyses)
names(results) <- dhe_subscales

# Print results with formatting
for(dhe_var in dhe_subscales) {
  cat("\n\n==============================================")
  cat("\nResults for", dhe_var)
  cat("\n==============================================")
  
  cat("\n\nSELF-ESTEEM MEDIATION")
  cat("\n----------------------")
  cat("\nCurrent model:\n")
  print(results[[dhe_var]]$selfesteem$current$parameters)
  cat("\nFit Indices:\n")
  print(results[[dhe_var]]$selfesteem$current$fit_indices)
  cat("\nMonte Carlo CI for indirect effect:\n")
  print(results[[dhe_var]]$selfesteem$current$monte_carlo_ci)
  
  cat("\nFull model:\n")
  print(results[[dhe_var]]$selfesteem$full$parameters)
  cat("\nFit Indices:\n")
  print(results[[dhe_var]]$selfesteem$full$fit_indices)
  cat("\nMonte Carlo CI for indirect effect:\n")
  print(results[[dhe_var]]$selfesteem$full$monte_carlo_ci)
  
  cat("\n\nDEPRESSION-SUICIDE MEDIATION")
  cat("\n---------------------------")
  cat("\nCurrent model:\n")
  print(results[[dhe_var]]$depression$current$parameters)
  cat("\nFit Indices:\n")
  print(results[[dhe_var]]$depression$current$fit_indices)
  cat("\nMonte Carlo CI for indirect effect:\n")
  print(results[[dhe_var]]$depression$current$monte_carlo_ci)
  
  cat("\nFull model:\n")
  print(results[[dhe_var]]$depression$full$parameters)
  cat("\nFit Indices:\n")
  print(results[[dhe_var]]$depression$full$fit_indices)
  cat("\nMonte Carlo CI for indirect effect:\n")
  print(results[[dhe_var]]$depression$full$monte_carlo_ci)
}

# Stop cluster
stopCluster(cl)

