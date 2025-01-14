library(pwr)




# Calculate f² from your actual R²
f2_actual <- 0.4097 / (1 - 0.4097)  # = 0.694

# Current model power analysis
current_power <- pwr.f2.test(
  u = 18,                    # predictors
  v = 166 - 18 - 1,         # df error (n - predictors - 1)
  f2 = f2_actual,
  sig.level = 0.05)

# Calculate power for conventional effect sizes
power_small <- pwr.f2.test(
  u = 18,
  v = 166 - 18 - 1,
  f2 = 0.02,                # small effect
  sig.level = 0.05)

power_medium <- pwr.f2.test(
  u = 18,
  v = 166 - 18 - 1,
  f2 = 0.15,                # medium effect
  sig.level = 0.05)

power_large <- pwr.f2.test(
  u = 18,
  v = 166 - 18 - 1,
  f2 = 0.35,                # large effect
  sig.level = 0.05)

print(current_power)
print(power_small)
print(power_medium)
print(power_large)



















# Function for precise small sample power analysis
small_sample_power <- function(dhe_var, n_sim = 1000) {
  # Parameter lookup for different DHE variables
  params_lookup <- list(
    isolation_mean = list(a = -0.210, b = -2.695, c = 0.977),
    harassment_mean = list(a = -0.139, b = -2.964, c = 0.937),
    vigilance_mean = list(a = 0.058, b = -3.029, c = 0.966),
    gender_expression_mean = list(a = -0.142, b = -3.196, c = 0.082),
    vicarious_mean = list(a = -0.318, b = -2.957, c = 0.345),
    family_mean = list(a = 0.032, b = -3.188, c = 1.084),
    victimization_mean = list(a = -0.165, b = -3.143, c = 0.673)
  )
  
  params <- params_lookup[[dhe_var]]
  n <- 65  # Your exact sample size
  
  sig_a <- sig_b <- sig_c <- sig_indirect <- 0
  convergence_count <- 0
  
  for(j in 1:n_sim) {
    # Generate data
    dhe <- rnorm(n, 0, 1)
    selfesteem <- params$a * dhe + rnorm(n, 0, sqrt(0.8))
    depression <- params$c * dhe + params$b * selfesteem + rnorm(n, 0, sqrt(0.7))
    
    data_sim <- data.frame(dhe = dhe, selfesteem = selfesteem, depression = depression)
    
    model <- '
      selfesteem ~ a*dhe
      depression ~ b*selfesteem + c*dhe
      indirect := a*b
    '
    
    tryCatch({
      fit <- sem(model, data = data_sim)
      results <- parameterEstimates(fit)
      
      sig_a <- sig_a + (results$pvalue[results$label == "a"] < 0.05)
      sig_b <- sig_b + (results$pvalue[results$label == "b"] < 0.05)
      sig_c <- sig_c + (results$pvalue[results$label == "c"] < 0.05)
      sig_indirect <- sig_indirect + (results$pvalue[results$label == "indirect"] < 0.05)
      
      convergence_count <- convergence_count + 1
    }, error = function(e) NULL)
  }
  
  # Calculate power and confidence intervals
  power_a <- sig_a/n_sim
  power_b <- sig_b/n_sim
  power_c <- sig_c/n_sim
  power_indirect <- sig_indirect/n_sim
  
  # Calculate confidence intervals
  ci <- function(p) {
    ci <- binom.test(p * n_sim, n_sim)$conf.int
    return(c(ci[1], ci[2]))
  }
  
  result <- data.frame(
    DHE_Variable = dhe_var,
    Power_a = power_a,
    CI_lower_a = ci(power_a)[1],
    CI_upper_a = ci(power_a)[2],
    Power_indirect = power_indirect,
    CI_lower_indirect = ci(power_indirect)[1],
    CI_upper_indirect = ci(power_indirect)[2],
    Convergence_Rate = convergence_count/n_sim
  )
  
  return(result)
}

# Run analysis for all variables
dhe_variables <- c("vigilance_mean", "harassment_mean", "gender_expression_mean",
                   "vicarious_mean", "family_mean", "victimization_mean", 
                   "isolation_mean")

# Use parallel processing to speed up computation
library(parallel)
cores <- min(detectCores() - 1, length(dhe_variables))
cl <- makeCluster(cores)
clusterExport(cl, c("small_sample_power"))
clusterEvalQ(cl, {
  library(lavaan)
})

results <- do.call(rbind, parLapply(cl, dhe_variables, small_sample_power))
stopCluster(cl)

# Format results as percentage
results_formatted <- results
percent_cols <- grep("Power|CI", names(results_formatted))
results_formatted[,percent_cols] <- round(results_formatted[,percent_cols] * 100, 1)

# Print results
print("Power Analysis Results for N=65:")
print("--------------------------------")
print(results_formatted)



































































# Define parameter lookup for all subscales
params_lookup <- list(
  isolation_mean = list(
    se = list(a = -0.210, b = -2.695, c = 0.977),
    ds = list(a = 1.588, b = 0.285, c = 0.784)
  ),
  harassment_mean = list(
    se = list(a = -0.139, b = -2.964, c = 0.937),
    ds = list(a = 1.311, b = 0.298, c = 0.652)
  ),
  vigilance_mean = list(
    se = list(a = 0.058, b = -3.029, c = 0.966),
    ds = list(a = 0.888, b = 0.320, c = 0.266)
  ),
  gender_expression_mean = list(
    se = list(a = -0.142, b = -3.196, c = 0.082),
    ds = list(a = 0.439, b = 0.317, c = 0.691)
  ),
  vicarious_mean = list(
    se = list(a = -0.318, b = -2.957, c = 0.345),
    ds = list(a = 1.387, b = 0.331, c = -0.182)
  ),
  family_mean = list(
    se = list(a = 0.032, b = -3.188, c = 1.084),
    ds = list(a = 0.982, b = 0.296, c = 0.566)
  ),
  victimization_mean = list(
    se = list(a = -0.165, b = -3.143, c = 0.673),
    ds = list(a = 1.027, b = 0.308, c = 0.893)
  )
)

small_sample_power <- function(dhe_var, n_sim = 1000) {
  params <- params_lookup[[dhe_var]]
  n <- 65  
  
  # Initialize counters
  sig_se_a <- sig_se_b <- sig_se_c <- sig_se_indirect <- sig_se_total <- 0
  sig_ds_a <- sig_ds_b <- sig_ds_c <- sig_ds_indirect <- sig_ds_total <- 0
  convergence_count <- 0
  
  # Debug first iteration
  debug <- TRUE
  
  for(j in 1:n_sim) {
    tryCatch({
      # Generate data with proper scaling
      dhe <- scale(rnorm(n))
      
      # Self-esteem mediation with proper error terms
      selfesteem <- dhe * params$se$a + scale(rnorm(n))
      phq9_se <- dhe * params$se$c + selfesteem * params$se$b + scale(rnorm(n))
      
      # Depression-suicide mediation
      phq9_ds <- dhe * params$ds$a + scale(rnorm(n))
      suicide <- dhe * params$ds$c + phq9_ds * params$ds$b + scale(rnorm(n))
      
      # Create datasets
      data_se <- data.frame(dhe = as.vector(dhe), 
                            selfesteem = as.vector(selfesteem), 
                            phq9 = as.vector(phq9_se))
      data_ds <- data.frame(dhe = as.vector(dhe), 
                            phq9 = as.vector(phq9_ds), 
                            suicide = as.vector(suicide))
      
      if(debug && j == 1) {
        cat("\nFirst iteration data check for", dhe_var, ":")
        cat("\nSE model correlations:")
        print(cor(data_se))
        cat("\nDS model correlations:")
        print(cor(data_ds))
      }
      
      # Fit models
      model_se <- '
        selfesteem ~ a*dhe
        phq9 ~ b*selfesteem + c*dhe
        indirect := a*b
        total := c + (a*b)
      '
      
      model_ds <- '
        phq9 ~ a*dhe
        suicide ~ b*phq9 + c*dhe
        indirect := a*b
        total := c + (a*b)
      '
      
      # Fit models with debugging
      fit_se <- sem(model_se, data = data_se)
      if(debug && j == 1) {
        cat("\nSE model fit:")
        print(summary(fit_se))
      }
      
      fit_ds <- sem(model_ds, data = data_ds)
      if(debug && j == 1) {
        cat("\nDS model fit:")
        print(summary(fit_ds))
      }
      
      # Get results
      results_se <- parameterEstimates(fit_se)
      results_ds <- parameterEstimates(fit_ds)
      
      # Check significance using z-values
      sig_se_a <- sig_se_a + (abs(results_se$z[results_se$label == "a"]) > 1.96)
      sig_se_b <- sig_se_b + (abs(results_se$z[results_se$label == "b"]) > 1.96)
      sig_se_c <- sig_se_c + (abs(results_se$z[results_se$label == "c"]) > 1.96)
      sig_se_indirect <- sig_se_indirect + (abs(results_se$z[results_se$label == "indirect"]) > 1.96)
      sig_se_total <- sig_se_total + (abs(results_se$z[results_se$label == "total"]) > 1.96)
      
      sig_ds_a <- sig_ds_a + (abs(results_ds$z[results_ds$label == "a"]) > 1.96)
      sig_ds_b <- sig_ds_b + (abs(results_ds$z[results_ds$label == "b"]) > 1.96)
      sig_ds_c <- sig_ds_c + (abs(results_ds$z[results_ds$label == "c"]) > 1.96)
      sig_ds_indirect <- sig_ds_indirect + (abs(results_ds$z[results_ds$label == "indirect"]) > 1.96)
      sig_ds_total <- sig_ds_total + (abs(results_ds$z[results_ds$label == "total"]) > 1.96)
      
      convergence_count <- convergence_count + 1
      
    }, error = function(e) {
      if(debug && j == 1) {
        cat("\nError in iteration", j, ":", conditionMessage(e))
      }
    })
    
    if(j %% 100 == 0) cat("\nCompleted iteration", j, "for", dhe_var)
  }
  
  # Create results dataframe
  result <- data.frame(
    DHE_Variable = dhe_var,
    SE_Power_a = sig_se_a/n_sim,
    SE_Power_b = sig_se_b/n_sim,
    SE_Power_c = sig_se_c/n_sim,
    SE_Power_indirect = sig_se_indirect/n_sim,
    SE_Power_total = sig_se_total/n_sim,
    DS_Power_a = sig_ds_a/n_sim,
    DS_Power_b = sig_ds_b/n_sim,
    DS_Power_c = sig_ds_c/n_sim,
    DS_Power_indirect = sig_ds_indirect/n_sim,
    DS_Power_total = sig_ds_total/n_sim,
    Convergence_Rate = convergence_count/n_sim
  )
  
  return(result)
}

# Run for all DHE variables
dhe_variables <- names(params_lookup)

# Use parallel processing
library(parallel)
cores <- min(detectCores() - 1, length(dhe_variables))
cl <- makeCluster(cores)

# Export necessary functions and libraries
clusterExport(cl, c("small_sample_power", "params_lookup"))
clusterEvalQ(cl, {
  library(lavaan)
})

# Run analyses
results <- do.call(rbind, parLapply(cl, dhe_variables, small_sample_power))

# Stop cluster
stopCluster(cl)

# Format results as percentages
results_formatted <- results
percent_cols <- grep("Power|Rate", names(results_formatted))
results_formatted[,percent_cols] <- round(results_formatted[,percent_cols] * 100, 1)

# Print results
print(results_formatted)


# Function to calculate CI
calculate_ci <- function(power, n_sim) {
  ci <- binom.test(power * n_sim, n_sim)$conf.int * 100
  return(ci)
}

# Create dataframe for CIs
ci_results <- data.frame(
  DHE_Variable = results_formatted$DHE_Variable
)

# Calculate CIs for all power estimates
for(col in names(results_formatted)) {
  if(grepl("Power", col)) {
    # Lower CI
    ci_results[[paste0(col, "_CI_lower")]] <- sapply(results_formatted[[col]], 
                                                     function(x) calculate_ci(x/100, 1000)[1])
    # Upper CI
    ci_results[[paste0(col, "_CI_upper")]] <- sapply(results_formatted[[col]], 
                                                     function(x) calculate_ci(x/100, 1000)[2])
  }
}

# Round CI values
ci_cols <- grep("CI", names(ci_results))
ci_results[,ci_cols] <- round(ci_results[,ci_cols], 1)

# Merge with original results
final_results <- merge(results_formatted, ci_results, by="DHE_Variable")

# Print final results with CIs
print(final_results)

