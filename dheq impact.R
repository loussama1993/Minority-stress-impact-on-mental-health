# Multiple regression model with bootstrapped confidence intervals (5000 resamples)

model2 <- lm(phq9_score ~ gender_binary + region + 
               what_is_your_sexual_orientation_ + 
               what_sex_were_you_assigned_at_birth_ +
               what_is_your_employment_status_ +
               vigilance_mean + harassment_mean + gender_expression_mean +
               vicarious_mean + family_mean + victimization_mean + isolation_mean, 
             data = data_cleaned)

summary(model2)


# 1. Check multicollinearity
vif(model2)

# 2. Check linearity, homoscedasticity
par(mfrow=c(2,2))
plot(model2)

# 3. Test residual independence
library(lmtest)
dwtest(model2)

# 4. Check outliers and influence
data_cleaned$std.residuals <- rstandard(model2)
data_cleaned$cooks.distance <- cooks.distance(model2)

# Identify outliers (|standardized residuals| > 3)
outliers <- which(abs(data_cleaned$std.residuals) > 3)
print("Potential outliers:")
print(outliers)

# Identify influential cases (Cook's distance > 4/n)
n <- nrow(data_cleaned)
influential <- which(data_cleaned$cooks.distance > 4/n)
print("Influential cases:")
print(influential)

# 5. Check residual normality
hist(residuals(model2), breaks=20, main="Histogram of Residuals")
qqPlot(model2)

library(car)

# Perform bootstrapping using Boot function from car package
set.seed(123)
boot_model <- Boot(model2, R=5000)

# Get confidence intervals
boot_ci <- confint(boot_model, level=0.95, type="perc")

# Create summary table
boot_results <- data.frame(
  Estimate = coef(model2),
  CI_lower = boot_ci[,1],
  CI_upper = boot_ci[,2]
)

# View results
print(boot_results)










# First let's create a function to compare models
compare_models <- function(models_list) {
  # Store results
  results <- data.frame(
    Model = names(models_list),
    R2 = numeric(length(models_list)),
    Adj_R2 = numeric(length(models_list)),
    AIC = numeric(length(models_list)),
    BIC = numeric(length(models_list))
  )
  
  for(i in seq_along(models_list)) {
    results$R2[i] <- summary(models_list[[i]])$r.squared
    results$Adj_R2[i] <- summary(models_list[[i]])$adj.r.squared
    results$AIC[i] <- AIC(models_list[[i]])
    results$BIC[i] <- BIC(models_list[[i]])
  }
  
  return(results)
}

# Your current model
model_base <- lm(phq9_score ~ gender_binary + region + 
                   what_is_your_sexual_orientation_ + 
                   what_sex_were_you_assigned_at_birth_ +
                   what_is_your_employment_status_ +
                   vigilance_mean + harassment_mean + gender_expression_mean +
                   vicarious_mean + family_mean + victimization_mean + isolation_mean, 
                 data = data_cleaned)

# alternative models
model_expanded <- lm(phq9_score ~ age + gender_binary + region + 
                       what_is_your_sexual_orientation_ + 
                       what_sex_were_you_assigned_at_birth_ +
                       what_is_your_employment_status_ +
                       have_you_ever_been_consulted_by_a_mental_health_professional__psychiatrist___psychologist___ +
                       have_you_ever_taken_an_antidepressant_or_anxiolytic_treatment__ +
                       vigilance_mean + harassment_mean + gender_expression_mean +
                       vicarious_mean + family_mean + victimization_mean + isolation_mean, 
                    data = data_cleaned)


# Compare models
models_list <- list(
  base = model_base,
  expanded = model_expanded
)

comparison_results <- compare_models(models_list)
print(comparison_results)

# Coefficient stability
library(sandwich)
library(lmtest)

# Modified function focusing on robust estimates
coef_stability <- function(model) {
  # Original model summary
  orig_sum <- summary(model)
  
  # Original coefficients and standard errors
  orig_coef <- coef(model)
  orig_se <- orig_sum$coefficients[, "Std. Error"]
  
  # Get robust standard errors using HC3 estimator
  robust_se <- sqrt(diag(vcovHC(model, type = "HC3")))
  
  # Calculate robust t-values and p-values
  robust_t <- orig_coef / robust_se
  robust_p <- 2 * pt(abs(robust_t), df = df.residual(model), lower.tail = FALSE)
  
  # Combine results
  stability_df <- data.frame(
    Variable = names(orig_coef),
    Estimate = orig_coef,
    Std_Error = orig_se,
    Robust_SE = robust_se,
    Original_p = orig_sum$coefficients[, "Pr(>|t|)"],
    Robust_p = robust_p
  )
  
  return(stability_df)
}

# Run the analysis
stability_results <- coef_stability(model_base)
print(stability_results)

# Compare models using robust standard errors
anova_robust <- waldtest(model_base, model_expanded, vcov = vcovHC)
print("\nRobust ANOVA comparison:")
print(anova_robust)

# Calculate change in coefficients between models
common_vars <- intersect(names(coef(model_base)), names(coef(model_expanded)))
coef_changes <- data.frame(
  Variable = common_vars,
  Base_Estimate = coef(model_base)[common_vars],
  Expanded_Estimate = coef(model_expanded)[common_vars],
  Perc_Change = 100 * (coef(model_expanded)[common_vars] - coef(model_base)[common_vars]) / abs(coef(model_base)[common_vars])
)
print("\nCoefficient changes:")
print(coef_changes)

