# Function to calculate effect size for Wilcoxon tests
wilcox_effect_size <- function(variable, group, data) {
  if(length(unique(group)) != 2) {
    stop("Group variable must have exactly 2 levels for Wilcoxon test")
  }
  test <- wilcox.test(variable ~ group, data=data, exact=FALSE)
  z_score <- qnorm(test$p.value/2)
  r <- abs(z_score/sqrt(nrow(data)))
  return(list(test = test, effect_size = r))
}

# Function to calculate effect size for Kruskal-Wallis
# Eta-squared = (H - k + 1)/(n - k), where H is KW statistic, k is number of groups, n is sample size
kw_effect_size <- function(variable, group, data) {
  test <- kruskal.test(variable ~ group, data=data)
  k <- length(unique(group))
  n <- length(variable)
  eta_sq <- max(0, (test$statistic - k + 1)/(n - k))  # Prevent negative values
  return(list(test = test, effect_size = eta_sq))
}

# For data_cleaned
cleaned_effects <- list(
  # Binary comparisons (r)
  phq9_gender = wilcox_effect_size(data_cleaned$phq9_score, data_cleaned$gender_binary, data_cleaned),
  phq9_region = wilcox_effect_size(data_cleaned$phq9_score, data_cleaned$region, data_cleaned),
  
  # Multiple groups (eta-squared)
  phq9_sex = kw_effect_size(data_cleaned$phq9_score, data_cleaned$what_sex_were_you_assigned_at_birth_, data_cleaned),
  phq9_orientation = kw_effect_size(data_cleaned$phq9_score, data_cleaned$what_is_your_sexual_orientation_, data_cleaned),
  phq9_employment = kw_effect_size(data_cleaned$phq9_score, data_cleaned$what_is_your_employment_status_, data_cleaned)
)

# For data_new
new_effects <- list(
  
  # Binary comparisons for Suicide (r)
  suicide_gender = wilcox_effect_size(data_new$suicide_score, data_new$gender_binary, data_new),
  suicide_region = wilcox_effect_size(data_new$suicide_score, data_new$region, data_new),
  
  
  # Multiple groups for Suicide (eta-squared)
  suicide_sex = kw_effect_size(data_new$suicide_score, data_new$what_sex_were_you_assigned_at_birth_, data_new),
  suicide_orientation = kw_effect_size(data_new$suicide_score, data_new$what_is_your_sexual_orientation_, data_new),
  suicide_employment = kw_effect_size(data_new$suicide_score, data_new$what_is_your_employment_status_, data_new),
  suicide_relationship = kw_effect_size(data_new$suicide_score, data_new$what_is_your_relationship_status_, data_new),
  phq9_relationship = kw_effect_size(data_new$phq9_score, data_new$what_is_your_relationship_status_, data_new)
)

# Print results with interpretation
print_effect_sizes <- function(effects_list) {
  for(name in names(effects_list)) {
    cat("\n", name, ":\n")
    cat("p-value =", effects_list[[name]]$test$p.value, "\n")
    effect_size <- round(effects_list[[name]]$effect_size, 3)
    
    # Check if it's a binary comparison (those with "gender" or "region" in name)
    if(grepl("gender|region", name)) {
      # For Wilcoxon (r)
      cat("Effect size =", effect_size,
          "(r: small ≈ 0.1, medium ≈ 0.3, large ≈ 0.5)\n")
    } else {
      # For Kruskal-Wallis (eta-squared)
      cat("Effect size =", effect_size, 
          "(eta-squared: small ≈ 0.01, medium ≈ 0.06, large ≈ 0.14)\n")
    }
  }
}

cat("\nEffect sizes for data_cleaned:")
print_effect_sizes(cleaned_effects)

cat("\nEffect sizes for data_new:")
print_effect_sizes(new_effects)



# Additional effect size calculations for data_new
new_effects$suicide_mental_health = wilcox_effect_size(
  data_new$suicide_score,
  data_new$have_you_ever_been_consulted_by_a_mental_health_professional__psychiatrist___psychologist___,
  data_new
)

new_effects$suicide_medication = wilcox_effect_size(
  data_new$suicide_score,
  data_new$have_you_ever_taken_an_antidepressant_or_anxiolytic_treatment__,
  data_new
)

# Additional effect size calculations for data_cleaned
cleaned_effects$phq9_mental_health = wilcox_effect_size(
  data_cleaned$phq9_score,
  data_cleaned$have_you_ever_been_consulted_by_a_mental_health_professional__psychiatrist___psychologist___,
  data_cleaned
)

cleaned_effects$phq9_medication = wilcox_effect_size(
  data_cleaned$phq9_score,
  data_cleaned$have_you_ever_taken_an_antidepressant_or_anxiolytic_treatment__,
  data_cleaned
)

# Print the new results
cat("\nAdditional effect sizes for data_new:")
print_effect_sizes(list(
  suicide_mental_health = new_effects$suicide_mental_health,
  suicide_medication = new_effects$suicide_medication
))

cat("\nAdditional effect sizes for data_cleaned:")
print_effect_sizes(list(
  phq9_mental_health = cleaned_effects$phq9_mental_health,
  phq9_medication = cleaned_effects$phq9_medication
))
