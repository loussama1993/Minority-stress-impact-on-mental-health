# Gender: Binary coding
data$gender_binary <- factor(ifelse(
  data$how_would_you_describe_yourself_today_ == "Cis Man" | 
    data$how_would_you_describe_yourself_today_ == "Cis Woman", 
  "Conforming", "Non-Conforming"),
  levels = c("Conforming", "Non-Conforming"))

# Sex assigned at birth
data$what_sex_were_you_assigned_at_birth_ <- factor(data$what_sex_were_you_assigned_at_birth_,
                            levels = c("Male", "Female", "Other"))

# Sexual orientation
data$what_is_your_sexual_orientation_ <- factor(data$what_is_your_sexual_orientation_,
                                  levels = c("Gay", "Lesbian", "Pansexual", "Queer", "Bisexual", "Other"))

# Employment status
data$what_is_your_employment_status_ <- factor(data$what_is_your_employment_status_,
                          levels = c("Full-Time Student", "Employed", "Unemployed"))

# Relationship status
data$what_is_your_relationship_status_ <- factor(data$what_is_your_relationship_status_,
                            levels = c("Single", "Committed Relationship", "Other"))

# Region
data$region <- factor(data$region,
                      levels = c("MENA region", "Western country"))


# Define variables by DHEQ subscales
dheq_vars <- list(
  # Vigilance
  vigilance = c(
    "watching_what_you_say_and_do_around_heterosexual_people",
    "pretending_that_you_have_an_opposite_sex_partner", 
    "pretending_that_you_are_heterosexual",
    "hiding_your_relationship_from_other_people",
    "avoiding_talking_about_your_current_or_past_relationships_when_you_are_at_work",
    "hiding_part_of_your_life_from_other_people"
  ),
  
  # Harassment/Discrimination
  harassment = c(
    "being_called_names_such_as__fag__or__dyke_",
    "people_staring_at_you_when_you_are_out_in_public_because_you_are_lgbtq_",
    "being_verbally_harassed_by_strangers_because_you_are_lgbtq_",
    "being_verbally_harassed_by_people_you_know_because_you_are_lgbtq_",
    "being_treated_unfairly_in_stores_or_restaurants_because_you_are_lgbtq_",
    "people_laughing_at_you_or_making_jokes_at_your_expense_because_you_are_lgbtq_"
  ),
  
  # Gender Expression
  gender_expression = c(
    "feeling_invisible_in_the_lgbtq__community_because_of_your_gender_expression",
    "being_harassed_in_public_because_of_your_gender_expression",
    "being_harassed_in_bathrooms_because_of_your_gender_expression",
    "feeling_like_you_don_t_fit_into_the_lgbtq__community_because_of_your_gender_expression",
    "difficulty_finding_clothes_that_you_are_comfortable_wearing_because_of_your_gender_expression",
    "being_misunderstood_by_people_because_of_your_gender_expression"
  ),
  
  # Vicarious Trauma
  vicarious = c(
    "hearing_about_lgbtq__people_you_know_being_treated_unfairly",
    "hearing_about_lgbtq__people_you_don_t_know_being_treated_unfairly",
    "hearing_about_hate_crimes__e_g___vandalism__physical_or_sexual_assault__that_happened_to_lgbtq__people_you_don_t_know",
    "hearing_other_people_being_called_names_such_as__fag__or__dyke_",
    "hearing_someone_make_jokes_about_lgbtq__people",
    "hearing_politicians_say_negative_things_about_lgbtq__people"
  ),
  
  # Family of Origin
  family = c(
    "family_members_not_accepting_your_partner_as_a_part_of_the_family",
    "your_family_avoiding_talking_about_your_lgbtq__identity",
    "being_rejected_by_your_mother_for_being_lgbt",
    "being_rejected_by_your_father_for_being_lgbtq_",
    "being_rejected_by_a_sibling_or_siblings_because_you_are_lgbtq_",
    "being_rejected_by_other_relatives_because_you_are_lgbtq_"
  ),
  
  # Victimization
  victimization = c(
    "being_punched__hit__kicked__or_beaten_because_you_are_lgbtq_",
    "being_assaulted_with_a_weapon_because_you_are_lgbtq_",
    "being_raped_or_sexually_assaulted_because_you_are_lgbtq_",
    "having_objects_thrown_at_you_because_you_are_lgbtq_"
  ),
  
  # Isolation
  isolation = c(
    "difficulty_finding_a_partner_because_you_are_lgbtq_",
    "difficulty_finding_lgbtq__friends",
    "having_very_few_people_you_can_talk_to_about_being_lgbtq_",
    "feeling_like_you_don_t_fit_in_with_other_lgbtq__people"
  )
)

# Function to calculate alpha for each subscale 
calculate_subscale_alpha <- function(data, vars) {
  alpha_result <- psych::alpha(data[, vars])
  return(list(
    alpha = alpha_result$total$raw_alpha,
    item_stats = alpha_result$item.stats
  ))
}

# Calculate alpha for each subscale - replace data with your dataset name
alphas <- lapply(dheq_vars, function(vars) {
  calculate_subscale_alpha(data, vars)  # Here data should match your dataset name
})

# Print results 
for(scale_name in names(alphas)) {
  cat("\n\n=== ", scale_name, " ===\n")
  cat("Alpha: ", round(alphas[[scale_name]]$alpha, 3), "\n\n")
  print(round(alphas[[scale_name]]$item_stats, 3))
}

# Create composite mean scores 
data$vigilance_mean <- rowMeans(data[, dheq_vars$vigilance], na.rm = TRUE)
data$harassment_mean <- rowMeans(data[, dheq_vars$harassment], na.rm = TRUE)
data$gender_expression_mean <- rowMeans(data[, dheq_vars$gender_expression], na.rm = TRUE)
data$vicarious_mean <- rowMeans(data[, dheq_vars$vicarious], na.rm = TRUE)
data$family_mean <- rowMeans(data[, dheq_vars$family], na.rm = TRUE)  
data$victimization_mean <- rowMeans(data[, dheq_vars$victimization], na.rm = TRUE)
data$isolation_mean <- rowMeans(data[, dheq_vars$isolation], na.rm = TRUE)

# Check descriptive statistics of the composite scores
summary_stats <- psych::describe(data[,c("vigilance_mean", 
                                         "harassment_mean",
                                         "gender_expression_mean",
                                         "vicarious_mean", 
                                         "family_mean",
                                         "victimization_mean",
                                         "isolation_mean")])

print(round(summary_stats, 3))

# Calculate quartiles and IQR for all subscales
quartile_stats <- sapply(data[,c("vigilance_mean", 
                                 "harassment_mean",
                                 "gender_expression_mean",
                                 "vicarious_mean", 
                                 "family_mean",
                                 "victimization_mean",
                                 "isolation_mean")], 
                         function(x) {
                           c(Q1 = quantile(x, 0.25),
                             Median = median(x),
                             Q3 = quantile(x, 0.75),
                             IQR = IQR(x))
                         })

# Round to 2 decimal places and print
print(round(quartile_stats, 2))

# Check normality for each subscale
check_normality <- function(data, vars_list) {
  normality_results <- list()
  
  for(scale_name in names(vars_list)) {
    mean_score <- paste0(scale_name, "_mean")
    
    # Shapiro-Wilk test
    sw_test <- shapiro.test(data[[mean_score]])
    
    # Skewness and kurtosis
    moments <- psych::describe(data[[mean_score]])
    
    normality_results[[scale_name]] <- list(
      shapiro = sw_test,
      skew = moments$skew,
      kurtosis = moments$kurtosis
    )
  }
  
  return(normality_results)
}

# Run normality checks
normality_checks <- check_normality(data, dheq_vars)

# Print normality results
for(scale_name in names(normality_checks)) {
  cat("\n\n=== ", scale_name, " ===\n")
  cat("Shapiro-Wilk test: W = ", round(normality_checks[[scale_name]]$shapiro$statistic, 3),
      ", p = ", round(normality_checks[[scale_name]]$shapiro$p.value, 3), "\n")
  cat("Skewness: ", round(normality_checks[[scale_name]]$skew, 3), "\n")
  cat("Kurtosis: ", round(normality_checks[[scale_name]]$kurtosis, 3), "\n")
}

# Create histograms for visual inspection
library(ggplot2)

plot_subscale_distributions <- function(data, vars_list) {
  plots <- list()
  
  for(scale_name in names(vars_list)) {
    mean_score <- paste0(scale_name, "_mean")
    
    plots[[scale_name]] <- ggplot(data, aes_string(x = mean_score)) +
      geom_histogram(bins = 30) +
      labs(title = paste("Distribution of", scale_name),
           x = "Mean Score",
           y = "Frequency") +
      theme_minimal()
  }
  
  return(plots)
}

# Create distribution plots
dist_plots <- plot_subscale_distributions(data, dheq_vars)

# Print plots (you may want to arrange them in a grid)
library(gridExtra)
do.call(grid.arrange, c(dist_plots, ncol = 2))

# Check for multicollinearity
library(car)
model <- lm(phq9_score ~ harassment_mean + vigilance_mean + gender_expression_mean + 
              vicarious_mean + family_mean + victimization_mean + isolation_mean,
            data=data)
vif(model)

