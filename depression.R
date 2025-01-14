# Calculate PHQ-9 score
phq9_cols <- c(
  "little_interest_or_pleasure_in_doing_things",
  "feeling_down__depressed__or_hopeless",
  "trouble_falling_or_staying_asleep__or_sleeping_too_much",
  "feeling_tired_or_having_little_energy",
  "poor_appetite_or_overeating",
  "feeling_bad_about_yourself_or_that_you_are_a_failure_or_have_let_yourself_or_your_family_down",
  "trouble_concentrating_on_things__such_as_reading_the_newspaper_or_watching_television",
  "moving_or_speaking_so_slowly_that_other_people_could_have_noticed__or_the_opposite_being_so_fidgety_or_restless_that_you_have_been_moving_around_a_lot_more_than_usual",
  "thoughts_that_you_would_be_better_off_dead__or_of_hurting_yourself"
)

data$phq9_score <- rowSums(data[, phq9_cols])

# Create PHQ-9 severity levels
data$phq9_severity <- cut(data$phq9_score, 
                          breaks = c(-Inf, 4, 9, 14, 19, Inf),
                          labels = c("Minimal", "Mild", "Moderate", "Moderately Severe", "Severe"))

summary(data$phq9_score)

# For phq9_score
median_score2 <- median(data$phq9_score)
iqr_score2 <- IQR(data$phq9_score)
# Print results
cat("Median:", median_score2, "\nIQR:", iqr_score2)

# For functional impairment
summary(data$if_you_checked_off_any_problems__how_difficult_have_these_problems_made_it_for_you_to_do_your_work__take_care_of_things_at_home__or_get_along_with_other_people_)
iqr_score3 <- IQR(data$if_you_checked_off_any_problems__how_difficult_have_these_problems_made_it_for_you_to_do_your_work__take_care_of_things_at_home__or_get_along_with_other_people_)
# Print results
cat("Median:", median_score3, "\nIQR:", iqr_score3)

summary(data$phq9_severity)


# Create visualizations of PHQ-9 scores
# Histogram of PHQ-9 scores
library(ggplot2)

# Score distribution
ggplot(data, aes(x = phq9_score)) +
  geom_histogram(binwidth = 1, fill = "skyblue", color = "black") +
  labs(title = "Distribution of PHQ-9 Scores",
       x = "PHQ-9 Score",
       y = "Frequency")

# Severity categories
ggplot(data, aes(x = phq9_severity)) +
  geom_bar(fill = "skyblue") +
  labs(title = "PHQ-9 Severity Categories",
       x = "Severity Level",
       y = "Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Create boxplots for key demographic variables
# By gender
ggplot(data, aes(x = how_would_you_describe_yourself_today_, y = phq9_score)) +
  geom_boxplot(fill = "skyblue") +
  labs(title = "PHQ-9 Scores by Gender Identity",
       x = "Gender Identity",
       y = "PHQ-9 Score") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# By employment status
ggplot(data, aes(x = what_is_your_employment_status_, y = phq9_score)) +
  geom_boxplot(fill = "lightgreen") +
  labs(title = "PHQ-9 Scores by Employment Status",
       x = "Employment Status",
       y = "PHQ-9 Score") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

