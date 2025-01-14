# Get the subset of data with complete suicide questions
# Remove all rows with any missing values
data_new <- na.omit(data_cleaned)

#create a composite suicidality score using these 4 variables based on the SBQ-R scoring guidelines
data_new$suicide_score <- data_new$have_you_ever_thought_about_or_attempted_to_kill_yourself_ +
  data_new$how_often_have_you_thought_about_killing_yourself_in_the_past_year_ +
  data_new$have_you_ever_told_someone_that_you_were_going_to_commit_suicide__or_that_you_might_do_it_ +
  data_new$how_likely_is_it_that_you_will_attempt_suicide_someday_

summary(data_new$suicide_score)

summary(data_new$i_have_high_self_esteem)
# For self-esteem
summary(data_new$i_have_high_self_esteem)
iqr_score1 <- IQR(data_new$i_have_high_self_esteem)
# Print results
cat("Median:", median_score1, "\nIQR:", iqr_score1)

# For suicide score
summary(data_new$suicide_score)
iqr_score <- IQR(data_new$suicide_score)
# Print results
cat("Median:", median_score, "\nIQR:", iqr_score)


# Create binary risk classification using cutoff score of 8
data_new$suicide_risk <- ifelse(data_new$suicide_score >= 8, 1, 0)

# If labels are needed, using more neutral terminology
data_new$suicide_risk_level <- factor(data_new$suicide_risk,
                                      levels = c(0, 1),
                                      labels = c("Lower risk", "Higher risk"))

table(data_new$suicide_risk_level)
# Primary mental health outcomes only
# PHQ-9 
wilcox.test(phq9_score ~ gender_binary, data=data_new)
wilcox.test(phq9_score ~ region, data=data_new)
wilcox.test(phq9_score ~ have_you_ever_been_consulted_by_a_mental_health_professional__psychiatrist___psychologist___, data=data_new)
wilcox.test(phq9_score ~ have_you_ever_taken_an_antidepressant_or_anxiolytic_treatment__, data=data_new)

# For multiple groups
kruskal.test(phq9_score ~ what_sex_were_you_assigned_at_birth_, data=data_new)
kruskal.test(phq9_score ~ what_is_your_sexual_orientation_, data=data_new)
kruskal.test(phq9_score ~ what_is_your_employment_status_, data=data_new)
kruskal.test(phq9_score ~ what_is_your_relationship_status_, data=data_new)


# SBQ-R
wilcox.test(suicide_score ~ gender_binary, data=data_new)
wilcox.test(suicide_score ~ region, data=data_new)
wilcox.test(suicide_score ~ have_you_ever_been_consulted_by_a_mental_health_professional__psychiatrist___psychologist___, data=data_new)
wilcox.test(suicide_score ~ have_you_ever_taken_an_antidepressant_or_anxiolytic_treatment__, data=data_new)

# For multiple groups
kruskal.test(suicide_score ~ what_sex_were_you_assigned_at_birth_, data=data_new)
kruskal.test(suicide_score ~ what_is_your_sexual_orientation_, data=data_new)
kruskal.test(suicide_score ~ what_is_your_employment_status_, data=data_new)
kruskal.test(suicide_score ~ what_is_your_relationship_status_, data=data_new)
# Get median and IQR for each region
aggregate(suicide_score ~ have_you_ever_taken_an_antidepressant_or_anxiolytic_treatment__, data=data_new, 
          FUN=function(x) c(median=median(x), 
                            q1=quantile(x,0.25),
                            q3=quantile(x,0.75)))


# Get median and IQR for each region
aggregate(suicide_score ~ region, data=data_new, 
          FUN=function(x) c(median=median(x), 
                            q1=quantile(x,0.25),
                            q3=quantile(x,0.75)))


# self esteem
wilcox.test(i_have_high_self_esteem ~ gender_binary, data=data_new)
wilcox.test(i_have_high_self_esteem ~ region, data=data_new)
wilcox.test(i_have_high_self_esteem ~ have_you_ever_been_consulted_by_a_mental_health_professional__psychiatrist___psychologist___, data=data_new)
wilcox.test(i_have_high_self_esteem ~ have_you_ever_taken_an_antidepressant_or_anxiolytic_treatment__, data=data_new)

# For multiple groups
kruskal.test(i_have_high_self_esteem ~ what_sex_were_you_assigned_at_birth_, data=data_new)
kruskal.test(i_have_high_self_esteem ~ what_is_your_sexual_orientation_, data=data_new)
kruskal.test(i_have_high_self_esteem ~ what_is_your_employment_status_, data=data_new)
kruskal.test(i_have_high_self_esteem ~ what_is_your_relationship_status_, data=data_new)







# Descriptive statistics for continuous variables in data_new
continuous_vars_new <- data_new[c("age", "phq9_score", "i_have_high_self_esteem", "suicide_score",
                                  "if_you_checked_off_any_problems__how_difficult_have_these_problems_made_it_for_you_to_do_your_work__take_care_of_things_at_home__or_get_along_with_other_people_",
                                  "vigilance_mean", "harassment_mean", "gender_expression_mean",
                                  "vicarious_mean", "family_mean", "victimization_mean", 
                                  "isolation_mean")]
summary_stats_new <- psych::describe(continuous_vars_new)
print(round(summary_stats_new, 2))




# Correlation matrix
vars_to_correlate_new <- c("age", "phq9_score", "i_have_high_self_esteem", "suicide_score",
                           "if_you_checked_off_any_problems__how_difficult_have_these_problems_made_it_for_you_to_do_your_work__take_care_of_things_at_home__or_get_along_with_other_people_",
                           "vigilance_mean", "harassment_mean", "gender_expression_mean",
                           "vicarious_mean", "family_mean", "victimization_mean", 
                           "isolation_mean")

cor_matrix_new <- cor(data_new[vars_to_correlate_new], 
                      method = "spearman",
                      use = "pairwise.complete.obs")
print(cor_matrix_new)

# Test significance
cor_pvalues_new <- cor.mtest(data_new[vars_to_correlate_new], 
                             method = "spearman")$p
print(cor_pvalues_new)

# Shorter names for correlation plot
shorter_names_new <- c("Age", "PHQ-9", "Self-Esteem", "SBQ-R", "Functional\nImpairment",
                       "Vigilance", "Harassment", "Gender\nExpression", 
                       "Vicarious", "Family", "Victimization", "Isolation")

# Create correlation matrix with shorter names
colnames(cor_matrix_new) <- shorter_names_new
rownames(cor_matrix_new) <- shorter_names_new

# Correlation plot
library(ggcorrplot)
ggcorrplot(cor_matrix_new, 
           hc.order = TRUE, 
           type = "upper",
           lab = TRUE,
           lab_size = 3,
           p.mat = cor_pvalues_new,
           insig = "blank")
table(data_new$what_is_your_relationship_status_)

