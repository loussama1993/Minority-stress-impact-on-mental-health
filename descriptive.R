# Create new dataframe with recoded variables
data_cleaned <- data




# Descriptive statistics for continuous variables
continuous_vars <- data_cleaned[c("age", "phq9_score",
                          "vigilance_mean", "harassment_mean", "gender_expression_mean",
                          "vicarious_mean", "family_mean", "victimization_mean", 
                          "isolation_mean")]

summary_stats <- psych::describe(continuous_vars)
print(round(summary_stats, 2))

# Frequency table for PHQ-9 severity
phq9_freq <- table(data_cleaned$phq9_severity)
phq9_prop <- prop.table(phq9_freq) * 100

severity_table <- data.frame(
  Frequency = as.numeric(phq9_freq),
  Percentage = round(as.numeric(phq9_prop), 1))
rownames(severity_table) <- names(phq9_freq)

print("PHQ-9 Severity Distribution:")
print(severity_table)



# For binary variables
wilcox.test(phq9_score ~ gender_binary, data=data_cleaned)
wilcox.test(phq9_score ~ region, data=data_cleaned)
wilcox.test(phq9_score ~ have_you_ever_been_consulted_by_a_mental_health_professional__psychiatrist___psychologist___, data=data_cleaned)
wilcox.test(phq9_score ~ have_you_ever_taken_an_antidepressant_or_anxiolytic_treatment__, data=data_cleaned)


# Chi-square test
chisq.test(table(data_cleaned$gender_binary, 
                 data_cleaned$what_is_your_employment_status_ == "Unemployed"))



# For binary variables (gender conformity and region)
# Gender
aggregate(phq9_score ~ gender_binary, data=data_cleaned, 
          FUN=function(x) c(median=median(x), 
                            q1=quantile(x,0.25),
                            q3=quantile(x,0.75)))

# Region 
aggregate(phq9_score ~ region, data=data_cleaned, 
          FUN=function(x) c(median=median(x), 
                            q1=quantile(x,0.25),
                            q3=quantile(x,0.75)))

# For employment status (multiple groups)
aggregate(phq9_score ~ what_is_your_employment_status_, data=data_cleaned, 
          FUN=function(x) c(median=median(x), 
                            q1=quantile(x,0.25),
                            q3=quantile(x,0.75)))


# Get median and IQR for each region
aggregate(phq9_score ~ have_you_ever_taken_an_antidepressant_or_anxiolytic_treatment__, data=data_cleaned, 
          FUN=function(x) c(median=median(x), 
                            q1=quantile(x,0.25),
                            q3=quantile(x,0.75)))

# Create boxplot
ggplot(data_cleaned, aes(x=region, y=phq9_score)) +
  geom_boxplot(fill="skyblue") +
  labs(title="PHQ-9 Scores by Region",
       x="Region",
       y="PHQ-9 Score")

# For multiple groups
kruskal.test(phq9_score ~ what_sex_were_you_assigned_at_birth_, data=data_cleaned)
kruskal.test(phq9_score ~ what_is_your_sexual_orientation_, data=data_cleaned)
kruskal.test(phq9_score ~ what_is_your_employment_status_, data=data_cleaned)

# If Kruskal-Wallis significant, run post-hoc:
library(dunn.test)
dunn.test(data_cleaned$phq9_score, data_cleaned$what_is_your_employment_status_, method="bonferroni")




# Create vector of variables for correlation
vars_to_correlate <- c("age", "phq9_score",
                       "if_you_checked_off_any_problems__how_difficult_have_these_problems_made_it_for_you_to_do_your_work__take_care_of_things_at_home__or_get_along_with_other_people_",
                       "vigilance_mean", "harassment_mean", "gender_expression_mean",
                       "vicarious_mean", "family_mean", "victimization_mean", 
                       "isolation_mean")

# Compute Spearman correlations
library(corrplot)
cor_matrix <- cor(data_cleaned[vars_to_correlate], 
                  method = "spearman",
                  use = "pairwise.complete.obs")
print(cor_matrix)
# Test significance
cor_pvalues <- cor.mtest(data_cleaned[vars_to_correlate], 
                         method = "spearman")$p
print(cor_pvalues)
# Shorten variable names for better visualization
shorter_names <- c("Age", "PHQ-9", "Functional\nImpairment", 
                   "Vigilance", "Harassment", "Gender\nExpression", 
                   "Vicarious", "Family", "Victimization", "Isolation")

# Create correlation matrix with shorter names
colnames(cor_matrix) <- shorter_names
rownames(cor_matrix) <- shorter_names


# try using the ggcorrplot package for a different style
library(ggcorrplot)
 ggcorrplot(cor_matrix, 
           hc.order = TRUE, 
            type = "upper",
           lab = TRUE,
          lab_size = 3,
           p.mat = cor_pvalues,
          insig = "blank")

 
 
 