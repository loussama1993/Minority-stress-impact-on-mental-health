# Load required packages
library(tidyverse)  # for data manipulation and visualization
library(psych)      # for psychological statistics
library(corrplot)   # for correlation plots
library(car)        # for statistical tests

# Read the data
# Since your data has some special characters in column names, let's clean them up
data <- read.csv("C:/Users/USER/Desktop/dissertation/New folder/project/Merged_Dataset.csv", stringsAsFactors = FALSE)

# Let's first look at the basic structure of our data
str(data)
summary(data)

# Clean column names
names(data) <- gsub("\\.", "_", names(data))
names(data) <- tolower(names(data))

# Create a cleaner dataframe with key variables
clean_data <- data %>%
  # Select key demographic variables
  select(
    ethnicity = please_select_the_option_that_best_describes_your_ethnicity_,
    location = where_do_you_live_,
    age,
    sex = what_sex_were_you_assigned_at_birth_,
    gender = how_would_you_describe_yourself_today_,
    orientation = what_is_your_sexual_orientation_,
    employment = what_is_your_employment_status_,
    relationship = what_is_your_relationship_status_,
    residential = regional_residential_context,
    mental_health_consult = have_you_ever_been_consulted_by_a_mental_health_professional__psychiatrist___psychologist___,
    medication = have_you_ever_taken_an_antidepressant_or_anxiolytic_treatment__
  )

# View basic demographics
summary(clean_data)


# Create age groups
clean_data <- clean_data %>%
  mutate(age_group = case_when(
    age >= 18 & age <= 24 ~ "18-24 years (Young Adults)",
    age >= 25 & age <= 34 ~ "25-34 years (Early Adulthood)",
    age >= 35 ~ "35+ (Midlife Adults)",
    TRUE ~ "Unknown"
  ))

# Create comprehensive demographic summary tables
demographic_tables <- list(
  # Age groups
  age_groups = table(clean_data$age_group),
  
  # Gender distribution
  gender = table(clean_data$gender),
  
  # Sex assigned at birth
  sex = table(clean_data$sex),
  
  # Sexual orientation distribution
  orientation = table(clean_data$orientation),
  
  # Ethnicity
  ethnicity = table(clean_data$ethnicity),
  
  # Employment status
  employment = table(clean_data$employment),
  
  # Location/Residence
  location = table(clean_data$location),
  
  # Relationship status
  relationship = table(clean_data$relationship),
  
  # Residential context
  residential = table(clean_data$residential),
  
  # Mental health consultation
  mental_health = table(clean_data$mental_health_consult),
  
  # Medication usage
  medication = table(clean_data$medication)
)

# Function to handle empty or problematic data
create_formatted_table <- function(x) {
  if(length(x) == 0 || all(is.na(x))) {
    return(data.frame(Category = character(),
                      Count = numeric(),
                      Percentage = numeric()))
  }
  
  # Remove NA values and create table
  x <- x[!is.na(x)]
  if(length(x) == 0) {
    return(data.frame(Category = character(),
                      Count = numeric(),
                      Percentage = numeric()))
  }
  
  tab <- table(x)
  counts <- as.data.frame(tab)
  percentages <- prop.table(tab) * 100
  
  result <- data.frame(
    Category = counts$x,
    Count = counts$Freq,
    Percentage = round(as.vector(percentages), 2)
  )
  return(result)
}

# Let's first check which variables we actually have in clean_data
print(names(clean_data))

# Create demographic tables one by one to identify any issues
demographic_tables <- list()

# Age groups
demographic_tables[["age_groups"]] <- create_formatted_table(clean_data$age_group)

# Gender
demographic_tables[["gender"]] <- create_formatted_table(clean_data$gender)

# Sex
demographic_tables[["sex"]] <- create_formatted_table(clean_data$sex)

# Sexual orientation
demographic_tables[["orientation"]] <- create_formatted_table(clean_data$orientation)

# Ethnicity
demographic_tables[["ethnicity"]] <- create_formatted_table(clean_data$ethnicity)

# Employment
demographic_tables[["employment"]] <- create_formatted_table(clean_data$employment)

# Location
demographic_tables[["location"]] <- create_formatted_table(clean_data$location)

# Relationship status
demographic_tables[["relationship"]] <- create_formatted_table(clean_data$relationship)

# Residential context
demographic_tables[["residential"]] <- create_formatted_table(clean_data$residential)

# Mental health consultation
demographic_tables[["mental_health"]] <- create_formatted_table(clean_data$mental_health_consult)

# Medication
demographic_tables[["medication"]] <- create_formatted_table(clean_data$medication)

# Display tables
for(name in names(demographic_tables)) {
  cat("\n\n===", toupper(name), "===\n")
  print(demographic_tables[[name]])
}

# Using dplyr
data <- data %>%
  mutate(region = case_when(
    where_do_you_live_ %in% c(
      "Egypt", "Kingdom Saudi Arabia", "UAE", "Qatar", "Bahrain", "Kuwait", "Oman",
      "Jordan", "Lebanon", "Iraq", "Syria", "Yemen", "Palestine", "Libya",
      "Tunisia", "Algeria", "Morocco", "Iran") ~ "MENA region",
    TRUE ~ "Western country"
  ))
table(data$region)


