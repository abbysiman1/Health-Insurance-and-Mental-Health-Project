# Health Insurance Project Code

# Install important packages and libraries
library(dplyr)
library(readr)
library(corrplot)
library(ggplot2)
library(tidyverse)


# Establishing file paths
file_path <- "~/Downloads/4810Project/NSDUH_2023_Tab.txt"

# Reading data
data_2023 <- read.csv(file_path)

# Parsing the data
data_2023 <- read_delim(file_path, delim = "\t")

ncol(data_2023)
nrow(data_2023)

#Create a new subset where people are over 18
data_subset_adults <- data_2023[data_2023$CATAG6>1, ]
data_subset_adults2 <- data_subset_adults[data_subset_adults$IRWRKSTAT<50, ]


#Variables used for recoding:
variables_to_recode <- c(
  "IRDSTHOP30", "IRDSTCHR30", "IRDSTNGD30", "IRDSTHOP12",
  "IRDSTCHR12", "IRDSTEFF12", "IRDSTNGD12", 
  "IRDSTNRV30", "IRDSTRST30","IRDSTNRV12", 
  "IRDSTRST12"
)
cleaned_data <- data_subset_adults2
#Recode each variable and update it directly in the cleaned_data dataset
for (var in variables_to_recode) {
  cleaned_data[[var]] <- ifelse(
    data_subset_adults2[[var]] == 99, 99, # Keep "LEGITIMATE SKIP" as is
    6 - data_subset_adults2[[var]]        # Reverse the scale for all other values
  )
}
#List of variables you want to recode
more_variables_to_recode <- c("IRDSTHOP30", "IRDSTCHR30", "IRDSTNGD30", "IRDSTHOP12", 
                              "IRDSTCHR12", "IRDSTEFF12", "IRDSTNGD12", "IRIMPHHLD", 
                              "IRIMPWORK", "IRDSTNRV30", "IRDSTRST30", "IRDSTNRV12", 
                              "IRDSTRST12", "IRIMPGOUT", "IRIMPSOC")
#Recode each variable and update it directly in the cleaned_data dataset
for (var in more_variables_to_recode) {
  cleaned_data[[var]] <- ifelse(
    cleaned_data[[var]] == 99, 0,   # Recode "LEGITIMATE SKIP" (99) as 0
    cleaned_data[[var]]             # Keep other values as they are
  )
}

#Depression Score:
#Creating a running total for the depression column
depression_variables <- c("IRDSTHOP30","IRDSTCHR30","IRDSTNGD30","IRDSTHOP12","IRDSTCHR12","IRDSTEFF12","IRDSTNGD12","IRIMPHHLD","IRIMPWORK")
#Create a new variable in cleaned_data that sums the depression-related variables
cleaned_data$depression_score <- rowSums(cleaned_data[depression_variables], na.rm = TRUE)

#Anxiety Score: 
#Creating a running total for the anxiety column
anxiety_variables <- c("IRDSTNRV30","IRDSTRST30","IRDSTNRV12","IRDSTRST12","IRIMPGOUT","IRIMPSOC")
#Create a new variable in cleaned_data that sums the anxiety-related variables
cleaned_data$anxiety_score <- rowSums(cleaned_data[anxiety_variables], na.rm = TRUE)

#Mental Health Score: 
#Combining depression and anxiety scores
cleaned_data$mental_health_score <- rowSums(cleaned_data[c(depression_variables, anxiety_variables)], na.rm = TRUE)


#Create education level, insurance type, and private coverage 
data_subset_one <- cleaned_data %>%
  mutate(
    education_level = case_when(
      IREDUHIGHST2 == 1 | IREDUHIGHST2 == 2 | IREDUHIGHST2 == 3 | IREDUHIGHST2 == 4 ~ 1, #Less than high school
      IREDUHIGHST2 == 5 | IREDUHIGHST2 == 6 | IREDUHIGHST2 == 7 ~ 2, #some high school, no diploma
      IREDUHIGHST2 == 8 ~ 3, #high school diploma / GED
      IREDUHIGHST2 == 9 ~ 4, #some college
      IREDUHIGHST2 == 10 ~ 5, #associates degree
      IREDUHIGHST2 == 11 ~ 6, #college graduate or higher
      TRUE ~ NA_real_
    ),
    insurance_type = case_when(
      IRINSUR4 == 2 ~ 0, #not covered
      IRMEDICR == 1 ~ 1, #covered by medicare
      IRMCDCHP == 1 ~ 2, #covered by medicaid
      IRPRVHLT == 1 ~ 3, #covered by private
      TRUE ~ NA_real_
    ),
    private_coverage = case_when(
      IRPRVHLT == 2 ~ 1, #not covered
      HLTINALC == 1 & HLTINDRG == 1 & HLTINMNT == 1 ~ 2, #covers for alcohol, drug, and mental health
      HLTINALC == 1 & HLTINDRG == 1 & HLTINMNT == 2 ~ 3, #covers for alcohol and drug
      HLTINALC == 1 & HLTINDRG == 2 & HLTINMNT == 1 ~ 4, #covers for alcohol and mental health
      HLTINALC == 2 & HLTINDRG == 1 & HLTINMNT == 1 ~ 5, #covers for drugs and mental health
      HLTINALC == 1 & HLTINDRG == 2 & HLTINMNT == 2 ~ 6, #covers for alcohol only
      HLTINALC == 2 & HLTINDRG == 1 & HLTINMNT == 2 ~ 7, #covers for drugs only
      HLTINALC == 2 & HLTINDRG == 2 & HLTINMNT == 1 ~ 8, #covers for mental health only
      TRUE ~ NA_real_
    ),
    college_educated = case_when(
      education_level == 5 | education_level == 6 ~ 1, # college educated
      education_level == 1 | education_level == 2 | education_level == 3 | education_level == 4 ~ 2, # not college educated
      TRUE ~ NA_real_
    ),
    health_binary = case_when(
      HEALTH2 == 1 | HEALTH2 == 2 ~ 1, # good health
      HEALTH2 == 3 | HEALTH2 == 4 ~ 2, # bad health
      TRUE ~ NA_real_
    ), 
    coverage_01 = case_when(
      IRINSUR4 == 2 ~ 0, #not covered
      IRINSUR4 == 1 ~ 1, #covered
      TRUE ~ NA_real_
    ),
    new_race = case_when(
      NEWRACE2 == 2 ~ 0, #black
      NEWRACE2 == 7 ~ 1, #hispanic
      NEWRACE2 == 1 ~ 2, #white
      NEWRACE2 == 3 | NEWRACE2 ==4 | NEWRACE2 == 5 | NEWRACE2 == 6 ~ 3, #other
      TRUE ~ NA_real_
    )
  )



colnames(data_subset_one)[colnames(data_subset_one) == "QUESTID2"] <- "id"
colnames(data_subset_one)[colnames(data_subset_one) == "CATAG6"] <- "age"
colnames(data_subset_one)[colnames(data_subset_one) == "HEALTH2"] <- "health"
colnames(data_subset_one)[colnames(data_subset_one) == "IRSEX"] <- "sex"
colnames(data_subset_one)[colnames(data_subset_one) == "new_race"] <- "race"
colnames(data_subset_one)[colnames(data_subset_one) == "SEXRACE"] <- "sex_race"
colnames(data_subset_one)[colnames(data_subset_one) == "HLCNOTYR"] <- "insurance_12"
colnames(data_subset_one)[colnames(data_subset_one) == "IRINSUR4"] <- "insurance_binary"
colnames(data_subset_one)[colnames(data_subset_one) == "GOVTPROG"] <- "govt_prog"
colnames(data_subset_one)[colnames(data_subset_one) == "POVERTY3"] <- "poverty"
colnames(data_subset_one)[colnames(data_subset_one) == "IRWRKSTAT"] <- "employment"
colnames(data_subset_one)[colnames(data_subset_one) == "COCLNEGMH"] <- "covid_mental"
colnames(data_subset_one)[colnames(data_subset_one) == "COCLFINANC"] <- "covid_financial"
colnames(data_subset_one)[colnames(data_subset_one) == "COMHAPTDL2"] <- "covid_appt"
colnames(data_subset_one)[colnames(data_subset_one) == "COMHTELE2"] <- "covid_tele"
colnames(data_subset_one)[colnames(data_subset_one) == "COMHAPTDL2"] <- "covid_appt"
colnames(data_subset_one)[colnames(data_subset_one) == "COMHRXDL2"] <- "covid_prescrip"
colnames(data_subset_one)[colnames(data_subset_one) == "COMHSVHLT2"] <- "covid_care"


#Create subset with variables desired
final_data <- data_subset_one[, c("id", "age", "race",
                                  
                                  "insurance_type","insurance_binary", "coverage_01",
                                  
                                  "depression_score", "anxiety_score", "mental_health_score")]

final_data <- final_data[!is.na(final_data$insurance_type) & !is.na(final_data$race) & !is.na(final_data$depression_score) & !is.na(final_data$anxiety_score) & !is.na(final_data$mental_health_score), ]

final_data

# Creating Log Scores
final_data$log_depression_score <- log(final_data$depression_score + 1)
final_data$log_anxiety_score <- log(final_data$anxiety_score + 1)
final_data$log_mental_health_score <- log(final_data$mental_health_score + 1)

summary(final_data)

# EDA

# Depression Score Histogram
ggplot(final_data, aes(x = depression_score)) +
  geom_histogram(binwidth = 1, fill = "skyblue", color = "black") +
  labs(title = "Histogram of Depression Scores", x = "Depression Score", y = "Count")

# Depression Log Score Histogram
ggplot(final_data, aes(x = log_depression_score)) +
  geom_histogram(binwidth = 1, fill = "skyblue", color = "black") +
  labs(title = "Histogram of Depression Scores", x = "Depression Score", y = "Count")

# Anxiety Score Histogram
ggplot(final_data, aes(x = anxiety_score)) +
  geom_histogram(binwidth = 1, fill = "salmon", color = "black") +
  labs(title = "Histogram of Anxiety Scores", x = "Anxiety Score", y = "Count")

# Mental Health Score Histogram
ggplot(final_data, aes(x = mental_health_score)) +
  geom_histogram(binwidth = 1, fill = "palegreen", color = "black") +
  labs(title = "Histogram of Mental Health Scores", x = "Mental Health Score", y = "Count")

# Summary Statistics
summary(final_data)

# Number of observations in the dataset
nrow(final_data)
# 43138 complete rows of observation

ncol(final_data)
# Double checking for missing values
colSums(is.na(final_data))

final_data$insurance_type <- factor(final_data$insurance_type,
                            levels = c(0, 1, 2, 3),
                            labels = c("Not Covered", "Medicare", "Medicaid", "Private"))
# Create summary table
insurance_summary <- as.data.frame(table(final_data$insurance_type))
colnames(insurance_summary) <- c("Insurance_Type", "Count")

# Add percentages
insurance_summary$Percentage <- round(100 * insurance_summary$Count / sum(insurance_summary$Count), 1)

# View it
print(insurance_summary)

# Create labels with percentages
insurance_summary$PieLabel <- paste0(insurance_summary$Insurance_Type,
                                     " (", insurance_summary$Percentage, "%)")

# Optional: define custom colors for clarity
custom_colors <- c("#FF9999", "#66B2FF", "#99FF99", "#FFD700")  # Not Covered, Medicare, Medicaid, Private

# Plot the pie chart
pie(insurance_summary$Count,
    labels = insurance_summary$PieLabel,
    main = "Insurance Type Distribution",
    col = custom_colors)


# Create summary table
race_summary <- as.data.frame(table(final_data$race))
colnames(race_summary) <- c("Race", "Count")

# Convert Race to numeric
race_summary$Race <- as.numeric(as.character(race_summary$Race))

# Calculate percentage
race_summary$Percentage <- round(100 * race_summary$Count / sum(race_summary$Count), 1)

# Assign readable labels (matching your coding)
race_labels <- c("Black", "Hispanic", "White", "Other")
race_summary$Label <- race_labels[race_summary$Race + 1]  # Add 1 because R indexes from 1

# Combine label and percentage for the pie chart
race_summary$PieLabel <- paste0(race_summary$Label, " (", race_summary$Percentage, "%)")

custom_colors <- c("#A6CEE3", "#B2DF8A", "#FDBF6F", "#CAB2D6")

# Plot the pie chart
pie(race_summary$Count,
    labels = race_summary$PieLabel,
    main = "Race Distribution",
    col = custom_colors)





# Mental Health Score by Race

#Is there a significant difference in insurance type and coverage between different races?

# Convert variables to factors
final_data$race <- factor(final_data$race,
                          levels = c("0", "1", "2", "3", "4", "5"),
                          labels = c("African American", "Native", 
                                     "Hispanic", "Asian", "White", "More than one race"))

final_data$insurance_type <- factor(final_data$insurance_type,
                                    levels = c("0", "1", "2", "3"),
                                    labels = c("Not Covered", "Medicare", "Medicaid", "Private"))

# Chi-Square Test (after making sure these are factors)
chi_race_insurance_test <- chisq.test(table(final_data$insurance_type, final_data$race))
chi_race_insurance_test

# Contingency Table
table(final_data$insurance_type, final_data$race)

# Plot
ggplot(final_data, aes(x = race, fill = insurance_type)) +
  geom_bar(position = "fill") +
  labs(title = "Insurance Type Distribution by Race",
       y = "Proportion", x = "Race") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 30, hjust = 1)) +
  scale_fill_discrete(name = "Insurance Coverage")


#Is there a significant difference in mental health score between different races?

#Anova test
anova_result <- aov(mental_health_score ~ race, data = final_data)
summary(anova_result)

#Boxplot
ggplot(final_data, aes(x = factor(race), y = mental_health_score)) +
  geom_boxplot() +
  labs(title = "Mental Health Score by Race", x = "Race", y = "Mental Health Score") +
  scale_x_discrete(labels = c("0" = "African American", 
                              "1" = "Native", 
                              "2" = "Hispanic", 
                              "3" = "Asian", 
                              "4" = "White", 
                              "5" = "More than one race")) +  
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for readability




#Is there a significant difference in mental health score between different races?

#Anova test
anova_result <- aov(anxiety_score ~ race, data = final_data)
summary(anova_result)

#Boxplot
ggplot(final_data, aes(x = factor(race), y = anxiety_score)) +
  geom_boxplot() +
  labs(title = "Anxiety Score by Race", x = "Race", y = "Anxiety Score") +
  scale_x_discrete(labels = c("0" = "African American", 
                              "1" = "Native", 
                              "2" = "Hispanic", 
                              "3" = "Asian", 
                              "4" = "White", 
                              "5" = "More than one race")) +  
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for readability


#Is there a significant difference in mental health score between different races?

#Anova test
anova_result <- aov(depression_score ~ race, data = final_data)
summary(anova_result)

#Boxplot
ggplot(final_data, aes(x = factor(race), y = depression_score)) +
  geom_boxplot() +
  labs(title = "Depression Score by Race", x = "Race", y = "Depression Score") +
  scale_x_discrete(labels = c("0" = "African American", 
                              "1" = "Native", 
                              "2" = "Hispanic", 
                              "3" = "Asian", 
                              "4" = "White", 
                              "5" = "More than one race")) +  
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for readability

