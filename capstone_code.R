# setup
library(dplyr)
library(tidyverse)
library(ggplot2)
load("~/Desktop/school/Spring 2025/STAT 4810/Project/Capstone_rproj/NSDUH_2023.Rdata")
data <- puf2023_102124
rm(puf2023_102124)

#### PART 1: CLEANING THE DATA
# eliminate unnecessary columns
data_subset <- select(data, QUESTID2, FILEDATE, COUTYP4, CATAG6, HEALTH2, IREDUHIGHST2, IRSEX, NEWRACE2, SEXRACE, IRMEDICR, IRMCDCHP, IRPRVHLT, GRPHLTIN, HLTINALC, HLTINDRG, HLTINMNT, HLCNOTYR, IRINSUR4, GOVTPROG, INCOME, POVERTY3, IRWRKSTAT, COCLNEGMH, COCLFINANC, COMHTELE2, COMHAPTDL2, COMHRXDL2, COMHSVHLT2, IRDSTNRV30, IRDSTHOP30, IRDSTRST30, IRDSTCHR30, IRDSTNGD30, IRDSTNRV12, IRDSTHOP12, IRDSTRST12, IRDSTCHR12, IRDSTEFF12, IRDSTNGD12, IRIMPGOUT, IRIMPSOC, IRIMPHHLD, IRIMPWORK)

#Create a new subset where people are over 18
data_subset_adults <- data_subset[data_subset$CATAG6>1, ]
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
      NEWRACE2 == 3 | NEWRACE2 ==4 ~ 2, #native
      NEWRACE2 == 5 ~ 3, #asian
      NEWRACE2 == 1 ~ 4, #white
      NEWRACE2 == 6 ~ 5, #other
      TRUE ~ NA_real_
    )
  )

# variable rename
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

#### PART 2: TWO-WAY ANOVA
# race histogram
ggplot(data = final_data, mapping = aes(x = race)) +
  geom_bar()
# insurance type histogram
ggplot(data = final_data, mapping = aes(x = insurance_type)) +
  geom_bar()

# visualize
ggplot(final_data, aes(x = insurance_type, y = mental_health_score, fill = race)) +
  stat_summary(fun = mean, geom = "bar", position = "dodge") +
  labs(title = "Mental Health Score by Race and Insurance Type",
       x = "Insurance Type",
       y = "Mental Health Score",
       fill = "Race") +
  scale_x_discrete(labels = c("Not Covered", "Medicare", "Medicaid", "Private")) +
  scale_fill_hue(labels = c("AfrAm", "Hispanic", "Native", "Asian", "White", "Mixed")) +
  theme_bw()

# ensure factored independent variables
final_data$race <- as.factor(final_data$race)
final_data$insurance_type <- as.factor(final_data$insurance_type)

# build and display the model
anova_model <- aov(mental_health_score ~ race * insurance_type, data = final_data)
summary(anova_model)
# plot(anova_model)

# find the mean mhs of each group
library(emmeans)
emmeans(anova_model, ~ race * insurance_type)
