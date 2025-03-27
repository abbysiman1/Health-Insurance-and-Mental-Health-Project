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
data_subset_adults <- data[data$CATAG6>1, ]
data_subset_adults2 <- data_subset_adults[data_subset_adults$IRWRKSTAT<50, ]

## MENTAL HEALTH RECODE
variables_to_recode <- c(
  "IRDSTNRV30", "IRDSTHOP30", "IRDSTRST30", "IRDSTCHR30", "IRDSTNGD30", 
  "IRDSTNRV12", "IRDSTHOP12", "IRDSTRST12", "IRDSTCHR12", "IRDSTEFF12", "IRDSTNGD12"
)

cleaned_data <- data_subset_adults2

# Recode each variable and update it directly in the cleaned_data dataset
for (var in variables_to_recode) {
  cleaned_data[[var]] <- ifelse(
    data_subset[[var]] == 99, 99, # Keep "LEGITIMATE SKIP" as is
    6 - data_subset[[var]]        # Reverse the scale for all other values
  )
}
# List of variables you want to recode
more_variables_to_recode <- c("IRDSTHOP30", "IRDSTCHR30", "IRDSTNGD30", "IRDSTHOP12", 
                              "IRDSTCHR12", "IRDSTEFF12", "IRDSTNGD12", "IRIMPHHLD", 
                              "IRIMPWORK", "IRDSTNRV30", "IRDSTRST30", "IRDSTNRV12", 
                              "IRDSTRST12", "IRIMPGOUT", "IRIMPSOC")

# Recode each variable and update it directly in the cleaned_data dataset
for (var in more_variables_to_recode) {
  cleaned_data[[var]] <- ifelse(
    cleaned_data[[var]] == 99, 0,   # Recode "LEGITIMATE SKIP" (99) as 0
    cleaned_data[[var]]             # Keep other values as they are
  )
}

# Creating a running total for the depression column
depression_variables <- c("IRDSTHOP30","IRDSTCHR30","IRDSTNGD30","IRDSTHOP12","IRDSTCHR12","IRDSTEFF12","IRDSTNGD12","IRIMPHHLD","IRIMPWORK")

# Create a new variable in cleaned_data that sums the depression-related variables
cleaned_data$depression_score <- rowSums(cleaned_data[depression_variables], na.rm = TRUE)

# Creating a running total for the anxiety column
anxiety_variables <- c("IRDSTNRV30","IRDSTRST30","IRDSTNRV12","IRDSTRST12","IRIMPGOUT","IRIMPSOC")

# Create a new variable in cleaned_data that sums the anxiety-related variables
cleaned_data$anxiety_score <- rowSums(cleaned_data[anxiety_variables], na.rm = TRUE)

# Combining depression and anxiety scores
cleaned_data$mental_health_score <- rowSums(cleaned_data[c(depression_variables, anxiety_variables)], na.rm = TRUE)

## SES RECODE
# change the order of IRWRKSTAT
cleaned_data <- cleaned_data %>% mutate(
    IRWRKSTAT2 = case_when(
      IRWRKSTAT == 4 ~ 1,
      IRWRKSTAT == 3 ~ 2,
      IRWRKSTAT == 2 ~ 3,
      IRWRKSTAT == 1 ~ 4,
      TRUE ~ NA_real_
    )
  )

# create a running total for SES
ses_variables <- c("GOVTPROG", "POVERTY3", "IRWRKSTAT2")

# create a new variable in cleaned_data that summarizes SES variables
cleaned_data$ses_score <- rowSums(cleaned_data[ses_variables], na.rm = TRUE)

## INSURANCE & DEMOGRAPHIC RECODE
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
      IRMEDICR == 1 ~ 1, #covered by medicare
      IRMCDCHP == 1 ~ 2, #covered by medicaid/chip
      IRPRVHLT == 1 & GRPHLTIN == 1 ~ 3, #covered by private, through employer
      IRPRVHLT == 1 & GRPHLTIN == 2 ~ 4, #covered by private, elsewhere
      IRINSUR4 == 2 ~ 5, #not covered
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
    )
  )



#Create subset with variables desired
final_data <- data_subset_one[, c("QUESTID2", "FILEDATE", "COUTYP4", "CATAG6",
                                     "HEALTH2", "education_level", "IRSEX",
                                     "NEWRACE2", "SEXRACE", "depression_score", "anxiety_score", 
                                     
                                     "insurance_type", "GRPHLTIN", "private_coverage", 
                                     "HLCNOTYR", "IRINSUR4",
                                     
                                     "GOVTPROG", "POVERTY3", "IRWRKSTAT",
                                     
                                     "COCLNEGMH", "COCLFINANC", "COMHTELE2", "COMHAPTDL2",
                                     "COMHRXDL2", "COMHSVHLT2", "ses_score")]

## VARIABLE RENAME
colnames(data_subset_one)[colnames(data_subset_one) == "QUESTID2"] <- "id"
colnames(data_subset_one)[colnames(data_subset_one) == "CATAG6"] <- "age"
colnames(data_subset_one)[colnames(data_subset_one) == "HEALTH2"] <- "health"
colnames(data_subset_one)[colnames(data_subset_one) == "IRSEX"] <- "sex"
colnames(data_subset_one)[colnames(data_subset_one) == "NEWRACE2"] <- "race"
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
final_data <- data_subset_one[, c("id", "age", "health", "education_level", 
                                  "sex", "race", "sex_race", "college_educated",
                                  "health_binary",
                                  
                                  "insurance_type", "private_coverage","insurance_12", 
                                  "insurance_binary",
                                  
                                  "govt_prog", "poverty", "employment",
                                  
                                  "covid_mental", "covid_financial", "covid_tele", "covid_appt",
                                  "covid_prescrip", "covid_care",
                                  
                                  "depression_score", "anxiety_score", "mental_health_score", "ses_score")]

# remove unnecessary variables from environment
# rm(data, cleaned_data, data_subset, data_subset_one, data_subset_adults, data_subset_adults2, data_subset_health, anxiety_variables, depression_variables, ses_variables, variables_to_recode, more_variables_to_recode, var)

#### PART 2: LINEAR REGRESSION & EDA
# simple LR to compare race & insurance coverage
race_insurance <- lm(formula = insurance_binary ~ race, data = final_data)
summary(race_insurance) # significant
ggplot(data = final_data, mapping = aes(x = race, fill = as.factor(insurance_binary))) +
  geom_bar() +
  theme_bw() +
  scale_x_continuous(breaks = 1:7) +
  scale_fill_manual(
    values = c("1" = "violetred", "2" = "palevioletred"),
    name = "Health Insurance",
    labels = c("Insured", "Uninsured")) +
  theme(legend.position = "bottom")

# simple LR to compare SES & insurance coverage
ses_insurance <- lm(formula = insurance_binary ~ ses_score, data = final_data)
summary(ses_insurance) # significant
ggplot(data = final_data, mapping = aes(x = ses_score, fill = as.factor(insurance_binary))) +
  geom_bar() +
  theme_bw() +
  scale_x_continuous(breaks = 1:10) +
  scale_fill_manual(
    values = c("1" = "seagreen1", "2" = "seagreen"),
    name = "Health Insurance",
    labels = c("Insured", "Uninsured")) +
  theme(legend.position = "bottom")

# multiple LR to compare race and SES to insurance coverage
mlr_mod <- lm(formula = insurance_binary ~ race + ses_score, data = final_data)
summary(mlr_mod) # significant

# simple LR to compare insurance coverage to mental health
insurance_mh <- lm(formula = mental_health_score ~ insurance_binary, data = final_data)
summary(insurance_mh) # significant
