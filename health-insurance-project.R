# Health Insurance Project Code

# Install important packages and libraries
install.packages('tidyverse',dependency=T)
library(readr)
library(dplyr)
library(ggplot2)

# Establishing file paths
file_path <- "~/Downloads/4810Project/NSDUH_2023_Tab.txt"

# Reading data
data_2023 <- read.csv(file_path)

# Parsing the data
data_2023 <- read_delim(file_path, delim = "\t")

# Selecting specific columns
data_subset <- select(data_2023, QUESTID2, FILEDATE, COUTYP4, CATAG6, HEALTH2, IREDUHIGHST2, IRSEX, NEWRACE2, SEXRACE, IRMEDICR, IRMCDCHP, IRPRVHLT, GRPHLTIN, HLTINALC, HLTINDRG, HLTINMNT, HLCNOTYR, IRINSUR4, GOVTPROG, INCOME, POVERTY3, IRWRKSTAT, COCLNEGMH, COCLFINANC, COMHTELE2, COMHAPTDL2, COMHRXDL2, COMHSVHLT2, IRDSTNRV30, IRDSTHOP30, IRDSTRST30, IRDSTCHR30, IRDSTNGD30, IRDSTNRV12, IRDSTHOP12, IRDSTRST12, IRDSTCHR12, IRDSTEFF12, IRDSTNGD12, IRIMPGOUT, IRIMPSOC, IRIMPHHLD, IRIMPWORK)

# Display the first few rows of the subset
head(data_subset)

# Recode
# OLD
#1 = All of the time
#2 = Most of the time
#3 = Some of the time
#4 = A little of the time
#5 = None of the time
#99 = LEGITIMATE SKIP

# NEW
# 5 = All of the time
# 4 = Most of the time
# 3 = Some of the time
# 2 = A little of the time
# 1 = None of the time
# 0 = LEGITIMATE SKIP


variables_to_recode <- c(
  "IRDSTNRV30", "IRDSTHOP30", "IRDSTRST30", "IRDSTCHR30", "IRDSTNGD30", 
  "IRDSTNRV12", "IRDSTHOP12", "IRDSTRST12", "IRDSTCHR12", "IRDSTEFF12", "IRDSTNGD12"
)

cleaned_data <- data_subset

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

# View the cleaned data to check the changes
head(cleaned_data)

# Creating a running total for the depression column
# Nine variables
# 0-45
# score > 36 ; depressed
depression_variables <- c("IRDSTHOP30","IRDSTCHR30","IRDSTNGD30","IRDSTHOP12","IRDSTCHR12","IRDSTEFF12","IRDSTNGD12","IRIMPHHLD","IRIMPWORK")
cleaned_data$depression_score <- rowSums(cleaned_data[depression_variables], na.rm = TRUE)
# Calculate mental health score and classify mental health status as 0 or 1
cleaned_data <- cleaned_data %>%
  mutate(
    depression_status = ifelse(depression_score > 36, 1, 0)  
  )

# Creating boxplot to represent Depression distribution
ggplot(cleaned_data, aes(x = depression_score)) +
  geom_boxplot(fill = "pink") +  # Set boxplot color to sky blue
  theme_minimal() +
  theme(axis.title.y = element_blank())

summary(cleaned_data$depression_score)
sum(is.na(cleaned_data$depression_score))

# View the cleaned data to check the new variable
head(cleaned_data)

# Creating a running total for the anxiety column
# Six variables
# 0-30
# score > 24 ; has anxiety
anxiety_variables <- c("IRDSTNRV30","IRDSTRST30","IRDSTNRV12","IRDSTRST12","IRIMPGOUT","IRIMPSOC")
cleaned_data$anxiety_score <- rowSums(cleaned_data[anxiety_variables], na.rm = TRUE)
# Calculate mental health score and classify mental health status as 0 or 1
cleaned_data <- cleaned_data %>%
  mutate(
    anxiety_status = ifelse(anxiety_score > 24, 1, 0)  
  )

# Creating boxplot to represent Depression distribution
ggplot(cleaned_data, aes(x = anxiety_score)) +
  geom_boxplot(fill = "pink") +  # Set boxplot color to sky blue
  theme_minimal() +
  theme(axis.title.y = element_blank())

# View the cleaned data to check the new variables
head(cleaned_data)

# Combining depression and anxiety scores
# 15 variables
# 0-75
# score > 60 depressed
cleaned_data$mental_health_score <- rowSums(cleaned_data[c(depression_variables, anxiety_variables)], na.rm = TRUE)

# Calculate mental health score and classify mental health status as 0 or 1
cleaned_data <- cleaned_data %>%
  mutate(
    mental_health_status = ifelse(mental_health_score > 60, 1, 0)  # 1 for Poor Mental Health, 0 for Good Mental Health
  )

# Creating boxplot to represent Depression distribution
ggplot(cleaned_data, aes(x = mental_health_score)) +
  geom_boxplot(fill = "pink") +  # Set boxplot color to sky blue
  theme_minimal() +
  theme(axis.title.y = element_blank())

sum(cleaned_data$depression_status == 1)
sum(cleaned_data$anxiety_status == 1)
sum(cleaned_data$mental_health_status == 1)

# Recoding demographic variables
#Create a new subset where people are over 18
cleaned_data <- cleaned_data[cleaned_data$CATAG6>1, ]
cleaned_data <- cleaned_data[cleaned_data$IRWRKSTAT<50, ]

#Create education level, insurance type, and private coverage 
#Create education level, insurance type, and private coverage 
#Create education level, insurance type, and private coverage 
cleaned_data <- cleaned_data %>%
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

# rename variables
colnames(cleaned_data)[colnames(cleaned_data) == "QUESTID2"] <- "id"
colnames(cleaned_data)[colnames(cleaned_data) == "CATAG6"] <- "age"
colnames(cleaned_data)[colnames(cleaned_data) == "HEALTH2"] <- "health"
colnames(cleaned_data)[colnames(cleaned_data) == "IRSEX"] <- "sex"
colnames(cleaned_data)[colnames(cleaned_data) == "NEWRACE2"] <- "race"
colnames(cleaned_data)[colnames(cleaned_data) == "SEXRACE"] <- "sex_race"
colnames(cleaned_data)[colnames(cleaned_data) == "HLCNOTYR"] <- "insurance_12"
colnames(cleaned_data)[colnames(cleaned_data) == "IRINSUR4"] <- "insurance_binary"
colnames(cleaned_data)[colnames(cleaned_data) == "GOVTPROG"] <- "govt_prog"
colnames(cleaned_data)[colnames(cleaned_data) == "POVERTY3"] <- "poverty"
colnames(cleaned_data)[colnames(cleaned_data) == "IRWRKSTAT"] <- "employment"
colnames(cleaned_data)[colnames(cleaned_data) == "COCLNEGMH"] <- "covid_mental"
colnames(cleaned_data)[colnames(cleaned_data) == "COCLFINANC"] <- "covid_financial"
colnames(cleaned_data)[colnames(cleaned_data) == "COMHAPTDL2"] <- "covid_appt"
colnames(cleaned_data)[colnames(cleaned_data) == "COMHTELE2"] <- "covid_tele"
colnames(cleaned_data)[colnames(cleaned_data) == "COMHAPTDL2"] <- "covid_appt"
colnames(cleaned_data)[colnames(cleaned_data) == "COMHRXDL2"] <- "covid_prescrip"
colnames(cleaned_data)[colnames(cleaned_data) == "COMHSVHLT2"] <- "covid_care"

# drop mental health question variables

#Create subset with variables desired
final_data <- cleaned_data[, c("id", "age", "health", "education_level", 
                                  "sex", "race", "sex_race", "college_educated",
                                  "health_binary",
                                  
                                  "insurance_type", "private_coverage","insurance_12", 
                                  "insurance_binary",
                                  
                                  "govt_prog", "poverty", "employment",
                                  
                                  "covid_mental", "covid_financial", "covid_tele", "covid_appt",
                                  "covid_prescrip", "covid_care",
                                  
                                  "depression_score", "anxiety_score", "mental_health_score",
                               "depression_status", "anxiety_status", "mental_health_status")]

