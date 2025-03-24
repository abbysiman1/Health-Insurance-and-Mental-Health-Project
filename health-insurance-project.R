# Health Insurance Project Code

# Install important packages

install.packages('tidyverse',dependency=T)

library(readr)
library(dplyr)

# Load important packages

# Establishing file paths
file_path <- "~/Downloads/4810Project/NSDUH_2023_Tab.txt"

# Reading data
data_2023 <- read.csv(file_path)

# Parsing the data
data_2023 <- read_delim(file_path, delim = "\t")

# Selecting specific columns
data_subset <- select(data_2023, QUESTID2, FILEDATE, COUTYP4, CATAGE, HEALTH2, IREDUHIGHST2, IRSEX, NEWRACE2, SEXRACE, IRMEDICR, IRMCDCHP, IRPRVHLT, GRPHLTIN, HLTINALC, HLTINDRG, HLTINMNT, HLCNOTYR, IRINSUR4, GOVTPROG, INCOME, POVERTY3, IRWRKSTAT, COCLNEGMH, COCLFINANC, COMHTELE2, COMHAPTDL2, COMHRXDL2, COMHSVHLT2, IRDSTNRV30, IRDSTHOP30, IRDSTRST30, IRDSTCHR30, IRDSTNGD30, IRDSTNRV12, IRDSTHOP12, IRDSTRST12, IRDSTCHR12, IRDSTEFF12, IRDSTNGD12, IRIMPGOUT, IRIMPSOC, IRIMPHHLD, IRIMPWORK)

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
depression_variables <- c("IRDSTHOP30","IRDSTCHR30","IRDSTNGD30","IRDSTHOP12","IRDSTCHR12","IRDSTEFF12","IRDSTNGD12","IRIMPHHLD","IRIMPWORK")

# Create a new variable in cleaned_data that sums the depression-related variables
cleaned_data$depression_score <- rowSums(cleaned_data[depression_variables], na.rm = TRUE)

# View the cleaned data to check the new variable
head(cleaned_data)

# Creating a running total for the anxiety column
anxiety_variables <- c("IRDSTNRV30","IRDSTRST30","IRDSTNRV12","IRDSTRST12","IRIMPGOUT","IRIMPSOC")

# Create a new variable in cleaned_data that sums the anxiety-related variables
cleaned_data$anxiety_score <- rowSums(cleaned_data[anxiety_variables], na.rm = TRUE)

# Combining depression and anxiety scores
cleaned_data$mental_health_score <- rowSums(cleaned_data[c(depression_variables, anxiety_variables)], na.rm = TRUE)

# View the cleaned data to check the new variables
head(cleaned_data)



