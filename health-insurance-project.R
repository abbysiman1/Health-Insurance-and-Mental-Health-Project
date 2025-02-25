# Health Insurance Project Code

# Install important packages

install.packages('tidyverse',dependency=T)

library(readr)

# Load important packages

# Establishing file paths
file_path <- "~/Downloads/4810Project/NSDUH_2023_Tab.txt"

# Reading data
data_2023 <- read.csv(file_path)

# Parsing the data
data_2023 <- read_delim(file_path, delim = "\t")




