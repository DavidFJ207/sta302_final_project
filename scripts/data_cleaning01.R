#### Preamble ####
# Purpose: Cleans the raw data and merges them into one dataset
# Author: Gadiel David Flores, Wendy
# Date: 09/27/24
# Contact: davidgadiel.flores@mail.toronto.com
# License: MIT
# Pre-requisites: Data needs to be extracted from Statistics Canada to file raw_data

#### Libraries ####
library(dplyr)
library(tidyverse)
library(readr)
library(here)

#### Data innit ####
raw_cpi <- read_csv(here::here("data/raw_data/cpi.csv"))
raw_gdp <- read_csv(here::here("data/raw_data/gdp.csv"))
raw_house_construction <- read_csv(here::here("data/raw_data/house_construction.csv"))
raw_housing_price <- read_csv(here::here("data/raw_data/housing_price.csv"))
raw_housing_absorbtion <- read_csv(here::here("data/raw_data/housing_absorbtion.csv"))
raw_financial <- read_csv(here::here("data/raw_data/financial.csv"))
#### Helper Function to Process Data ####

# Processing and extracting rows from datasets
process_data <- function(data, row_num, variable_name, drop_columns = c(1, 2), group_and_average = TRUE) {
  data_processed <- data %>%
    filter(row_number() == row_num) %>%
    select(-all_of(drop_columns)) %>% 
    t() %>%
    as.data.frame(stringsAsFactors = FALSE)
  
  # Convert to numeric
  data_processed$Value <- round(as.numeric(gsub(",", "", unlist(data_processed))))
  
  # Change to quarterly data
  if (group_and_average) {
    num_rows <- nrow(data_processed)
    num_quarters <- ceiling(num_rows / 3)
    quarters <- seq(from = as.Date("1997-07-01"), by = "3 months", length.out = num_quarters)
    quarters <- rep(quarters, each = 3)[1:num_rows]
    
    # Combine the data with the quarterly labels
    data_processed <- data_processed %>%
      mutate(Quarter = quarters)
    
    # Group by quarter and calculate the average
    data_processed <- data_processed %>%
      group_by(Quarter) %>%
      summarise(!!variable_name := round(mean(Value, na.rm = TRUE)))
  }
  
  return(data_processed)
}

# Process construction data
process_construction_data <- function(data, row_num) {
  processed_data <- data %>%
    filter(row_number() == row_num) %>%
    select(-1, -2) 
  # Convert to numeric vector
  vector_data <- as.vector(unlist(processed_data[1, ]))
  vector_data <- gsub(",", "", vector_data)
  vector_data <- as.numeric(vector_data)
  return(vector_data)
}

#### Initialize housing price ####
cost_data_quarterly <- process_data(raw_housing_price, 11, "Quarterly_Average")

# number of quarters
num_quarters <- nrow(cost_data_quarterly)

#### CPI, GDP, Absorbtion Datasets ####
data_list <- list(
  list(data = raw_housing_absorbtion, row = 12, name = "Detached_Absorption_Quarterly_Avg"),
  list(data = raw_housing_absorbtion, row = 13, name = "Semi_Absorption_Quarterly_Avg"),
  list(data = raw_housing_absorbtion, row = 15, name = "Detached_Unabsorbed_Quarterly_Avg"),
  list(data = raw_housing_absorbtion, row = 16, name = "Semi_Unabsorbed_Quarterly_Avg"),
  list(data = raw_gdp, row = 13, name = "GDP_Quarterly_Avg"),
  list(data = raw_cpi, row = 11, name = "CPI_Quarterly_Avg")
)
# Process each dataset and merge it into cost_data_quarterly
for (params in data_list) {
  processed_data <- process_data(params$data, params$row, params$name)
  cost_data_quarterly <- cost_data_quarterly %>%
    left_join(processed_data, by = "Quarter")
}



#### Construction Dataset ####
detached <- process_construction_data(raw_house_construction, 13)
semi <- process_construction_data(raw_house_construction, 15)
under_detached <- process_construction_data(raw_house_construction, 19)
completed_semi <- process_construction_data(raw_house_construction, 21)
completed_detached <- process_construction_data(raw_house_construction, 25)
under_semi <- process_construction_data(raw_house_construction, 27)

# length matches cost_data_quarterly
num_quarters <- nrow(cost_data_quarterly)

# Adjust length of each vector to match the number of rows in cost_data_quarterly
adjust_length <- function(vec, num_quarters) {
  if (length(vec) > num_quarters) {
    vec <- vec[1:num_quarters] # Trim if longer
  } else if (length(vec) < num_quarters) {
    vec <- c(vec, rep(NA, num_quarters - length(vec))) # Pad with NAs if shorter
  }
  return(vec)
}

detached <- adjust_length(detached, num_quarters)
semi <- adjust_length(semi, num_quarters)
under_detached <- adjust_length(under_detached, num_quarters)
completed_semi <- adjust_length(completed_semi, num_quarters)
completed_detached <- adjust_length(completed_detached, num_quarters)
under_semi <- adjust_length(under_semi, num_quarters)

# Add vector as a new column to cost_data_quarterly
cost_data_quarterly <- cost_data_quarterly %>%
  mutate(
    Starting_Detached_Construction = detached,
    Starting_Semi_Construction = semi,
    Under_Construction_Detached = under_detached,
    Under_Construction_Semi = under_semi,
    Completed_Construction_Semi = completed_semi,
    Completed_Construction_Detached = completed_detached
  )

#### Financial Dataset ####
# Extract the relevant row from raw_financial and select columns
rates_raw <- raw_financial %>%
  filter(row_number() == 12) %>%
  select(-1) 

# Convert to a numeric vector
cleaned_rates <- as.numeric(gsub(",", "", as.vector(unlist(rates_raw[1, ]))))

# Initialize 'quarter_rates'
quarter_rates <- numeric()

# Rounding function
round_to_quarter <- function(x) {
  round(x * 4) / 4
}

# Change from monthly to quarter
for (i in seq(1, length(cleaned_rates), by = 3)) {
  chunk <- cleaned_rates[i:min(i + 2, length(cleaned_rates))]
  if (length(chunk) == 3) {
    avg_chunk <- round_to_quarter(mean(chunk, na.rm = TRUE))
    quarter_rates <- c(quarter_rates, avg_chunk)
  }
}

# Initialize rate_increase
rate_increase <- numeric(length(quarter_rates))

# Rate increase
for (i in seq_along(quarter_rates)[-length(quarter_rates)]) {
  rate_increase[i] <- ifelse(quarter_rates[i + 1] > quarter_rates[i], 1, 0)
}

# Add an extra 0
rate_increase[length(quarter_rates)] <- 0

cost_data_quarterly <- cost_data_quarterly %>%
  mutate(rates = rate_increase)

#### Save the cleaned and merged dataset ####
write_csv(cost_data_quarterly, "data/analysis_data/analysis_data.csv")
