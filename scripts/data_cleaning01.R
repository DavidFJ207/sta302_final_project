#### Preamble ####
# Purpose: Cleans the raw data and merges them into one dataset
# Author: Gadiel David Flores, Wendy
# Date: 09/27/24
# Contact: davidgadiel.flores@mail.toronto.com
# License: MIT
# Pre-requisites: Data needs to be extracted from Statistics Canada to file raw_data

#### Work space setup ####
library(dplyr)
library(tidyverse)
library(readr)

#### Data innit ####
raw_cpi <- read_csv(here::here("data/raw_data/cpi.csv"))
raw_gdp <- read_csv(here::here("data/raw_data/gdp.csv"))
raw_house_construction <- read_csv(here::here("data/raw_data/house_construction.csv"))
raw_housing_price <- read_csv(here::here("data/raw_data/housing_price.csv"))
raw_housing_absorbtion <- read_csv(here::here("data/raw_data/housing_absorbtion.csv"))

#### Helper Function to Process Data ####
process_data <- function(data, row_num, variable_name) {
  data_processed <- data %>%
    filter(row_number() == row_num) %>% 
    select(-1) %>% 
    t() %>% 
    as.data.frame()
  names(data_processed) <- "Value"
  data_processed$Value <- as.numeric(gsub(",", "", data_processed$Value))
  
  # Calculate the number of rows and create quarterly labels
  num_rows <- nrow(data_processed)
  num_quarters <- ceiling(num_rows / 3)
  quarters <- seq(from = as.Date("1997-07-01"), by = "3 months", length.out = num_quarters)
  quarters <- rep(quarters, each = 3)[1:num_rows]
  
  # Combine the data with the quarterly labels
  data_processed <- data_processed %>%
    mutate(Quarter = quarters)
  
  # Group by quarter and calculate the average
  data_quarterly <- data_processed %>%
    group_by(Quarter) %>%
    summarise(!!variable_name := mean(Value, na.rm = TRUE))
  
  return(data_quarterly)
}

#### Process All Relevant Datasets ####
# Define a list of parameters for processing the datasets
data_list <- list(
  list(data = raw_housing_absorbtion, row = 12, name = "Detached_Absorption_Quarterly_Avg"),
  list(data = raw_housing_absorbtion, row = 13, name = "Semi_Absorption_Quarterly_Avg"),
  list(data = raw_housing_absorbtion, row = 15, name = "Detached_Unabsorbed_Quarterly_Avg"),
  list(data = raw_housing_absorbtion, row = 16, name = "Semi_Unabsorbed_Quarterly_Avg"),
  list(data = raw_gdp, row = 13, name = "GDP_Quarterly_Avg"),
  list(data = raw_cpi, row = 11, name = "CPI_Quarterly_Avg")
)

# Filter the 13th row
starting_construction_detached_data <- raw_house_construction %>%
  filter(row_number() == 13)
starting_construction_detached_data <- starting_construction_detached_data %>%
  select(-1,-2) 
# Filter the 14th row
starting_construction_semi_data <- raw_house_construction %>%
  filter(row_number() == 15)
starting_construction_semi_data <- starting_construction_semi_data %>%
  select(-1,-2) 
# Filter the 20th row 
under_construction_detached <- raw_house_construction %>%
  filter(row_number() == 19)
under_construction_detached <- under_construction_detached %>%
  select(-1,-2) 
# Filter the 22th row 
completed_construction_semi <- raw_house_construction %>%
  filter(row_number() == 21)
completed_construction_semi <- completed_construction_semi %>%
  select(-1,-2) 
# Filter the 26th row 
completed_construction_detached <-raw_house_construction %>%
  filter(row_number() == 25)
completed_construction_detached <- completed_construction_detached %>%
  select(-1,-2) 
# Filter the 28th row of the second column
under_construction_semi <- raw_house_construction %>%
  filter(row_number() == 27)
under_construction_semi <- under_construction_semi %>%
  select(-1,-2) 

# Initialize combined data with housing price processing
cost_data_quarterly <- process_data(raw_housing_price, 11, "Quarterly_Average")

# Process each dataset and merge it into the main dataset
for (params in data_list) {
  processed_data <- process_data(params$data, params$row, params$name)
  cost_data_quarterly <- cost_data_quarterly %>%
    left_join(processed_data, by = "Quarter")
}

#### Save the cleaned and merged dataset ####
write_csv(cost_data_quarterly, "data/analysis_data/analysis_data.csv")
