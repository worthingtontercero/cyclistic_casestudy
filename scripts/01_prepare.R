# ============================
# Script: prepare.R
# Purpose: Load and inspect raw Divvy datasets (2019 Q1, 2020 Q1)
# ============================

# Load required packages
library(tidyverse)
library(lubridate)

# Define file paths (raw data)
path_2019 <- "data_raw/Divvy_Trips_2019_Q1.csv"
path_2020 <- "data_raw/Divvy_Trips_2020_Q1.csv"

# Load datasets
trips_2019 <- readr::read_csv(path_2019)
trips_2020 <- readr::read_csv(path_2020)

# Inspect structure and row counts
glimpse(trips_2019)
glimpse(trips_2020)

cat("Row count (2019 Q1):", nrow(trips_2019), "\n")
cat("Row count (2020 Q1):", nrow(trips_2020), "\n")
