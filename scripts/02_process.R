# scripts/02_process.R
# Purpose: Clean & standardize Divvy 2019 Q1 and 2020 Q1, derive features, save outputs.

suppressPackageStartupMessages({
  library(tidyverse)
  library(lubridate)
})

# ---- paths ----
raw_2019 <- "data_raw/Divvy_Trips_2019_Q1.csv"
raw_2020 <- "data_raw/Divvy_Trips_2020_Q1.csv"
stopifnot(file.exists(raw_2019), file.exists(raw_2020))

# ---- load ----
d19_raw <- readr::read_csv(raw_2019, show_col_types = FALSE, progress = FALSE)
d20_raw <- readr::read_csv(raw_2020, show_col_types = FALSE, progress = FALSE)

# ---- normalize helpers ----
norm_str <- function(x) {
  x %>%
    as.character() %>%
    stringr::str_trim() %>%
    na_if("") %>%
    na_if("NA") %>%
    na_if("Null") %>%
    na_if("NULL")
}

# ---- standardize schemas explicitly per vintage ----
# 2019_Q1 columns (typical):
# trip_id, start_time, end_time, bikeid, tripduration,
# from_station_id/name, to_station_id/name, usertype, gender, birthyear
d19 <- d19_raw %>%
  transmute(
    ride_id            = as.character(trip_id),
    started_at         = start_time,
    ended_at           = end_time,
    rideable_type      = NA_character_,  # not present in 2019
    start_station_name = from_station_name,
    start_station_id   = as.character(from_station_id),
    end_station_name   = to_station_name,
    end_station_id     = as.character(to_station_id),
    member_casual      = recode(usertype,
                                "Subscriber" = "member",
                                "Customer"   = "casual",
                                .default = tolower(as.character(usertype)))
  ) %>%
  mutate(
    across(c(ride_id, rideable_type,
             start_station_name, start_station_id,
             end_station_name,   end_station_id,
             member_casual), norm_str)
  )

# 2020_Q1 columns (typical):
# ride_id, rideable_type, started_at, ended_at,
# start_station_name/id, end_station_name/id, member_casual, start_lat/lng, end_lat/lng
d20 <- d20_raw %>%
  transmute(
    ride_id            = as.character(ride_id),
    rideable_type      = norm_str(rideable_type),
    started_at, ended_at,
    start_station_name = start_station_name,
    start_station_id   = as.character(start_station_id),
    end_station_name   = end_station_name,
    end_station_id     = as.character(end_station_id),
    member_casual      = recode(tolower(as.character(member_casual)),
                                "member" = "member",
                                "casual" = "casual",
                                .default = tolower(as.character(member_casual)))
  ) %>%
  mutate(
    across(c(ride_id, rideable_type,
             start_station_name, start_station_id,
             end_station_name,   end_station_id,
             member_casual), norm_str)
  )

# ---- parse datetimes, derive features, quality filters ----
clean_one <- function(df, label) {
  df %>%
    mutate(
      # interpret timestamps as local Chicago time
      started_at = ymd_hms(started_at, tz = "America/Chicago", quiet = TRUE),
      ended_at   = ymd_hms(ended_at,   tz = "America/Chicago", quiet = TRUE),
      ride_length_min = as.numeric(difftime(ended_at, started_at, units = "mins")),
      dow        = wday(started_at, label = TRUE, abbr = TRUE, week_start = 1),  # Mon..Sun
      month      = floor_date(started_at, "month"),
      hour       = hour(started_at),
      source_qtr = label
    ) %>%
    # basic data quality
    filter(!is.na(started_at), !is.na(ended_at)) %>%
    filter(ride_length_min > 0, ride_length_min < 1440) %>%  # drop <=0 and >24h
    mutate(
      member_casual = recode(member_casual,
                             "subscriber" = "member",
                             "customer"   = "casual",
                             .default = member_casual),
      member_casual = factor(member_casual, levels = c("member", "casual"))
    )
}

c19 <- clean_one(d19, "2019_Q1")
c20 <- clean_one(d20, "2020_Q1")

# ---- combine & save ----
dir.create("data_cleaned", showWarnings = FALSE, recursive = TRUE)

readr::write_csv(c19, "data_cleaned/trips_2019_Q1_clean.csv")
readr::write_csv(c20, "data_cleaned/trips_2020_Q1_clean.csv")

all_trips <- bind_rows(c19, c20)
readr::write_csv(all_trips, "data_cleaned/trips_combined_Q1_2019_2020.csv")
saveRDS(all_trips, "data_cleaned/trips_combined_Q1_2019_2020.rds")

# ---- quick sanity prints ----
cat("Rows after cleaning (combined):", nrow(all_trips), "\n")
print(summary(all_trips$ride_length_min))
print(table(all_trips$member_casual, all_trips$source_qtr, useNA = "ifany"))
