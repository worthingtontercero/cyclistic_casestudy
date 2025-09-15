# scripts/tables.R
# Purpose: Build publication-quality tables for the Cyclistic case
# Inputs: CSVs produced by scripts/03_analyze.R
# Outputs: HTML tables saved to report/

suppressPackageStartupMessages({
  library(tidyverse)
  library(readr)
  library(scales)
  library(gt)
})

# ---- dirs ----
dir.create("report", showWarnings = FALSE, recursive = TRUE)

# ---- load tables created by 03_analyze.R ----
rides_by_type      <- readr::read_csv("visuals/tab_rides_by_type.csv", show_col_types = FALSE)
duration_by_type   <- readr::read_csv("visuals/tab_duration_by_type.csv", show_col_types = FALSE)
weekday_by_type    <- readr::read_csv("visuals/tab_weekday_by_type.csv", show_col_types = FALSE)
hourly_by_type     <- readr::read_csv("visuals/tab_hourly_by_type.csv", show_col_types = FALSE)
top_start_stations <- readr::read_csv("visuals/tab_top_start_stations.csv", show_col_types = FALSE)

# enforce consistent ordering & casing
lvl <- c("member", "casual")
fmt_rider <- function(x) stringr::str_to_title(as.character(x))

# ---- Table 1: Rider type overview (rides + duration stats) ----
overview <- rides_by_type %>%
  filter(!is.na(member_casual)) %>%
  mutate(member_casual = factor(member_casual, levels = lvl)) %>%
  arrange(member_casual) %>%
  mutate(share = rides / sum(rides)) %>%
  left_join(
    duration_by_type %>%
      select(member_casual, avg_min, median_min, p95_min),
    by = "member_casual"
  ) %>%
  mutate(
    Rider = fmt_rider(member_casual),
    Rides = rides,
    Share = share,
    `Avg (min)`    = avg_min,
    `Median (min)` = median_min,
    `95th pct (min)` = p95_min
  ) %>%
  select(Rider, Rides, Share, `Avg (min)`, `Median (min)`, `95th pct (min)`)

overview_gt <- overview %>%
  gt() %>%
  tab_header(title = "Rider Type Overview — Q1 2019 & Q1 2020") %>%
  fmt_number(columns = c(Rides), decimals = 0, use_seps = TRUE) %>%
  fmt_percent(columns = c(Share), decimals = 1) %>%
  fmt_number(columns = c(`Avg (min)`, `Median (min)`, `95th pct (min)`), decimals = 1) %>%
  cols_align(align = "right", columns = everything()) %>%
  tab_source_note(source_note = "Data: Divvy Trip Data (Q1 2019 & Q1 2020); cleaning scripts/02_process.R; summaries scripts/03_analyze.R")

gtsave(overview_gt, "report/table_rider_type_overview.html")

# ---- Table 2: Weekday distribution (rides) by rider type ----
weekday_levels <- c("Mon","Tue","Wed","Thu","Fri","Sat","Sun")
weekday_tab <- weekday_by_type %>%
  filter(!is.na(member_casual), !is.na(weekday)) %>%
  mutate(
    member_casual = factor(member_casual, levels = lvl),
    weekday = factor(as.character(weekday), levels = weekday_levels)
  ) %>%
  arrange(member_casual, weekday) %>%
  group_by(member_casual) %>%
  mutate(share_within_type = rides / sum(rides)) %>%
  ungroup() %>%
  select(member_casual, weekday, rides, share_within_type)

weekday_wide <- weekday_tab %>%
  select(member_casual, weekday, rides) %>%
  pivot_wider(names_from = weekday, values_from = rides, values_fill = 0) %>%
  arrange(member_casual) %>%
  mutate(Rider = fmt_rider(member_casual)) %>%
  select(Rider, all_of(weekday_levels))

weekday_gt <- weekday_wide %>%
  gt() %>%
  tab_header(title = "Rides by Weekday — Q1 2019 & Q1 2020") %>%
  fmt_number(columns = all_of(weekday_levels), decimals = 0, use_seps = TRUE) %>%
  cols_align(align = "right", columns = all_of(weekday_levels)) %>%
  tab_source_note(source_note = "Rides by calendar weekday; week starts Monday.")

gtsave(weekday_gt, "report/table_weekday_by_type.html")

# ---- Table 3: Hourly distribution (rides) by rider type ----
hourly_tab <- hourly_by_type %>%
  filter(!is.na(member_casual), !is.na(hour)) %>%
  mutate(member_casual = factor(member_casual, levels = lvl),
         hour = as.integer(hour)) %>%
  arrange(member_casual, hour)

hour_levels <- as.character(0:23)
hourly_wide <- hourly_tab %>%
  mutate(hour = factor(hour, levels = 0:23, labels = hour_levels)) %>%
  select(member_casual, hour, rides) %>%
  pivot_wider(names_from = hour, values_from = rides, values_fill = 0) %>%
  arrange(member_casual) %>%
  mutate(Rider = fmt_rider(member_casual)) %>%
  select(Rider, all_of(hour_levels))

hourly_gt <- hourly_wide %>%
  gt() %>%
  tab_header(title = "Rides by Hour of Day — Q1 2019 & Q1 2020") %>%
  fmt_number(columns = all_of(hour_levels), decimals = 0, use_seps = TRUE) %>%
  cols_align(align = "right", columns = all_of(hour_levels)) %>%
  tab_source_note(source_note = "Hour is local start time (0–23).")

gtsave(hourly_gt, "report/table_hourly_by_type.html")

# ---- Table 4: Top start stations by rider type (Top 10 each) ----
tops_fmt <- top_start_stations %>%
  filter(!is.na(member_casual), !is.na(start_station_name)) %>%
  mutate(
    Rider = fmt_rider(member_casual),
    `Start station` = start_station_name,
    Rides = rides
  ) %>%
  arrange(Rider, desc(Rides)) %>%
  select(Rider, `Start station`, Rides)

tops_gt <- tops_fmt %>%
  gt(groupname_col = "Rider") %>%
  tab_header(title = "Top 10 Start Stations by Rider Type — Q1 2019 & Q1 2020") %>%
  fmt_number(columns = c(Rides), decimals = 0, use_seps = TRUE) %>%
  cols_align(align = "left", columns = c(`Start station`)) %>%
  cols_align(align = "right", columns = c(Rides)) %>%
  tab_source_note(source_note = "Top 10 per rider segment; ties broken by ride count.")

gtsave(tops_gt, "report/table_top_start_stations.html")

message("Tables generated: see 'report/' for HTML outputs.")
