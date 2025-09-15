# scripts/03_analyze.R
# Purpose: Summaries + visuals for the Cyclistic case (Q1 2019 & Q1 2020)

suppressPackageStartupMessages({
  library(tidyverse)
  library(lubridate)
  library(scales)
})

# ---- load ----
d <- readr::read_csv("data_cleaned/trips_combined_Q1_2019_2020.csv", show_col_types = FALSE) %>%
  mutate(
    # enforce consistent type/order for plotting & tables
    member_casual = factor(member_casual, levels = c("member", "casual"))
  )

# ---- dirs ----
dir.create("visuals", showWarnings = FALSE, recursive = TRUE)
dir.create("docs",    showWarnings = FALSE, recursive = TRUE)

# Optional: consistent minimalist styling
theme_set(theme_minimal(base_size = 12))

# ---- summary tables ----
rides_by_type <- d %>%
  filter(!is.na(member_casual)) %>%
  count(member_casual, name = "rides")

duration_by_type <- d %>%
  filter(!is.na(member_casual)) %>%
  group_by(member_casual) %>%
  summarise(
    rides      = n(),
    avg_min    = mean(ride_length_min, na.rm = TRUE),
    median_min = median(ride_length_min, na.rm = TRUE),
    p95_min    = as.numeric(quantile(ride_length_min, 0.95, na.rm = TRUE)),
    .groups    = "drop"
  )

weekday_by_type <- d %>%
  filter(!is.na(member_casual)) %>%
  mutate(weekday = wday(started_at, label = TRUE, week_start = 1)) %>%
  count(member_casual, weekday, name = "rides")

hourly_by_type <- d %>%
  filter(!is.na(member_casual)) %>%
  mutate(hour = hour(started_at)) %>%
  count(member_casual, hour, name = "rides")

top_start_stations <- d %>%
  filter(!is.na(member_casual), !is.na(start_station_name)) %>%
  count(member_casual, start_station_name, name = "rides") %>%
  group_by(member_casual) %>%
  slice_max(rides, n = 10, with_ties = FALSE) %>%
  arrange(member_casual, desc(rides)) %>%
  ungroup()

# ---- save tables ----
readr::write_csv(rides_by_type,      "visuals/tab_rides_by_type.csv")
readr::write_csv(duration_by_type,   "visuals/tab_duration_by_type.csv")
readr::write_csv(weekday_by_type,    "visuals/tab_weekday_by_type.csv")
readr::write_csv(hourly_by_type,     "visuals/tab_hourly_by_type.csv")
readr::write_csv(top_start_stations, "visuals/tab_top_start_stations.csv")

# ---- visuals ----

# p1: total rides by rider type
p1 <- rides_by_type %>%
  ggplot(aes(member_casual, rides, fill = member_casual)) +
  geom_col(show.legend = FALSE) +
  scale_y_continuous(labels = label_comma(), expand = expansion(mult = c(0, .05))) +
  labs(title = "Total rides by rider type", x = NULL, y = "Rides")

ggsave("visuals/01_total_rides_by_type.png", p1, width = 7, height = 4, dpi = 150)

# p2: duration metrics by rider type (avg & median)
p2 <- duration_by_type %>%
  select(member_casual, avg_min, median_min) %>%
  pivot_longer(-member_casual, names_to = "metric", values_to = "minutes") %>%
  ggplot(aes(member_casual, minutes, fill = metric)) +
  geom_col(position = "dodge") +
  labs(title = "Ride duration by rider type", x = NULL, y = "Minutes")

ggsave("visuals/02_duration_by_type.png", p2, width = 7, height = 4, dpi = 150)

# p3: rides by weekday
p3 <- weekday_by_type %>%
  ggplot(aes(weekday, rides, fill = member_casual)) +
  geom_col(position = "dodge") +
  scale_x_discrete(drop = FALSE) +
  scale_y_continuous(labels = label_comma(), expand = expansion(mult = c(0, .05))) +
  labs(title = "Rides by weekday and rider type", x = "Weekday", y = "Rides")

ggsave("visuals/03_rides_by_weekday.png", p3, width = 8, height = 4, dpi = 150)

# p4: hourly ride counts
p4 <- hourly_by_type %>%
  ggplot(aes(hour, rides, color = member_casual)) +
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks = 0:23) +
  labs(title = "Hourly ride counts", x = "Hour of day", y = "Rides")

ggsave("visuals/04_hourly_rides.png", p4, width = 8, height = 4, dpi = 150)

# p5: top 10 start stations by rider type
p5 <- top_start_stations %>%
  mutate(start_station_name = fct_reorder(start_station_name, rides)) %>%
  ggplot(aes(start_station_name, rides, fill = member_casual)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  facet_wrap(~ member_casual, scales = "free_y") +
  labs(title = "Top 10 start stations by rider type", x = NULL, y = "Rides")

ggsave("visuals/05_top_start_stations.png", p5, width = 9, height = 6, dpi = 150)

# ---- session info for reproducibility ----
capture.output(sessionInfo(), file = "docs/sessionInfo.txt", append = TRUE)

message("Analysis complete. See the 'visuals/' folder for charts and CSVs.")
