# scripts/05_more_plots.R
# Extra visuals: percentage views, duration distribution, rideable_type split, OD flows

suppressPackageStartupMessages({
  library(tidyverse)
  library(lubridate)
  library(scales)
})

dir.create("visuals", showWarnings = FALSE, recursive = TRUE)

d <- readr::read_csv("data_cleaned/trips_combined_Q1_2019_2020.csv", show_col_types = FALSE) %>%
  mutate(member_casual = factor(member_casual, levels = c("member", "casual")))

theme_set(theme_minimal(base_size = 12))

# ---- 6) Weekday % stacked bar ----
wk_pct <- d %>%
  filter(!is.na(member_casual)) %>%
  mutate(weekday = wday(started_at, label = TRUE, week_start = 1)) %>%
  count(weekday, member_casual, name = "rides") %>%
  group_by(weekday) %>%
  mutate(pct = rides / sum(rides)) %>%
  ungroup()

p6 <- wk_pct %>%
  ggplot(aes(weekday, pct, fill = member_casual)) +
  geom_col() +
  scale_x_discrete(drop = FALSE) +
  scale_y_continuous(labels = label_percent(accuracy = 1), expand = expansion(mult = c(0, .02))) +
  labs(title = "Share of rides by weekday", x = "Weekday", y = "Percent", fill = "Rider type")

ggsave("visuals/06_weekday_share.png", p6, width = 8, height = 4.5, dpi = 150)
readr::write_csv(wk_pct, "visuals/tab_weekday_share_long.csv")

# ---- 7) Ride-length distribution (log x) ----
# Trim extreme tail for clarity; keep within QA-clean bound (<= 240 min here)
p7 <- d %>%
  filter(!is.na(member_casual), ride_length_min > 0, ride_length_min <= 240) %>%
  ggplot(aes(ride_length_min, fill = member_casual)) +
  geom_histogram(binwidth = 2, position = "identity", alpha = 0.5) +
  scale_x_continuous(trans = "log10", labels = label_number(accuracy = 1)) +
  labs(title = "Ride duration distribution (0–240 min, log scale)",
       x = "Minutes (log10)", y = "Rides", fill = "Rider type")

ggsave("visuals/07_duration_distribution.png", p7, width = 8, height = 4.5, dpi = 150)

# ---- 8) Rideable type composition ----
if ("rideable_type" %in% names(d)) {
  type_tab <- d %>%
    filter(!is.na(member_casual), !is.na(rideable_type)) %>%
    count(member_casual, rideable_type, name = "rides") %>%
    group_by(member_casual) %>%
    mutate(pct = rides / sum(rides)) %>%
    ungroup()

  readr::write_csv(type_tab, "visuals/tab_rideable_type_split.csv")

  p8 <- type_tab %>%
    ggplot(aes(rideable_type, pct, fill = member_casual)) +
    geom_col(position = position_dodge(width = 0.8)) +
    scale_y_continuous(labels = label_percent(accuracy = 1), expand = expansion(mult = c(0, .02))) +
    labs(title = "Rideable type split by rider type", x = "Rideable type", y = "Percent", fill = "Rider type")

  ggsave("visuals/08_rideable_type_split.png", p8, width = 8, height = 4.5, dpi = 150)
}

# ---- 9) Top origin → destination flows (edge list) ----
od_top <- d %>%
  filter(!is.na(member_casual), !is.na(start_station_name), !is.na(end_station_name)) %>%
  count(member_casual, start_station_name, end_station_name, name = "rides") %>%
  group_by(member_casual) %>%
  slice_max(rides, n = 20, with_ties = FALSE) %>%
  ungroup()

readr::write_csv(od_top, "visuals/tab_top_od_flows.csv")

p9 <- od_top %>%
  mutate(pair = paste(start_station_name, "→", end_station_name)) %>%
  group_by(member_casual) %>%
  mutate(pair = fct_reorder(pair, rides)) %>%
  ungroup() %>%
  ggplot(aes(pair, rides, fill = member_casual)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  scale_y_continuous(labels = label_number_si(accuracy = 1), expand = expansion(mult = c(0, .05))) +
  facet_wrap(~ member_casual, scales = "free_y") +
  labs(title = "Top 20 origin → destination flows by rider type", x = NULL, y = "Rides")

ggsave("visuals/09_top_od_flows.png", p9, width = 10, height = 7, dpi = 150)

message("05_more_plots: saved 06–09_* in visuals/ plus CSVs.")
