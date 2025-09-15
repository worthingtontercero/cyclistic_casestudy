# scripts/06_geo.R
# Purpose: Create ONE interactive station map (size = total rides, color = member share)

suppressPackageStartupMessages({
  library(tidyverse)
  library(readr)
  library(leaflet)
  library(htmlwidgets)
  library(scales)
  library(viridisLite)
})

# --- helper ------------------------------------------------------------------
find_file <- function(dir, pattern) {
  f <- list.files(dir, pattern = pattern, full.names = TRUE, ignore.case = TRUE)
  if (length(f) == 0) stop("No file matching '", pattern, "' in ", dir)
  f[1]
}

# --- paths -------------------------------------------------------------------
dir_clean <- "data_cleaned"
dir_raw   <- "data_raw"
out_dir   <- "visuals/maps"

# Clean maps folder (remove clutter) and recreate
if (dir.exists(out_dir)) unlink(list.files(out_dir, full.names = TRUE), recursive = TRUE)
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# Files we need
f19_clean <- find_file(dir_clean, "2019.*Q1.*clean\\.csv$")   # cleaned 2019 (names + rider type)
f20_raw   <- find_file(dir_raw,   "2020.*Q1.*\\.csv$")        # raw 2020 (has start_lat/start_lng)

# --- load --------------------------------------------------------------------
d19 <- read_csv(f19_clean, show_col_types = FALSE)
r20 <- read_csv(f20_raw,   show_col_types = FALSE)

# Harmonize 2019 names if needed
if ("from_station_name" %in% names(d19)) {
  d19 <- d19 |> rename(start_station_name = from_station_name)
}
if (!"member_casual" %in% names(d19) && "usertype" %in% names(d19)) {
  d19 <- d19 |> mutate(member_casual = if_else(tolower(usertype) == "subscriber", "member", "casual"))
}

# --- station coordinate lookup from 2020 raw ---------------------------------
need <- c("start_station_name","start_lat","start_lng")
miss <- setdiff(need, names(r20))
if (length(miss)) stop("Raw 2020 file missing: ", paste(miss, collapse = ", "))

station_coords <- r20 |>
  filter(!is.na(start_station_name), !is.na(start_lat), !is.na(start_lng)) |>
  group_by(start_station_name) |>
  summarise(
    lat = median(start_lat, na.rm = TRUE),
    lng = median(start_lng, na.rm = TRUE),
    .groups = "drop"
  ) |>
  rename(station = start_station_name)

# 2019 points (join coords); 2020 points (use raw coords directly)
d19_pts <- d19 |>
  select(member_casual, start_station_name) |>
  rename(station = start_station_name) |>
  left_join(station_coords, by = "station")

d20_pts <- r20 |>
  transmute(
    member_casual,
    station = start_station_name,
    lat = start_lat,
    lng = start_lng
  )

pts <- bind_rows(d19_pts, d20_pts) |>
  filter(!is.na(lat), !is.na(lng), !is.na(member_casual), !is.na(station))

# --- station-level summary ----------------------------------------------------
stn <- pts %>%
  count(station, lat, lng, member_casual, name = "rides") %>%
  tidyr::pivot_wider(names_from = member_casual, values_from = rides, values_fill = 0) %>%
  mutate(
    total = member + casual,
    share_member = if_else(total > 0, member / total, NA_real_),
    radius = 4 + 12 * rescale(sqrt(total), to = c(0, 1))   # size by total
  )

pal_share <- colorNumeric(viridis(256), domain = c(0, 1), na.color = "#cccccc")

m <- leaflet(stn, options = leafletOptions(minZoom = 10)) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addCircleMarkers(
    lng = ~lng, lat = ~lat,
    radius = ~radius,
    fillColor = ~pal_share(share_member),
    fillOpacity = 0.9, stroke = FALSE,
    popup = ~sprintf(
      "<b>%s</b><br/>Total: %s<br/>Members: %s<br/>Casuals: %s<br/>Member share: %s",
      station,
      comma(total), comma(member), comma(casual),
      ifelse(is.na(share_member), "NA", percent(share_member))
    )
  ) %>%
  addLegend("bottomright", pal = pal_share, values = ~share_member,
            title = "Member share (0 = casual heavy, 1 = member heavy)")

htmlwidgets::saveWidget(m, file.path(out_dir, "04_stations_both_types.html"), selfcontained = TRUE)
message("Saved: visuals/maps/04_stations_both_types.html (only map kept)")
