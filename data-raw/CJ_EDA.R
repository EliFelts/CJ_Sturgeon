#######################################
# Some exploratory analysis that Phil #
# and I went through to understand    #
# the data but that didn't really fit #
# into shiny summaries or anything.   #
# else                                #
#######################################


library(tidyverse)
library(arrow)
library(readxl)
library(suncalc)
library(sf)
library(slider)

source("R/identify_cluster.R")
source("R/interpolate_hourly.R")
source("R/fish_animations.R")
source("R/detect_delim.R")
source("R/read_detections.R")

filtered_fish_detections <- read_feather("data-raw/filtered_detection_data")

### Get a sense of whether max depth
### at a receiver is/was reflecting the
# bottom for each location

depth_location_summary <- filtered_fish_detections |>
  mutate(detection_month = month(detection_datetime_local)) |>
  filter(sensor_type == "depth") |>
  group_by(detection_month, location_id) |>
  summarize(max_depth = max(real_sensor, na.rm = T))


# look for when fish were potentially caught

zero_detections <- filtered_fish_detections |>
  filter(
    sensor_type == "depth",
    real_sensor < 3
  )

catch.query <- filtered_fish_detections |>
  filter(sensor_type == "depth") |>
  arrange(fish_id, detection_datetime) |>
  group_by(fish_id) |>
  mutate(
    depth_roll_med = slide_dbl(real_sensor, median, .before = 12, .complete = TRUE),
    depth_roll_sd  = slide_dbl(real_sensor, sd, .before = 12, .complete = TRUE)
  ) |>
  mutate(shallow_flag = real_sensor < (depth_roll_med - 2 * depth_roll_sd)) |>
  mutate(
    shallow_run = with(rle(shallow_flag), rep(lengths, lengths))
  ) %>%
  mutate(
    capture_candidate = shallow_flag & shallow_run >= 3
  )

dat_events <- catch.query %>%
  arrange(fish_id, detection_datetime) %>%
  group_by(fish_id) %>%
  mutate(
    flag_change = capture_candidate != lag(capture_candidate, default = FALSE),
    run_id = cumsum(flag_change)
  ) %>%
  ungroup()

candidate_sequences <- dat_events %>%
  filter(capture_candidate) %>%
  group_by(fish_id, run_id) %>%
  summarise(
    start_time = min(detection_datetime),
    end_time = max(detection_datetime),
    n_points = n(),
    min_depth = min(real_sensor, na.rm = TRUE),
    median_depth = median(real_sensor, na.rm = TRUE),
    baseline_depth = median(depth_roll_med, na.rm = TRUE),
    min_depth_ratio = min(real_sensor / depth_roll_med, na.rm = TRUE),
    .groups = "drop"
  )
