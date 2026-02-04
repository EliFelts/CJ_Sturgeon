####################################
# Code for processing new data and #
# creating necessary tables and    #
# summaries for Shiny app.         #
####################################

# load libraries


# remotes::install_github("EliFelts/IDFGtelemetry", force=TRUE)

library(tidyverse)
library(arrow)
library(readxl)
library(suncalc)
library(sf)
library(tictoc)
library(IDFGtelemetry)



# define the path to where shared files are stored (will change
# to Sharepoint once we get that setup)


shared_parent.dir <- "~/Library/CloudStorage/OneDrive-SunnysideInsights/CJ_Telemetry_Sync"

# Read in deployment locations

deploy_locations.df <- read_excel(path = str_c(shared_parent.dir, "deployment_locations.xlsx", sep = "/"))

# Read in deployments and join to locations; drop any that
# are not in CJ or don't have a start_datetime; also,
# designate explicitly that the start and end dates
# here are Mountain Time

deployments.df <- read_excel(path = str_c(shared_parent.dir, "deployment_data.xlsx", sep = "/")) |>
  left_join(deploy_locations.df, by = "location_id") |>
  filter(
    waterbody == "CJ Strike Reservoir",
    !is.na(start_datetime)
  ) |>
  mutate(receiver_model = word(internal_receiver_id, 1, sep = "-")) |>
  mutate(
    start_datetime = force_tz(start_datetime, "America/Denver"),
    end_datetime = force_tz(end_datetime, "America/Denver")
  )


# first make a list of all the files in that folder

spec_sheet.files <- list.files(
  path = str_c(shared_parent.dir, "spec_sheets", sep = "/"),
  full.names = T
)

# read in all the spec sheets files
# and bind together into a single table;
# just a note here that I think Innovasea
# usually sends these as xlsx files but the
# one Phil shared with me is a csv. If you end
# up adding more, they will need to be saved as
# csv for the following code to work in termes
# of reading in every file and binding together

spec.read <- map(
  spec_sheet.files,
  ~ read_csv(.x)
) %>%
  bind_rows()

# now reformat some columns and only keep desired fields
# for the transmitter table

transmitters.df <- spec.read %>%
  mutate(
    tag_life_days = as.numeric(`Est tag life (days)`),
    minimum_delay = as.numeric(`Step 1 Min Delay (sec)`),
    maximum_delay = as.numeric(`Step 1 Max Delay (sec)`),
    mean_delay = (minimum_delay + maximum_delay) / 2,
    ship_date = ymd(`Ship Date`)
  ) %>%
  select(
    serial_number = `Serial No.`,
    tag_family = `Tag Family`,
    tag_life_days,
    minimum_delay,
    maximum_delay,
    mean_delay,
    power = `Step 2 Power (L/H)`,
    ship_date,
    researcher = Researcher
  ) %>%
  distinct()

# construct a separate table for VUE Tag IDs, as
# there can be more than one per transmitter; in this
# data set specifically all of these transmitters
# have 2 VUE Tag ID because they have depth and
# temperature sensors which each have their own

acoustic_id.df <- spec.read %>%
  mutate(
    range = as.numeric(Range),
    sensor_type = `Sensor type`,
    sensor_slope = as.numeric(Slope),
    sensor_intercept = as.numeric(Intercept)
  ) %>%
  mutate(
    sensor_type = case_when(
      sensor_type == "T" ~ "temperature",
      sensor_type == "P" ~ "depth",
      TRUE ~ sensor_type
    ),
    sensor_minimum = case_when(
      sensor_type == "temperature" ~ -5,
      sensor_type == "depth" ~ 0,
      TRUE ~ NA
    ),
    sensor_maximum = case_when(
      sensor_type == "temperature" ~ 35,
      sensor_type == "depth" ~ range,
      TRUE ~ NA
    )
  ) %>%
  select(
    acoustic_tag_id = `VUE Tag ID`,
    serial_number = `Serial No.`,
    sensor_type, sensor_minimum,
    sensor_maximum, sensor_slope,
    sensor_intercept
  )

# read in fish data; for right now i just put all the tags on
# here in order so i could work through the code
# but they didn't actually have transmitter id

# note that there are ways to code these if you end up
# reusing transmitters; this code doesn't have that built in
# now because that hasn't happened in these fish

fish.df <- read_excel(path = str_c(shared_parent.dir, "Telemetry_TaggedSturgeon_Info.xlsx", sep = "/")) |>
  mutate(
    fish_id = str_c(serial_number, date_caught, "STG", sep = "_"),
    species = "STG",
    release_datetime = force_tz(date_caught, "America/Denver")
  ) |>
  left_join(transmitters.df, by = "serial_number") |>
  mutate(fish_end_date = release_datetime + days(tag_life_days)) |>
  select(fish_id,
    serial_number, species, release_datetime,
    latitude = release_location_lat, longitude = release_location_long,
    fork_length_cm = fish_fork_length, fish_end_date, mean_delay
  )

# all detection csv files should be placed in the
# shared OneDrive folder (migrate to sharepoint once setup)

# make a list of every csv file in the directory

detections.files <- list.files(
  path = str_c(shared_parent.dir, "detection_csv", sep = "/"),
  pattern = "\\.csv$",
  full.names = T
)

# sometimes can encounter downloaded files that are blank, which causes them
# to get read in with different column types and then they
# can't be bound together; here, a function is used to
# drop those cases when it's just a blank csv, so basically
# this will read in every csv and bind them all together
# into a single data frame

detections.read <- map_dfr(detections.files, function(file) {
  df <- read_csv(file,
    col_types = cols(
      `Date and Time (UTC)` = col_datetime(format = ""),
      Receiver = col_character(),
      Transmitter = col_character(),
      `Sensor Value` = col_double(),
      .default = col_skip()
    )
  )

  if (nrow(df) > 0) {
    df
  } else {
    NULL
  }
}) %>%
  distinct() %>%
  select(
    detection_datetime = `Date and Time (UTC)`,
    internal_receiver_id = Receiver,
    acoustic_tag_id = Transmitter,
    raw_sensor = `Sensor Value`
  )

# join to acoustic id table so real sensor values
# can be calculated for each detection, where applicable

detections_acousticid.join <- detections.read %>%
  left_join(acoustic_id.df, by = "acoustic_tag_id") %>%
  mutate(
    real_sensor = if_else(
      !is.na(raw_sensor),
      sensor_slope * raw_sensor + sensor_intercept,
      NA_real_
    ),
    detection_datetime_local = with_tz(detection_datetime, tz = "America/Denver")
  )

# create a range-based join to use when joining
# detections to deployments, so that they are joined
# to the receiver deployments based on matching the receiver id
# and the detection date falling within the start and
# end of a particular deployment

deployment_by <- join_by(internal_receiver_id, between(detection_datetime_local, start_datetime, end_datetime))

# now perform the join to deployments; note that nested
# within this the deployment end_datetime is given a far-future
# time stamp if it is currently NA because those are
# actively deployed receivers

detections_deployments.join <- detections_acousticid.join %>%
  left_join(
    deployments.df %>%
      mutate(end_datetime = coalesce(end_datetime, as.POSIXct("9999-12-31"))),
    by = deployment_by
  )


# create another range-based join to join detection to fish
# based off the date range that the fish had that
# particular transmitter; this is necessary because
# some transmitters have been used in multiple fish

fish_by <- join_by(serial_number, between(detection_datetime_local, release_datetime, fish_end_date))

# now perform the join to fish; similar to receivers there
# is a a step in here where end_date is given a far-future
# time stamp if it is currently NA because they haven't been
# confirmed as being removed

detection_fish.join <- detections_deployments.join %>%
  filter(!is.na(detection_datetime)) %>%
  left_join(
    fish.df,
    by = fish_by
  ) %>%
  select(fish_id, species, serial_number, acoustic_tag_id,
    detection_datetime, detection_datetime_local, internal_receiver_id,
    sensor_type, real_sensor, sensor_maximum,
    location_id, deployment_id,
    latitude = latitude.x,
    longitude = longitude.x
  )

nodt <- detections_deployments.join |>
  filter(is.na(detection_datetime))

# now add in some other categorizations for the detections
# for season and whether at day versus night

detection_sunlight.dat <- tibble(detection_date = seq(min(detection_fish.join$detection_datetime),
  max(detection_fish.join$detection_datetime),
  by = "days"
)) %>%
  mutate(
    detection_date = as_date(detection_date),
    getSunlightTimes(
      date = detection_date,
      lat = 42.97197,
      lon = -115.8568,
      tz = "America/Denver"
    )
  ) %>%
  select(detection_date, dawn, dusk)


detections_sunlight.join <- detection_fish.join %>%
  filter(!is.na(deployment_id)) |>
  mutate(
    detection_datetime = with_tz(detection_datetime, tz = "America/Denver"),
    detection_date = as_date(detection_datetime)
  ) %>%
  left_join(detection_sunlight.dat, by = "detection_date") %>%
  mutate(
    daylight_category = case_when(
      detection_datetime >= dawn & detection_datetime <= dusk ~ "day",
      TRUE ~ "night"
    ),
    season = case_when(
      month(detection_date) %in% c(12, 1, 2) ~ "winter",
      month(detection_date) %in% 3:5 ~ "spring",
      month(detection_date) %in% 6:8 ~ "summer",
      month(detection_date) %in% 9:11 ~ "fall"
    )
  )

# get only detections associated with fish and that can
# be associated with a known location, and
# find suspected false detections flagged by being
# isolated from any other detections for a given
# fish/deployment combination by more than 1 hour;
# to adjust the window for this criteria change
# the value in min_gap > x; currently, this
# uses the convention of 30X the nominal delay
# of slowest transmitters; so basically it's saying that for
# transmitters with a nominal delay of 180 s - which
# is what most in this data set are - if a single detection
# occurs in isolation by 1.5 hours (3600 s) from any others for a given fish
# at a given location then it is likely false and due
# to tag collision or other mechanisms; note that
# this uses location and fish id, not necessarily receiver
# and transmitter id

fish_detections.dat <- detections_sunlight.join |>
  filter(
    !is.na(fish_id),
    !is.na(location_id)
  ) |>
  arrange(location_id, fish_id, detection_datetime) |>
  group_by(location_id, fish_id) |>
  mutate(
    lag_gap = as.numeric(detection_datetime - lag(detection_datetime), units = "secs"),
    lead_gap = as.numeric(lead(detection_datetime) - detection_datetime, units = "secs"),
    lag_gap = replace_na(lag_gap, Inf),
    lead_gap = replace_na(lead_gap, Inf),
    min_gap = pmin(lag_gap, lead_gap),
    flag_false = min_gap > 5400
  )

# flag fish that show up in clustered
# patterns at a single receiver in a way that
# suggests they may potentially be dead; i wrote
# a custom function for this; see the documentation
# for IDFGtelemetry::indentify_cluster() for details
# of what it's doing; right now i think
# it's not quite sensitive enough but it's difficult
# to set hard and fast criteria for what clusters
# indicate a fish may be dead in range of a receiver;
# this function takes ~ 5-10 minutes to run

flag_clustered_fish.df <- identify_cluster(detection.df = fish_detections.dat)

# summarize relevant detection history metrics for individuals;
# this will be used to evaluate criteria for whether to
# treat individuals as part of the sample (i.e. drop those
# suspected to have died shortly after tagging) and if
# they need to be right-censored then when that should occur

# right now, dropping out detections that were flagged
# as potentially false because that's standard telemetry
# analysis protocol...there are arguments to go either way on
# that, but it probably won't make a huge difference in determining
# fish status

fish_battery.df <- fish.df %>%
  left_join(transmitters.df, by = "serial_number") %>%
  select(fish_id, tag_life_days)

individual_detection.table <- fish_detections.dat %>%
  filter(flag_false == F) %>% # remove false detections
  left_join(fish.df, by = c(
    "fish_id", "species",
    "serial_number"
  )) %>%
  left_join(fish_battery.df, by = "fish_id") |>
  mutate(days_at_large = (as.numeric(detection_datetime) - as.numeric(release_datetime)) / 86400) |>
  group_by(fish_id) %>%
  summarize(
    n_detections = n(),
    unique_locations = n_distinct(location_id),
    max_days_at_large = round(as.numeric(max(days_at_large, na.rm = TRUE))),
    latest_detection = max(detection_datetime, na.rm = T),
    .latest_idx = which.max(replace_na(detection_datetime, as.POSIXct("1970-01-01", tz = "UTC"))),
    latest_location = location_id[.latest_idx],
    n_oversensormax = sum(real_sensor > sensor_maximum, na.rm = T),
    start_oversensormax = min(detection_datetime[real_sensor > sensor_maximum], na.rm = T),
    fish_end_date = first(fish_end_date),
    latest_receiver = internal_receiver_id[.latest_idx],
    latest_lat = latitude.x[.latest_idx],
    latest_lon = longitude.x[.latest_idx],
    .groups = "drop"
  ) |>
  mutate(
    latest_download = max(fish_detections.dat$detection_datetime),
    presumed_active = ifelse(latest_download < fish_end_date, TRUE, FALSE),
    time_since_last = case_when(
      presumed_active == TRUE ~ (as.numeric(latest_download) - as.numeric(latest_detection)) / 86400,
      presumed_active == FALSE ~ (as.numeric(fish_end_date) - as.numeric(latest_detection)) / 86400
    )
  )


# left join to original fish table so
# those that were never detected are included

fish_summary_join <- fish.df |>
  select(
    fish_id, serial_number, release_datetime, fish_end_date,
    species, fork_length_cm
  ) |>
  left_join(individual_detection.table, by = c("fish_id", "fish_end_date")) |>
  left_join(fish_battery.df, by = "fish_id") |>
  mutate(
    across(
      c(n_detections, unique_locations),
      ~ coalesce(.x, 0)
    ),
    battery_end_date = release_datetime + days(tag_life_days)
  ) |>
  left_join(flag_clustered_fish.df, by = "fish_id")

# assign status as excluded, right censored,
# or active based on criteria; also make
# columns indicating reasons for these
# determinations; make a few things factors
# bc that works better in some of the shiny
# table displays; right now i'm gonna set these
# to not right censor; right now no flagged clusters
# either and not using max gap, so just take out
# the part about flagging

fish_summary_state <- fish_summary_join |>
  mutate(
    status = case_when(
      n_detections == 0 ~ "excluded",
      max_days_at_large <= 30 ~ "excluded",
      fish_end_date < battery_end_date ~ "right_censored",
      # time_since_last >= 365 & fish_end_date == battery_end_date ~ "right_censored",
      # fish_end_date == battery_end_date & battery_end_date < today() ~ "right_censored",
      TRUE ~ "active"
    ),
    exclude_reason = case_when(
      n_detections == 0 ~ "never_detected",
      !is.na(max_days_at_large) & max_days_at_large <= 30 ~ "handling_window",
      TRUE ~ NA
    ),
    censor_reason = case_when(
      fish_end_date < battery_end_date ~ "known_mortality",
      time_since_last >= 365 & fish_end_date == battery_end_date ~ "missing_year",
      fish_end_date == battery_end_date & battery_end_date < today() ~ "battery_expired",
      TRUE ~ NA
    ),
    # flags = case_when(
    #   !is.na(max_days_at_large) & max_days_at_large > 30 & fish_end_date == battery_end_date & !is.na(flagged_runs) ~ "suspicious_cluster",
    #   !is.na(max_days_at_large) & max_days_at_large > 30 & fish_end_date == battery_end_date & is.na(flagged_runs) & max_gap_days >= 200 ~ "long_gap",
    #   TRUE ~ NA
    # ),
    censor_date = case_when(
      censor_reason == "known_mortality" ~ fish_end_date,
      # censor_reason == "missing_year" ~ latest_detection + days(1),
      # censor_reason == "battery_expired" ~ battery_end_date,
      TRUE ~ NA
    ),
    status = factor(status,
      levels = c("active", "right_censored", "excluded")
    ),
    censor_reason = factor(censor_reason,
      levels = c(
        "battery_expired",
        "known_mortality",
        "missing_year"
      )
    )
  )

# write fish summary table as an output. At this point this
# table is informative for the shiny app as well as
# subsequent analysis where we need to know which fish should
# be considered alive at any given time; note that the filters
# for dropping fish from analysis are still being refined;
# this is a good start, with some additional candidates
# being flagged, and a few other situations that need to
# be considered


write_feather(fish_summary_state, "shiny_pieces/individual_summary")

# summarize depth reading by day for individuals
# that had depth tags


individual_dailydepth.summary <- fish_detections.dat |>
  filter(
    !flag_false,
    sensor_type == "depth",
    real_sensor > 0
  ) |>
  group_by(fish_id, detection_date) |>
  summarize(
    min_depth_ft = min(real_sensor * 3.28084),
    median_depth_ft = median(real_sensor * 3.28084),
    max_depth_ft = max(real_sensor * 3.28084),
    count = n(),
    sensor_maximum = first(sensor_maximum) * 3.28084
  ) |>
  mutate(pt_class = ifelse(median_depth_ft > sensor_maximum, "exceed", "normal"))

# write daily depth summaries for use in Shiny

write_feather(individual_dailydepth.summary, "shiny_pieces/individual_dailydepth_summary")

# summarize daily detections by location for individuals

individual_daily.summary <- fish_detections.dat %>%
  filter(!flag_false) %>%
  group_by(fish_id, location_id, detection_date) %>%
  summarize(count = n()) |>
  left_join(fish.df, by = "fish_id")

# write daily detection summaries for use in Shiny

write_feather(individual_daily.summary, "shiny_pieces/individual_daily_summary")

## summarize locations where individuals
## have been detected for use in maps of
# individual fish

individual_receiver_summary <- fish_detections.dat %>%
  filter(
    !is.na(fish_id),
    !flag_false
  ) %>%
  group_by(fish_id, location_id) %>%
  summarize(
    detections = n(),
    earliest_date = min(detection_date),
    latest_datetime = max(detection_datetime),
    latest_date = as_date(latest_datetime),
    unique_days = n_distinct(detection_date),
    latitude = first(latitude),
    longitude = first(longitude)
  ) %>%
  group_by(fish_id) %>%
  mutate(
    most_recent_receiver = latest_datetime == max(latest_datetime, na.rm = T),
    recent_status = case_when(
      most_recent_receiver == TRUE ~ "Latest",
      TRUE ~ "Previous"
    )
  )

write_feather(individual_receiver_summary, "shiny_pieces/individual_receiver_summary")

# summarizing deployments for shiny app

# daily counts by deployment

location_daily <- fish_detections.dat %>%
  filter(!flag_false) |>
  group_by(location_id, species, detection_date) %>%
  summarize(
    detections = n(),
    individuals = n_distinct(fish_id)
  )

write_feather(
  location_daily,
  "shiny_pieces/deployment_daily"
)

# active receivers

location_latest <- location_daily %>%
  group_by(location_id) %>%
  slice(which.max(detection_date)) %>%
  select(location_id,
    most_recent = detection_date
  )

# summarize active deployments...this will be robust
# to a situation where you add a new receiver at a
# location where another one was lost; also,
# right now the firmwarer/battery change dates
# are just placeholders in case we wanted to add
# that actual info in later

active_deployments <- deployments.df |>
  group_by(location_id, latitude, longitude) |>
  arrange(end_datetime) |>
  summarize(
    last_removed = max(end_datetime),
    internal_receiver_id = paste(sort(unique(internal_receiver_id)), collapse = ", "),
    end_reason = last(end_reason),
    .groups = "drop"
  ) |>
  mutate(status = case_when(
    is.na(end_reason) ~ "Active",
    TRUE ~ "Inactive"
  )) |>
  left_join(location_latest, by = "location_id") %>%
  mutate(
    battery_change_date = today(),
    firmware_update_date = today(),
    days_since_change = as.numeric(Sys.Date() - as.Date(battery_change_date)),
    batt_percent = ((425 - days_since_change)) / 425 * 100
  )

write_feather(
  active_deployments,
  "shiny_pieces/active_deployments"
)

# grab a piece that allows plotting of when a location
# had an active receiver

location_coverage <- deployments.df |>
  filter(location_id %in% active_deployments$location_id)

write_feather(
  location_coverage,
  "shiny_pieces/location_coverage"
)


# get unique daily fish by receiver

receiver_uniquefish <- fish_detections.dat %>%
  filter(!flag_false) |>
  distinct(fish_id, detection_date, internal_receiver_id, .keep_all = T)

write_feather(
  receiver_uniquefish,
  "shiny_pieces/receiver_uniquefish"
)

# export raw fish detections

write_feather(
  fish_detections.dat,
  "shiny_pieces/fish_detection_data"
)
