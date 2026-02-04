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

fish.df <- read_csv(file = str_c(shared_parent.dir, "Telemetry_TaggedSturgeon_Info.xlsx", sep = "/"))

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

detection_fish.join <- detections_receivers.join %>%
  filter(!is.na(detection_datetime)) %>%
  left_join(
    fish.df,
    by = fish_by
  ) %>%
  select(fish_id, species, species_name, serial_number, acoustic_tag_id,
    detection_datetime, detection_datetime_local, internal_receiver_id,
    sensor_type, real_sensor, sensor_maximum,
    location_id, deployment_id,
    latitude = latitude.x,
    longitude = longitude.x
  )
