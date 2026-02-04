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
