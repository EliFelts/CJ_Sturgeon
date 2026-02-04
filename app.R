library(shiny)
library(stringr)
library(dplyr)
library(lubridate)
library(ggplot2)
library(leaflet)
library(leafem)
library(leaflet.minicharts)
library(bslib)
library(bsicons)
library(conflicted)
library(arrow)
library(DT)
library(plotly)
library(shinyWidgets)
library(tidyr)
library(fontawesome)
library(scales)
library(readr)
library(ggokabeito)

conflicts_prefer(
  DT::renderDT,
  dplyr::filter,
  dplyr::lag,
  plotly::layout,
  DT::renderDataTable,
  DT::dataTableOutput()
)

# read in shiny pieces


receiver_uniquefish <- read_feather("shiny_pieces/receiver_uniquefish")

individual_summary <- read_feather("shiny_pieces/individual_summary") |>
  select(-.latest_idx)

individual_daily_summary <- read_feather("shiny_pieces/individual_daily_summary")

individual_dailydepth_summary <- read_feather("shiny_pieces/individual_dailydepth_summary")

individual_receiver_summary <- read_feather("shiny_pieces/individual_receiver_summary")

deployment_daily <- read_feather("shiny_pieces/deployment_daily")

# bring in deployment data

active_deployments <- read_feather("shiny_pieces/active_deployments")

active_deployments_filters <- active_deployments |>
  filter(status == "Active")

location_coverage <- read_feather("shiny_pieces/location_coverage")

# data frame of lost receivers

lost_receivers <- deployments.df |>
  filter(end_reason == "lost")

write_feather(
  lost_receivers,
  "shiny_pieces/lost_receivers"
)

lost_receivers <- read_feather("shiny_pieces/lost_receivers")
