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

individual_summary <- read_feather("shiny_pieces/individual_summary") |>
  select(-.latest_idx)
