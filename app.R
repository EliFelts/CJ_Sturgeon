# check that necessary packages are installed, if not
# install them

required_packages <- c(
  "tidyverse", "shiny", "bslib", "bsicons",
  "DT", "leaflet", "leafem", "arrow",
  "shinyWidgets", "conflicted", "plotly",
  "tidyr", "fontawesome", "scales",
  "ggokabeito", "readr"
)

installed_packages <- rownames(installed.packages())

for (pkg in required_packages) {
  if (!pkg %in% installed_packages) {
    install.packages(pkg)
  }
}

library(shiny)
library(tidyverse)
library(leaflet)
library(leafem)
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

individual_daily_summary <- read_feather("shiny_pieces/individual_daily_summary") |>
  mutate(location_id = factor(location_id))

individual_dailydepth_summary <- read_feather("shiny_pieces/individual_dailydepth_summary")

individual_receiver_summary <- read_feather("shiny_pieces/individual_receiver_summary")

deployment_daily <- read_feather("shiny_pieces/deployment_daily")

# bring in deployment data

active_deployments <- read_feather("shiny_pieces/active_deployments")

active_deployments_filters <- active_deployments |>
  filter(status == "Active")

location_coverage <- read_feather("shiny_pieces/location_coverage")

lost_receivers <- read_feather("shiny_pieces/lost_receivers")

# build base leaflet map

leaflet_base <- leaflet() %>%
  addProviderTiles(providers$Esri.WorldTopoMap, group = "Topographic") %>%
  addProviderTiles(providers$Esri.WorldImagery, group = "Imagery") %>%
  addProviderTiles(providers$OpenStreetMap.Mapnik, group = "Roads") %>%
  setView(lng = -115.8568, lat = 42.97197, zoom = 11) %>%
  addMouseCoordinates() %>%
  addLayersControl(
    baseGroups = c("Topographic", "Imagery", "Roads"),
    options = layersControlOptions(collapsed = FALSE)
  )

# make a palette to show most recent detection location
# for individual map

recent_pal <- colorFactor(
  palette = c("cyan", "magenta"),
  levels = c("Previous", "Latest")
)

# make a palette to distinguish active
# vs removed detection locations

status_pal <- colorFactor(
  palette = c("blue", "red"),
  levels = c("Active", "Inactive")
)

# make a palette for battery life

pal_battery <- colorNumeric(
  palette = colorRampPalette(c("#d73027", "#fee08b", "#1a9850"))(256),
  domain = c(0, 100), # battery %
  na.color = "transparent"
)


location_pal <- c(
  "CJ_STGALLEY_UPPER" = "#E69F00",
  "CJ_STGALLEY_LOWER" = "#56B4E9",
  "CJ_BOWL_UPPER" = "#009E73",
  "CJ_BOWL_LOWER" = "#F0E442",
  "CJ_HOMESTEAD" = "#0072B2"
)


# build user interface

ui <- page_navbar(
  title = "CJ Strike Acoustic Telemetry Studies",
  theme = bs_theme(bootswatch = "flatly"),
  id = "nav",
  header = tags$head(tags$style(HTML("
  /* Compact text inside bslib value boxes */
  .bslib-value-box .value-box-title {
    font-size: 0.75rem !important;
  }

  .bslib-value-box .value-box-value {
    font-size: 0.9rem !important;
  }

  .bslib-value-box .value-box-subtitle,
  .bslib-value-box p {
    font-size: 0.7rem !important;
  }

  /* Optionally shrink icon */
  .bslib-value-box .value-box-showcase {
    font-size: 1.2rem !important;
  }
"))),
  sidebar = sidebar(
    width = 300,
    id = "sb",
    collapsible = TRUE,
    open = TRUE,
    conditionalPanel(
      "input.nav==`Explore Fish Detections`",
      accordion(
        accordion_panel(
          "User Inputs",
          pickerInput("species_filter",
            label = "Choose one or more species",
            choices = sort(unique(individual_summary$species)),
            selected = "STG",
            multiple = FALSE,
            options = list(
              `actions-box` = TRUE
            )
          ),
          uiOutput("detection_slider")
        )
      )
    ),
    conditionalPanel(
      "input.nav==`Explore Individuals`"
    ),
    conditionalPanel(
      "input.nav==`Deployment Diagnostics`",
      accordion()
    )
  ),
  nav_panel(
    "Explore Fish Detections",
    layout_columns(
      value_box(
        title = "Active Tags Today",
        value = textOutput("activetags_text"),
        textOutput("activetags_spp"),
        textOutput("detection_count"),
        showcase = icon("fish"),
        max_height = "200px"
      ),
      value_box(
        title = "Active Receivers",
        value = nrow(active_deployments),
        showcase = icon("headphones"),
        max_height = "200px"
      ),
      value_box(
        title = "Latest Detection",
        value = textOutput("latest_det_value"),
        showcase = bs_icon("cloud-download"),
        max_height = "200px"
      )
    ),
    page_fillable(
      layout_columns(
        card(card_header("Latest Detections"),
          leafletOutput("latest_det_map"),
          height = "65vh",
          full_screen = TRUE
        )
      )
    )
  ),
  nav_panel(
    "Explore Individuals",
    # layout_columns(
    #   value_box(
    #     title = "",
    #     value = uiOutput("selected_individual_fish"),
    #     max_height = "200px"
    #   )
    # ),
    page_fillable(
      layout_columns(
        card(
          card_header("Individual Fish Summaries"),
          dataTableOutput("individual_table1"),
          full_screen = TRUE,
          height = "25vh"
        ),
        col_widths = 12,
      ),
      layout_columns(
        card(card_header("Selection Map"),
          leafletOutput("individual_map"),
          full_screen = TRUE,
          height = "65vh"
        ),
        card(
          card_header("Daily Detection Count"),
          plotlyOutput("individual_daily_plot"),
          full_screen = TRUE,
          height = "65vh"
        ),
        card(
          card_header("Daily Median Depth"),
          plotlyOutput("individual_dailydepth_plot"),
          full_screen = TRUE,
          height = "65vh"
        ),
        col_widths = c(4, 4, 4)
      )
    )
  ),
  nav_panel(
    "Deployment Diagnostics",
    layout_columns(
      value_box(
        title = "Active Locations",
        value = nrow(active_deployments_filters),
        showcase = icon("satellite-dish"),
        max_height = "200px"
      ),
      value_box(
        title = "Unique Fish Detections",
        value = label_number(scale = 1e-6, accuracy = 0.1, suffix = " Million")(sum(deployment_daily$detections)),
        showcase = bs_icon("bar-chart", style = "color: dodgerblue;"),
        max_height = "200px"
      ),
      uiOutput("receiver_summary")
    ),
    page_fillable(
      layout_columns(
        card(card_header("Active Locations (Click points for details)"),
          leafletOutput("receiver_map"),
          full_screen = TRUE,
          height = "65vh"
        ),
        card(card_header("Detections at Selected Location"),
          plotlyOutput("receiver_det_plot"),
          full_screen = TRUE,
          height = "65vh"
        )
      )
    )
  )
)

server <- function(input, output, session) {
  # collapse sidebar when leaving group page, nothing
  # there on the other pages

  observeEvent(input$nav,
    {
      if (identical(input$nav, "Explore Fish Detections")) {
        bslib::sidebar_toggle("sb", open = TRUE)
      } else {
        bslib::sidebar_toggle("sb", open = FALSE)
      }
    },
    ignoreInit = TRUE
  )


  # make a reactive of the tagged fish based
  # on UI filters

  fish_reactive <- reactive({
    dat <- individual_summary %>%
      filter(
        species %in% input$species_filter,
        status == "active"
      )
  })


  # make the value for number of active
  # tags a reactive value box

  output$activetags_text <- renderText({
    req(fish_reactive())

    nrow(fish_reactive())
  })

  output$activetags_spp <- renderText({
    req(fish_reactive())

    dat <- fish_reactive() %>%
      arrange(species)

    str_c(
      "Species: ",
      str_c(unique(dat$species), collapse = ", ")
    )
  })


  # filter latest detections based on User
  # selected species

  #
  # latest_det_reactive <- reactive({
  #   latest %>%
  #     filter(species_name %in% input$species_filter) %>%
  #     group_by(fish_id) %>%
  #     slice(which.max(detection_datetime))
  # })

  # make the value of the latest detection a textoutput
  # to be funneled into a value box

  output$latest_det_value <- renderText({
    dat <- fish_reactive()

    format(
      max(dat$latest_detection),
      "%B %e %Y"
    )
  })

  # make a slider input that's reactive to
  # the selected species

  output$detection_slider <- renderUI({
    req(fish_reactive())

    dat <- fish_reactive()

    slider_min <- today() - years(1)
    slider_max <- as.Date(max(dat$latest_detection))
    slider_start <- slider_max - days(30)


    sliderInput("detection_filter",
      label = "Select date range",
      min = slider_min,
      max = slider_max,
      value = c(slider_start, slider_max),
      timeFormat = "%Y-%m-%d"
    )
  })


  # reactive summary data to build map

  map_summary_reactive <- reactive({
    spp_summary <- receiver_uniquefish %>%
      filter(
        detection_date >= min(input$detection_filter),
        detection_date <= max(input$detection_filter),
        species %in% input$species_filter
      ) %>%
      group_by(species) %>%
      mutate(
        detected_system = n_distinct(fish_id),
        species = factor(species, level = c(
          "BLT", "LKT", "NPK",
          "RBT", "WAE"
        ))
      ) %>%
      group_by(location_id, species, .drop = F) %>%
      summarize(
        detected_receiver = n_distinct(fish_id),
        detected_system = first(detected_system),
        receiver_percent = detected_receiver / detected_system * 100
      ) %>%
      right_join(active_deployments, by = "location_id") %>%
      ungroup() %>%
      mutate(plot_radius = sqrt(receiver_percent) * 2)
  })

  # output text of number detected in the reactive selection

  output$detection_count <- renderText({
    dat <- map_summary_reactive() %>%
      ungroup() %>%
      group_by(species) %>%
      summarize(detected_system = first(detected_system)) %>%
      filter(!is.na(detected_system))

    str_c(
      "Unique individuals, selected date range: ",
      dat$detected_system
    )
  })

  # make the reactive leaflet of latest
  # detections of tags that are currently active

  output$latest_det_map <- renderLeaflet({
    dat <- map_summary_reactive()

    leaflet_base %>%
      addCircleMarkers(
        data = dat,
        lng = ~longitude,
        lat = ~latitude,
        popup = ~ lapply(
          str_c(
            "<b>", "Location: ",
            "</b>", location_id,
            "<br>",
            "<b>", "Unique Individuals: ",
            "</b>", detected_receiver
          ),
          HTML
        ),
        radius = ~plot_radius
      )
  })

  # make a data table object of the individual summaries

  output$individual_table1 <- renderDataTable({
    dat <- individual_summary %>%
      mutate(
        days_since_last = round(time_since_last)
      ) |>
      select(
        fish_id, fork_length_cm, n_detections,
        unique_locations, latest_location, days_since_last,
        status, censor_reason
      )

    datatable(dat,
      filter = "top",
      selection = list(mode = "single", target = "row"),
      extensions = c("FixedHeader", "Scroller"),
      options = list(
        pageLength = 25,
        scrollY = "720px",
        scrollX = TRUE,
        fixedHeader = TRUE,
        deferRender = TRUE,
        scroller = TRUE,
        dom = "lrtip"
      )
    )
  })

  # Get selected row from selection in datatable

  individual_reactive <- reactive({
    selected_individual <- input$individual_table1_rows_selected

    dat <- individual_summary[selected_individual, ]
  })

  # filter the individual summary data based on
  # datatable selection

  individual_summary_reactive <- reactive({
    req(individual_reactive())

    dat <- individual_reactive()

    output <- individual_summary %>%
      filter(fish_id %in% dat$fish_id)
  })

  # for now just a test to make sure the table
  # selection is working properly

  # output$selected_individual_fish <- renderUI({
  #   req(individual_reactive())
  #   req(individual_summary_reactive())
  #
  #   dat <- individual_summary_reactive() |>
  #     mutate(
  #       status_print = case_when(
  #         status %in% c("excluded") ~ str_c(status, " (", exclude_reason, ")", sep = ""),
  #         status %in% c("right_censored") ~ str_c(status, " (", censor_reason, ")", sep = ""),
  #         TRUE ~ status
  #       ),
  #       length_in = round(total_length_in, 1)
  #     )
  #
  #   HTML(str_c("<span><b>Fish ID:</b> ", dat$fish_id, "</span>",
  #     "<br>",
  #     "<span><b>Species:</b> ", dat$species, "</span>",
  #     "<br>",
  #     "<span><b>Length at tagging:</b> ", dat$fork_length_cm, "</span>",
  #     "<br>",
  #     "<span><b>Status:</b> ", dat$status_print, "</span>",
  #     "<br>",
  #     "<span><b>Total Detections:</b> ", comma(dat$n_detections), "</span>",
  #     "<br>",
  #     "<span><b>Unique Locations:</b> ", comma(dat$unique_locations), "</span>",
  #     sep = " "
  #   ))
  # })

  # make a map for the individual page

  output$individual_map <- renderLeaflet({
    leaflet_base %>%
      addLegend(
        pal = recent_pal,
        values = c("Previous", "Latest")
      )
  })

  # keep track and update individual map with
  # selected individual from datatable

  selected_individual <- reactiveVal(NULL)

  # make the observer to update the individual
  # map reactively

  observeEvent(input$individual_table1_rows_selected, {
    req(input$individual_table1_rows_selected)

    # selected_fish <- input$individual_table1_rows_selected
    #
    # current <- input$individual_table1_rows_current
    #
    # row_idx <- if (length(current)) current[selected_fish] else selected_fish

    dat <- individual_summary_reactive()

    map.dat <- individual_receiver_summary %>%
      filter(fish_id == dat$fish_id)

    leafletProxy("individual_map") %>%
      clearGroup("selection") %>%
      addCircleMarkers(
        data = map.dat,
        lng = ~longitude,
        lat = ~latitude,
        fillColor = ~ recent_pal(recent_status),
        color = ~ recent_pal(recent_status),
        fillOpacity = 0.8,
        group = "selection",
        popup = ~ str_c(
          "<b>", "Location ID: ", "</b>", location_id,
          "<br>",
          "<b>", "Earliest: ", "</b>", earliest_date,
          "<br>",
          "<b>", "Latest: ", "</b>", latest_date,
          "<br>",
          "<b>", "Unique Days: ", "</b>", unique_days,
          "<br>",
          "<b>", "Total Detections: ", "</b>", comma(detections)
        )
      )
  })

  # make a plot of daily detections for selected individual

  output$individual_daily_plot <- renderPlotly({
    req(input$individual_table1_rows_selected)

    dat <- individual_summary_reactive()

    plot.dat <- individual_daily_summary %>%
      filter(fish_id %in% dat$fish_id)

    release_ref <- plot.dat %>%
      summarize(release_date = first(release_datetime)) %>%
      pull(release_date)

    detection_max <- 86400 / first(plot.dat$mean_delay)

    censor_label <- case_when(
      dat$censor_reason == "known_mortality" ~ "Mortality Date: ",
      dat$censor_reason == "missing_year" ~ "Censored Date (Disappeared) ",
      dat$censor_reason == "battery_expired" ~ "Battery End Date: ",
      TRUE ~ NA
    )

    plot1 <- ggplot() +
      geom_rect(
        data = dat,
        aes(
          xmin = flagged_runs_start,
          xmax = flagged_runs_end,
          ymin = 0,
          ymax = detection_max,
          text = str_c("Flagged Detection Pattern")
        ),
        inherit.aes = F,
        fill = "red",
        alpha = 0.25
      ) +
      geom_col(
        data = plot.dat,
        aes(
          x = detection_date, y = count, fill = location_id,
          text = str_c(" Detection Date: ", detection_date,
            "<br>",
            "Location: ", location_id,
            "<br>",
            "Detections: ", comma(count),
            "<br>",
            "Percent of Max:", round(count / detection_max * 100),
            sep = " "
          )
        )
      ) +
      scale_fill_manual(
        values = location_pal,
        drop = FALSE
      ) +
      geom_hline(
        data = plot.dat,
        aes(
          yintercept = detection_max,
          text = str_c("Max Daily Detections: ", detection_max)
        ),
        linetype = "dashed"
      ) +
      geom_vline(
        data = dat,
        aes(
          xintercept = censor_date,
          text = str_c(censor_label, as_date(censor_date))
        ),
        linetype = "dashed", color = "red"
      ) +
      geom_vline(
        data = dat,
        aes(
          xintercept = release_datetime,
          text = str_c("Release Date: ", as_date(release_datetime))
        ),
        linetype = "dashed", color = "blue"
      ) +
      theme_bw() +
      labs(
        x = "Detection Date", y = "Number of Detections",
        fill = "Location"
      )


    ggplotly(plot1, tooltip = c("text"))
  })

  # make individual daily depth plot

  output$individual_dailydepth_plot <- renderPlotly({
    req(input$individual_table1_rows_selected)

    dat <- individual_summary_reactive()

    plot.dat <- individual_dailydepth_summary %>%
      filter(fish_id %in% dat$fish_id)

    req(nrow(plot.dat) > 0)

    plot1 <- plot.dat |>
      ggplot(aes(x = detection_date, y = median_depth_ft, color = pt_class)) +
      geom_point(
        aes(text = str_c(
          "Detection Date: ", detection_date,
          "<br>",
          "Min Depth (ft): ", round(min_depth_ft, 1),
          "<br>",
          "Median Depth (ft): ", round(median_depth_ft, 1),
          "<br>",
          "Max Depth (ft): ", round(max_depth_ft, 1),
          "<br>",
          "Number of Detections: ", count
        )),
        show.legend = F
      ) +
      scale_color_manual(values = c(exceed = "red", normal = "black")) +
      scale_y_reverse(limits = c(NA, 0)) +
      theme_bw() +
      labs(x = "Detection Date", y = "Median Depth (ft)")


    ggplotly(plot1, tooltip = c("text")) |>
      layout(showlegend = F)
  })


  # make a map for the receiver page

  output$receiver_map <- renderLeaflet({
    leaflet_base %>%
      addCircleMarkers(
        data = active_deployments,
        lat = ~latitude,
        lng = ~longitude,
        layerId = ~location_id,
        group = "Location Points",
        color = "black",
        label = ~ str_c(location_id),
        # fillColor = ~ colorNumeric("BuPu", domain = c(0, 100))(batt_percent),
        fillColor = ~ status_pal(status),
        fillOpacity = 0.8, weight = 1
      ) %>%
      addCircleMarkers(
        data = lost_receivers,
        lat = ~latitude,
        lng = ~longitude,
        group = "Lost Receivers",
        popup = ~ str_c(
          "<b>", "Location ID: ", "</b>", location_id,
          "<br>",
          "<b>", "Receiver ID: ", "</b>", internal_receiver_id,
          "<br>",
          "<b>", "Latest Date: ", "</b>", end_datetime
        ),
        fillColor = "black",
        color = "black",
        radius = 8,
        clusterOptions = markerClusterOptions()
      ) |>
      addLayersControl(
        overlayGroups = c("Location Points", "Lost Receivers"),
        options = layersControlOptions(collapsed = F)
      ) |>
      hideGroup(c("Lost Receivers")) |>
      addLegend(
        position = "bottomright",
        pal = status_pal,
        values = c("Active", "Inactive"),
        title = "Deployment Status"
      )
  })

  # keep track of the currently selected receiver id

  selected_id <- reactiveVal(NULL)

  # observe selected markers on receiver map


  observeEvent(input$receiver_map_marker_click,
    {
      click <- input$receiver_map_marker_click

      if (is.null(click) || is.null(click$id) || length(click$id) == 0 || is.na(click$id)) {
        return()
      }

      # Only respond to clicks on active receivers
      if (!identical(click$group, "Location Points")) {
        return()
      }

      receiver_selection <- click$id

      if (identical(selected_id(), receiver_selection)) {
        return()
      }
      selected_id(receiver_selection)

      sel_dat <- active_deployments %>% dplyr::filter(location_id == receiver_selection)
      if (nrow(sel_dat) != 1) {
        return()
      }

      leafletProxy("receiver_map") %>%
        clearGroup("selection") %>%
        addCircleMarkers(
          data = sel_dat,
          lng = ~longitude, lat = ~latitude,
          label = ~ str_c(location_id),
          fillColor = "red", color = "red",
          radius = 10,
          group = "selection"
        )
    },
    ignoreInit = TRUE
  )


  # grab all the data for selected receiver

  selected_receiver_row <- reactive({
    req(selected_id())

    row <- active_deployments %>%
      dplyr::filter(location_id == selected_id()) |>
      dplyr::summarize(
        location_id = first(location_id),
        internal_receiver_id = first(internal_receiver_id),
        # internal_receiver_id = paste(sort(unique(internal_receiver_id)), collapse = ", "),
        battery_change_date = dplyr::first(battery_change_date),
        batt_percent = dplyr::first(batt_percent),
        firmware_update_date = dplyr::first(firmware_update_date),

        # "latest detection for location" — take max across receivers just in case
        most_recent = max(most_recent, na.rm = TRUE),
        .groups = "drop"
      )

    req(nrow(row) == 1) # critical: prevents crashes if duplicates/missing

    row

    # active_deployments %>%
    #   filter(location_id == selected_id())
  })


  # make a reactive value box to summarize stuff from
  # the selected receiver

  output$receiver_summary <- renderUI({
    row <- selected_receiver_row()

    value_box(
      title = "Selected Location Info",
      value = row$location_id,
      showcase = icon("dot-circle", style = "color:red;"),
      p(
        strong("Receiver ID(s):"), str_c(row$internal_receiver_id), br(),
        strong("Battery Change:"), format(row$battery_change_date, "%B %e, %Y"), br(),
        strong("Est. Battery Remaining:"), str_c(round(row$batt_percent), " %"), br(),
        strong("Firmware Update:"), format(row$firmware_update_date, "%B %e, %Y"), br(),
        strong("Latest Detection:"), format(row$most_recent, "%B %e, %Y")
      ),
      width = 4,
      min_height = "200px",
      max_height = "200px"
    )
  })


  # make a reactive plot output to check
  # that expected data are present for a
  # selected receiver

  output$receiver_det_plot <- renderPlotly({
    req(selected_id())

    # if location_id duplicates exist, still want a plot, so don't req(nrow==1) here
    plot_dat <- deployment_daily %>%
      dplyr::filter(location_id == selected_id())

    coverage.dat <- location_coverage |>
      dplyr::filter(location_id == selected_id()) |>
      mutate(end_datetime = as.POSIXct(coalesce(end_datetime, max(plot_dat$detection_date))))

    plot_max <- plot_dat |>
      group_by(detection_date) |>
      mutate(total_detections = sum(detections))

    req(nrow(plot_dat) > 0)

    receiver_plot <- ggplot(plot_dat, aes(
      x = detection_date, y = detections
    )) +
      geom_rect(
        data = coverage.dat, aes(
          xmin = start_datetime, xmax = end_datetime,
          ymin = 0,
          ymax = max(plot_max$total_detections),
          text = str_c("Active Deployment Period")
        ),
        inherit.aes = F,
        fill = "grey",
        alpha = 0.25
      ) +
      geom_col(
        aes(text = str_c(
          "<b>Date: </b>", detection_date,
          "<br><b>Species: </b>", species,
          "<br><b>Detection Count: </b>", comma(detections),
          "<br><b>Unique Individuals: </b>", comma(individuals)
        ))
      ) +
      scale_x_date(date_labels = "%B %Y") +
      theme_bw() +
      labs(x = "Date", y = "Number of Detections", fill = "")

    ggplotly(receiver_plot, tooltip = "text")
  })
}

shinyApp(ui, server)
