server <- function(input, output, session) {

  # Dynamic dropdown for Ecosystem Component
  output$dynamic_ecosystem_subcomponent <- renderUI({
    req(input$metric)
    components <- unique(all_data$dropdown_data$ecosystem_component[all_data$dropdown_data$metric == input$metric])
    selectInput(
      inputId = "ecosystemsubcomponent",
      label = "Ecosystem sub-component:",
      choices = components,
      selected = components[1]
    )
  })

  output$dynamic_options <- renderUI({
    req(input$metric, input$ecosystemsubcomponent)
    options <- all_data$dropdown_data$options[all_data$dropdown_data$metric == input$metric & all_data$dropdown_data$ecosystem_component == input$ecosystemsubcomponent]
    if (length(options) > 0) {
      options_list <- strsplit(options, "\\|")[[1]]
      selectInput(
        inputId = "options",
        label = "Indicator metric",#paste(input$ecosystemcomponent, "Options:"),
        choices = options_list,
        selected = options_list[1],
        width = "100%"
      )
    }
  })

  output$marinepark_name <- renderUI({
    h3(input$marine_park)
  })

  output$network_name <- renderUI({
    h3(paste(input$network, "Network"))
  })

  # Helper function to create safe IDs by replacing spaces with underscores
  make_safe_id <- function(name) {
    gsub(" ", "_", name)
  }

  # bs_themer() # Turn this on if want to see real-time theming

  # Update the parks choices based on the selected network
  observeEvent(input$network, {
    selected_network <- input$network

    parks <- all_data$file_info %>%
      dplyr::filter(network == selected_network) %>%
      dplyr::distinct(marine_park) %>%
      dplyr::filter(!marine_park %in% c("South-west Network", "North-west Network")) %>%
      dplyr::pull(marine_park)

    updateRadioButtons(session, "marine_park", choices = c(parks))
  })


  # # Render the Leaflet map for Demersal fish
  # output$demersal_fish_map <- renderLeaflet({
  #
  #   points <- all_data$metadata
  #
  #   leaflet(points) %>%
  #     # Base groups
  #     addTiles(group = "OSM (default)") %>%
  #     addProviderTiles(providers$Esri.WorldImagery, group = "World Imagery (satellite)") %>%
  #     # Add the raster tile layer using the provided URL
  #     addTiles(
  #       urlTemplate = "https://dev.globalarchive.org/cog/tiles/{z}/{x}/{y}.png?file_path=synthesis_14/p_cti.fit_predicted.tif&colormap_name=viridis",
  #       attribution = "© GlobalArchive",
  #       group = "Predicted CTI"
  #     ) %>%
  #
  #     addTiles(
  #       urlTemplate = "https://dev.globalarchive.org/cog/tiles/{z}/{x}/{y}.png?file_path=synthesis_14/p_mature.fit_predicted.tif&colormap_name=viridis",
  #       attribution = "© GlobalArchive",
  #       group = "Predicted larger Lm large-bodied generalist carnivores"
  #     ) %>%
  #
  #     addTiles(
  #       urlTemplate = "https://dev.globalarchive.org/cog/tiles/{z}/{x}/{y}.png?file_path=synthesis_14/p_pinkies.fit_predicted.tif&colormap_name=viridis",
  #       attribution = "© GlobalArchive",
  #       group = "Predicted smaller Lm Pink Snapper"
  #     ) %>%
  #
  #     addTiles(
  #       urlTemplate = "https://dev.globalarchive.org/cog/tiles/{z}/{x}/{y}.png?file_path=synthesis_14/p_richness.fit_predicted.tif&colormap_name=viridis",
  #       attribution = "© GlobalArchive",
  #       group = "Predicted Species Richness"
  #     ) %>%
  #
  #     addMarkers(
  #       ~longitude_dd, ~latitude_dd,  # Coordinates for the markers
  #       # label = ~label,  # Add labels to the markers
  #       # popup = ~label,  # Popup text for markers
  #       group = "Sampling locations",
  #       icon = makeAwesomeIcon(icon = 'info-circle', markerColor = 'blue')  # Nice-looking icons
  #     ) %>%
  #     fitBounds(
  #       lng1 = min(points$longitude_dd), lat1 = min(points$latitude_dd),
  #       lng2 = max(points$longitude_dd), lat2 = max(points$latitude_dd)  # Set bounds to the extent of the points
  #     ) %>%
  #
  #     # Layers control
  #     addLayersControl(
  #       baseGroups = c(
  #         "OSM (default)",
  #         "World Imagery (satellite)"
  #       ),
  #       overlayGroups = c("Sampling locations",
  #                         "Predicted CTI",
  #                         "Predicted Species Richness",
  #                         "Predicted larger Lm large-bodied generalist carnivores",
  #                         "Predicted smaller Lm Pink Snapper"),
  #       options = layersControlOptions(collapsed = FALSE)
  #     )
  # })

  output$condition_plot <- renderPlot({

    req(input$toggle, input$network)

    plot_list <- all_data$file_info

    if(input$toggle %in% "Marine Park"){

      chosen_plot <- plot_list %>%
        dplyr::filter(network %in% input$network) %>%
        dplyr::filter(marine_park %in% input$marine_park) %>%
        dplyr::filter(metric %in% input$options)

    } else {

      chosen_plot <- plot_list %>%
        dplyr::filter(network %in% input$network) %>%
        dplyr::filter(marine_park %in% paste(input$network, "Network")) %>%
        dplyr::filter(metric %in% input$options)
    }

    print(unique(chosen_plot$file))

    chosen_file <- readRDS(here::here(paste(unique(chosen_plot$file))))
    plot(chosen_file)

  })

  condition_plot_height <- reactive({

    req(input$toggle, input$network)

    if(input$toggle %in% "Marine Park"){

      chosen_plot <- plot_list %>%
        dplyr::filter(network %in% input$network) %>%
        dplyr::filter(marine_park %in% input$marine_park) %>%
        dplyr::filter(metric %in% input$options)

    } else {

      chosen_plot <- plot_list %>%
        dplyr::filter(network %in% input$network) %>%
        dplyr::filter(marine_park %in% paste(input$network, "Network")) %>%
        dplyr::filter(metric %in% input$options)
    }

    num_years <- as.numeric(unique(chosen_plot$years))

    height <- 50 + num_years * 160

    return(height)
  })

  output$condition_plot_ui <- renderUI({
    plotOutput("condition_plot", height = condition_plot_height())
  })


  output$temporal_plot <- renderPlot({

    req(input$toggle, input$network)

    plot_list <- all_data$temporal_file_info

    if(input$toggle %in% "Marine Park"){

    chosen_plot <- plot_list %>%
      dplyr::filter(network %in% input$network) %>%
      dplyr::filter(marine_park %in% input$marine_park) %>%
      dplyr::filter(metric %in% input$options)

    } else {

      chosen_plot <- plot_list %>%
        dplyr::filter(network %in% input$network) %>%
        dplyr::filter(marine_park %in% paste(input$network, "Network")) %>%
        dplyr::filter(metric %in% input$options)
    }

    print(unique(chosen_plot$file))

    chosen_file <- readRDS(here::here(paste(unique(chosen_plot$file))))
    plot(chosen_file)

  })

  output$australia_map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      addMarkers(
        lng = 133.7751, lat = -25.2744,
        popup = "Australia"
      ) %>%
      setView(lng = 133.7751, lat = -25.2744, zoom = 4)
  })

}
