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

  # Helper function to dynamically generate plot UI and render plots
  generate_plots_park <- function(chosen_metric, output_id_prefix) {

    observeEvent(input$marine_park, {
      req(input$marine_park)

      message("viewing metric")
      message(chosen_metric)

      message("viewing data")
      filtered_data <- all_data$file_info %>%
        dplyr::filter(marine_park %in% c(input$marine_park)) %>%
        dplyr::filter(metric == chosen_metric) %>%
        dplyr::mutate(years = as.numeric(years)) %>%
        dplyr::glimpse()

      if (nrow(filtered_data) > 0) {

        output[[paste0(output_id_prefix, "_plots")]] <- renderUI({
          plot_list <-
            lapply(1:length(unique(filtered_data$metric)), function(i) {

              plotOutput(make_safe_id(paste0(output_id_prefix)), height = 50 + unique(filtered_data$years) * 160)

            })

          do.call(tagList, plot_list)

        })

      } else {

        NULL

      }

      lapply(seq_len(nrow(filtered_data)), function(i) {
        plot_id <- make_safe_id(paste0(output_id_prefix))

        output[[plot_id]] <- renderPlot({
          plot_object <- readRDS(here::here(filtered_data$file[i]))
          plot_object
        })
      })
    })
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


  # Render the Leaflet map for Demersal fish
  output$demersal_fish_map <- renderLeaflet({

    points <- all_data$metadata


    leaflet(points) %>%
      # Base groups
      addTiles(group = "OSM (default)") %>%
      addProviderTiles(providers$Esri.WorldImagery, group = "World Imagery (satellite)") %>%
      # Add the raster tile layer using the provided URL
      addTiles(
        urlTemplate = "https://dev.globalarchive.org/cog/tiles/{z}/{x}/{y}.png?file_path=synthesis_14/p_cti.fit_predicted.tif&colormap_name=viridis",
        attribution = "© GlobalArchive",
        group = "Predicted CTI"
      ) %>%

      addTiles(
        urlTemplate = "https://dev.globalarchive.org/cog/tiles/{z}/{x}/{y}.png?file_path=synthesis_14/p_mature.fit_predicted.tif&colormap_name=viridis",
        attribution = "© GlobalArchive",
        group = "Predicted larger Lm large-bodied generalist carnivores"
      ) %>%

      addTiles(
        urlTemplate = "https://dev.globalarchive.org/cog/tiles/{z}/{x}/{y}.png?file_path=synthesis_14/p_pinkies.fit_predicted.tif&colormap_name=viridis",
        attribution = "© GlobalArchive",
        group = "Predicted smaller Lm Pink Snapper"
      ) %>%

      addTiles(
        urlTemplate = "https://dev.globalarchive.org/cog/tiles/{z}/{x}/{y}.png?file_path=synthesis_14/p_richness.fit_predicted.tif&colormap_name=viridis",
        attribution = "© GlobalArchive",
        group = "Predicted Species Richness"
      ) %>%

      addMarkers(
        ~longitude_dd, ~latitude_dd,  # Coordinates for the markers
        # label = ~label,  # Add labels to the markers
        # popup = ~label,  # Popup text for markers
        group = "Sampling locations",
        icon = makeAwesomeIcon(icon = 'info-circle', markerColor = 'blue')  # Nice-looking icons
      ) %>%
      fitBounds(
        lng1 = min(points$longitude_dd), lat1 = min(points$latitude_dd),
        lng2 = max(points$longitude_dd), lat2 = max(points$latitude_dd)  # Set bounds to the extent of the points
      ) %>%

      # Layers control
      addLayersControl(
        baseGroups = c(
          "OSM (default)",
          "World Imagery (satellite)"
        ),
        overlayGroups = c("Sampling locations",
                          "Predicted CTI",
                          "Predicted Species Richness",
                          "Predicted larger Lm large-bodied generalist carnivores",
                          "Predicted smaller Lm Pink Snapper"),
        options = layersControlOptions(collapsed = FALSE)
      )
  })


  output$geo_lm <- renderPlot({
    all_data$geo_lm
  })

  output$geo_sr <- renderPlot({
    all_data$geo_sr
  })

  output$geo_cti <- renderPlot({
    all_data$geo_cti
  })

  output$sw30 <- renderPlot({
    all_data$sw30
  })

  output$sw70 <- renderPlot({
    all_data$sw70
  })

  output$sw200 <- renderPlot({
    all_data$sw200
  })

  output$sw_cti <- renderPlot({
    readRDS(here::here("plots/condition/demersal_fish/South-west_South-west Network_Community Temperature Index_2.rds"))
  })

  output$sw_lbc <- renderPlot({
    readRDS(here::here("plots/condition/demersal_fish/South-west_South-west Network_Abundance of large-bodied generalist carnivores greater than Lm_2.rds"))
  })

  output$nw_cti <- renderPlot({
    readRDS(here::here("plots/condition/demersal_fish/North-west_North-west Network_Community Temperature Index_2.rds"))
  })

  output$nw_lbc <- renderPlot({
    readRDS(here::here("plots/condition/demersal_fish/North-west_North-west Network_Abundance of large-bodied generalist carnivores greater than Lm_2.rds"))
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

  # Call the helper function for "Community Temperature Index"
  generate_plots_park("Community Temperature Index",
                 "community_temperature_index")

  generate_plots_park("Abundance of large-bodied generalist carnivores greater than Lm",
                 "abundance_of_large_bodied_generalist_carnivores")

}
