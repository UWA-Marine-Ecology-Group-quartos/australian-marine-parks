server <- function(input, output, session) {

  # bs_themer() # Turn this on if want to see real-time theming

  # Update the parks choices based on the selected network
  observeEvent(input$network, {
    selected_network <- input$network

    parks <- networks_and_parks %>%
      dplyr::filter(network == selected_network) %>%
      dplyr::pull(park)

    updateRadioButtons(session, "park", choices = c(parks))
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

}
