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
    leaflet() %>%
      addTiles() %>%
      # Add the raster tile layer using the provided URL
      addTiles(
        urlTemplate = "https://dev.globalarchive.org/cog/tiles/{z}/{x}/{y}.png?file_path=synthesis_22/sw-networkp_immature.fit.tif&colormap_name=viridis",
        attribution = "© GlobalArchive"
      ) %>%
      # Set the view to a specific location and zoom level (adjust as needed)
      setView(lng = 115.5, lat = -32.0, zoom = 6)
  })

}
