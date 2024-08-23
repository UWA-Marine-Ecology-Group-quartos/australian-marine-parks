server <- function(input, output, session) {

  # Render the Leaflet map for Demersal fish
  output$demersal_fish_map <- renderLeaflet({
    leaflet(data = dummy_points) %>%
      addTiles() %>%
      setView(lng = 115.5, lat = -34.4, zoom = 8) %>%  # Center the map on SW WA
      addMarkers(~lng, ~lat, popup = ~paste("Sample point", 1:100))
  })

}
