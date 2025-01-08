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
    req(input$toggle, input$network, input$marine_park)

    h3(input$marine_park)
  })

  output$network_name <- renderUI({
    req(input$toggle, input$network, input$marine_park)

    h3(paste(input$network, "Network"))
  })

  # Helper function to create safe IDs by replacing spaces with underscores
  make_safe_id <- function(name) {
    gsub(" ", "_", name)
  }

  # bs_themer() # Turn this on if want to see real-time theming

  # Update the parks choices based on the selected network


  # Reset marine park input when switching toggle
  observeEvent(input$toggle, {
    if (input$toggle == "Network") {
      updateSelectInput(session, "marine_park", selected = NULL)
    }
  })

  output$dynamic_marine_park <- renderUI({
    req(input$toggle, input$network)

    selected_network <- input$network

    parks <- all_data$file_info %>%
      dplyr::filter(network == selected_network) %>%
      dplyr::distinct(marine_park) %>%
      dplyr::filter(!marine_park %in% c("South-west Network", "North-west Network")) %>%
      dplyr::pull(marine_park)

    radioButtons("marine_park",
                 "Marine Park:",
                 choices = parks)
  })

  observeEvent(input$network, {
    selected_network <- input$network

    parks <- all_data$file_info %>%
      dplyr::filter(network == selected_network) %>%
      dplyr::distinct(marine_park) %>%
      dplyr::filter(!marine_park %in% c("South-west Network", "North-west Network")) %>%
      dplyr::pull(marine_park)

    updateRadioButtons(session, "marine_park", choices = c(parks), selected = parks[1])
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

  # Reactive to handle the condition filtered dataset
  condition_filtered_data <- reactive({
    req(input$toggle, input$network)

    plot_list <- all_data$file_info

    if (input$toggle == "Marine Park") {
      req(input$marine_park)  # Ensure marine_park input is selected
      plot_list %>%
        dplyr::filter(network %in% input$network) %>%
        dplyr::filter(marine_park %in% input$marine_park) %>%
        dplyr::filter(metric %in% input$options)
    } else {
      plot_list %>%
        dplyr::filter(network %in% input$network) %>%
        dplyr::filter(marine_park %in% paste(input$network, "Network")) %>%
        dplyr::filter(metric %in% input$options)
    }
  })

  output$condition_plot <- renderPlot({

    req(condition_filtered_data())

    chosen_plot <- condition_filtered_data()
    validate(need(nrow(chosen_plot) > 0, "No data available for the selected filters."))

    chosen_file <- readRDS(here::here(unique(chosen_plot$file)))
    plot(chosen_file)

  })

  # Reactive height for the plot
  condition_plot_height <- reactive({
    req(condition_filtered_data())

    chosen_plot <- condition_filtered_data()
    if (nrow(chosen_plot) > 0) {
      num_years <- as.numeric(unique(chosen_plot$years))
      height <- 50 + num_years * 160
    } else {
      height <- 200  # Default height if no data
    }
    return(height)
  })

  output$condition_plot_ui <- renderUI({

    req(input$toggle, input$network)

    print(paste("plot height", condition_plot_height()))

    plotOutput("condition_plot", height = paste0(condition_plot_height(), "px"))

  })


  output$temporal_plot <- renderPlot({

    req(input$toggle, input$network)

    plot_list <- all_data$temporal_file_info

    if(input$toggle %in% "Marine Park"){
      req(input$marine_park) # Ensure marine_park input is available
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

    # print(unique(chosen_plot$file))
    if (nrow(chosen_plot) > 0) {
    chosen_file <- readRDS(here::here(paste(unique(chosen_plot$file))))
    plot(chosen_file)
    } else {

      return(NULL)
    }

  })

  output$temporal_plot_ui <- renderUI({

    req(input$toggle, input$network)

    plotOutput("temporal_plot")
  })


  metadata_filtered_data <- reactive({
    req(input$toggle, input$network)

    metadata <- all_data$metadata

    if (input$toggle == "Marine Park") {
      req(input$marine_park)  # Ensure marine_park input is selected
      metadata %>%
        dplyr::filter(network %in% input$network) %>%
        dplyr::filter(marine_park %in% input$marine_park)
    } else {
      metadata %>%
        dplyr::filter(network %in% input$network)
    }
  })

  raster_predicted_data <- reactive({
    req(input$toggle, input$network, input$options)

    raster_list <- all_data$raster_data %>%
      dplyr::filter(estimate %in% c("Probability"))

    if (input$toggle == "Marine Park") {
      req(input$marine_park)  # Ensure marine_park input is selected
      raster_list %>%
        dplyr::filter(network %in% input$network) %>%
        dplyr::filter(marine_park %in% input$marine_park) %>%
        dplyr::filter(metric %in% input$options)
    } else {
      raster_list %>%
        dplyr::filter(network %in% input$network) %>%
        dplyr::filter(marine_park %in% paste(input$network, "Network")) %>%
        dplyr::filter(metric %in% input$options)
    }
  })

  raster_error_data <- reactive({
    req(input$toggle, input$network, input$options)

    raster_list <- all_data$raster_data %>%
      dplyr::filter(estimate %in% c("Error"))

    if (input$toggle == "Marine Park") {
      req(input$marine_park)  # Ensure marine_park input is selected
      raster_list %>%
        dplyr::filter(network %in% input$network) %>%
        dplyr::filter(marine_park %in% input$marine_park) %>%
        dplyr::filter(metric %in% input$options)
    } else {
      raster_list %>%
        dplyr::filter(network %in% input$network) %>%
        dplyr::filter(marine_park %in% paste(input$network, "Network")) %>%
        dplyr::filter(metric %in% input$options)
    }
  })

  output$australia_map <- renderLeaflet({
    req(input$toggle, input$network, input$options)

    points <- metadata_filtered_data()

    if (nrow(points) == 0) {
      points <- tibble(
        latitude_dd = c(-25.0, -25.1),
        longitude_dd = c(133.0, 133.1)
      )
    }

    metric_title <- unique(raster_predicted_data()$metric)

    # Initial Leaflet map
    map <- leaflet(points) %>%
      addTiles() %>%
      # addProviderTiles(providers$Esri.WorldImagery, group = "World Imagery (satellite)") %>%
      addMarkers(~longitude_dd, ~latitude_dd, group = "Sampling locations") %>%
      fitBounds(
        lng1 = min(points$longitude_dd), lat1 = min(points$latitude_dd),
        lng2 = max(points$longitude_dd), lat2 = max(points$latitude_dd)
      ) %>%
      addLayersControl(
        # baseGroups = c("OSM (default)", "World Imagery (satellite)"),
        overlayGroups = c("Sampling locations"),
        options = layersControlOptions(collapsed = FALSE)
      )


    # Add tiles only if raster_predicted_data() has valid data
    if (!is.null(raster_predicted_data()) && nrow(raster_predicted_data()) > 0) {
      map <- map %>%
        addTiles(
          urlTemplate = paste(unique(raster_predicted_data()$tile_service_url)),
          attribution = "© GlobalArchive",
          group = "Predicted"
        )
    }

    # Add tiles only if raster_error_data() has valid data
    if (!is.null(raster_error_data()) && nrow(raster_error_data()) > 0) {
      map <- map %>%
        addTiles(
          urlTemplate = paste(unique(raster_error_data()$tile_service_url)),
          attribution = "© GlobalArchive",
          group = "Error"
        ) %>%
        hideGroup("Error")  # Ensure "Error" is hidden initially
    }

    # Add custom radio buttons with title as a control
    map %>%
      htmlwidgets::onRender(
        glue::glue(
          "function(el, x) {{
           var map = this;
           var customControl = L.control({{position: 'topright'}});  // Position of the control

           customControl.onAdd = function(map) {{
             var div = L.DomUtil.create('div', 'leaflet-bar');
             div.innerHTML = `
               <div style='text-align: center; margin-bottom: 8px; font-weight: bold;'>
                 {metric_title}
               </div>
               <form>
                 <label><input type='radio' name='layer' value='Predicted' checked> Predicted</label><br>
                 <label><input type='radio' name='layer' value='Error'> Error</label>
               </form>`;
             div.style.backgroundColor = 'white';
             div.style.padding = '10px';
             div.style.border = '2px solid gray';
             return div;
           }};

           customControl.addTo(map);

           // Listen for changes in the radio buttons
           var radioButtons = document.querySelectorAll('input[name=\"layer\"]');
           radioButtons.forEach(function(rb) {{
             rb.addEventListener('change', function(e) {{
               Shiny.setInputValue('layer_toggle', e.target.value, {{priority: 'event'}});  // Send selected value to Shiny
             }});
           }});
         }}"
        )
      )
  })

  # Observe the radio button input and update the map
  observe({
    req(input$layer_toggle)  # Ensure toggle input is available

    map_proxy <- leafletProxy("australia_map")

    # Show/hide layers based on the selected radio button
    if (input$layer_toggle == "Predicted") {
      map_proxy %>%
        showGroup("Predicted") %>%
        hideGroup("Error")
    } else if (input$layer_toggle == "Error") {
      map_proxy %>%
        showGroup("Error") %>%
        hideGroup("Predicted")
    }
  })
}
