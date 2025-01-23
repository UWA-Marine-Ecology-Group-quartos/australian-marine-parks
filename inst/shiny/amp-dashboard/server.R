server <- function(input, output, session) {

  # bs_themer() # Turn this on if want to see real-time theming

  observe({
    active_panel <- input$navbar_id %>% glimpse

    # if (active_panel == "Summary Statistics") {
    #   shinyjs::hide("main_sidebar")
    # } else {
    #   shinyjs::show("main_sidebar")
    # }
  })

  # Dynamic dropdown for Ecosystem Component
  output$dynamic_ecosystem_subcomponent <- renderUI({
    req(input$metric)
    components <- unique(all_data$dropdown_data$ecosystem_component[all_data$dropdown_data$metric == input$metric])
    selectInput(
      inputId = "ecosystemsubcomponent",
      label = "Ecosystem sub-component:",
      choices = components,
      selected = components[1],
      width = "100%"
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

  render_marinepark_name <- function() {
    req(input$toggle, input$network, input$marine_park)
    h3(HTML(paste0("<b>", input$marine_park)))
  }

  output$marinepark_name_1 <- renderUI(render_marinepark_name())
  output$marinepark_name_2 <- renderUI(render_marinepark_name())

  render_network_name <- function() {
    req(input$toggle, input$network, input$marine_park)
    h3(HTML(paste0("<b>", input$network, " Network")))
  }

  output$network_name_1 <- renderUI(render_network_name())
  output$network_name_2 <- renderUI(render_network_name())

  output$ecosystem_subcomponent_name <- renderUI({
    req(input$ecosystemsubcomponent)

    h5(HTML(paste0("<i>", input$ecosystemsubcomponent)))

  })

  output$metric_name <- renderUI({
    req(input$options)

    h5(HTML(paste0("<i>", input$options)))

  })

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

  # Reactive to handle the condition filtered dataset
  condition_filtered_data <- reactive({
    req(input$toggle, input$network)

    plot_list <- all_data$file_info

    if (input$toggle == "Marine Park") {
      req(input$marine_park)  # Ensure marine_park input is selected

      # message("view conditional data marine park")

      plot_list %>%
        dplyr::filter(network %in% input$network) %>%
        dplyr::filter(marine_park %in% input$marine_park) %>%
        dplyr::filter(metric %in% input$ecosystemsubcomponent) #%>% glimpse
    } else {

      # message("view conditional data network")

      plot_list %>%
        dplyr::filter(network %in% input$network) %>%
        dplyr::filter(marine_park %in% paste(input$network, "Network")) %>%
        dplyr::filter(metric %in% input$ecosystemsubcomponent) #%>% glimpse
    }
  })

  output$condition_plot_ui <- renderUI({
    req(condition_filtered_data())

    chosen_plot <- condition_filtered_data()

    # Check if data is valid
    # if (is.null(chosen_plot) || nrow(chosen_plot) == 0 || !"file" %in% colnames(chosen_plot)) {
    # Fallback to plain text
    # return(tags$p("No condition data available for the selected filters.", style = "font-size: 18px; color: gray; text-align: center;"))
    # } else {
    # Render the plot


    validate(need(nrow(chosen_plot) > 0, "No condition data available for the selected filters."))
    plotOutput("condition_plot", height = condition_plot_height())
    # }
  })

  output$condition_plot <- renderPlot({

    req(condition_filtered_data())

    chosen_plot <- condition_filtered_data()

    # Debug chosen_plot
    # message("Chosen plot: ", ifelse(is.null(chosen_plot), "NULL", paste(nrow(chosen_plot), "rows")))

    # Safely read and plot
    file_path <- here::here(unique(chosen_plot$file))
    if (!file.exists(file_path)) {
      stop("File does not exist: ", file_path)
    }
    chosen_file <- readRDS(file_path)
    plot(chosen_file)

  })

  # Reactive height for the plot
  condition_plot_height <- reactive({
    req(condition_filtered_data())

    chosen_plot <- condition_filtered_data()

    if (!is.null(chosen_plot) && nrow(chosen_plot) > 0) {
      num_years <- as.numeric(chosen_plot$years)

      if (num_years == 1) {
        height <- 200
      } else {
        height <- num_years * 175  # Adjust the calculation as needed
      }
    } else {
      height <- 100  # Default height if no data
    }

    # message("Dynamic plot height: ", height)
    return(height)
  })

  output$condition_plot_ui <- renderUI({

    req(input$toggle, input$network)
    plotOutput("condition_plot", height = paste0(condition_plot_height(), "px"))

  })

  output$dynamic_text <- renderUI({
    req(input$toggle, input$network)

    if (input$toggle == "Marine Park") {
      req(input$marine_park)  # Ensure marine_park input is selected
      text <- all_data$text_data %>%
        dplyr::filter(network %in% input$network) %>%
        dplyr::filter(marine_park %in% input$marine_park) %>%
        dplyr::filter(ecosystem_condition %in% input$ecosystemsubcomponent)
    } else {
      text <- all_data$text_data %>%
        dplyr::filter(network %in% input$network) %>%
        dplyr::filter(marine_park %in% paste(input$network, "Network")) %>%
        dplyr::filter(ecosystem_condition %in% input$ecosystemsubcomponent)
    }

    h6(unique(text$text))
  })

  # Temporal plot filtered data ----
  temporal_filtered_data <- reactive({
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
  })


  output$temporal_plot <- renderPlot({

    req(temporal_filtered_data())

    chosen_plot <- temporal_filtered_data()
    # validate(need(nrow(chosen_plot) > 0, "No temporal data available for the selected filters."))

    chosen_file <- readRDS(here::here(unique(chosen_plot$file)))
    plot(chosen_file)

  })

  # Reactive height for the plot
  temporal_plot_height <- reactive({
    req(temporal_filtered_data())

    chosen_plot <- temporal_filtered_data()

    if (nrow(chosen_plot) > 0) {

      num_depths <- as.numeric(unique(chosen_plot$depth_classes))

      if(num_depths == 1){

        height <- 250

      } else {

        height <- num_depths * 250

      }

    } else {
      height <- 50  # Default height if no data
    }

    return(height)
  })

  # UI for temporal plot ----
  output$temporal_plot_ui <- renderUI({

    req(input$toggle, input$network)
    plotOutput("temporal_plot", height = paste0(temporal_plot_height(), "px"))

  })

  # Create filtered metadata ----
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

  # Create filtered predicted rasters ----
  raster_predicted_data <- reactive({
    req(input$toggle, input$network, input$options)

    raster_list <- all_data$raster_data %>%
      dplyr::filter(estimate %in% c("Probability", "Mean"))

    # message("view chosen raster dataset")

    if (input$toggle == "Marine Park") {
      req(input$marine_park)  # Ensure marine_park input is selected
      raster_list %>%
        dplyr::filter(network %in% input$network) %>%
        dplyr::filter(marine_park %in% input$marine_park) %>%
        dplyr::filter(metric %in% input$options) #%>% glimpse()
    } else {
      raster_list %>%
        dplyr::filter(network %in% input$network) %>%
        dplyr::filter(marine_park %in% paste(input$network, "Network")) %>%
        dplyr::filter(metric %in% input$options) #%>% glimpse()
    }
  })

  # Create filtered error rasters ----
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

  # Create map for Dashboard ----
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

    icon <- iconList(blue = makeIcon("images/marker_blue.png", iconWidth = 40, iconHeight =40))

    map.dat <- dat

    boss.habitat.highlights.popups <- filter(map.dat, source %in% c("boss.habitat.highlights"))
    bruv.habitat.highlights.popups <- filter(map.dat, source %in% c("bruv.habitat.highlights"))
    fish.highlights.popups <- filter(map.dat, source %in% c("fish.highlights"))
    threed.model.popups <- filter(map.dat, source %in% c("3d.model"))
    image.popups <- filter(map.dat, source %in% c('image'))

    # Having this in the global.R script breaks now - make icons on server side
    icon.bruv.habitat <- iconList(blue = makeIcon("images/marker_green.png", iconWidth = 40, iconHeight =40))
    icon.boss.habitat <- iconList(blue = makeIcon("images/marker_pink.png", iconWidth = 40, iconHeight =40))
    icon.fish <- iconList(blue = makeIcon("images/marker_yellow.png", iconWidth = 40, iconHeight =40))
    icon.models <- iconList(blue = makeIcon("images/marker_purple.png", iconWidth = 40, iconHeight =40))

    # Initial Leaflet map ----
    map <- leaflet(points) %>%
      addTiles() %>%
      # addProviderTiles(providers$Esri.WorldImagery, group = "World Imagery (satellite)") %>%
      #addMarkers(~longitude_dd, ~latitude_dd, group = "Sampling locations") %>%

      addMarkers(data = points, ~longitude_dd, ~latitude_dd,
                 icon = icon,
                 # popup = bruv.habitat.highlights.popups$popup,
                 clusterOptions = markerClusterOptions(iconCreateFunction =
                                                         JS("
                                          function(cluster) {
                                             return new L.DivIcon({
                                               html: '<div style=\"background-color:rgba(0, 123, 255, 0.9)\"><span>' + cluster.getChildCount() + '</div><span>',
                                               className: 'marker-cluster'
                                             });
                                           }")),
                 group = "Sampling locations"#,
                 #popupOptions=c(closeButton = TRUE,minWidth = 0,maxWidth = 700)
      )%>%


      fitBounds(
        lng1 = min(points$longitude_dd), lat1 = min(points$latitude_dd),
        lng2 = max(points$longitude_dd), lat2 = max(points$latitude_dd)
      ) %>%

      # Ngari Capes Marine Parks
      addPolygons(data = ngari.mp, weight = 1, color = "black",
                  fillOpacity = 0.8, fillColor = "#7bbc63",
                  group = "State Marine Parks", label=ngari.mp$Name) %>%

      # State Marine Parks
      addPolygons(data = state.mp, weight = 1, color = "black",
                  fillOpacity = 0.8, fillColor = ~state.pal(zone),
                  group = "State Marine Parks", label=state.mp$COMMENTS) %>%

      # Add a legend
      addLegend(pal = state.pal, values = state.mp$zone, opacity = 1,
                title="State Zones",
                position = "bottomright", group = "State Marine Parks") %>%

      # Commonwealth Marine Parks
      addPolygons(data = commonwealth.mp, weight = 1, color = "black",
                  fillOpacity = 0.8, fillColor = ~commonwealth.pal(zone),
                  group = "Australian Marine Parks", label=commonwealth.mp$ZoneName) %>%

      # Add a legend
      addLegend(pal = commonwealth.pal, values = commonwealth.mp$zone, opacity = 1,
                title="Australian Marine Park Zones",
                position = "bottomright", group = "Australian Marine Parks") %>%

      # stereo-BRUV habitat videos
      addMarkers(data=bruv.habitat.highlights.popups,
                 icon = icon.bruv.habitat,
                 popup = bruv.habitat.highlights.popups$popup,
                 #label = bruv.habitat.highlights.popups$sample,
                 clusterOptions = markerClusterOptions(iconCreateFunction =
                                                         JS("
                                          function(cluster) {
                                             return new L.DivIcon({
                                               html: '<div style=\"background-color:rgba(124, 248, 193, 0.9)\"><span>' + cluster.getChildCount() + '</div><span>',
                                               className: 'marker-cluster'
                                             });
                                           }")),
                 group = "FishNClips",
                 popupOptions=c(closeButton = TRUE,minWidth = 0,maxWidth = 700))%>%

      # BOSS habitat videos
      addMarkers(data=boss.habitat.highlights.popups,
                 icon = icon.boss.habitat,
                 popup = boss.habitat.highlights.popups$popup,
                 #label = boss.habitat.highlights.popups$sample,
                 clusterOptions = markerClusterOptions(iconCreateFunction =
                                                         JS("
                                          function(cluster) {
                                             return new L.DivIcon({
                                               html: '<div style=\"background-color:rgba(248, 124, 179, 0.9)\"><span>' + cluster.getChildCount() + '</div><span>',
                                               className: 'marker-cluster'
                                             });
                                           }")),
                 group = "FishNClips",
                 popupOptions=c(closeButton = TRUE,minWidth = 0,maxWidth = 700))%>%

      # stereo-BRUV fish videos
      addMarkers(data=fish.highlights.popups,
                 icon = icon.fish,
                 popup = fish.highlights.popups$popup,
                 clusterOptions = markerClusterOptions(iconCreateFunction =
                                                         JS("
                                          function(cluster) {
                                             return new L.DivIcon({
                                               html: '<div style=\"background-color:rgba(241, 248, 124,0.9)\"><span>' + cluster.getChildCount() + '</div><span>',
                                               className: 'marker-cluster'
                                             });
                                           }")),
                 group = "FishNClips",
                 popupOptions=c(closeButton = TRUE,minWidth = 0,maxWidth = 700))%>%

      # 3D models
      addMarkers(data=threed.model.popups,
                 icon = icon.models,
                 popup = threed.model.popups$popup,
                 clusterOptions = markerClusterOptions(iconCreateFunction =
                                                         JS("
                                          function(cluster) {
                                             return new L.DivIcon({
                                               html: '<div style=\"background-color:rgba(131, 124, 248,0.9)\"><span>' + cluster.getChildCount() + '</div><span>',
                                               className: 'marker-cluster'
                                             });
                                           }")),
                 group = "FishNClips",
                 popupOptions=c(closeButton = TRUE, minWidth = 0,maxWidth = 700)
      )%>%

      addControl(html = html_legend, position = "bottomleft", className = "fishnclips-legend") %>%

      addLayersControl(
        # baseGroups = c("OSM (default)", "World Imagery (satellite)"),
        overlayGroups = c("Australian Marine Parks",
                          "State Marine Parks",
                          "Sampling locations",
                          "FishNClips"),
        options = layersControlOptions(collapsed = FALSE),
        position = "bottomright"
      )  %>% # Ensure "Predicted" is hidden initially
      hideGroup("State Marine Parks") %>%
      hideGroup("Australian Marine Parks")%>%
      hideGroup("FishNClips")


    # Add tiles only if raster_predicted_data() has valid data ----
    if (!is.null(raster_predicted_data()) && nrow(raster_predicted_data()) > 0) {

      # message(paste0("raster available:", unique(raster_predicted_data()$tile_service_url)))
      # Blue = low, yellow = high

      map <- map %>%
        addTiles(
          urlTemplate = paste(unique(raster_predicted_data()$tile_service_url)),
          attribution = "© GlobalArchive",
          group = "Predicted"
        ) %>%
        addLegend(
          position = "bottomright",
          pal = colorNumeric(palette = viridisLite::turbo(256, direction = -1),  #(reverse here)
                             domain = c(raster_predicted_data()$min, raster_predicted_data()$max)
          ),
          values = seq(
            from = raster_predicted_data()$min,
            to = raster_predicted_data()$max,
            length.out = 5
          ),  # Define 5 fixed values for the legend
          title = "Predicted",
          labFormat = labelFormat(transform = function(x) sort(x, decreasing = TRUE)),
          opacity = 1,
          group = "Predicted"
        )
    }

    # Add tiles only if raster_error_data() has valid data ----
    if (!is.null(raster_error_data()) && nrow(raster_error_data()) > 0) {
      map <- map %>%
        addTiles(
          urlTemplate = paste(unique(raster_error_data()$tile_service_url)),
          attribution = "© GlobalArchive",
          group = "Error"
        ) %>%
        hideGroup("Error")  # Ensure "Error" is hidden initially
    }

    # Add custom radio buttons with title as a control ----
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

  observe({
    input$australia_map_groups
    shinyjs::runjs(sprintf("
      var isVisible = %s.includes('FishNClips');
      var legend = document.querySelector('.fishnclips-legend');
      if (legend) {
        legend.style.display = isVisible ? 'block' : 'none';
      }
    ", jsonlite::toJSON(input$australia_map_groups)))
  })


  # Observe the radio button input and update the map ----
  observe({
    req(input$layer_toggle)  # Ensure toggle input is available

    map_proxy <- leafletProxy("australia_map")

    # Show/hide layers based on the selected radio button
    if (input$layer_toggle == "Predicted") {
      map_proxy %>%
        showGroup("Predicted") %>%
        hideGroup("Error")%>%
        clearControls() %>%  # Clear all existing controls
        addLegend(
          position = "bottomright",
          pal = colorNumeric(palette = viridisLite::turbo(256, direction = -1),
                             domain = c(raster_predicted_data()$min, raster_predicted_data()$max)),
          values = seq(
            from = raster_predicted_data()$min,
            to = raster_predicted_data()$max,
            length.out = 5
          ),  # Define 5 fixed values for the legend
          title = "Predicted",
          labFormat = labelFormat(transform = function(x) sort(x, decreasing = TRUE)),
          opacity = 1
        )
    } else if (input$layer_toggle == "Error") {
      map_proxy %>%
        showGroup("Error") %>%
        hideGroup("Predicted")%>%
        clearControls() %>%  # Clear all existing controls
        addLegend(
          position = "bottomright",
          pal = colorNumeric(palette = viridisLite::plasma(256, direction = -1),
                             domain = c(raster_error_data()$min, raster_error_data()$max)),
          # values = c(, ),
          values = seq(
            from = raster_error_data()$min,
            to = raster_error_data()$max,
            length.out = 5
          ),  # Define 5 fixed values for the legend
          title = "Error",
          labFormat = labelFormat(transform = function(x) sort(x, decreasing = TRUE)),
          opacity = 1
        )
    }
  })



  output$fishnclips <- renderLeaflet({

    # map.dat <- map.dat() # call in filtered data
    map.dat <- dat

    points <- metadata_filtered_data()

    boss.habitat.highlights.popups <- filter(map.dat, source %in% c("boss.habitat.highlights"))
    bruv.habitat.highlights.popups <- filter(map.dat, source %in% c("bruv.habitat.highlights"))
    fish.highlights.popups <- filter(map.dat, source %in% c("fish.highlights"))
    threed.model.popups <- filter(map.dat, source %in% c("3d.model"))
    image.popups <- filter(map.dat, source %in% c('image'))

    # Having this in the global.R script breaks now - make icons on server side
    icon.bruv.habitat <- iconList(blue = makeIcon("images/marker_green.png", iconWidth = 40, iconHeight =40))
    icon.boss.habitat <- iconList(blue = makeIcon("images/marker_pink.png", iconWidth = 40, iconHeight =40))
    icon.fish <- iconList(blue = makeIcon("images/marker_yellow.png", iconWidth = 40, iconHeight =40))
    icon.models <- iconList(blue = makeIcon("images/marker_purple.png", iconWidth = 40, iconHeight =40))

    # lng1 <- min(map.dat$longitude)
    # lat1 <- min(map.dat$latitude)
    # lng2 <- max(map.dat$longitude)
    # lat2 <- max(map.dat$latitude)

    leaflet <- leaflet() %>%
      addProviderTiles('Esri.WorldImagery', group = "World Imagery") %>%
      addTiles(group = "Open Street Map")%>%
      addControl(html = html_legend, position = "bottomleft") %>%
      # flyToBounds(lng1, lat1, lng2, lat2)%>%
      fitBounds(
        lng1 = min(points$longitude_dd), lat1 = min(points$latitude_dd),
        lng2 = max(points$longitude_dd), lat2 = max(points$latitude_dd)
      ) %>%

      # stereo-BRUV habitat videos
      addMarkers(data=bruv.habitat.highlights.popups,
                 icon = icon.bruv.habitat,
                 popup = bruv.habitat.highlights.popups$popup,
                 #label = bruv.habitat.highlights.popups$sample,
                 clusterOptions = markerClusterOptions(iconCreateFunction =
                                                         JS("
                                          function(cluster) {
                                             return new L.DivIcon({
                                               html: '<div style=\"background-color:rgba(124, 248, 193, 0.9)\"><span>' + cluster.getChildCount() + '</div><span>',
                                               className: 'marker-cluster'
                                             });
                                           }")),
                 group="BRUV Habitat imagery",
                 popupOptions=c(closeButton = TRUE,minWidth = 0,maxWidth = 700))%>%

      # BOSS habitat videos
      addMarkers(data=boss.habitat.highlights.popups,
                 icon = icon.boss.habitat,
                 popup = boss.habitat.highlights.popups$popup,
                 #label = boss.habitat.highlights.popups$sample,
                 clusterOptions = markerClusterOptions(iconCreateFunction =
                                                         JS("
                                          function(cluster) {
                                             return new L.DivIcon({
                                               html: '<div style=\"background-color:rgba(248, 124, 179, 0.9)\"><span>' + cluster.getChildCount() + '</div><span>',
                                               className: 'marker-cluster'
                                             });
                                           }")),
                 group="BOSS Habitat imagery",
                 popupOptions=c(closeButton = TRUE,minWidth = 0,maxWidth = 700))%>%

      # stereo-BRUV fish videos
      addMarkers(data=fish.highlights.popups,
                 icon = icon.fish,
                 popup = fish.highlights.popups$popup,
                 clusterOptions = markerClusterOptions(iconCreateFunction =
                                                         JS("
                                          function(cluster) {
                                             return new L.DivIcon({
                                               html: '<div style=\"background-color:rgba(241, 248, 124,0.9)\"><span>' + cluster.getChildCount() + '</div><span>',
                                               className: 'marker-cluster'
                                             });
                                           }")),
                 group="Fish highlights",
                 popupOptions=c(closeButton = TRUE,minWidth = 0,maxWidth = 700))%>%

      # 3D models
      addMarkers(data=threed.model.popups,
                 icon = icon.models,
                 popup = threed.model.popups$popup,
                 clusterOptions = markerClusterOptions(iconCreateFunction =
                                                         JS("
                                          function(cluster) {
                                             return new L.DivIcon({
                                               html: '<div style=\"background-color:rgba(131, 124, 248,0.9)\"><span>' + cluster.getChildCount() + '</div><span>',
                                               className: 'marker-cluster'
                                             });
                                           }")),
                 group="3D models",
                 popupOptions=c(closeButton = TRUE, minWidth = 0,maxWidth = 700)
      )%>%


      # Ngari Capes Marine Parks
      addPolygons(data = ngari.mp, weight = 1, color = "black",
                  fillOpacity = 0.8, fillColor = "#7bbc63",
                  group = "State Marine Parks", label=ngari.mp$Name)%>%

      # State Marine Parks
      addPolygons(data = state.mp, weight = 1, color = "black",
                  fillOpacity = 0.8, fillColor = ~state.pal(zone),
                  group = "State Marine Parks", label=state.mp$COMMENTS)%>%

      # Add a legend
      addLegend(pal = state.pal, values = state.mp$zone, opacity = 1,
                title="State Zones",
                position = "bottomright", group = "State Marine Parks")%>%

      # Commonwealth Marine Parks
      addPolygons(data = commonwealth.mp, weight = 1, color = "black",
                  fillOpacity = 0.8, fillColor = ~commonwealth.pal(zone),
                  group = "Australian Marine Parks", label=commonwealth.mp$ZoneName)%>%

      # Add a legend
      addLegend(pal = commonwealth.pal, values = commonwealth.mp$zone, opacity = 1,
                title="Australian Marine Park Zones",
                position = "bottomright", group = "Australian Marine Parks")%>%

      addLayersControl(
        baseGroups = c("World Imagery","Open Street Map"),
        overlayGroups = c("Fish highlights",
                          "BRUV Habitat imagery","BOSS Habitat imagery",
                          "3D models",
                          "State Marine Parks",
                          "Australian Marine Parks"), options = layersControlOptions(collapsed = FALSE)) #%>%

    # hideGroup("Australian Marine Parks") %>%
    # hideGroup("State Marine Parks")

    return(leaflet)

  })


  # Fish images ----
  # Network image
  output$ui_network <- renderUI({

    req(input$network)
    network <- stringr::str_replace_all(tolower(input$network), c(" " = ".", "-" = "."))


    img(src = paste0("networks/", network, ".jpg"), align = "right", width = "100%", style = "margin-bottom: 10px;")
  })

  # Park image
  output$ui_marine_park <- renderUI({

    req(input$marine_park)

    park <- stringr::str_replace_all(tolower(input$marine_park), c(" marine park" = "", " " = ".", "-" = ".")) %>%
      glimpse

    img(src = paste0("parks/", park, ".jpg"), align = "right", width = "100%", style = "margin-bottom: 10px;")
  })

  # Valuebox text

  summary_data <- reactive({
    req(input$toggle, input$network)

    raster_list <- all_data$summary_data

    if (input$toggle == "Marine Park") {
      req(input$marine_park)  # Ensure marine_park input is selected
      raster_list %>%
        dplyr::filter(network %in% input$network) %>%
        dplyr::filter(marine_park %in% input$marine_park)
    } else {
      raster_list %>%
        dplyr::filter(network %in% input$network) %>%
        dplyr::filter(marine_park %in% paste(input$network, "Network"))
    }
  })

  output$fish_counted <- renderText({

    data <- summary_data() %>% filter(metric %in% "fish_counted")
    unique(data$value)

  })

  output$fish_species <- renderText({
    data <- summary_data() %>% filter(metric %in% "fish_species")
    unique(data$value)
  })

  output$hours_watched <- renderText({
    data <- summary_data() %>% filter(metric %in% "hours_watched")
    unique(data$value)
  })

  output$bruvs_deployed <- renderText({
    data <- summary_data() %>% filter(metric %in% "bruvs_deployed")
    unique(data$value)
  })

  # TODO link this with GA when Nik has created links
  output$ui_open_ga_button <- renderUI({
    shiny::a(
      h4(#icon("th"),
        icon("globe"), # Changed icon to "globe"
        paste0("View synthesis dataset on GlobalArchive"),
        class = "custom-button btn btn-default action-button",
        style = "font-weight:600"),
      target = "_blank",
      href = paste0("https://dev.globalarchive.org/ui/main/syntheses/"
                    # ,input$slider # could put synthesis ID here
      )
    )
  })

  output$ui_method_button <- renderUI({

    data <- all_data$method_data

    if (input$toggle == "Marine Park") {

      req(input$marine_park)  # Ensure marine_park input is selected

      data <- data %>%
        dplyr::filter(network %in% input$network) %>%
        dplyr::filter(marine_park_or_area %in% input$marine_park) %>%
        dplyr::filter(ecosystem_condition %in% input$ecosystemsubcomponent) %>%
        glimpse

    } else {

      data <- data %>%
        dplyr::filter(network %in% input$network) %>%
        dplyr::filter(ecosystem_condition %in% input$ecosystemsubcomponent) %>%
        dplyr::filter(marine_park_or_area %in% paste(input$network, "Network")) %>%
        glimpse

    }

    # If data is empty or method is NA, return NULL
    if (nrow(data) == 0 || is.na(data$method)) {
      return(NULL)
    }

    # Extract methods
    methods <- unique(unlist(strsplit(data$method, ", "))) %>% glimpse()

    # Dynamically create buttons for each method
    buttons <- list()

    if ("stereo-BRUV" %in% methods) {

      bruv_button <- shiny::a(
        h2(img(src = "stereo-BRUV_filled_transparent_colour.png",
               height = "100px"#,
               #style = "margin-left: 15px;" # Adjust the value as needed)
        ),
        "stereo-BRUVs",
        class = "custom-button btn btn-default action-button", # use primary for blue
        style = "font-weight:600; width: 350px; text-align: center;"),
        href = paste0("https://benthic-bruvs-field-manual.github.io/")
      )
    } else{

      bruv_button <- ""
    }

    if ("stereo-BOSS" %in% methods) {
      boss_button <- shiny::a(
        h2(img(src = "frame_transparent.png",
               height = "100px"#,
               #style = "margin-left: 15px;" # Adjust the value as needed)
        ),
        "stereo-BOSS",
        class = "custom-button btn btn-default action-button",
        style = "font-weight:600; width: 350px; text-align: center;"),
        target = "_blank",
        href = paste0("https://drop-camera-field-manual.github.io/")
      )
    } else {

      boss_button <- ""

    }

    addition <- NULL

    print(length(methods))

    if (length(methods) > 1) {

      message("includes both")
      addition <- h1("+")

    }

    # Wrap buttons in a div for proper alignment
    tagList(div(width = "100%", style = "display: flex; gap: 25px; justify-content: center; align-items: center;", bruv_button, addition, boss_button))

  })


  # End of server ----
}
