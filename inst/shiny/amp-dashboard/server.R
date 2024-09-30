server <- function(input, output, session) {

  # Helper function to create safe IDs by replacing spaces with underscores
  make_safe_id <- function(name) {
    gsub(" ", "_", name)
  }

  # Helper function to dynamically generate plot UI and render plots
  generate_plots <- function(metric, output_id_prefix) {

    observeEvent(input$marine_park, {
      req(input$marine_park)

      filtered_data <- file_info %>%
        dplyr::filter(marine_park %in% c(input$marine_park)) %>%
        dplyr::filter(metric %in% c(metric)) %>%
        dplyr::glimpse()

      if (nrow(filtered_data) > 0) {

        output[[paste0(output_id_prefix, "_plots")]] <- renderUI({
          plot_list <-
            lapply(1:length(unique(filtered_data$depth)), function(i) {

              plotOutput(make_safe_id(paste0(output_id_prefix, "_", filtered_data$depth[i])))

            })

          do.call(tagList, plot_list)

        })

      } else {

        NULL

      }

      lapply(seq_len(nrow(filtered_data)), function(i) {
        plot_id <- make_safe_id(paste0(output_id_prefix, "_", filtered_data$depth[i]))

        output[[plot_id]] <- renderPlot({
          plot_object <- readRDS(here::here(filtered_data$file[i]))
          plot_object
        })
      })
    })
  }



#
#
#
#       # Remove previous UI elements to prevent residual plots
#       # removeUI(selector = paste0("#", output_id_prefix, "_plots"), multiple = TRUE)
#
#       # Filter files based on the selected marine park and metric
#       metric_files <- subset(file_info, marine_park == input$marine_park & metric == metric)
#
#       # Dynamically generate plot UI for the selected metric
#       output[[paste0(output_id_prefix, "_plots")]] <- renderUI({
#         req(input$marine_park)
#
#         # If no matching files, return nothing
#         if (nrow(metric_files) == 0) {
#           return(NULL)
#         }
#
#         # Create a list of plots with safe IDs
#         plot_list <- lapply(seq_len(nrow(metric_files)), function(i) {
#           plotOutput(make_safe_id(paste0(output_id_prefix, "_", metric_files$depth[i])))
#         })
#
#         do.call(tagList, plot_list)
#       })
#
#       # Dynamically render the plots
#       lapply(seq_len(nrow(metric_files)), function(i) {
#         plot_id <- make_safe_id(paste0(output_id_prefix, "_", metric_files$depth[i]))
#
#         output[[plot_id]] <- renderPlot({
#           plot_object <- readRDS(here::here(metric_files$file[i]))
#           plot_object
#         })
#       })
#     })
#   }
  # bs_themer() # Turn this on if want to see real-time theming

  # Update the parks choices based on the selected network
  observeEvent(input$network, {
    selected_network <- input$network

    parks <- all_data$file_info %>%
      dplyr::filter(network == selected_network) %>%
      dplyr::distinct(marine_park) %>%
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


  # Call the helper function for "Community Temperature Index"
  generate_plots("Community Temperature Index", "community_temperature_index")
  generate_plots("Trophic group by abundance by size class", "trophic_group_by_abundance_by_size_class")

}
