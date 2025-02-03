fishmetric_plot <- function(prediction_limits) {

  plot_list <- list()

  if ("p_richness.fit" %in% names(dat)) {
    gg_richness <- ggplot() +
      geom_spatraster(data = dat,
                      aes(fill = p_richness.fit)) +
      scale_fill_viridis_c(na.value = "transparent", direction = -1) +
      labs(fill = "Species\nRichness", x = NULL, y = NULL, title = "Whole assemblage") +
      new_scale_fill() +
      geom_sf(data = ausc, fill = "seashell2", colour = "black", size = 0.2) +
      geom_sf(data = marine_parks_amp, aes(colour = zone), fill = NA, show.legend = F,
              linewidth = 0.75) +
      scale_colour_manual(name = "Australian Marine Parks",
                          values = with(marine_parks_amp, setNames(colour, zone))) +
      coord_sf(xlim = c(prediction_limits[1], prediction_limits[2]), ylim = c(prediction_limits[3], prediction_limits[4]), crs = 4326) +
      theme_minimal()
    plot_list[["richness"]] <- gg_richness  # Store the plot
  }

  if ("p_cti.fit" %in% names(dat)) {
    gg_cti <- ggplot() +
      geom_spatraster(data = dat,
                      aes(fill = p_cti.fit)) +
      scale_fill_viridis_c(na.value = "transparent", direction = -1) +
      labs(fill = "CTI", x = NULL, y = NULL) +
      new_scale_fill() +
      geom_sf(data = ausc, fill = "seashell2", colour = "black", size = 0.2) +
      geom_sf(data = marine_parks_amp, aes(colour = zone), fill = NA, show.legend = F,
              linewidth = 0.75) +
      scale_colour_manual(name = "Australian Marine Parks",
                          values = with(marine_parks_amp, setNames(colour, zone))) +
      coord_sf(xlim = c(prediction_limits[1], prediction_limits[2]), ylim = c(prediction_limits[3], prediction_limits[4]), crs = 4326) +
      theme_minimal()
    plot_list[["cti"]] <- gg_cti  # Store the plot
  }

  if ("p_mature.fit" %in% names(dat)) {
    gg_mature <- ggplot() +
      geom_spatraster(data = dat, aes(fill = p_mature.fit)) +
      scale_fill_viridis_c(na.value = "transparent", direction = -1) +
      labs(fill = "> Lm", x = NULL, y = NULL, title = "Large bodied carnivores") +
      new_scale_fill() +
      geom_sf(data = ausc, fill = "seashell2", colour = "black", size = 0.2) +
      geom_sf(data = marine_parks_amp, aes(colour = zone), fill = NA, show.legend = F,
              linewidth = 0.75) +
      scale_colour_manual(name = "Australian Marine Parks",
                          values = with(marine_parks_amp, setNames(colour, zone))) +
      coord_sf(xlim = c(prediction_limits[1], prediction_limits[2]), ylim = c(prediction_limits[3], prediction_limits[4]), crs = 4326) +
      theme_minimal()
    plot_list[["mature"]] <- gg_mature  # Store the plot
  }

  if ("p_immature.fit" %in% names(dat)) {
    gg_immature <- ggplot() +
      geom_spatraster(data = dat,
                      aes(fill = p_immature.fit)) +
      scale_fill_viridis_c(na.value = "transparent", direction = -1) +
      labs(fill = "< Lm", x = NULL, y = NULL) +
      new_scale_fill() +
      geom_sf(data = ausc, fill = "seashell2", colour = "black", size = 0.2) +
      geom_sf(data = marine_parks_amp, aes(colour = zone), fill = NA, show.legend = F,
              linewidth = 0.75) +
      scale_colour_manual(name = "Australian Marine Parks",
                          values = with(marine_parks_amp, setNames(colour, zone))) +
      coord_sf(xlim = c(prediction_limits[1], prediction_limits[2]), ylim = c(prediction_limits[3], prediction_limits[4]), crs = 4326) +
      theme_minimal()
    plot_list[["immature"]] <- gg_immature  # Store the plot
  }

  if (length(plot_list) > 0) {
    combined_plot <- wrap_plots(plot_list, ncol = 2)
    print(combined_plot)
  } else {
    message("No plots were created.")
  }
}
