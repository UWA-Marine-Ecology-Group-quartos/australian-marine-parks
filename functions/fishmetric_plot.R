fishmetric_plot <- function(prediction_limits, dat, year = NULL) {

  plot_list <- list()
  yr_lab <- if (!is.null(year)) paste0(" (", year, ")") else ""

  if ("p_richness.fit" %in% names(dat)) {
    gg_richness <- ggplot() +
      geom_spatraster(data = dat, aes(fill = p_richness.fit)) +
      scale_fill_viridis_c(na.value = "transparent", direction = -1) +
      labs(fill = "Species\nRichness", x = NULL, y = NULL,
           title = paste0("Whole assemblage", yr_lab)) +
      new_scale_fill() +
      geom_sf(data = ausc, fill = "seashell2", colour = "black", size = 0.2) +
      geom_sf(data = marine_parks_amp, aes(colour = zone), fill = NA,
              show.legend = FALSE, linewidth = 0.75) +
      scale_colour_manual(name = "Australian Marine Parks",
                          values = with(marine_parks_amp, setNames(colour, zone))) +
      coord_sf(xlim = c(prediction_limits[1], prediction_limits[2]),
               ylim = c(prediction_limits[3], prediction_limits[4]), crs = 4326) +
      theme_minimal()
    plot_list[["richness"]] <- gg_richness
  }

  if ("p_cti.fit" %in% names(dat)) {
    gg_cti <- ggplot() +
      geom_spatraster(data = dat, aes(fill = p_cti.fit)) +
      scale_fill_viridis_c(na.value = "transparent", direction = -1) +
      labs(fill = "CTI", x = NULL, y = NULL, title = paste0("CTI", yr_lab)) +
      new_scale_fill() +
      geom_sf(data = ausc, fill = "seashell2", colour = "black", size = 0.2) +
      geom_sf(data = marine_parks_amp, aes(colour = zone), fill = NA,
              show.legend = FALSE, linewidth = 0.75) +
      scale_colour_manual(name = "Australian Marine Parks",
                          values = with(marine_parks_amp, setNames(colour, zone))) +
      coord_sf(xlim = c(prediction_limits[1], prediction_limits[2]),
               ylim = c(prediction_limits[3], prediction_limits[4]), crs = 4326) +
      theme_minimal()
    plot_list[["cti"]] <- gg_cti
  }

  if ("p_b20.fit" %in% names(dat)) {
    gg_b20 <- ggplot() +
      geom_spatraster(data = dat, aes(fill = p_b20.fit)) +
      scale_fill_viridis_c(na.value = "transparent", direction = -1) +
      labs(fill = "B20*", x = NULL, y = NULL,
           title = paste0("Biomass non-pelagic bony fish > 20cm", yr_lab)) +
      new_scale_fill() +
      geom_sf(data = ausc, fill = "seashell2", colour = "black", size = 0.2) +
      geom_sf(data = marine_parks_amp, aes(colour = zone), fill = NA,
              show.legend = FALSE, linewidth = 0.75) +
      scale_colour_manual(name = "Australian Marine Parks",
                          values = with(marine_parks_amp, setNames(colour, zone))) +
      coord_sf(xlim = c(prediction_limits[1], prediction_limits[2]),
               ylim = c(prediction_limits[3], prediction_limits[4]), crs = 4326) +
      theme_minimal()
    plot_list[["b20"]] <- gg_b20
  }

  if ("p_abundance.fit" %in% names(dat)) {
    gg_abund <- ggplot() +
      geom_spatraster(data = dat, aes(fill = p_abundance.fit)) +
      scale_fill_viridis_c(na.value = "transparent", direction = -1) +
      labs(fill = "Abundance", x = NULL, y = NULL,
           title = paste0("Total abundance", yr_lab)) +
      new_scale_fill() +
      geom_sf(data = ausc, fill = "seashell2", colour = "black", size = 0.2) +
      geom_sf(data = marine_parks_amp, aes(colour = zone), fill = NA,
              show.legend = FALSE, linewidth = 0.75) +
      scale_colour_manual(name = "Australian Marine Parks",
                          values = with(marine_parks_amp, setNames(colour, zone))) +
      coord_sf(xlim = c(prediction_limits[1], prediction_limits[2]),
               ylim = c(prediction_limits[3], prediction_limits[4]), crs = 4326) +
      theme_minimal()
    plot_list[["abundance"]] <- gg_abund
  }

  if (length(plot_list) > 0) {
    combined_plot <- wrap_plots(plot_list, ncol = 2)
    print(combined_plot)
  } else {
    message("No plots were created.")
  }
}
