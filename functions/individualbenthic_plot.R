individualbenthic_plot <- function(prediction_limits) {
  ggplot() +
    geom_spatraster(data = pred_rast) +
    scale_fill_viridis_c(na.value = "transparent", name = "Probability", direction = -1) +
    new_scale_fill() +
    geom_sf(data = ausc, fill = "seashell2", colour = "black", size = 0.2) +
    geom_sf(data = marine_parks_amp, aes(colour = zone), fill = NA, show.legend = F,
            linewidth = 0.75) +
    scale_colour_manual(name = "Australian Marine Parks",
                        values = with(marine_parks_amp, setNames(colour, zone))) +
    coord_sf(xlim = c(prediction_limits[1], prediction_limits[2]), ylim = c(prediction_limits[3], prediction_limits[4]), crs = 4326) +
    labs(x = NULL, y = NULL, fill = "Probability",
         colour = NULL) +
    theme_minimal() +
    facet_wrap(~lyr, ncol = 2)
}
