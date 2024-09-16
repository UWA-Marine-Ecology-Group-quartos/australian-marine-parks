plot_dhw <- function(prediction_limits) {
  ggplot() +
    geom_spatraster(data = dhw) +
    scale_fill_viridis_c(na.value = NA) +
    geom_sf(data = aus) +
    facet_wrap(~lyr) +
    theme_minimal()  +
    labs(fill = "DHW (°C/weeks)") +
    coord_sf(xlim = c(prediction_limits[1], prediction_limits[2]),
             ylim = c(prediction_limits[3], prediction_limits[4]), crs = 4326)
}
