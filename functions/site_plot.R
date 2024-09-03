site_plot <- function(site_limits, # Tighter zoom for this plot
                      annotation_labels) {
  ggplot() +
    geom_spatraster_contour_filled(data = bathy,
                                   breaks = c(0, -30, -70, -200, -700, -2000, -4000, -10000), alpha = 4/5) +
    scale_fill_grey(start = 1, end = 0.5 , guide = "none") +
    geom_sf(data = ausc, fill = "seashell2", colour = "grey80", size = 0.1) +
    new_scale_fill() +
    geom_sf(data = terrnp, aes(fill = leg_catego), colour = NA, alpha = 0.8) +
    terr_fills +
    new_scale_fill() +
    geom_sf(data = marine_parks_state, aes(fill = zone), colour = NA, alpha = 0.4) +
    state_fills +
    new_scale_fill() +
    geom_sf(data = marine_parks_amp, aes(fill = zone), colour = NA, alpha = 0.8) +
    amp_fills +
    new_scale_fill() +
    labs(x = NULL, y = NULL) +
    new_scale_fill() +
    geom_sf(data = cwatr, colour = "firebrick", alpha = 1, size = 0.2, lineend = "round") +
    geom_sf(data = metadata, alpha = 1, shape = 10, size = 0.8, colour = "indianred4") +
    annotate("text", x = annotation_labels$x,
             y = annotation_labels$y,
             label = annotation_labels$label, size = 1.65,
             fontface = "italic") +
    coord_sf(xlim = c(site_limits[1], site_limits[2]), ylim = c(site_limits[3], site_limits[4]), crs = 4326) +
    theme_minimal() +
    theme(panel.grid = element_blank())
}
