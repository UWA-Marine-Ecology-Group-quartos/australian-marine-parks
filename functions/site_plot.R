site_plot <- function(site_limits, # Tighter zoom for this plot
                      annotation_labels) {
  ggplot() +
    geom_spatraster_contour_filled(data = bathy,
                                   breaks = c(0, -30, -70, -200, -700, -2000, -4000, -10000), alpha = 4/5) +
    # scale_fill_grey(start = 1, end = 0.5 , guide = "none") +
    scale_fill_manual(values = c("#FFFFFF", "#EFEFEF", "#DEDEDE", "#CCCCCC", "#B6B6B6", "#9E9E9E", "#808080"),
                      guide = "none") +
    geom_sf(data = ausc, fill = "seashell2", colour = "grey80", size = 0.1) +
    new_scale_fill() +
    geom_sf(data = terrnp, aes(fill = leg_catego), colour = NA, alpha = 0.8) +
    terr_fills +
    new_scale_fill() +
    geom_sf(data = marine_parks_state, aes(fill = zone), colour = NA, alpha = 0.4) +
    scale_fill_manual(name = "State Marine Parks", guide = "legend",
                      values = with(marine_parks_state, setNames(colour, zone))) +
    new_scale_fill() +
    geom_sf(data = marine_parks_amp, aes(fill = zone), colour = NA, alpha = 0.8) +
    scale_fill_manual(name = "Australian Marine Parks", guide = "legend",
                      values = with(marine_parks_amp, setNames(colour, zone))) +
    new_scale_fill() +
    labs(x = NULL, y = NULL) +
    new_scale_fill() +
    geom_sf(data = cwatr, colour = "firebrick", alpha = 1, size = 0.2, lineend = "round") +
    geom_sf(data = metadata, alpha = 1, shape = 4, size = 0.8, aes(colour = method)) +
    scale_colour_manual(values = c("BRUV" = "#E1BE6A",
                                   "BOSS" = "#40B0A6"),
                        name = "Method") +
    annotate("text", x = annotation_labels$x,
             y = annotation_labels$y,
             label = annotation_labels$label, size = 1.65,
             fontface = "italic") +
    coord_sf(xlim = c(site_limits[1], site_limits[2]), ylim = c(site_limits[3], site_limits[4]), crs = 4326) +
    theme_minimal() +
    theme(panel.grid = element_blank())
}
