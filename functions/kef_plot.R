kef_plot <- function(plot_limits, annotation_labels) {
  ggplot() +
    geom_sf(data = ausc, fill = "seashell2", colour = "grey80", size = 0.1) +
    geom_sf(data = terrnp, aes(fill = leg_catego), alpha = 4/5, colour = NA, show.legend = F) +
    labs(fill = "Terrestrial Managed Areas") +
    terr_fills +
    new_scale_fill() +
    geom_sf(data = kef, aes(fill = abbrv), alpha = 0.7, color = NA) +
    scale_fill_manual(name = "Key Ecological Features", guide = "legend",
                      values = with(kef, setNames(colour, abbrv))) +
    # kef_fills +
    new_scale_fill() +
    geom_sf(data = terrnp, aes(fill = leg_catego), colour = NA, alpha = 0.8, show.legend = F) +
    terr_fills +
    new_scale_fill() +
    geom_sf(data = marine_parks_state, aes(fill = zone), colour = NA) +
    scale_fill_manual(name = "State Marine Parks", guide = "legend",
                      values = with(marine_parks_state, setNames(colour, zone))) +
    new_scale_colour() +
    geom_sf(data = marine_parks_amp, aes(colour = zone), fill = NA, linewidth = 0.4, alpha = 0.3) +
    scale_colour_manual(name = "Australian Marine Parks", guide = "legend",
                      values = with(marine_parks_amp, setNames(colour, zone))) +
    new_scale_colour() +
    geom_sf(data = cwatr, colour = "firebrick", alpha = 1, linewidth = 0.4, lineend = "round") +
    labs(x = NULL, y = NULL,  fill = "Key Ecological Features") +
    annotate("text", x = annotation_labels$x,
             y = annotation_labels$y,
             label = annotation_labels$label, size = 1.65,
             fontface = "italic") +
    coord_sf(xlim = c(plot_limits[1], plot_limits[2]), ylim = c(plot_limits[3], plot_limits[4]), crs = 4326) +
    theme_minimal() +
    theme(panel.grid = element_blank())
}
