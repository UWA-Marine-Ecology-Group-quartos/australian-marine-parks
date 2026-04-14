dominantbenthos_plot_single <- function(pred_plot, prediction_limits) {

  ggplot() +
    geom_tile(data = pred_plot, aes(x = x, y = y, fill = p_inverts.alpha, alpha = p_inverts.fit)) +
    scale_alpha_continuous(range = c(0, 1), guide = "none", name = "Sessile invertebrates") +
    scale_fill_gradient(
      low = "white", high = "deeppink3",
      name = "Sessile\ninvertebrates",
      na.value = "transparent",
      breaks = c(0, 0.5, 1),
      labels = c("0", "0.5", "1")
    ) +
    new_scale_fill() +
    new_scale("alpha") +
    geom_tile(data = pred_plot, aes(x = x, y = y, fill = p_sand.alpha, alpha = p_sand.fit)) +
    scale_alpha_continuous(range = c(0, 1), guide = "none", name = "Sand") +
    scale_fill_gradient(
      low = "white", high = "wheat",
      name = "Sand",
      na.value = "transparent",
      breaks = c(0, 0.5, 1),
      labels = c("0", "0.5", "1")
    ) +
    new_scale_fill() +
    new_scale("alpha") +
    geom_tile(data = pred_plot, aes(x = x, y = y, fill = p_rock.alpha, alpha = p_rock.fit)) +
    scale_alpha_continuous(range = c(0, 1), guide = "none", name = "Rock") +
    scale_fill_gradient(
      low = "white", high = "grey40",
      name = "Rock",
      na.value = "transparent",
      breaks = c(0, 0.5, 1),
      labels = c("0", "0.5", "1")
    ) +
    new_scale_fill() +
    new_scale("alpha") +
    geom_tile(data = pred_plot, aes(x = x, y = y, fill = p_macro.alpha, alpha = p_macro.fit)) +
    scale_alpha_continuous(range = c(0, 1), guide = "none", name = "Macroalgae") +
    scale_fill_gradient(
      low = "white", high = "darkorange4",
      name = "Macroalgae",
      na.value = "transparent",
      breaks = c(0, 0.5, 1),
      labels = c("0", "0.5", "1")
    ) +
    new_scale_fill() +
    new_scale("alpha") +
    geom_tile(data = pred_plot, aes(x = x, y = y, fill = p_seagrass.alpha, alpha = p_seagrass.fit)) +
    scale_alpha_continuous(range = c(0, 1), guide = "none", name = "Seagrass") +
    scale_fill_gradient(
      low = "white", high = "forestgreen",
      name = "Seagrass",
      na.value = "transparent",
      breaks = c(0, 0.5, 1),
      labels = c("0", "0.5", "1")
    ) +
    geom_contour(
      data = bathy,
      aes(x = x, y = y, z = Depth),
      colour = "black",
      breaks = c(-30, -70, -200),
      linewidth = 0.1
    ) +
    geom_sf(data = ausc, fill = "seashell2", colour = "black", linewidth = 0.2) +
    geom_sf(
      data = marine_parks_amp,
      aes(colour = zone),
      fill = NA,
      show.legend = FALSE,
      linewidth = 0.6
    ) +
    geom_sf(data = cwatr, colour = "firebrick", linewidth = 0.6) +
    scale_colour_manual(
      name = "Australian Marine Parks",
      values = with(marine_parks_amp, setNames(colour, zone))
    ) +
    coord_sf(
      xlim = c(prediction_limits[1], prediction_limits[2]),
      ylim = c(prediction_limits[3], prediction_limits[4]),
      crs = 4326,
      expand = FALSE
    ) +
    labs(x = NULL, y = NULL, colour = NULL) +
    theme_minimal() +
    theme(
      axis.title = element_blank(),
      axis.text = element_text(size = 8),
      axis.ticks = element_line(linewidth = 0.2),
      panel.grid.major = element_line(linewidth = 0.2, colour = "grey85"),
      panel.grid.minor = element_blank(),
      legend.title = element_text(size = 8),
      legend.text = element_text(size = 7),
      legend.key.height = unit(0.45, "cm"),
      legend.key.width = unit(0.45, "cm"),
      plot.margin = margin(2, 2, 2, 2, unit = "mm")
    )
}
