dominantbenthos_plot <- function(prediction_limits) {
  ggplot() +
    geom_tile(data = pred_plot, aes(x = x, y = y, fill = p_inverts.alpha, alpha = p_inverts.fit)) +
    scale_alpha_continuous(range = c(0, 1), guide = "none", name = "Sessile invertebrates") +
    scale_fill_gradient(low = "white", high = "deeppink3", name = "Sessile invertebrates", na.value = "transparent") +
    new_scale_fill() +
    new_scale("alpha") +
    geom_tile(data = pred_plot, aes(x = x, y = y, fill = p_sand.alpha, alpha = p_sand.fit)) +
    scale_alpha_continuous(range = c(0, 1), guide = "none", name = "Sand") +
    scale_fill_gradient(low = "white", high = "wheat", name = "Sand", na.value = "transparent") +
    new_scale_fill() +
    new_scale("alpha") +
    # geom_tile(data = pred_plot, aes(x = x, y = y, fill = p_rock.alpha, alpha = p_rock.fit)) +
    # scale_alpha_continuous(range = c(0, 1), guide = "none", name = "Rock") +
    # scale_fill_gradient(low = "white", high = "grey40", name = "Rock", na.value = "transparent") +
    # new_scale_fill() +
    # new_scale("alpha") +
    # geom_tile(data = pred_plot, aes(x = x, y = y, fill = p_macro.alpha, alpha = p_macro.fit)) +
    # scale_alpha_continuous(range = c(0, 1), guide = "none", name = "Macroalgae") +
    # scale_fill_gradient(low = "white", high = "darkorange4", name = "Macroalgae", na.value = "transparent") +
    # new_scale_fill() +
    # new_scale("alpha") +
    # geom_tile(data = pred_plot, aes(x = x, y = y, fill = p_seagrass.alpha, alpha = p_seagrass.fit)) +
    # scale_alpha_continuous(range = c(0, 1), guide = "none", name = "Seagrass") +
    # scale_fill_gradient(low = "white", high = "forestgreen", name = "Seagrass", na.value = "transparent") +
    # new_scale_fill() +
    # new_scale("alpha") +
    geom_sf(data = ausc, fill = "seashell2", colour = "black", size = 0.2) +
    labs(x = "", y = "") +
    geom_sf(data = marine_parks_amp, aes(colour = zone), fill = NA, show.legend = F,
            linewidth = 0.75) +
    scale_colour_manual(name = "Australian Marine Parks",
                        values = with(marine_parks_amp, setNames(colour, zone))) +
    theme_minimal() +
    coord_sf(xlim = c(prediction_limits[1], prediction_limits[2]), ylim = c(prediction_limits[3], prediction_limits[4]), crs = 4326)
}
