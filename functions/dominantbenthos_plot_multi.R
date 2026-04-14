dominantbenthos_plot_multi <- function(dat_list, prediction_limits) {

  yrs <- names(dat_list)

  if (is.null(yrs) || any(yrs == "")) {
    stop("dat_list must be a named list")
  }

  # ------------------------------------------------------------
  # Extract dominant benthos data + combined SE rasters by year
  # ------------------------------------------------------------
  dom_plot_list <- vector("list", length(dat_list))
  se_list <- vector("list", length(dat_list))

  for (i in seq_along(dat_list)) {
    dat <- dat_list[[i]]

    pred_class <- as.data.frame(dat, xy = TRUE) %>%
      dplyr::mutate(year = yrs[i])

    dom_plot_list[[i]] <- normalise_se(data = pred_class)
    se_list[[i]] <- dat[["mean_se"]]
  }

  # Shared SE limits across years
  se_vals <- unlist(lapply(se_list, terra::values))
  se_limits <- range(se_vals, na.rm = TRUE)

  # ------------------------------------------------------------
  # Theme variants
  # ------------------------------------------------------------
  theme_left <- theme(
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

  theme_inner <- theme_left +
    theme(
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank()
    )

  theme_top <- theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  )

  build_base <- function(i, show_x = TRUE) {

    y_theme <- if (i == 1) theme_left else theme_inner
    x_theme <- if (show_x) theme() else theme_top

    list(
      geom_contour(
        data = bathy,
        aes(x = x, y = y, z = Depth),
        colour = "black",
        breaks = c(-30, -70, -200),
        linewidth = 0.1
      ),
      geom_sf(data = ausc, fill = "seashell2", colour = "black", linewidth = 0.2),
      geom_sf(
        data = marine_parks_amp,
        aes(colour = zone),
        fill = NA,
        show.legend = FALSE,
        linewidth = 0.6
      ),
      geom_sf(data = cwatr, colour = "firebrick", linewidth = 0.6),
      scale_colour_manual(
        name = "Australian Marine Parks",
        values = with(marine_parks_amp, setNames(colour, zone))
      ),
      coord_sf(
        xlim = c(prediction_limits[1], prediction_limits[2]),
        ylim = c(prediction_limits[3], prediction_limits[4]),
        crs = 4326,
        expand = FALSE
      ),
      labs(x = NULL, y = NULL, colour = NULL),
      theme_minimal(),
      y_theme,
      x_theme,
      theme(
        plot.title = element_text(hjust = 0.5, face = "bold", size = 10)
      )
    )
  }

  # ------------------------------------------------------------
  # Top row: dominant benthos panels
  # ------------------------------------------------------------
  p_dom <- lapply(seq_along(yrs), function(i) {

    pred_plot <- dom_plot_list[[i]]

    ggplot() +
      geom_tile(data = pred_plot, aes(x = x, y = y, fill = p_inverts.alpha, alpha = p_inverts.fit)) +
      scale_alpha_continuous(range = c(0, 1), guide = "none", name = "Sessile invertebrates") +
      scale_fill_gradient(low = "white", high = "deeppink3", name = "Sessile\ninvertebrates", na.value = "transparent",
                          breaks = c(0, 0.5, 1),
                          labels = c("0", "0.5", "1")) +
      new_scale_fill() +
      new_scale("alpha") +
      geom_tile(data = pred_plot, aes(x = x, y = y, fill = p_sand.alpha, alpha = p_sand.fit)) +
      scale_alpha_continuous(range = c(0, 1), guide = "none", name = "Sand") +
      scale_fill_gradient(low = "white", high = "wheat", name = "Sand", na.value = "transparent",
                          breaks = c(0, 0.5, 1),
                          labels = c("0", "0.5", "1")) +
      new_scale_fill() +
      new_scale("alpha") +
      geom_tile(data = pred_plot, aes(x = x, y = y, fill = p_rock.alpha, alpha = p_rock.fit)) +
      scale_alpha_continuous(range = c(0, 1), guide = "none", name = "Rock") +
      scale_fill_gradient(low = "white", high = "grey40", name = "Rock", na.value = "transparent",
                          breaks = c(0, 0.5, 1),
                          labels = c("0", "0.5", "1")) +
      new_scale_fill() +
      new_scale("alpha") +
      geom_tile(data = pred_plot, aes(x = x, y = y, fill = p_macro.alpha, alpha = p_macro.fit)) +
      scale_alpha_continuous(range = c(0, 1), guide = "none", name = "Macroalgae") +
      scale_fill_gradient(low = "white", high = "darkorange4", name = "Macroalgae", na.value = "transparent",
                          breaks = c(0, 0.5, 1),
                          labels = c("0", "0.5", "1")) +
      new_scale_fill() +
      new_scale("alpha") +
      geom_tile(data = pred_plot, aes(x = x, y = y, fill = p_seagrass.alpha, alpha = p_seagrass.fit)) +
      scale_alpha_continuous(range = c(0, 1), guide = "none", name = "Seagrass") +
      scale_fill_gradient(low = "white", high = "forestgreen", name = "Seagrass", na.value = "transparent",
                          breaks = c(0, 0.5, 1),
                          labels = c("0", "0.5", "1")) +
      ggtitle(yrs[i]) +
      build_base(i, show_x = FALSE)
  })

  # ------------------------------------------------------------
  # Bottom row: combined SE panels
  # ------------------------------------------------------------
  p_se <- lapply(seq_along(yrs), function(i) {
    ggplot() +
      geom_spatraster(data = se_list[[i]], maxcell = Inf) +
      scale_fill_viridis_c(
        option = "A",
        na.value = "transparent",
        name = "Normalised\ncombined SE",
        limits = se_limits,
        oob = scales::squish
      ) +
      build_base(i, show_x = TRUE)
  })

  # ------------------------------------------------------------
  # Row labels
  # ------------------------------------------------------------
  row_label_plot <- function(label) {
    ggplot() +
      theme_void() +
      annotate(
        "text", x = 0.5, y = 0.5,
        label = label, angle = 90,
        fontface = "bold", size = 4
      )
  }

  dom_label <- row_label_plot("Predicted Habitat Probability")
  se_label  <- row_label_plot("Standard Error")

  # ------------------------------------------------------------
  # Combine
  # ------------------------------------------------------------
  dom_row <- dom_label + wrap_plots(p_dom, nrow = 1, guides = "collect") +
    plot_layout(widths = c(0.06, 1))

  se_row <- se_label + wrap_plots(p_se, nrow = 1, guides = "collect") +
    plot_layout(widths = c(0.06, 1))

  p_out <- (dom_row / se_row) +
    plot_layout(heights = c(1, 1), guides = "collect") &
    theme(
      legend.position = "bottom",
      legend.direction = "horizontal",
      legend.box = "horizontal",
      legend.box.just = "centre",
      legend.justification = "centre",

      legend.title = element_text(
        size = 7,
        margin = margin(b = 10, r = 3)
      ),
      legend.text = element_text(size = 6),

      legend.key.height = unit(0.3, "cm"),
      legend.key.width  = unit(0.35, "cm"),

      legend.spacing.x = unit(1, "mm"),
      legend.spacing.y = unit(0.5, "mm"),
      legend.spacing   = unit(0.5, "mm"),

      legend.box.margin = margin(0, 0, 0, 0),
      panel.spacing = unit(0.5, "mm"),
      plot.margin = margin(2, 2, 2, 2, unit = "mm")
    )

  return(p_out)
}
