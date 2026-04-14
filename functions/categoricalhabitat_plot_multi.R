categoricalhabitat_plot_multi <- function(dat_list, prediction_limits) {

  yrs <- names(dat_list)

  if (is.null(yrs) || any(yrs == "")) {
    stop("dat_list must be a named list")
  }

  pred_cat <- purrr::map_dfr(seq_along(dat_list), function(i) {
    dat_list[[i]] %>%
      as.data.frame(xy = TRUE) %>%
      dplyr::mutate(year = yrs[i]) %>%
      normalise_se()
  }) %>%
    dplyr::mutate(
      year = factor(year, levels = yrs),
      dom_tag = as.character(dom_tag),
      dom_tag = dplyr::case_when(
        dom_tag %in% c("sand", "Sand") ~ "Sand",
        dom_tag %in% c("macro", "macroalgae", "Macroalgae") ~ "Macroalgae",
        dom_tag %in% c("seagrass", "seagrasses", "Seagrass", "Seagrasses") ~ "Seagrass",
        dom_tag %in% c("rock", "Rock") ~ "Rock",
        dom_tag %in% c("sessile invertebrates", "Sessile Invertebrates", "inverts", "Inverts") ~ "Sessile invertebrates",
        TRUE ~ dom_tag
      ),
      dom_tag = factor(
        dom_tag,
        levels = c("Rock", "Sessile invertebrates", "Macroalgae", "Seagrass", "Sand")
      )
    )

  ggplot() +
    geom_tile(
      data = pred_cat,
      aes(x = x, y = y, fill = dom_tag)
    ) +
    scale_fill_manual(
      name = "Habitat",
      limits = c("Rock", "Sessile invertebrates", "Macroalgae", "Seagrass", "Sand"),
      values = c(
        "Rock" = "grey40",
        "Sessile invertebrates" = "plum",
        "Macroalgae" = "darkgoldenrod4",
        "Seagrass" = "forestgreen",
        "Sand" = "wheat"
      ),
      na.value = "transparent",
      drop = FALSE
    ) +
    labs(x = NULL, y = NULL, fill = NULL) +
    new_scale_color() +
    geom_contour(
      data = bathy,
      aes(x = x, y = y, z = Depth),
      colour = "black",
      breaks = c(-30, -70, -200),
      linewidth = 0.2
    ) +
    geom_sf(data = ausc, fill = "seashell2", colour = "grey80", linewidth = 0.5) +
    geom_sf(
      data = marine_parks_amp,
      aes(colour = zone),
      fill = NA,
      linewidth = 1.2,
      show.legend = FALSE
    ) +
    scale_colour_manual(
      values = with(marine_parks_amp, setNames(colour, zone))
    ) +
    new_scale_color() +
    geom_sf(
      data = wasanc,
      colour = "#bfd054",
      fill = NA,
      linewidth = 0.7,
      show.legend = FALSE
    ) +
    new_scale_color() +
    geom_sf(
      data = cwatr,
      colour = "red",
      linewidth = 0.9
    ) +
    coord_sf(
      xlim = c(prediction_limits[1], prediction_limits[2]),
      ylim = c(prediction_limits[3], prediction_limits[4]),
      crs = 4326
    ) +
    facet_wrap(~year, nrow = 1) +
    theme_minimal() +
    theme(
      panel.background = element_rect(fill = "white", colour = NA),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      legend.position = "bottom",
      legend.direction = "horizontal",
      legend.box = "horizontal",
      legend.text = element_text(size = 6),
      legend.title = element_blank()
    )
}
