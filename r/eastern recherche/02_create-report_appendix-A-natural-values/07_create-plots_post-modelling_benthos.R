###
# Project: NESP 4.20 - Marine Park Dashboard reporting
# Data:    Habitat data synthesis & habitat models derived from FSSgam
# Task:    Create post-modelling habitat figures for marine park reporting
# Author:  Claude Spencer
# Date:    June 2024
###

# Clear your environment
rm(list = ls())

# Set the study name
script_dir <- dirname(
  rstudioapi::getActiveDocumentContext()$path
)

config <- yaml::read_yaml(
  file.path(script_dir, "00_config.yml")
)

name <- config$name
park <- config$park
years <- config$years

# Load libraries
library(tidyverse)
library(terra)
library(sf)
library(ggnewscale)
library(scales)
library(tidyterra)
library(patchwork)
library(scatterpie)
library(CheckEM)
library(grid)
library(viridis)
library(geos)

# Load functions
file.sources <- list.files(pattern = "*.R", path = "functions/", full.names = TRUE)
sapply(file.sources, source, .GlobalEnv)

# TODO Set cropping extent - larger than most zoomed out plot
e <- ext(123.1, 124.0, -34.7, -33.9)

# Load necessary spatial files
ausc <- st_read("data/south-west network/spatial/shapefiles/aus-shapefile-w-investigator-stokes.shp") %>%
  st_crop(e) %>%
  st_transform(4326)

marine_parks <- st_read("data/south-west network/spatial/shapefiles/western-australia_marine-parks-all.shp") %>%
  dplyr::filter(name %in% c("Eastern Recherche")) # TODO select relevant parks

marine_parks_amp <- marine_parks %>%
  dplyr::filter(epbc %in% "Commonwealth") %>%
  st_transform(4326)

marine_parks_state <- marine_parks %>%
  dplyr::filter(epbc %in% "State") %>%
  st_transform(4326)

npz <- marine_parks[marine_parks$zone %in% "National Park Zone", ]
wasanc <- marine_parks[marine_parks$zone %in% "Sanctuary Zone", ]

cwatr <- st_read("data/south-west network/spatial/shapefiles/amb_coastal_waters_limit.shp") %>%
  st_make_valid() %>%
  st_crop(e) %>%
  st_transform(4326)

cwatr_offset <- st_as_sf(geos_offset_curve(as_geos_geometry(cwatr), distance = 0.003))
st_crs(cwatr_offset) <- 4326

# Load the bathymetry data (GA 250m resolution)
bathy <- rast("data/south-west network/spatial/rasters/AusBathyTopo__Australia__2024_250m_MSL_cog.tif") %>%
  crop(e) %>%
  clamp(upper = 0, lower = -250, values = FALSE) %>%
  trim() %>%
  as.data.frame(xy = TRUE, na.rm = TRUE)

names(bathy)[3] <- "Depth"

# Map pretty habitat names to raster layer prefixes in dat
habitat_lookup <- c(
  "Sand" = "sand",
  "Macroalgae" = "macro",
  "Reef" = "reef",
  "Sessile invertebrates" = "inverts"
)

# Optional habitat colours for other functions if needed
hab_cols <- c(
  "Sand" = "wheat",
  "Macroalgae" = "darkgoldenrod4",
  "Reef" = "darkorange",
  "Sessile invertebrates" = "plum"
)

# TODO Plot extent
prediction_limits <- c(123.1, 124.0, -34.7, -33.9)

# Read all years once
dat_list <- setNames(vector("list", length(years)), years)

for (yr in years) {
  message("Reading year: ", yr)

  dat_list[[as.character(yr)]] <- readRDS(
    paste0(
      "output/model-output/", park, "/habitat/",
      name, "_predicted-habitat_", yr, ".rds"
    )
  )
}

dominantbenthos_plot_single <- function(pred_plot, prediction_limits) {

  ggplot() +
    new_scale_fill() +
    new_scale("alpha") +
    geom_tile(data = pred_plot, aes(x = x, y = y, fill = p_reef.alpha, alpha = p_reef.fit)) +
    scale_alpha_continuous(range = c(0, 1), guide = "none", name = "Reef") +
    scale_fill_gradient(
      low = "white", high = "darkorange",
      name = "Reef",
      na.value = "transparent",
      breaks = c(0, 0.5, 1),
      labels = c("0", "0.5", "1")
    ) +
    new_scale_fill() +
    new_scale("alpha") +
    geom_tile(data = pred_plot, aes(x = x, y = y, fill = p_inverts.alpha, alpha = p_inverts.fit)) +
    scale_alpha_continuous(range = c(0, 1), guide = "none") +
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
    geom_tile(data = pred_plot, aes(x = x, y = y, fill = p_macro.alpha, alpha = p_macro.fit)) +
    scale_alpha_continuous(range = c(0, 1), guide = "none", name = "Macroalgae") +
    scale_fill_gradient(
      low = "white", high = "darkorange4",
      name = "Macroalgae",
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

categoricalhabitat_plot_single <- function(pred_plot, prediction_limits) {

  pred_cat <- pred_plot %>%
    dplyr::mutate(
      dom_tag = as.character(dom_tag),
      dom_tag = dplyr::case_when(
        dom_tag %in% c("sand", "Sand") ~ "Sand",
        dom_tag %in% c("macro", "macroalgae", "Macroalgae") ~ "Macroalgae",
        dom_tag %in% c("sessile invertebrates", "Sessile Invertebrates", "inverts", "Inverts") ~ "Sessile invertebrates",
        dom_tag %in% c("reef", "Reef") ~ "Reef",
        TRUE ~ dom_tag
      ),
      dom_tag = factor(
        dom_tag,
        levels = c("Sessile invertebrates", "Macroalgae", "Reef", "Sand")  # <-- add Reef
      )
    )

  ggplot() +
    geom_tile(
      data = pred_cat,
      aes(x = x, y = y, fill = dom_tag)
    ) +
    scale_fill_manual(
      name = "Habitat",
      limits = c("Sessile invertebrates", "Macroalgae", "Reef", "Sand"),
      values = c(
        "Sessile invertebrates" = "plum",
        "Macroalgae" = "darkgoldenrod4",
        "Reef" = "darkorange",
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

normalise_se <- function(data) {
  if ("p_sand.se.fit" %in% colnames(data)) {
    data <- data %>%
      dplyr::mutate(p_sand.alpha = 1 - (p_sand.se.fit - min(p_sand.se.fit, na.rm = TRUE)) /
                      (max(p_sand.se.fit, na.rm = TRUE) - min(p_sand.se.fit, na.rm = TRUE)))
  }


  if ("p_macro.se.fit" %in% colnames(data)) {
    data <- data %>%
      dplyr::mutate(p_macro.alpha = 1 - (p_macro.se.fit - min(p_macro.se.fit, na.rm = TRUE)) /
                      (max(p_macro.se.fit, na.rm = TRUE) - min(p_macro.se.fit, na.rm = TRUE)))
  }

  if ("p_inverts.se.fit" %in% colnames(data)) {
    data <- data %>%
      dplyr::mutate(p_inverts.alpha = 1 - (p_inverts.se.fit - min(p_inverts.se.fit, na.rm = TRUE)) /
                      (max(p_inverts.se.fit, na.rm = TRUE) - min(p_inverts.se.fit, na.rm = TRUE)))
  }

  if ("p_black.se.fit" %in% colnames(data)) {
    data <- data %>%
      dplyr::mutate(p_black.alpha = 1 - (p_black.se.fit - min(p_black.se.fit, na.rm = TRUE)) /
                      (max(p_black.se.fit, na.rm = TRUE) - min(p_black.se.fit, na.rm = TRUE)))
  }

  if ("p_reef.se.fit" %in% colnames(data)) {
    data <- data %>%
      dplyr::mutate(p_reef.alpha = 1 - (p_reef.se.fit - min(p_reef.se.fit, na.rm = TRUE)) /
                      (max(p_reef.se.fit, na.rm = TRUE) - min(p_reef.se.fit, na.rm = TRUE)))
  }
  return(data)
}


# -------------------------------------------------------------------
# PART 1: Single-year plots (categorical + dominant benthos)
# -------------------------------------------------------------------
for (yr in years) {

  message("Building per-year plots for: ", yr)

  dat <- dat_list[[as.character(yr)]]

  pred_class <- as.data.frame(dat, xy = TRUE) %>%
    dplyr::mutate(year = yr)

  pred_plot <- normalise_se(data = pred_class)

  p_cat <- categoricalhabitat_plot_single(
    pred_plot = pred_plot,
    prediction_limits = prediction_limits
  )

  print(p_cat)

  ggsave(
    filename = paste0(
      "plots/", park, "/habitat/", name,
      "_predicted-habitat-categorical_", yr, ".png"
    ),
    plot = p_cat,
    height = 6,
    width = 8,
    dpi = 600,
    units = "in",
    bg = "white"
  )

  saveRDS(p_cat,
    paste0(
      "plots/", park, "/habitat/", name,
      "_predicted-habitat-categorical_", yr, ".rds"
    )
  )

  p_dom <- dominantbenthos_plot_single(
    pred_plot = pred_plot,
    prediction_limits = prediction_limits
  ) +
    theme(
      legend.position = "bottom",
      legend.direction = "horizontal",
      legend.box = "horizontal",
      legend.box.just = "left",
      legend.text = element_text(size = 5),
      legend.title = element_text(size = 7),
      legend.key.size = unit(0.5, "cm"),
      legend.margin = margin(t = -0.1, unit = "cm")
    )

  print(p_dom)

  ggsave(
    filename = paste0(
      "plots/", park, "/habitat/", name,
      "_predicted-dominant-habitat_", yr, ".png"
    ),
    plot = p_dom,
    height = 6,
    width = 8,
    dpi = 600,
    units = "in",
    bg = "white"
  )

  saveRDS(p_dom,
          paste0(
            "plots/", park, "/habitat/", name,
            "_predicted-dominant-habitat_", yr, ".rds"
          )
  )
}

# -------------------------------------------------------------------
# PART 2: Multi-year categorical and dominant benthos + combined SE plot
# -------------------------------------------------------------------
dominantbenthos_plot_multi <- function(dat_list, prediction_limits) {

  yrs <- names(dat_list)

  if (is.null(yrs) || any(yrs == "")) {
    stop("dat_list must be a named list")
  }

  dom_plot_list <- vector("list", length(dat_list))
  se_list <- vector("list", length(dat_list))

  for (i in seq_along(dat_list)) {
    dat <- dat_list[[i]]

    pred_class <- as.data.frame(dat, xy = TRUE) %>%
      dplyr::mutate(year = yrs[i])

    dom_plot_list[[i]] <- normalise_se(data = pred_class)
    se_list[[i]] <- dat[["mean_se"]]
  }

  se_vals <- unlist(lapply(se_list, terra::values))
  se_limits <- range(se_vals, na.rm = TRUE)

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
      # Sand (bottom)
      geom_tile(data = pred_plot, aes(x = x, y = y, fill = p_sand.alpha, alpha = p_sand.fit)) +
      scale_alpha_continuous(range = c(0, 1), guide = "none") +
      scale_fill_gradient(
        low = "white", high = "wheat",
        name = "Sand",
        na.value = "transparent",
        breaks = c(0, 0.5, 1),
        labels = c("0", "0.5", "1")
      ) +
      new_scale_fill() +
      new_scale("alpha") +
      # Reef
      geom_tile(data = pred_plot, aes(x = x, y = y, fill = p_reef.alpha, alpha = p_reef.fit)) +
      scale_alpha_continuous(range = c(0, 1), guide = "none") +
      scale_fill_gradient(
        low = "white", high = "darkorange",
        name = "Reef",
        na.value = "transparent",
        breaks = c(0, 0.5, 1),
        labels = c("0", "0.5", "1")
      ) +
      new_scale_fill() +
      new_scale("alpha") +
      # Macroalgae
      geom_tile(data = pred_plot, aes(x = x, y = y, fill = p_macro.alpha, alpha = p_macro.fit)) +
      scale_alpha_continuous(range = c(0, 1), guide = "none") +
      scale_fill_gradient(
        low = "white", high = "darkorange4",
        name = "Macroalgae",
        na.value = "transparent",
        breaks = c(0, 0.5, 1),
        labels = c("0", "0.5", "1")
      ) +
      new_scale_fill() +
      new_scale("alpha") +
      # Sessile invertebrates (top)
      geom_tile(data = pred_plot, aes(x = x, y = y, fill = p_inverts.alpha, alpha = p_inverts.fit)) +
      scale_alpha_continuous(range = c(0, 1), guide = "none") +
      scale_fill_gradient(
        low = "white", high = "deeppink3",
        name = "Sessile\ninvertebrates",
        na.value = "transparent",
        breaks = c(0, 0.5, 1),
        labels = c("0", "0.5", "1")
      ) +
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
      legend.title = element_text(size = 7, margin = margin(b = 10, r = 3)),
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


p_dom_se <- dominantbenthos_plot_multi(
  dat_list = dat_list,
  prediction_limits = prediction_limits
)

print(p_dom_se)

ggsave(
  filename = paste0(
    "plots/", park, "/habitat/", name,
    "_predicted-dominant-benthos-and-combined-se_",
    paste(years, collapse = "-"), ".png"
  ),
  plot = p_dom_se,
  height = 7,
  width = 8,
  dpi = 900,
  units = "in",
  bg = "white"
)

saveRDS(p_dom_se,
        paste0(
          "plots/", park, "/habitat/", name,
          "_predicted-dominant-benthos-and-combined-se_",
          paste(years, collapse = "-"), ".rds"
        ))

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
        dom_tag %in% c("reef", "Reef") ~ "Reef",
        dom_tag %in% c("sessile invertebrates", "Sessile Invertebrates", "inverts", "Inverts") ~ "Sessile invertebrates",
        TRUE ~ dom_tag
      ),
      dom_tag = factor(
        dom_tag,
        levels = c("Sessile invertebrates", "Macroalgae", "Reef", "Sand")
      )
    )
  habitat_colours <- c(
    "Sessile invertebrates" = "plum",
    "Macroalgae" = "darkgoldenrod4",
    "Reef" = "darkorange",
    "Sand" = "wheat"
  )
  ngari_colours <- wasanc %>%
    st_drop_geometry() %>%
    distinct(zone, colour) %>%
    arrange(zone) %>%
    pull(colour)
  ggplot() +
    geom_tile(
      data = pred_cat,
      aes(x = x, y = y, fill = dom_tag)
    ) +
    scale_fill_manual(
      name = "Habitat",
      limits = names(habitat_colours),
      values = habitat_colours,
      na.value = "transparent",
      drop = FALSE
    ) +
    guides(
      fill = guide_legend(
        order = 1,
        override.aes = list(
          colour = NA,
          fill = unname(habitat_colours),
          linewidth = 0.5
        )
      )
    ) +
    labs(x = NULL, y = NULL) +
    geom_contour(
      data = bathy,
      aes(x = x, y = y, z = Depth),
      colour = "black",
      breaks = c(-30, -70, -200),
      linewidth = 0.2
    ) +
    geom_sf(data = ausc, fill = "seashell2", colour = "grey80", linewidth = 0.5) +
    new_scale_color() +
    geom_sf(
      data = wasanc,
      aes(colour = zone),
      fill = NA,
      linewidth = 0.7,
      show.legend = TRUE
    ) +
    scale_colour_manual(
      name = "State Marine Park",
      guide = "legend",
      values = with(wasanc, setNames(colour, zone))
    ) +
    guides(
      colour = guide_legend(
        order = 3,
        override.aes = list(
          colour = ngari_colours,
          fill = NA,
          linewidth = 1.2
        )
      )
    ) +
    new_scale_color() +
    geom_sf(
      data = marine_parks_amp,
      aes(colour = zone),
      fill = NA,
      linewidth = 1.2,
      show.legend = TRUE
    ) +
    scale_colour_manual(
      name = "Australian Marine Park",
      guide = "legend",
      values = with(marine_parks_amp, setNames(colour, zone))
    ) +
    guides(
      colour = guide_legend(
        order = 2,
        override.aes = list(
          fill = NA,
          linewidth = 1.2
        )
      )
    ) +
    geom_sf(
      data = st_buffer(cwatr_offset, dist = 0.005),
      colour = "red",
      linewidth = 0.9
    ) +
    coord_sf(
      xlim = c(prediction_limits[1], prediction_limits[2]),
      ylim = c(prediction_limits[3], prediction_limits[4]),
      crs = 4326,
      expand = FALSE
    ) +
    facet_wrap(~year, nrow = 1) +
    theme_minimal() +
    theme(
      panel.background = element_rect(fill = "white", colour = NA),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      legend.position = "bottom",
      legend.direction = "vertical",
      legend.box = "horizontal",
      legend.text = element_text(size = 10),
      legend.title = element_text(size = 10, face = "bold"),
      strip.text = element_text(size = 12, face = "bold")
    )
}

p_cat_multi <- categoricalhabitat_plot_multi(
  dat_list = dat_list,
  prediction_limits = prediction_limits
)

print(p_cat_multi)

ggsave(
  filename = paste0(
    "plots/", park, "/habitat/", name,
    "_predicted-habitat-categorical_",
    paste(years, collapse = "-"), ".png"
  ),
  plot = p_cat_multi,
  height = 5,
  width = 10,
  dpi = 600,
  units = "in",
  bg = "white"
)

saveRDS(p_cat_multi,
        paste0(
          "plots/", park, "/habitat/", name,
          "_predicted-habitat-categorical_",
          paste(years, collapse = "-"), ".rds"
        ))

# -------------------------------------------------------------------
# PART 3: Multi-year individual habitat plots
# -------------------------------------------------------------------
for (habitat_name in names(habitat_lookup)) {

  message("Building individual habitat plot for: ", habitat_name)

  layer_stub <- habitat_lookup[[habitat_name]]

  p_hab <- individualbenthic_plot(
    habitat_name = habitat_name,
    layer_stub = layer_stub,
    dat_list = dat_list,
    prediction_limits = prediction_limits,
    pred_limits = NULL,   # use c(0, 1) for a fixed probability scale across taxa
    se_limits = NULL      # auto-scale within habitat across years
  )

  print(p_hab)

  out_name <- habitat_name %>%
    str_to_lower() %>%
    str_replace_all("\\s+", "-")

  ggsave(
    filename = paste0(
      "plots/", park, "/habitat/", name,
      "_predicted-individual-habitat_", out_name, "_",
      paste(years, collapse = "-"), ".png"
    ),
    plot = p_hab,
    height = 5,
    width = 8,
    dpi = 900,
    units = "in",
    bg = "white"
  )

  saveRDS(p_hab,
          paste0(
            "plots/", park, "/habitat/", name,
            "_predicted-individual-habitat_", out_name, "_",
            paste(years, collapse = "-"), ".rds"
          ))
}

# -------------------------------------------------------------------
# PART 4: Control plots by taxa, facetted by depth class
# -------------------------------------------------------------------
controldata_benthos <- function(dat, year, amp_abbrv) {

  marine_parks <- st_read("data/south-west network/spatial/shapefiles/western-australia_marine-parks-all.shp") %>%
    CheckEM::clean_names() %>%
    dplyr::filter(epbc == "Commonwealth") %>%
    dplyr::mutate(zone_new = case_when(
      str_detect(zone, "Habitat Protection Zone") ~ paste(amp_abbrv, "HPZ"),
      str_detect(zone, "National Park Zone")       ~ paste(amp_abbrv, "NPZ (IUCN II)"),
      str_detect(zone, "Special Purpose Zone")     ~ paste(amp_abbrv, "other zones"),
      str_detect(zone, "Multiple Use Zone")        ~ paste(amp_abbrv, "other zones"),
      str_detect(zone, "Recreational Use Zone")    ~ paste(amp_abbrv, "other zones"),
      TRUE ~ NA_character_
    ))

  preds <- readRDS(paste0("data/", park, "/spatial/rasters/",
                          name, "_bathymetry-derivatives.rds")) %>%
    crop(dat)

  tempdat_v <- vect(as.data.frame(dat, xy = TRUE), geom = c("x", "y"), crs = "epsg:4326")
  tempdat <- cbind(as.data.frame(dat, xy = TRUE), terra::extract(preds[[1]], tempdat_v, ID = FALSE))

  depth_qs <- c(-2000, -200, -70, -30, 0)
  class_values <- 4:1
  reclass_matrix <- cbind(depth_qs[-length(depth_qs)], depth_qs[-1], class_values)
  edc <- classify(preds$geoscience_depth, rcl = reclass_matrix) %>%
    as.polygons() %>%
    st_as_sf()

  areas <- st_intersection(edc, marine_parks) %>%
    dplyr::mutate(area = st_area(.)) %>%
    dplyr::filter(area > units::set_units(625000, "m^2")) %>%
    dplyr::mutate(
      depth_contour = case_when(
        geoscience_depth == 1 ~ "shallow",
        geoscience_depth == 2 ~ "mesophotic",
        geoscience_depth == 3 ~ "rariphotic",
        geoscience_depth == 4 ~ "deep"
      ),
      filter = "no"
    ) %>%
    dplyr::select(zone, depth_contour, filter) %>%
    as.data.frame() %>%
    dplyr::select(-geometry)

  areas_shallow <- dplyr::filter(areas, depth_contour %in% "shallow") %>% dplyr::distinct(zone, filter, .keep_all = TRUE)
  areas_meso    <- dplyr::filter(areas, depth_contour %in% "mesophotic") %>% dplyr::distinct(zone, filter, .keep_all = TRUE)
  areas_rari    <- dplyr::filter(areas, depth_contour %in% "rariphotic") %>% dplyr::distinct(zone, filter, .keep_all = TRUE)

  replacement_se <- c(
    "macroalgae_se" = "p_macro.se.fit",
    "reef_se"       = "p_reef.se.fit",
    "sand_se"       = "p_sand.se.fit",
    "inverts_se"    = "p_inverts.se.fit"
  )

  replacement_mean <- c(
    "macroalgae" = "p_macro.fit",
    "reef"       = "p_reef.fit",
    "sand"       = "p_sand.fit",
    "inverts"    = "p_inverts.fit"
  )

  out <- list(shallow = NULL, meso = NULL, rari = NULL)

  # SHALLOW (0-30 m)
  if (any(tempdat$geoscience_depth < 0 & tempdat$geoscience_depth > -30, na.rm = TRUE)) {

    shallow <- preds[[1]] %>% clamp(upper = 0, lower = -30, values = FALSE)
    dat.shallow <- dat %>% terra::mask(shallow)

    errors.shallow <- terra::extract(dat.shallow, marine_parks) %>%
      dplyr::group_by(ID) %>%
      dplyr::summarise(
        dplyr::across(dplyr::ends_with(".se.fit"), \(x) mean(x, na.rm = TRUE)),
        .groups = "drop"
      ) %>%
      dplyr::mutate(ID = as.character(ID), year = year) %>%
      dplyr::rename(dplyr::any_of(replacement_se)) %>%
      dplyr::select(ID, year, dplyr::any_of(c("macroalgae_se", "reef_se", "sand_se", "inverts_se")))

    means.shallow <- terra::extract(dat.shallow, marine_parks) %>%
      dplyr::group_by(ID) %>%
      dplyr::summarise(
        dplyr::across(dplyr::matches("^p_.*(?<!\\.se)\\.fit$", perl = TRUE), \(x) mean(x, na.rm = TRUE)),
        .groups = "drop"
      ) %>%
      dplyr::mutate(ID = as.character(ID), year = year) %>%
      dplyr::rename(dplyr::any_of(replacement_mean)) %>%
      dplyr::select(ID, year, dplyr::any_of(c("macroalgae", "reef", "sand", "inverts")))

    out$shallow <- as.data.frame(marine_parks) %>%
      tibble::rownames_to_column() %>%
      dplyr::rename(ID = rowname) %>%
      dplyr::left_join(errors.shallow, by = "ID") %>%
      dplyr::left_join(means.shallow,  by = c("ID", "year")) %>%
      dplyr::left_join(areas_shallow,  by = "zone") %>%
      dplyr::filter(filter == "no") %>%
      dplyr::select(zone_new, year, dplyr::any_of(c(
        "macroalgae", "macroalgae_se",
        "reef", "reef_se",
        "sand", "sand_se",
        "inverts", "inverts_se"
      ))) %>%
      dplyr::group_by(zone_new, year) %>%
      dplyr::summarise(
        dplyr::across(dplyr::everything(), \(x) mean(x, na.rm = TRUE)),
        .groups = "drop"
      )
  }

  # MESOPHOTIC (30-70 m)
  if (any(tempdat$geoscience_depth < -30 & tempdat$geoscience_depth > -70, na.rm = TRUE)) {

    meso <- preds[[1]] %>% clamp(upper = -30, lower = -70, values = FALSE)
    dat.meso <- dat %>% terra::mask(meso)

    errors.meso <- terra::extract(dat.meso, marine_parks) %>%
      dplyr::group_by(ID) %>%
      dplyr::summarise(
        dplyr::across(dplyr::ends_with(".se.fit"), \(x) mean(x, na.rm = TRUE)),
        .groups = "drop"
      ) %>%
      dplyr::mutate(ID = as.character(ID), year = year) %>%
      dplyr::rename(dplyr::any_of(replacement_se)) %>%
      dplyr::select(ID, year, dplyr::any_of(c("macroalgae_se", "reef_se", "sand_se", "inverts_se")))

    means.meso <- terra::extract(dat.meso, marine_parks) %>%
      dplyr::group_by(ID) %>%
      dplyr::summarise(
        dplyr::across(dplyr::matches("^p_.*(?<!\\.se)\\.fit$", perl = TRUE), \(x) mean(x, na.rm = TRUE)),
        .groups = "drop"
      ) %>%
      dplyr::mutate(ID = as.character(ID), year = year) %>%
      dplyr::rename(dplyr::any_of(replacement_mean)) %>%
      dplyr::select(ID, year, dplyr::any_of(c("macroalgae", "reef", "sand", "inverts")))

    out$meso <- as.data.frame(marine_parks) %>%
      tibble::rownames_to_column() %>%
      dplyr::rename(ID = rowname) %>%
      dplyr::left_join(errors.meso, by = "ID") %>%
      dplyr::left_join(means.meso,  by = c("ID", "year")) %>%
      dplyr::left_join(areas_meso,  by = "zone") %>%
      dplyr::filter(filter == "no") %>%
      dplyr::select(zone_new, year, dplyr::any_of(c(
        "macroalgae", "macroalgae_se",
        "reef", "reef_se",
        "sand", "sand_se",
        "inverts", "inverts_se"
      ))) %>%
      dplyr::group_by(zone_new, year) %>%
      dplyr::summarise(
        dplyr::across(dplyr::everything(), \(x) mean(x, na.rm = TRUE)),
        .groups = "drop"
      )
  }

  # RARIPHOTIC (70-200 m)
  if (any(tempdat$geoscience_depth < -70 & tempdat$geoscience_depth > -200, na.rm = TRUE)) {

    rari <- preds[[1]] %>% clamp(upper = -70, lower = -200, values = FALSE)
    dat.rari <- dat %>% terra::mask(rari)

    errors.rari <- terra::extract(dat.rari, marine_parks) %>%
      dplyr::group_by(ID) %>%
      dplyr::summarise(
        dplyr::across(dplyr::ends_with(".se.fit"), \(x) mean(x, na.rm = TRUE)),
        .groups = "drop"
      ) %>%
      dplyr::mutate(ID = as.character(ID), year = year) %>%
      dplyr::rename(dplyr::any_of(replacement_se)) %>%
      dplyr::select(ID, year, dplyr::any_of(c("macroalgae_se", "reef_se", "sand_se", "inverts_se")))

    means.rari <- terra::extract(dat.rari, marine_parks) %>%
      dplyr::group_by(ID) %>%
      dplyr::summarise(
        dplyr::across(dplyr::matches("^p_.*(?<!\\.se)\\.fit$", perl = TRUE), \(x) mean(x, na.rm = TRUE)),
        .groups = "drop"
      ) %>%
      dplyr::mutate(ID = as.character(ID), year = year) %>%
      dplyr::rename(dplyr::any_of(replacement_mean)) %>%
      dplyr::select(ID, year, dplyr::any_of(c("macroalgae", "reef", "sand", "inverts")))

    out$rari <- as.data.frame(marine_parks) %>%
      tibble::rownames_to_column() %>%
      dplyr::rename(ID = rowname) %>%
      dplyr::left_join(errors.rari, by = "ID") %>%
      dplyr::left_join(means.rari,  by = c("ID", "year")) %>%
      dplyr::left_join(areas_rari,  by = "zone") %>%
      dplyr::filter(filter == "no") %>%
      dplyr::select(zone_new, year, dplyr::any_of(c(
        "macroalgae", "macroalgae_se",
        "reef", "reef_se",
        "sand", "sand_se",
        "inverts", "inverts_se"
      ))) %>%
      dplyr::group_by(zone_new, year) %>%
      dplyr::summarise(
        dplyr::across(dplyr::everything(), \(x) mean(x, na.rm = TRUE)),
        .groups = "drop"
      )
  }

  out
}
# Create the data (makes a dataframe for each ecosystem depth contour)
control_all <- purrr::map(years, \(yy) {
  dat_yy <- readRDS(
    paste0(
      "output/model-output/", park, "/habitat/",
      name, "_predicted-habitat_", yy, ".rds"
    )
  )
  controldata_benthos(dat = dat_yy, year = yy, amp_abbrv = "ERMP") # TODO set park abbreviations
})

park_dat.shallow <- purrr::map_dfr(control_all, "shallow") %>%
  dplyr::mutate(depth_class = "Shallow (0 - 30 m)")

park_dat.meso <- purrr::map_dfr(control_all, "meso") %>%
  dplyr::mutate(depth_class = "Mesophotic (30 - 70 m)")

park_dat.rari <- purrr::map_dfr(control_all, "rari") %>%
  dplyr::mutate(depth_class = "Rariphotic (70 - 200 m)")

park_dat.control <- dplyr::bind_rows(
  park_dat.shallow,
  park_dat.meso,
  park_dat.rari
) %>%
  dplyr::mutate(
    depth_class = factor(
      depth_class,
      levels = c(
        "Shallow (0 - 30 m)",
        "Mesophotic (30 - 70 m)",
        "Rariphotic (70 - 200 m)"
      )
    )
  )

# Taxa to plot
taxa_lookup <- c(
  "macroalgae" = "Macroalgae",
  "reef"       = "Reef",
  "sand"       = "Sand",
  "inverts"    = "Sessile invertebrates"
)

controlplot_benthos <- function(data, taxa, amp_abbrv,
                                taxa_label = NULL,
                                depth_levels = c("Shallow (0 - 30 m)",
                                                 "Mesophotic (30 - 70 m)",
                                                 "Rariphotic (70 - 200 m)")) {
  mean_col <- taxa
  se_col   <- paste0(taxa, "_se")

  if (is.null(taxa_label)) {
    taxa_label <- dplyr::case_when(
      taxa == "macroalgae" ~ "Macroalgae",
      taxa == "reef"       ~ "Reef",
      taxa == "sand"       ~ "Sand",
      taxa == "inverts"    ~ "Sessile invertebrates",
      TRUE ~ stringr::str_to_title(taxa)
    )
  }

  req_cols <- c("year", "zone_new", "depth_class", mean_col, se_col)
  if (!all(req_cols %in% names(data))) {
    stop("Data is missing one or more required columns: ",
         paste(setdiff(req_cols, names(data)), collapse = ", "))
  }

  plot_dat <- data %>%
    dplyr::filter(!is.na(.data[[mean_col]])) %>%
    dplyr::mutate(
      depth_class = factor(depth_class, levels = depth_levels),
      zone_new = factor(
        zone_new,
        levels = c(
          paste(amp_abbrv, "other zones")
        )
      )
    )

  if (nrow(plot_dat) == 0) {
    message("No data available to plot for ", taxa_label)
    return(NULL)
  }

  fill_vals <- setNames(
    c("#b9e6fb"),
    c(
      paste(amp_abbrv, "other zones")
    )
  )

  shape_vals <- setNames(
    c(21, 21),
    c(
      paste(amp_abbrv, "other zones")
    )
  )

  p <- ggplot(
    data = plot_dat,
    aes(x = year, y = .data[[mean_col]], fill = zone_new, shape = zone_new)
  ) +
    geom_errorbar(
      aes(
        ymin = pmax(.data[[mean_col]] - .data[[se_col]], 0),
        ymax = .data[[mean_col]] + .data[[se_col]]
      ),
      width = 0.8,
      position = position_dodge(width = 0.6)
    ) +
    geom_point(
      size = 3,
      position = position_dodge(width = 0.6),
      stroke = 0.2,
      color = "black",
      alpha = 0.8
    ) +
    geom_vline(
      xintercept = 2023.5,
      linetype = "dashed",
      color = "black",
      linewidth = 0.5,
      alpha = 0.5
    ) +
    facet_wrap(~depth_class, ncol = 1, scales = "free_y") +
    theme_classic() +
    scale_x_continuous(breaks = c(2022, 2025)) +
    coord_cartesian(xlim = c(2021, 2026), ylim = c(0, NA)) +
    scale_fill_manual(values = fill_vals, name = "Marine Parks", drop = FALSE) +
    scale_shape_manual(values = shape_vals, name = "Marine Parks", drop = FALSE) +
    labs(x = "Year", y = "Mean predicted probability", title = taxa_label) +
    theme(
      legend.position = "right",
      strip.background = element_blank(),
      strip.text = element_text(face = "bold")
    )

  return(p)
}
for (taxa_code in names(taxa_lookup)) {

  message("Building control plot for taxon: ", taxa_lookup[[taxa_code]])

  p_taxa <- controlplot_benthos(
    data = park_dat.control,
    taxa = taxa_code,
    amp_abbrv = "ERMP", # TODO set park abbreviations
    taxa_label = taxa_lookup[[taxa_code]]
  )

  if (!is.null(p_taxa)) {

    print(p_taxa)

    out_name <- taxa_lookup[[taxa_code]] %>%
      stringr::str_to_lower() %>%
      stringr::str_replace_all("\\s+", "-")

    ggsave(
      filename = paste0(
        "plots/", park, "/habitat/", name, "_control-plot_", out_name, ".png"
      ),
      plot = p_taxa,
      height = 4,
      width = 6,
      dpi = 300,
      units = "in",
      bg = "white"
    )

    saveRDS(p_taxa,
            paste0(
              "plots/", park, "/habitat/", name,
              "_control-plot_", out_name, ".rds"
            ))
  }
}

# ---- Scatterpie data prep ----

# TODO Set the extent of the study
e <- ext(123.2, 123.8, -34.57, -33.9)

# Load the bathymetry data (GA 250m resolution)
bathy <- rast("data/south-west network/spatial/rasters/AusBathyTopo__Australia__2024_250m_MSL_cog.tif") %>%
  crop(e) %>%
  clamp(upper = 0, lower = -250, values = FALSE) %>%
  trim() %>%
  as.data.frame(xy = TRUE, na.rm = TRUE)

names(bathy)[3] <- "Depth"

metadata_bathy_derivatives <- readRDS(
  paste0("data/", park, "/tidy/", name, "_metadata-bathymetry-derivatives.rds")
) %>%
  clean_names()

benthos <- readRDS(
  paste0("data/", park, "/tidy/", name, "_benthos-count.RDS")
) %>%
  dplyr::rename(
    Macroalgae = macroalgae,
    Reef = reef,
    Sand = sand,
    "Sessile invertebrates" = sessile_invertebrates
  ) %>%
  left_join(metadata_bathy_derivatives, by = c("campaignid", "sample", "year", "status")) %>%
  arrange(desc(Sand))

hab_fills <- scale_fill_manual(
  name = "Habitat",
  limits = c("Sessile invertebrates", "Macroalgae", "Reef", "Sand"),
  values = c(
    "Sessile invertebrates" = "plum",
    "Macroalgae" = "darkgoldenrod4",
    "Reef" = "darkorange",
    "Sand" = "wheat"
  )
)

wampa_fills <- scale_fill_manual(values = c(
  # "Marine Management Area" = "#b7cfe1",
  # "Conservation Area" = "#b3a63d",
  "Sanctuary Zone" = "#bfd054",
  "General Use Zone" = "#bddde1",
  # "Recreation Area" = "#f4e952",
  "Special Purpose Zone" = "#c5bcc9"
  # "Marine Nature Reserve" = "#bfd054"
),
name = "State Marine Parks")

depth_fills <- scale_fill_manual(
  values = c("#a7cfe0", "#9acbec", "#98c4f7", "#a3bbff", "#81a1fc"),
  guide = "none"
)

site_limits <- c(123.2, 123.8, -34.57, -33.9) # TODO set limits

scatterpie_plot_single <- function(benthos_year, site_limits, pie_radius = 0.004) {
  ggplot() +
    geom_contour_filled(
      data = bathy,
      aes(x, y, z = Depth, fill = after_stat(level)),
      color = "black",
      breaks = c(-30, -70, -200, -700, -2000, -4000),
      linewidth = 0.1
    ) +
    depth_fills +
    new_scale_fill() +
    geom_sf(data = ausc, fill = "seashell2", colour = "black", linewidth = 0.1) +
    geom_sf(data = npz, fill = "#7bbc63", alpha = 2/5, colour = NA) +
    geom_sf(data = cwatr, colour = "firebrick", alpha = 4/5, linewidth = 0.3) +
    new_scale_fill() +
    geom_scatterpie(
      data = benthos_year,
      aes(x = longitude_dd, y = latitude_dd, r = pie_radius),
      cols = c(
        "Sand",
        "Sessile invertebrates",
        "Macroalgae"
      ),
      colour = NA
    ) +
    labs(x = "Longitude", y = "Latitude", fill = "Habitat") +
    hab_fills +
    coord_sf(
      xlim = c(site_limits[1], site_limits[2]),
      ylim = c(site_limits[3], site_limits[4]),
      crs = 4326
    ) +
    theme_minimal() +
    theme(
      panel.background = element_rect(fill = "#b9d1d6", colour = NA),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      legend.position = "bottom",
      legend.direction = "horizontal",
      legend.box = "horizontal"
    )
}
for (yr in years) {

  message("Year: ", yr)

  benthos_year <- benthos %>%
    dplyr::filter(as.character(year) == as.character(yr)) %>%
    dplyr::filter(
      is.finite(longitude_dd),
      is.finite(latitude_dd)
    ) %>%
    dplyr::arrange(desc(Sand))

  p_scatterpie <- scatterpie_plot_single(
    benthos_year = benthos_year,
    site_limits = site_limits,
    pie_radius = 0.005
  )

  print(p_scatterpie)

  ggsave(
    filename = paste0(
      "plots/", park, "/habitat/", name, "_scatterpie_", yr, ".png"
    ),
    plot = p_scatterpie,
    height = 6,
    width = 10,
    dpi = 300,
    bg = "white"
  )

  saveRDS(p_scatterpie,
          paste0(
            "plots/", park, "/habitat/", name,
            "_scatterpie_", yr, ".rds"
          ))
}

scatterpie_plot_multi <- function(benthos, years, site_limits, pie_radius = 0.004) {
  benthos_plot <- benthos %>%
    dplyr::filter(as.character(year) %in% as.character(years)) %>%
    dplyr::filter(
      is.finite(longitude_dd),
      is.finite(latitude_dd)
    ) %>%
    dplyr::mutate(
      year = factor(year, levels = years)
    ) %>%
    dplyr::arrange(year, desc(Sand))

  ggplot() +
    geom_contour_filled(
      data = bathy,
      aes(x, y, z = Depth, fill = after_stat(level)),
      color = "black",
      breaks = c(-30, -70, -200, -700, -2000, -4000),
      linewidth = 0.1
    ) +
    depth_fills +
    new_scale_fill() +
    geom_sf(data = ausc, fill = "seashell2", colour = "black", linewidth = 0.1) +
    geom_sf(data = npz, fill = "#7bbc63", alpha = 2/5, colour = NA) +
    geom_sf(data = cwatr, colour = "firebrick", alpha = 4/5, linewidth = 0.3) +
    new_scale_fill() +
    geom_scatterpie(
      data = benthos_plot,
      aes(x = longitude_dd, y = latitude_dd, r = pie_radius),
      cols = c(
        "Sand",
        "Sessile invertebrates",
        "Reef",
        "Macroalgae"
      ),
      colour = NA
    ) +
    labs(x = "Longitude", y = "Latitude", fill = "Habitat") +
    hab_fills +
    facet_wrap(~year, nrow = 1) +
    coord_sf(
      xlim = c(site_limits[1], site_limits[2]),
      ylim = c(site_limits[3], site_limits[4]),
      crs = 4326
    ) +
    theme_minimal() +
    theme(
      panel.background = element_rect(fill = "#b9d1d6", colour = NA),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      legend.position = "bottom",
      legend.direction = "horizontal",
      legend.box = "horizontal"
    )
}

p_scatterpie_multi <- scatterpie_plot_multi(
  benthos = benthos,
  years = years,
  site_limits = site_limits,
  pie_radius = 0.005
)

print(p_scatterpie_multi)

ggsave(
  filename = paste0(
    "plots/", park, "/habitat/", name, "_scatterpie_",
    paste(years, collapse = "-"), ".png"
  ),
  plot = p_scatterpie_multi,
  height = 6,
  width = 10,
  dpi = 300,
  bg = "white"
)

saveRDS(p_scatterpie_multi,
        paste0(
          "plots/", park, "/habitat/", name,
          "_scatterpie_",
          paste(years, collapse = "-"), ".rds"
        ))
