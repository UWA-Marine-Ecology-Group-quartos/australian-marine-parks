###
# Project: NESP 5.6 Project - South west Corner Report
# Data:    Marine Parks, bathymetry, terrestrial parks, aus outline
# Task:    Two Rocks & Geographe zone maps — network style, faceted
# Author:  Annika Leunig
# Date:    May 2026
# Outputs: 1. Two rocks plot for Nicole article

# ==============================================================================
# 1. SET UP AND LOAD DATA
# ==============================================================================
# Clear environment
rm(list = ls())

# Set study name
name <- "south-west"
park <- "network"

# Load libraries
library(tidyverse)
library(sf)
library(terra)
library(tidyterra)
library(ggnewscale)
library(cowplot)
library(metR)

# Set cropping and plot extents
e                <- ext(106.0, 145.0, -45.0, -22.0)
tworocks_limits  <- c(114.7, 116.0, -32.0, -31.3)
geographe_limits <- c(114.4, 115.9, -33.9, -33.1)

# ── Load spatial files  ───────────────────────────────────────────────────────
sf_use_s2(TRUE)
# Aus outline, terrestrial parks and coastal waters outline
aus <- st_read("data/south-west network/spatial/shapefiles/STE_2021_AUST_GDA2020.shp") %>%
  st_make_valid()

capad <- st_read("data/south-west network/spatial/shapefiles/Collaborative_Australian_Protected_Areas_Database_(CAPAD)_2022_-_Marine.shp")

terrnp <- st_read("data/south-west network/spatial/shapefiles/Collaborative_Australian_Protected_Areas_Database_(CAPAD)_2024_-_Terrestrial__.shp") %>%
  dplyr::filter(TYPE %in% c("Nature Reserve", "National Park"))

cwatr <- st_read("data/south-west network/spatial/shapefiles/amb_coastal_waters_limit.shp") %>%
  st_make_valid() %>%
  st_crop(e)

# Marine parks
marine_parks <- st_read("data/south-west network/spatial/shapefiles/south-and-western-australia_marine-parks-all.shp") %>%
  dplyr::filter(name %in% c("Abrolhos", "Abrolhos Islands", "Bremer", "Eastern Recherche",
                            "Ngari Capes", "Geographe", "South-west Corner",
                            "Great Australian Bight", "Jurien", "Murat", "Jurien Bay",
                            "Perth Canyon", "Southern Kangaroo Island", "Twilight",
                            "Two Rocks", "Western Eyre", "Western Kangaroo Island",
                            "Nuyts Archipelgo", "Thorny Passage", "Sir Joseph Banks Group",
                            "Investigator", "West coast Bays", "Southern Spencer Gulf",
                            "Upper Spencer Gulf", "Cottesloe Reef", "Rottnest", "Marmion",
                            "Shoalwater Islands", "Shark Bay"))

marine_parks_amp <- marine_parks %>%
  dplyr::filter(epbc %in% "Commonwealth")

marine_parks_TR <- marine_parks %>%
  dplyr::filter(name %in% "Two Rocks")

marine_parks_state <- marine_parks %>%
  dplyr::filter(epbc %in% "State") %>%
  dplyr::mutate(
    zone = case_when(
      zone == "Reef Observation Area"   ~ "Sanctuary Zone",
      zone == "National Park Zone"      ~ "Sanctuary Zone",
      zone == "Habitat Protection Zone" ~ "Recreational Use Zone",
      TRUE                              ~ zone
    ),
    colour = case_when(
      zone == "Other State Marine Park Zone" ~ "#f7d0dc",
      zone == "Sanctuary Zone"               ~ "#bfd4a5",
      TRUE                                   ~ colour
    )
  )

# Bathymetry data
bathy <- rast("data/south-west network/spatial/rasters/AusBathyTopo__Australia__2024_250m_MSL_cog.tif") %>%
  crop(e) %>%
  clamp(upper = 0, values = FALSE)
names(bathy) <- "Depth"

cities <- data.frame(
  city   = c("Darwin", "Brisbane", "Sydney", "Canberra", "Adelaide", "Melbourne", "Perth"),
  x      = c(130.8444, 153.0260, 151.2093, 149.1310, 138.6007, 144.9631, 115.8617),
  y      = c(-12.4637, -27.4705, -33.8688, -35.2802, -34.9285, -37.8136, -31.9514),
  hjust  = c(0,         0,         0,        0,         0,         1,         1),
  offset = c(0.7,       0.7,       0.7,      0.7,       0.7,      -0.7,      -0.01)
)
cities$lab_x <- cities$x + cities$offset

# ==============================================================================
# 2. HELPER FUNCTIONS
# ==============================================================================
filter_to_extent <- function(layer, limits) {
  box <- st_as_sfc(st_bbox(
    c(xmin = limits[1], xmax = limits[2], ymin = limits[3], ymax = limits[4]),
    crs = st_crs(4326)
  ))
  dplyr::filter(layer, st_intersects(geometry, box, sparse = FALSE)[, 1])
}

thin_breaks <- function(limits, step = 0.2) {
  b <- seq(from = floor(min(limits)   / step) * step,
           to   = ceiling(max(limits) / step) * step,
           by   = step)
  b[seq(1, length(b), by = 2)]
}

zone_levels <- c(
  "National Park Zone",
  "Habitat Protection Zone",
  "Multiple Use Zone",
  "Special Purpose Zone"
)

marine_parks_TR <- marine_parks_TR %>%
  mutate(zone = factor(zone, levels = zone_levels))

# ==============================================================================
# 3. PANEL FUNCTION
# ==============================================================================

make_zone_panel <- function(plot_limits, mp_amp, mp_state, break_step = 0.1) {

  x_breaks <- thin_breaks(plot_limits[1:2], step = break_step)
  y_breaks <- thin_breaks(abs(plot_limits[3:4]), step = break_step) * -1

  ggplot() +

    # Bathymetry filled contours
    geom_spatraster_contour_filled(data   = bathy,
                                   breaks = c(0, -30, -70, -200, -700, -2000, -4000, -6000),
                                   colour = NA, show.legend = FALSE, maxcell = 5e6) +
    scale_fill_manual(values = c("#FFFFFF", "#EFEFEF", "#DEDEDE", "#CCCCCC",
                                 "#B6B6B6", "#9E9E9E", "#808080")) +
    new_scale_fill() +

    # Bathymetry contour lines
    geom_spatraster_contour(data        = bathy,
                            breaks      = c(-30, -70, -200, -700, -2000, -4000, -6000),
                            colour      = "white", alpha = 3/5, linewidth = 0.1,
                            show.legend = FALSE, maxcell = 5e6) +

    # Landmass
    geom_sf(data = aus, fill = "seashell2", colour = "grey80", linewidth = 0.1) +

    # Australian Marine Parks (all, alpha 0.3) — carries the legend
    geom_sf(data = mp_amp, aes(fill = zone), colour = NA, alpha = 0.3) +
    scale_fill_manual(
      name   = "Australian Marine Parks",
      guide  = guide_legend(order = 1, ncol = 1,
                            title.position = "top",
                            override.aes   = list(alpha = 0.8)),
      values = with(mp_amp, setNames(colour, zone)),
      breaks = zone_levels,
      drop   = TRUE
    ) +
    new_scale_fill() +

    # Two Rocks (alpha 0.8) — drawn on top for emphasis, excluded from legend
    geom_sf(data = marine_parks_TR, aes(fill = zone), colour = NA, alpha = 0.8) +
    scale_fill_manual(
      guide  = "none",
      values = with(mp_amp, setNames(colour, zone)),
      breaks = zone_levels,
      drop   = TRUE
    ) +
    new_scale_fill() +

    # State Marine Parks (order 3)
    geom_sf(data = mp_state, aes(fill = zone), colour = NA, alpha = 0.3) +
    scale_fill_manual(
      name  = "State Marine Parks",
      guide = guide_legend(order = 3, ncol = 1,
                           title.position = "top"),
      values = with(mp_state, setNames(colour, zone)),
      breaks = c(
        "Sanctuary Zone",
        "General Use Zone",
        "Recreational Use Zone",
        "Special Purpose Zone"
      )
    ) +

    # Coastal waters limit (order 4)
    geom_sf(data  = cwatr, aes(colour = "Coastal Waters Limit"),
            linewidth = 0.1, lineend = "round") +
    scale_colour_manual(name   = NULL,
                        values = c("Coastal Waters Limit" = "firebrick"),
                        guide  = guide_legend(order = 4,
                                              override.aes = list(linewidth = 0.8))) +

    coord_sf(xlim = plot_limits[1:2], ylim = plot_limits[3:4],
             crs = 4326, expand = FALSE) +
    scale_x_continuous(breaks = x_breaks) +
    scale_y_continuous(breaks = y_breaks) +
    labs(x = NULL, y = NULL) +
    geom_point(data = cities, aes(x = x, y = y),
               shape = 9, size = 1) +
    geom_text(data = cities, aes(x = lab_x, y = y, label = city, hjust = hjust),
              size = 4.5) +
    theme_minimal() +
    theme(
      legend.key.size  = unit(0.45, "cm"),
      legend.text      = element_text(size = 8),
      legend.title     = element_text(size = 9),
      legend.position  = "left",
      legend.box       = "vertical",
      legend.direction = "vertical",
      panel.grid       = element_blank(),
      panel.background = element_rect(fill = "white", colour = NA),
      plot.background  = element_rect(fill = "white", colour = NA),
      panel.border     = element_rect(colour = "grey80", fill = NA, linewidth = 0.5),
      axis.ticks       = element_line(colour = "grey80", linewidth = 0.3),
      axis.text        = element_text(size = 8, colour = "grey40"),
      plot.margin      = margin(2, 2, 2, 2)
    )
}

# ==============================================================================
# 4. ZOOM-IN MAP FUNCTION (LEGEND ON BOTTOM)
# ==============================================================================
make_zone_plot_left_legend <- function(plot_limits,
                                       inset_xlim   = c(108, 138),
                                       inset_ylim   = c(-40, -24),
                                       break_step   = 0.2,
                                       show_inset   = TRUE,
                                       save_name    = NULL,
                                       width        = 10,
                                       height       = 8) {

  mp_amp   <- filter_to_extent(marine_parks_amp,   plot_limits)
  mp_state <- filter_to_extent(marine_parks_state, plot_limits)

  p_map <- make_zone_panel(plot_limits, mp_amp, mp_state, break_step = break_step)

  legend_single <- cowplot::get_legend(p_map + theme(
    legend.position  = "bottom",
    legend.box       = "horizontal",
    legend.direction = "horizontal",
    legend.text      = element_text(size = 9),
    legend.title     = element_text(size = 11),
    legend.key.size  = unit(0.5, "cm"),
    legend.spacing.y = unit(0.2, "cm")
  ))

  if (show_inset) {

    p_inset_single <- ggplot(data = aus) +
      geom_sf(fill = "seashell1", colour = "grey90", linewidth = 0.05, alpha = 4/5) +
      geom_sf(data = capad, alpha = 5/6, colour = "grey85", linewidth = 0.02) +
      annotate("rect",
               xmin = plot_limits[1], xmax = plot_limits[2],
               ymin = plot_limits[3], ymax = plot_limits[4],
               colour = "grey25", fill = "white", alpha = 1/5, linewidth = 0.3) +
      coord_sf(xlim = inset_xlim, ylim = inset_ylim) +
      theme_bw() +
      theme(axis.text        = element_blank(),
            axis.ticks       = element_blank(),
            panel.grid.major = element_blank(),
            panel.border     = element_rect(colour = "grey70"))

    bottom_row_single <- cowplot::plot_grid(
      legend_single,
      p_inset_single,
      nrow       = 1,
      rel_widths = c(1, 0.35)
    )

  } else {

    bottom_row_single <- cowplot::plot_grid(
      legend_single,
      nrow = 1
    )

  }

  p_map_nl <- p_map + theme(legend.position = "none",
                            plot.margin    = margin(0, 0, 0, 0))

  fig <- cowplot::plot_grid(
    p_map_nl,
    bottom_row_single,
    ncol        = 1,
    rel_heights = c(1, 0.3)
  ) +
    theme(plot.background = element_rect(fill = "white", colour = NA),
          plot.margin     = margin(5, 5, 5, 5))

  print(fig)

  if (!is.null(save_name)) {
    dir.create(paste0("plots/", park, "/spatial/MPA_zoom-ins/"), recursive = TRUE, showWarnings = FALSE)
    ggsave(paste(paste0("plots/", park, "/spatial/MPA_zoom-ins/", name), paste0(save_name, ".png"), sep = "-"),
           plot   = fig,
           dpi    = 600,
           width  = width,
           height = height,
           bg     = "white")
  }

  return(invisible(fig))
}

# ==============================================================================
# 5. FIGURES 1-11: INDIVIDUAL PARK ZOOM-INS (assemble and save)
# ==============================================================================

# ── Two Rocks ─────────────────────────────────────────────────────────────────
make_zone_plot_left_legend(
  plot_limits = c(114.7, 116.0, -32.0, -31.3),
  inset_xlim  = c(108, 138),
  inset_ylim  = c(-40, -24),
  break_step  = 0.2,
  show_inset  = TRUE,
  save_name   = "tworocks-MPs_nicole",
  width       = 9,
  height      = 7
)
