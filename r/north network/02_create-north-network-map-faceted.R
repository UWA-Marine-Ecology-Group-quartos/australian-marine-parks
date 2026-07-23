###
# Project: NESP 5.6 Project - South west Corner Report
# Data:    Marine Parks, bathymetry, terrestrial parks, aus outline
# Task:    Two Rocks & Geographe zone maps — network style, faceted
# Author:  Annika Leunig
# Date:    May 2026
# Outputs: 1. Two Rocks & Geographe faceted zone map
#          2. Individual park zoom-in zone maps (Abrolhos, Jurien Bay, Two Rocks,
#             Rottnest Island Canyon, Geographe, Bremer Bay, SWC east, Eastern
#             Recherche, Great Australian Bight, Murat & Western Eyre,
#             Kangaroo Island)
###

# Table of contents
#     1.  Set up and load data
#     2.  Helper functions
#     3.  Panel function
#     4.  Build Two Rocks & Geographe panels
#     5.  FIGURE 1: TWO ROCKS & GEOGRAPHE FACETED MAP (assemble and save)
#     6.  Zoom-in map function (legend on left)
#     7.  FIGURES 2-12: Individual park zoom-ins (assemble and save)


# ==============================================================================
# 1. SET UP AND LOAD DATA
# ==============================================================================
# Clear environment
rm(list = ls())

# Set study name
name <- "north"
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
e                <- ext(120.0, 145.0, -20.0, -8.0)
# tworocks_limits  <- c(114.7, 116.0, -32.0, -31.3)
# geographe_limits <- c(114.4, 115.9, -33.9, -33.1)

# ── Load spatial files  ───────────────────────────────────────────────────────
sf_use_s2(TRUE)
# Aus outline, terrestrial parks and coastal waters outline
aus <- st_read("data/north network/spatial/shapefiles/STE_2021_AUST_GDA2020.shp") %>%
  st_make_valid()

# CAPAD Marine 2024 — used for the inset overview maps AND as the source of
# Commonwealth zone RES_NUMBER labels (see below)
capad <- st_read("data/north network/spatial/shapefiles/Collaborative_Australian_Protected_Areas_Database_(CAPAD)_2024_-_Marine.shp")

# Commonwealth AMP zone labels: last 5 characters of RES_NUMBER (e.g. "npz03"),
# plotted at each zone's CAPAD-supplied LONGITUDE/LATITUDE point
capad_amp_labels <- capad %>%
  st_drop_geometry() %>%
  dplyr::filter(EPBC == "Commonwealth",
                TYPE == "Australian Marine Park",
                RES_NUMBER != "") %>%
  dplyr::mutate(label = stringr::str_sub(RES_NUMBER, -5, -1)) %>%
  sf::st_as_sf(coords = c("LONGITUDE", "LATITUDE"), crs = 4326, remove = FALSE)

terrnp <- st_read("data/north network/spatial/shapefiles/Collaborative_Australian_Protected_Areas_Database_(CAPAD)_2024_-_Terrestrial__.shp") %>%
  dplyr::filter(TYPE %in% c("Nature Reserve", "National Park"))

cwatr <- st_read("data/north network/spatial/shapefiles/amb_coastal_waters_limit.shp") %>%
  st_make_valid() %>%
  st_crop(e)

# Marine parks
marine_parks <- st_read("data/north network/spatial/shapefiles/north-network-australia_marine-parks-all.shp") %>%
  dplyr::filter(name %in% c("Arafura", "Arnhem", "Gulf of Carpentaria", "Joseph Bonaparte Gulf",
                            "Limmen", "Oceanic Shoals", "Wessel", "West Cape York","North Kimberley",
                            "Garig Gunak Barlu", "Limmen Bight", "Eight Mile Creek", "Morning Inlet",
                            "Staaten-Gilbert", "Nassau River", "Pine River Bay",
                            "Dhimurru", "Thuwathu/Walalu", "Anindilyakwa", "Djelk", #IPAs
                            "Crocodile Islands Maringa"))

plot(marine_parks)

marine_parks_amp <- marine_parks %>%
  dplyr::filter(epbc %in% "Commonwealth")


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
bathy <- rast("data/north network/spatial/rasters/Australian_Bathymetry_and_Topography_2023_250m_MSL_cog.tif") %>%
  crop(e) %>%
  clamp(upper = 0, values = FALSE)
names(bathy) <- "Depth"

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

# to manually set the tick marks for the plots
thin_breaks <- function(limits, step = 0.2) {
  b <- seq(from = floor(min(limits)   / step) * step,
           to   = ceiling(max(limits) / step) * step,
           by   = step)
  b[seq(1, length(b), by = 2)]
}

# ==============================================================================
# 3. PANEL FUNCTION
# ==============================================================================

make_zone_panel <- function(plot_limits, mp_amp, mp_state, label_data = NULL, break_step = 0.1) {

  x_breaks <- thin_breaks(plot_limits[1:2], step = break_step)
  y_breaks <- thin_breaks(abs(plot_limits[3:4]), step = break_step) * -1

  # Coastal waters limit dummy data for legend
  cwatr_legend <- data.frame(x = 1, y = 1, label = "Coastal Waters Limit")

  if (is.null(label_data)) {
    label_data <- capad_amp_labels[0, ]
  }

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

    # Australian Marine Parks
    geom_sf(data = mp_amp, aes(fill = zone), colour = NA, alpha = 0.8) +
    scale_fill_manual(name   = "Australian Marine Parks",
                      guide  = guide_legend(order = 1, ncol = 1,
                                            title.position = "top"),
                      values = with(mp_amp, setNames(colour, zone)),
                      breaks = c("National Park Zone", "Habitat Protection Zone",
                                 "Multiple Use Zone", "Special Purpose Zone")) +
    new_scale_fill() +

    # Commonwealth AMP zone labels (last 5 characters of CAPAD RES_NUMBER,
    # e.g. "npz03"), placed at each zone's CAPAD reference point
    geom_sf_text(data          = label_data,
                 aes(label     = label),
                 size          = 2.2,
                 colour        = "grey15",
                 fontface      = "bold",
                 check_overlap = TRUE) +

    # Terrestrial parks
    geom_sf(data = terrnp, aes(fill = TYPE), colour = NA, alpha = 0.8) +
    scale_fill_manual(name   = "Terrestrial Parks",
                      guide  = guide_legend(order = 2, ncol = 1,
                                            title.position = "top"),
                      values = c("National Park"  = "#c4cea6",
                                 "Nature Reserve" = "#e4d0bb")) +
    new_scale_fill() +

    # State Marine Parks
    geom_sf(data = mp_state, aes(fill = zone), colour = NA, alpha = 0.6) +
    scale_fill_manual(name   = "State Marine Parks",
                      guide  = guide_legend(order = 2, ncol = 1,
                                            title.position = "top"),
                      values = with(mp_state, setNames(colour, zone)),
                      breaks = c("Sanctuary Zone", "General Use Zone",
                                 "Recreational Use Zone", "Special Purpose Zone",
                                 "Other State Marine Park Zone")) +

    # Coastal waters limit — mapped to colour for legend entry
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
# 4. BUILD TWO ROCKS & GEOGRAPHE PANELS
# ==============================================================================
# Call functions
# tr_amp   <- filter_to_extent(marine_parks_amp,   tworocks_limits)
# tr_state <- filter_to_extent(marine_parks_state, tworocks_limits)
# tr_labels <- filter_to_extent(capad_amp_labels,  tworocks_limits)
#
# geo_amp   <- filter_to_extent(marine_parks_amp,   geographe_limits)
# geo_state <- filter_to_extent(marine_parks_state, geographe_limits)
# geo_labels <- filter_to_extent(capad_amp_labels,  geographe_limits)
#
# p_tr  <- make_zone_panel(tworocks_limits,  tr_amp,  tr_state,  label_data = tr_labels,  break_step = 0.1)
# p_geo <- make_zone_panel(geographe_limits, geo_amp, geo_state, label_data = geo_labels, break_step = 0.1)

# Build the legend
# legend <- cowplot::get_legend(p_tr + theme(
#   legend.position  = "left",
#   legend.box       = "vertical",
#   legend.direction = "vertical",
#   legend.text      = element_text(size = 9),
#   legend.title     = element_text(size = 11),
#   legend.key.size  = unit(0.5, "cm"),
#   legend.spacing.y = unit(0.2, "cm")
# ))
#
# # Build the inset map
# p_inset <- ggplot(data = aus) +
#   geom_sf(fill = "seashell1", colour = "grey90", linewidth = 0.05, alpha = 4/5) +
#   geom_sf(data = capad, alpha = 5/6, colour = "grey85", linewidth = 0.02) +
#   annotate("rect",
#            xmin = tworocks_limits[1],  xmax = tworocks_limits[2],
#            ymin = tworocks_limits[3],  ymax = tworocks_limits[4],
#            colour = "grey25", fill = "white", alpha = 1/5, linewidth = 0.3) +
#   annotate("rect",
#            xmin = geographe_limits[1], xmax = geographe_limits[2],
#            ymin = geographe_limits[3], ymax = geographe_limits[4],
#            colour = "grey25", fill = "white", alpha = 1/5, linewidth = 0.3) +
#   annotate("text",
#            x     = mean(tworocks_limits[1:2]),
#            y     = tworocks_limits[4],
#            label = "Two Rocks",
#            size  = 2.5, colour = "grey20", hjust = 0.5, vjust = -0.5) +
#   annotate("text",
#            x     = mean(geographe_limits[1:2]),
#            y     = geographe_limits[3],
#            label = "Geographe",
#            size  = 2.5, colour = "grey20", hjust = 0.5, vjust = 1.5) +
#   coord_sf(xlim = c(112, 122), ylim = c(-36, -28)) +
#   theme_bw() +
#   theme(axis.text        = element_blank(),
#         axis.ticks       = element_blank(),
#         panel.grid.major = element_blank(),
#         panel.border     = element_rect(colour = "grey70"))

# # ==============================================================================
# # 5. FIGURE 1: TWO ROCKS & GEOGRAPHE FACETED MAP (assemble and save)
# # ==============================================================================
# # ── Assemble ──────────────────────────────────────────────────────────────────
# label_tr  <- ggdraw() + draw_label("Two Rocks",  size = 14, angle = 90)
# label_geo <- ggdraw() + draw_label("Geographe",  size = 14, angle = 90)
#
# p_tr_nl  <- p_tr  + theme(legend.position = "none", plot.margin = margin(0, 0, 0, 0))
# p_geo_nl <- p_geo + theme(legend.position = "none", plot.margin = margin(0, 0, 0, 0))
#
# row_tr <- cowplot::plot_grid(
#   label_tr, p_tr_nl,
#   nrow = 1, rel_widths = c(0.06, 1)
# )
#
# row_geo <- cowplot::plot_grid(
#   label_geo, p_geo_nl,
#   nrow = 1, rel_widths = c(0.06, 1)
# )
#
# maps_grid <- cowplot::plot_grid(
#   row_tr,
#   row_geo,
#   ncol        = 1,
#   rel_heights = c(1, 1)
# )
#
# left_col <- cowplot::plot_grid(
#   legend,
#   NULL,
#   p_inset,
#   ncol        = 1,
#   rel_heights = c(0.45, 0.15, 0.45)
# )
#
# figure <- cowplot::plot_grid(
#   left_col,
#   maps_grid,
#   nrow       = 1,
#   rel_widths = c(0.32, 1)
# ) +
#   theme(plot.background = element_rect(fill = "white", colour = NA),
#         plot.margin     = margin(5, 5, 5, 5))
#
# # ── Save ──────────────────────────────────────────────────────────────────────
# ggsave(paste(paste0("plots/", park, "/spatial/", name),
#              "tworocks-geographe-MPs.png", sep = "-"),
#        plot   = figure,
#        dpi    = 600,
#        width  = 9,
#        height = 9,
#        bg     = "white")
#

# ==============================================================================
# 6. ZOOM-IN MAP FUNCTION (LEGEND ON LEFT)
# ==============================================================================
# Function
make_zone_plot_left_legend <- function(plot_limits,
                                       inset_xlim   = c(108, 138),
                                       inset_ylim   = c(-40, -24),
                                       break_step   = 0.2,
                                       show_inset   = TRUE,
                                       save_name    = NULL,
                                       width        = 10,
                                       height       = 6) {

  mp_amp    <- filter_to_extent(marine_parks_amp,   plot_limits)
  mp_state  <- filter_to_extent(marine_parks_state, plot_limits)
  mp_labels <- filter_to_extent(capad_amp_labels,   plot_limits)

  p_map <- make_zone_panel(plot_limits, mp_amp, mp_state, label_data = mp_labels, break_step = break_step)

  # Legend
  legend_single <- cowplot::get_legend(p_map + theme(
    legend.position  = "left",
    legend.box       = "vertical",
    legend.direction = "vertical",
    legend.text      = element_text(size = 9),
    legend.title     = element_text(size = 11),
    legend.key.size  = unit(0.5, "cm"),
    legend.spacing.y = unit(0.2, "cm")
  ))

  # Inset
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

    left_col_single <- cowplot::plot_grid(
      legend_single,
      NULL,
      p_inset_single,
      ncol        = 1,
      rel_heights = c(1, 0.1, 0.45)
    )

  } else {

    left_col_single <- cowplot::plot_grid(
      legend_single,
      ncol = 1
    )

  }

  # Assemble full figure
  p_map_nl <- p_map + theme(legend.position = "none",
                            plot.margin    = margin(0, 0, 0, 15))

  fig <- cowplot::plot_grid(
    left_col_single,
    p_map_nl,
    nrow       = 1,
    rel_widths = c(0.32, 1)
  ) +
    theme(plot.background = element_rect(fill = "white", colour = NA),
          plot.margin     = margin(5, 5, 5, 5))


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
# 7. FIGURES 2-12: INDIVIDUAL PARK ZOOM-INS (assemble and save)
# ==============================================================================
# ── Arafura ───────────────────────────────────────────────────────────────────

make_zone_plot_left_legend(
  plot_limits = c(131.5, 135.5, -12.5, -8.5),
  inset_xlim  = c(126.0, 142.5),
  inset_ylim  = c(-18, -8.5),
  break_step  = 0.5,
  show_inset = TRUE,
  save_name   = "arafura-MPs",
  width       = 7.75,
  height      = 4.75
)

# ── Arnhem ────────────────────────────────────────────────────────────────────
make_zone_plot_left_legend(
  plot_limits = c(133.0, 134.8, -12.5, -10.5),
  inset_xlim  = c(126.0, 142.5),
  inset_ylim  = c(-18, -8.5),
  break_step  = 0.2,
  show_inset = TRUE,
  save_name   = "arnhem-MPs",
  width       = 7.5,
  height      = 3.5
)

# ── Gulf of Carpentaria ───────────────────────────────────────────────────────
make_zone_plot_left_legend(
  plot_limits = c(138.0, 142.6, -17.5, -13.8),
  inset_xlim  = c(126.0, 142.5),
  inset_ylim  = c(-18, -8.5),
  break_step  = 0.2,
  show_inset = TRUE,
  save_name   = "gulf-of-carpentaria-MPs",
  width       = 8.5,
  height      = 5.5
)

# ── Joseph Bonaparte Gulf ─────────────────────────────────────────────────────
make_zone_plot_left_legend(
  plot_limits = c(126.5, 130.5, -15.5, -13),
  inset_xlim  = c(126.0, 142.5),
  inset_ylim  = c(-18, -8.5),
  break_step  = 0.2,
  show_inset = TRUE,
  save_name   = "joseph-bonaparte-gulf-MPs",
  width       = 8,
  height      = 4.5
)

# ── Limmen ────────────────────────────────────────────────────────────────────
make_zone_plot_left_legend(
  plot_limits = c(135.0, 137.1, -16.0, -14),
  inset_xlim  = c(126.0, 142.5),
  inset_ylim  = c(-18, -8.5),
  break_step  = 0.1,
  show_inset = TRUE,
  save_name   = "limmen-MPs",
  width       = 7.5,
  height      = 5.5
)

# ── Oceanic Shoals ────────────────────────────────────────────────────────────
make_zone_plot_left_legend(
  plot_limits = c(125.5, 132, -13.6, -9),
  inset_xlim  = c(126.0, 142.5),
  inset_ylim  = c(-18, -8.5),
  break_step  = 0.8,
  show_inset = TRUE,
  save_name   = "oceanic-shoals-MPs",
  width       = 9,
  height      = 4.0
)

# ── West Cape York ────────────────────────────────────────────────────────────
make_zone_plot_left_legend(
  plot_limits = c(139.5, 142.7, -12.5, -9.5),
  inset_xlim  = c(126.0, 142.5),
  inset_ylim  = c(-18, -8.5),
  break_step  = 0.2,
  show_inset = TRUE,
  save_name   = "west-cape-york-MPs",
  width       = 8,
  height      = 6
)

# ── Wessel ────────────────────────────────────────────────────────────────────
make_zone_plot_left_legend(
  plot_limits = c(136.0, 137.8, -12.5, -10.5),
  inset_xlim  = c(126.0, 142.5),
  inset_ylim  = c(-18, -8.5),
  break_step  = 0.2,
  show_inset = TRUE,
  save_name   = "wessel-MPs",
  width       = 8,
  height      = 5
)
marine_parks %>%
  filter(name == "North Kimberley") %>%
  st_bbox()
# ==============================================================================
# End of script
# ==============================================================================
