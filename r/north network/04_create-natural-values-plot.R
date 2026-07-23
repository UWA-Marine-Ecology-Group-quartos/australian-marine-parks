###
# Project: NESP 5.6 Project - South west Corner Report
# Data:    Natural values ecosystems (NESP MERI), Commonwealth marine parks,
#          terrestrial parks and aus outline
# Task:    Creating natural values (benthic ecosystem) map — north network
# Author:  Annika Leunig
# Date:    July 2026
# Outputs: 1. North network natural values map (original source colours,
#             predicted reef layer removed, Commonwealth marine parks only)
###

# Table of contents
#     1.  Set up and load data
#     2.  CRS, colours and other housekeeping
#     3.  Network-scale map function
#     4.  FIGURE 1: North network natural values map
#     5.  Individual park functions — hillshade past 200m
#     6.  FIGURE 2: North Kimberley (worked example)


# ==============================================================================
# 1. LOAD DATA AND SETUP
# ==============================================================================

# Clear environment
rm(list = ls())

# Set study name (folder structure)
name <- "north"
park <- "network"

# Load libraries
library(sf)
library(terra)
library(tidyverse)
library(tidyterra)
library(ggnewscale)
library(cowplot)
library(ggplot2)
library(dplyr)

# Set cropping extent (matches north network KEF script)
e <- ext(120, 148, -21, -8)

# Aus outline
aus <- st_read("data/north network/spatial/shapefiles/STE_2021_AUST_GDA2020.shp") %>%
  st_make_valid()

# Commonwealth marine parks only (same filter as network_map() KEF script)
marine_parks <- st_read("data/north network/spatial/shapefiles/north-network-australia_marine-parks-all.shp") %>%
  dplyr::filter(name %in% c("Arafura", "Arnhem", "Gulf of Carpenteria", "Joseph Bonaparte Gulf",
                            "Limmen", "Oceanic Shoals", "Wessel", "West Cape York","North Kimberley",
                            "Garig Gunak Barlu", "Limmen Bight", "Eight Mile Creek", "Morning Inlet",
                            "Staaten-Gilbert", "Nassau River", "Pine River Bay",
                            "Dhimurru", "Thuwathu/Walalu", "Anindilyakwa", "Djelk", #IPAs
                            "Crocodile Islands Maringa")) %>%
  glimpse()

marine_parks_amp <- marine_parks %>%
  dplyr::filter(epbc %in% "Commonwealth")

# Terrestrial parks for mapping
terrnp <- st_read("data/south-west network/spatial/shapefiles/Collaborative_Australian_Protected_Areas_Database_(CAPAD)_2024_-_Terrestrial__.shp") %>%
  dplyr::filter(TYPE %in% c("Nature Reserve", "National Park"))

# Natural values ecosystem — NESP MERI raster (has its own embedded colour table)
naturalvalues <- rast("data/north network/spatial/rasters/NESP_MERI_Natural_Values_Ecosystems.tif") %>%
  crop(e)

# Bathymetry — used for hillshade background on individual park figures only
bathy <- rast("data/north network/spatial/rasters/AusBathyTopo__Australia__2024_250m_MSL_cog.tif") %>%
  crop(e)


# ==============================================================================
# 2. CRS, COLOURS, AND OTHER HOUSEKEEPING
# ==============================================================================

target_crs <- "EPSG:4326"

if (!same.crs(naturalvalues, target_crs)) naturalvalues <- project(naturalvalues, target_crs, method = "near")
if (!same.crs(bathy, target_crs))         bathy         <- project(bathy,         target_crs, method = "bilinear")
if (st_crs(aus)          != st_crs(4326)) aus          <- st_transform(aus,          4326)
if (st_crs(terrnp)       != st_crs(4326)) terrnp       <- st_transform(terrnp,       4326)
if (st_crs(marine_parks_amp) != st_crs(4326)) marine_parks_amp <- st_transform(marine_parks_amp, 4326)

# Clip natural values to the 200m shelf — past 200m the hillshade base is
# shown alone with no ecosystem colour on top (same pattern as the old
# south-west script's mask_250, but at the 200m break requested here)
mask_200 <- ifel(bathy >= -200, 1, NA)
mask_200_resamp <- resample(mask_200, naturalvalues, method = "near")
naturalvalues_clipped <- mask(naturalvalues, mask_200_resamp)

# Class names (1:18, same lookup as the south-west scripts)
nv_lookup <- c(
  "1"  = "Shelf unvegetated sediments",
  "2"  = "Upper slope sediments",
  "3"  = "Mid slope sediments",
  "4"  = "Lower slope reef and sediments",
  "5"  = "Abyssal reef and sediments",
  "6"  = "Seamount sediments",
  "7"  = "Shelf incising canyons",
  "8"  = "Oceanic shallow coral reefs",
  "9"  = "Shelf vegetated sediments",
  "10" = "Shallow coral reefs",
  "11" = "Shallow rocky reefs",
  "12" = "Mesophotic coral reefs",
  "13" = "Mesophotic rocky reefs",
  "14" = "Oceanic mesophotic coral reefs",
  "15" = "Rariphotic shelf reefs",
  "16" = "Upper slope reefs",
  "17" = "Mid slope reefs",
  "18" = "Seamount reefs"
)

# Original colours — pulled directly from the raster's own embedded colour
# table (NESP_MERI_Natural_Values_Ecosystems.tif), NOT the approximate
# R-named colours (hab_colours) used in the south-west scripts.
hab_colours_original <- c(
  "Shelf unvegetated sediments"      = "#A2D9FF",
  "Upper slope sediments"            = "#5171E2",
  "Mid slope sediments"              = "#B13DFF",
  "Lower slope reef and sediments"   = "#4098C4",
  "Abyssal reef and sediments"       = "#0012D9",
  "Seamount sediments"               = "#42ECD0",
  "Shelf incising canyons"           = "#848484",
  "Oceanic shallow coral reefs"      = "#EEA6F1",
  "Shelf vegetated sediments"        = "#29D000",
  "Shallow coral reefs"              = "#A17456",
  "Shallow rocky reefs"              = "#C15E7D",
  "Mesophotic coral reefs"           = "#E0A800",
  "Mesophotic rocky reefs"           = "#F427E3",
  "Oceanic mesophotic coral reefs"   = "#E7689F",
  "Rariphotic shelf reefs"           = "#DF0003",
  "Upper slope reefs"                = "#FFE400",
  "Mid slope reefs"                  = "#B1C706",
  "Seamount reefs"                   = "#9EED7C"
)


# ==============================================================================
# 3. NETWORK-SCALE MAP FUNCTION — natural values only, no predicted reef
# ==============================================================================

naturalvalues_map_north <- function(plot_limits,
                                    ocean_colour = "#2b3a4a",
                                    show_legend  = TRUE,
                                    title        = NULL,
                                    break_step   = 2.0) {

  require(tidyverse); require(terra); require(sf); require(ggnewscale); require(cowplot)

  ext_plot <- ext(plot_limits[1], plot_limits[2], plot_limits[3], plot_limits[4])
  nv_crop  <- crop(naturalvalues, ext_plot)

  nv_df <- as.data.frame(nv_crop, xy = TRUE, na.rm = TRUE)
  colnames(nv_df)[3] <- "value"
  nv_df$classname <- nv_lookup[as.character(nv_df$value)]
  nv_df <- dplyr::filter(nv_df, !is.na(classname))

  present_classes <- unique(nv_df$classname)
  present_colours <- hab_colours_original[names(hab_colours_original) %in% present_classes]

  # Keep legend in the same class order as nv_lookup
  level_order <- names(nv_lookup)[names(nv_lookup) %in% as.character(nv_df$value)]
  level_order <- unname(nv_lookup[level_order])
  nv_df$classname <- factor(nv_df$classname, levels = level_order)

  x_breaks <- seq(floor(plot_limits[1] / break_step) * break_step,
                  ceiling(plot_limits[2] / break_step) * break_step,
                  by = break_step)
  y_breaks <- seq(floor(plot_limits[3] / break_step) * break_step,
                  ceiling(plot_limits[4] / break_step) * break_step,
                  by = break_step)

  # ── Main map — legends suppressed here, built separately below ──────────
  p_map <- ggplot() +

    # Natural values ecosystem layer — original source colours
    geom_tile(data = nv_df, aes(x = x, y = y, fill = classname)) +
    scale_fill_manual(values = present_colours[level_order], breaks = level_order, guide = "none") +

    # Land
    new_scale_fill() +
    geom_sf(data = aus, fill = "seashell2", colour = "grey80", linewidth = 0.1) +

    # Terrestrial parks
    new_scale_fill() +
    geom_sf(data = terrnp, aes(fill = TYPE), colour = NA) +
    scale_fill_manual(
      values = c("National Park"  = "#c4cea6",
                 "Nature Reserve" = "#e4d0bb"),
      guide  = "none"
    ) +

    # Commonwealth marine park boundaries
    geom_sf(data = marine_parks_amp, fill = alpha("grey70", 0.3),
            colour = alpha("white", 0.7), linewidth = 0.35) +

    coord_sf(xlim = plot_limits[1:2], ylim = plot_limits[3:4], crs = 4326, expand = FALSE) +
    scale_x_continuous(breaks = x_breaks) +
    scale_y_continuous(breaks = y_breaks) +
    labs(x = NULL, y = NULL, title = title) +
    theme_minimal() +
    theme(
      legend.position  = "none",
      panel.grid       = element_blank(),
      panel.background = element_rect(fill = ocean_colour, colour = NA),
      plot.background  = element_rect(fill = "white",      colour = NA),
      axis.text        = element_text(size = 10, colour = "grey40"),
      axis.ticks       = element_line(colour = "grey60"),
      plot.title       = if (!is.null(title)) element_text(face = "bold", size = 14, hjust = 0)
      else                 element_blank(),
      plot.margin      = margin(t = 0, r = 0, b = 0, l = 0)
    )

  if (!show_legend) return(p_map)

  # ── Benthic ecosystem legend — ncol = 3, same as set in this script ──────
  dummy_df <- data.frame(x = 1, y = 1, classname = factor(level_order, levels = level_order))

  legend_benthic <- ggplot(dummy_df, aes(x = x, y = y, fill = classname)) +
    geom_tile() +
    scale_fill_manual(
      name   = "Benthic ecosystem",
      values = present_colours[level_order],
      breaks = level_order,
      guide  = guide_legend(ncol = 3, direction = "horizontal",
                            title.position = "top", title.hjust = 0, byrow = TRUE)
    ) +
    theme_void() +
    theme(
      legend.key.size  = unit(0.45, "cm"),
      legend.text      = element_text(size = 10),
      legend.title     = element_text(size = 10, face = "bold"),
      legend.position  = "bottom"
    )

  # ── Terrestrial parks legend — ncol = 1, same as set in this script ──────
  tp_df <- data.frame(x = 1, y = 1,
                      tp = factor(c("National Park", "Nature Reserve"),
                                  levels = c("National Park", "Nature Reserve")))

  legend_tp <- ggplot(tp_df, aes(x = x, y = y, fill = tp)) +
    geom_tile() +
    scale_fill_manual(
      name   = "Terrestrial Parks",
      values = c("National Park" = "#c4cea6", "Nature Reserve" = "#e4d0bb"),
      guide  = guide_legend(ncol = 1, title.position = "top")
    ) +
    theme_void() +
    theme(
      legend.key.size  = unit(0.45, "cm"),
      legend.text      = element_text(size = 10),
      legend.title     = element_text(size = 10, face = "bold"),
      legend.position  = "top"
    )

  # Benthic ecosystem legend sits next to Terrestrial Parks legend, not stacked
  legend_row <- cowplot::plot_grid(
    cowplot::get_legend(legend_benthic),
    cowplot::get_legend(legend_tp),
    nrow       = 1,
    rel_widths = c(4, 1.1)
  )

  p <- cowplot::plot_grid(
    p_map,
    legend_row,
    ncol        = 1,
    rel_heights = c(1, 0.25)
  ) +
    theme(plot.background = element_rect(fill = "white", colour = NA))

  return(p)
}


# ==============================================================================
# 4. FIGURE 1: North network natural values map
# ==============================================================================

network_limits <- c(126, 143, -18, -9)

figure_north_nv <- naturalvalues_map_north(
  plot_limits  = network_limits,
  show_legend  = TRUE,
  break_step   = 2.0
)

ggsave(paste(paste0("plots/", park, "/spatial/benthic_habitat/", name),
             "network-natural-values.png", sep = "-"),
       plot   = figure_north_nv,
       dpi    = 600,
       width  = 11,
       height = 7.5,
       bg     = "white")


# ==============================================================================
# 5. INDIVIDUAL PARK FUNCTIONS — hillshade base, NV coloured to 200m only
# ==============================================================================
# ⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀
# ⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⣀⣤⣶⣾⣿⣿⣿⣿⠿⣿⣿⣿⣿⡿⢿⣿⣿⣿⣿⣶⣶⣄⣀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀
# ⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⣠⣴⣶⠿⠟⠉⠁⠀⠉⠉⠉⠀⠀⠛⠋⣉⣥⣶⣿⣿⣿⣿⣿⡿⣿⣿⣿⣿⣶⣄⡀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀
# ⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢀⣠⣴⣿⠟⠋⠁⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⣰⣾⣿⡿⠟⢋⣭⣿⣿⣶⣶⣿⣿⣿⣿⣿⣿⣿⣆⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀
# ⠀⠀⠀⠀⠀⠀⠀⠀⠀⢀⣴⣿⡿⠋⠁⣠⣤⠀⠀⢀⣴⠇⠀⠀⠀⢀⣤⣾⡿⠟⠉⠀⠾⠿⢿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣷⣦⣤⡀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀
# ⠀⠀⠀⠀⠀⠀⠀⢀⣴⣿⣿⣯⣶⣾⣿⣟⣡⣤⣾⢟⡏⠀⠀⠀⠉⠉⠉⠁⠀⠀⣶⣶⣶⣶⣶⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⡝⢷⣦⠀⠀⠀⠀⠀⠀⠀⠀
# ⠀⠀⠀⠀⠀⠀⢠⣾⣿⣿⣿⣿⣿⣿⣿⣿⡿⣯⣶⡟⠁⠀⠀⠀⠀⠀⠀⠀⠀⠀⠙⢻⣟⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⡟⠂⣹⣿⣄⠀⠀⠀⠀⠀⠀
# ⠀⠀⠀⠀⠀⢠⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⠀⢀⡀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠈⢉⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⡟⢷⣟⣿⣿⣆⠀⠀⠀⠀⠀
# ⠀⠀⠀⠀⢠⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣧⡀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠈⢴⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⢦⣿⣿⣿⣿⣧⠀⠀⠀⠀
# ⠀⠀⠀⢠⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣧⣤⣀⡀⠀⠀⠀⠀⢀⣀⣤⣼⣿⣿⣿⠿⠟⠛⠛⠉⠉⠋⠀⠀⠙⢻⣿⡝⢢⣿⣿⣿⣿⣿⠀⠀⠀⠀
# ⠀⠀⢀⣿⣿⣿⣿⣿⣿⣿⠧⠿⠿⠟⠛⣿⠿⠿⠿⠿⠿⠿⢿⣿⣿⣿⣿⣿⣿⡿⠿⠿⠛⠛⠛⠉⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⣽⣿⣾⣿⣿⣿⣿⣿⡀⠀⠀⠀
# ⠀⠀⣾⣿⣿⣿⣿⣿⣿⠃⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢹⣿⣿⣿⣿⣿⣿⣿⣷⠀⠀⠀
# ⠀⠀⣿⣿⣿⣿⣿⣿⡇⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⣀⣀⡠⠤⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠸⣿⣿⣿⣿⣿⣿⣿⣿⠀⠀⠀
# ⠀⠀⣿⣿⣿⣿⣿⣿⠃⠀⠀⠀⠀⠚⠛⠒⠒⠒⠂⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⣀⣠⣤⠤⠶⠒⠒⠒⠢⢤⣄⠀⠀⠸⣿⣿⣿⣿⣿⣿⣿⠀⠀⠀
# ⠀⠀⣿⣿⣿⣿⣿⡇⠀⠀⠀⠠⠖⠒⠶⠶⠦⢤⣀⣀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢀⡤⠴⠖⠒⠛⠉⠁⢀⣀⡤⠶⠒⢚⢛⡒⠲⢮⡓⠀⢀⣸⣿⣿⣿⣿⣿⣿⠀⠀⠀
# ⠀⠀⣿⣿⣿⣿⣿⡇⠀⠀⠀⢀⣰⠶⠶⠶⠦⢤⣀⠉⠙⠲⠶⢤⣀⣀⡀⠀⠀⠀⠀⠀⠀⣀⣀⡤⠶⠞⠉⢁⡤⠶⠋⠉⠉⠉⠲⢄⠙⠂⠀⢹⣿⣿⣿⣿⣿⡏⠀⠀⠀
# ⠀⠀⠘⣿⣿⣿⣿⡇⠀⠀⠀⠈⣡⣷⣦⣀⡀⠀⠉⠙⠲⠦⣄⡀⠈⠉⠉⠛⠒⠒⠒⠒⠊⠉⠁⠀⠀⠀⠀⠉⠀⣀⣤⣶⣾⣷⣦⣄⠀⠀⠀⠈⣿⣿⣿⣿⣿⣧⣀⡀⠀
# ⣾⡗⠶⣿⣿⣿⣿⠁⠀⠀⢀⣾⡿⠿⢿⣿⣿⣷⣦⣄⡀⠀⠈⠉⠳⠦⣄⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢀⣠⣴⣿⣿⡿⠿⠻⠿⠿⢿⣧⠀⠀⠀⢻⣿⣿⣿⣿⣹⣦⢹⡀
# ⣿⣿⣆⠈⣿⣿⡟⠀⠀⠀⣸⡏⠀⠀⠀⠈⠉⠛⠿⢿⣿⣷⣄⡀⠀⠀⠀⠀⠀⠀⠀⠀⠀⡀⠀⢀⣴⣾⡿⠿⠛⠉⠀⠀⠀⠀⠀⠀⠹⡆⠀⠀⠈⣿⣿⡿⠉⠹⢿⢸⠇
# ⠘⣿⣯⠀⠹⣿⡇⠀⠀⢠⡏⠁⠀⠀⢀⠴⠶⠶⠦⣄⡈⠙⠻⢷⣄⡀⠀⢀⣀⣀⠀⣀⣀⢹⡦⠾⠋⠁⢀⣠⣶⣚⣉⣿⠛⠲⢦⡀⠀⠹⡄⠀⠀⣿⡿⣵⠃⠀⣾⡟⠀
# ⠀⠹⣿⡀⠀⣿⡇⠀⠀⠘⠁⠀⠀⠴⣿⡶⠒⣿⣽⣺⢿⣲⣄⡀⠈⠹⣀⠈⠉⠉⠀⠉⠁⢸⡇⠀⢀⣴⡿⠛⣽⠉⣿⣍⠙⢶⣄⡃⠀⠀⠀⠀⠀⣿⢳⠃⠀⣠⠟⠀⠀
# ⠀⠀⠙⣧⠀⢻⣇⠀⠀⠀⠀⠀⢰⣶⡿⠀⢸⣿⣶⣿⠆⠈⠻⢷⡆⠀⣽⣆⠀⠀⠀⠀⢠⣿⡇⠰⣿⢟⠀⠐⣿⣷⣿⠟⠀⡼⣻⠟⠀⠀⠀⠀⠀⠈⣿⠀⢠⡟⠀⠀⠀
# ⠀⠀⠀⢻⠀⠀⡁⠀⠀⠀⠀⠀⢠⢉⡳⢤⣀⠉⣛⣛⣀⣸⡛⠋⠁⠀⠋⢘⠆⠀⠀⠀⢸⣻⠅⠀⠈⠀⢰⠤⠬⠭⠡⠤⠴⠞⠁⣀⡀⠀⠀⠀⠀⠀⣸⠄⣼⠀⠀⠀⠀
# ⠀⠀⠀⢸⣷⠈⣏⠀⠀⠀⢀⡴⠾⢟⣓⣶⣾⣾⣿⣟⣉⣉⠀⠀⠀⠀⠀⢸⡇⠀⠀⠀⠀⠁⠀⠀⠀⠀⠈⠯⣒⣒⣒⣒⣒⠦⠽⠦⠝⠀⠀⠀⠀⢀⠉⠁⡟⠀⠀⠀⠀
# ⠀⠀⠀⢸⡁⠀⠻⠀⠀⢰⠋⠀⠀⠀⠀⠈⠉⠉⠉⠉⠉⠁⠀⠀⠀⠀⠀⢸⣧⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢠⡾⠃⢰⠇⠀⠀⠀⠀
# ⠀⠀⠀⠸⣇⠀⠀⠀⢰⡏⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⣠⡴⠖⠦⢴⡿⠉⠀⠀⠀⠀⠀⢸⡿⠛⠲⢤⡀⠀⠀⠀⠀⠀⠀⠀⢀⣄⠀⠀⠀⣾⠁⠀⣾⠀⠀⠀⠀⠀
# ⠀⠀⠀⠀⠻⣷⣤⡴⢿⡇⠀⠀⠀⠀⠀⠀⠀⠀⠀⢀⣴⣾⣿⠃⠀⠀⠈⠃⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢸⣿⣦⡀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⣿⠗⠚⠁⠀⠀⠀⠀⠀
# ⠀⠀⠀⠀⠀⠀⠀⠀⢸⡇⠀⠀⠀⠀⠀⠀⠀⠀⢠⣿⠿⠋⠘⢦⡀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢀⣤⡟⠉⠙⠻⣦⣄⠀⠀⠀⠀⠀⠀⠀⠀⣿⠀⠀⠀⠀⠀⠀⠀⠀
# ⠀⠀⠀⠀⠀⠀⠀⠀⢸⢹⡇⠀⠀⠀⠀⠀⠀⣠⠛⠁⠀⠀⠀⠀⠙⠛⠛⢶⣤⠀⠀⠀⠀⢀⣴⠞⠛⠃⠀⠀⠀⠀⠀⠈⠙⠢⡀⠀⠀⠀⠀⢸⡇⠀⠀⠀⠀⠀⠀⠀⠀
# ⠀⠀⠀⠀⠀⠀⠀⠀⣼⣿⡇⠀⠀⣸⠀⠀⠀⠘⣷⠀⠀⠀⠀⠀⠀⠀⠀⠀⠈⠻⣷⣤⣴⠟⠁⠀⠀⠀⠀⠀⣀⣤⣤⡄⠀⠀⠘⢦⠀⠀⢠⣼⡇⠀⠀⠀⠀⠀⠀⠀⠀
# ⠀⠀⠀⠀⠀⠀⠀⠀⡇⢸⡇⠀⠀⢹⣇⠀⠀⠀⠈⠛⠓⠦⣤⡀⠀⠀⠀⠀⠀⠀⠈⠉⠀⠀⠀⠀⢀⣤⠖⠛⠁⠀⠀⠋⠀⠀⠀⠀⣼⠀⢠⡟⠀⠀⠀⠀⠀⠀⠀⠀⠀
# ⠀⠀⠀⠀⠀⠀⠀⠀⣷⠀⢻⣄⠀⠀⢿⣆⠀⠀⠀⠀⠀⠐⢦⡙⠻⣿⡓⠶⠤⠤⢤⡤⠤⠴⠚⠛⠁⣤⠀⠀⠀⠀⠀⠀⠀⠀⠀⣸⠃⢀⡾⠁⠀⠀⠀⠀⠀⠀⠀⠀⠀
# ⠀⠀⠀⠀⠀⠀⠀⠀⢻⡆⠀⠹⣧⡀⠀⠻⣷⣤⠀⠀⠀⠀⠀⠙⢦⣄⡁⠀⠀⠀⠀⠀⠀⢀⣠⣴⠟⠁⠀⠀⠀⠀⠀⠀⠀⠀⡼⠃⣠⠞⠁⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀
# ⠀⠀⠀⠀⠀⠀⠀⠀⠈⢿⣄⠀⠈⠻⣦⣄⠈⢻⣷⠀⠀⠀⠀⠀⠀⠈⠉⠛⠛⠛⠛⠛⠛⠉⠁⠀⠀⠀⠀⠀⠀⠀⠀⠀⣠⣾⣡⠜⠁⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀
# ⠀⠀⠀⠀⠀⠀⠀⠀⠀⠈⠻⣷⣄⠀⠈⢿⣷⣄⣿⣧⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⣠⣾⡿⠛⠁⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀
# ⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠈⠻⢿⣶⣤⣙⣿⣿⣿⣷⣄⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢀⣴⣿⡿⠋⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀
# ⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠉⠛⠿⢿⣿⣿⣿⣿⣷⣤⣀⣀⣀⠀⠀⠀⠀⠀⠀⠀⣀⣀⣠⣶⣾⣿⠿⠋⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀
# ⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠉⠉⠙⠻⠿⠿⠿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⠿⠟⠋⠁⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀
# ⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠉⠉⠉⠉⠁⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀
# --- Helper: sanity-check the plotted aspect ratio for a given extent ---
check_ratio <- function(l) {
  mean_lat <- (l[3] + l[4]) / 2
  cos_lat  <- cos(mean_lat * pi / 180)
  rendered <- (l[2] - l[1]) / (l[4] - l[3]) * cos_lat
  cat(sprintf("w: %.4f  h: %.4f  raw_ratio: %.3f  rendered_ratio: %.3f\n",
              l[2]-l[1], l[4]-l[3], (l[2]-l[1])/(l[4]-l[3]), rendered))
}

# Shelf classes only — same list the old south-west script used to decide
# which NV classes sit "on the shelf" and get coloured over the hillshade
shelf_classes <- c(
  "Shelf unvegetated sediments",
  "Shelf vegetated sediments",
  "Oceanic shallow coral reefs",
  "Shallow coral reefs",
  "Shallow rocky reefs",
  "Mesophotic coral reefs",
  "Mesophotic rocky reefs",
  "Oceanic mesophotic coral reefs",
  "Rariphotic shelf reefs",
  "Upper slope reefs",
  "Upper slope sediments",
  "Shelf incising canyons"
)

# --- FUNCTION: hillshade background + NV colour on top, clipped to 200m ---
naturalvalues_map_hillshade_north <- function(plot_limits,
                                              use_clipped = TRUE,
                                              hs_altitude = 40,
                                              hs_azimuth  = 270,
                                              show_legend = TRUE,
                                              title       = NULL,
                                              break_step  = 0.2) {

  require(tidyverse); require(terra); require(sf); require(ggnewscale)

  ext_plot  <- ext(plot_limits[1], plot_limits[2], plot_limits[3], plot_limits[4])
  nv_source <- if (use_clipped) naturalvalues_clipped else naturalvalues

  # --- Hillshade from bathymetry (rendered everywhere, incl. past 200m) ---
  bathy_crop <- crop(bathy, ext_plot)
  slope      <- terrain(bathy_crop, v = "slope",  unit = "radians")
  aspect     <- terrain(bathy_crop, v = "aspect", unit = "radians")
  hs         <- shade(slope, aspect, angle = hs_altitude, direction = hs_azimuth, normalize = TRUE)
  hs_df      <- as.data.frame(hs, xy = TRUE, na.rm = TRUE)
  colnames(hs_df)[3] <- "hillshade"

  # --- Natural values — shelf classes only, clipped to 200m ---
  nv_crop <- crop(nv_source, ext_plot)
  nv_df   <- as.data.frame(nv_crop, xy = TRUE, na.rm = TRUE)
  colnames(nv_df)[3] <- "value"
  nv_df$classname <- nv_lookup[as.character(nv_df$value)]
  nv_df <- dplyr::filter(nv_df, !is.na(classname), classname %in% shelf_classes)

  present_classes <- unique(nv_df$classname)
  present_colours <- hab_colours_original[names(hab_colours_original) %in% present_classes]

  level_order <- names(nv_lookup)[names(nv_lookup) %in% as.character(nv_df$value)]
  level_order <- unname(nv_lookup[level_order])
  nv_df$classname <- factor(nv_df$classname, levels = level_order)

  x_breaks <- seq(floor(plot_limits[1] / break_step) * break_step,
                  ceiling(plot_limits[2] / break_step) * break_step,
                  by = break_step)
  y_breaks <- seq(floor(plot_limits[3] / break_step) * break_step,
                  ceiling(plot_limits[4] / break_step) * break_step,
                  by = break_step)

  nv_guide <- if (show_legend) guide_legend(order = 1, ncol = 1, title.position = "top") else "none"

  p <- ggplot() +

    # --- Hillshade base (visible everywhere, including >200m) ---
    geom_tile(data = hs_df, aes(x = x, y = y, fill = hillshade),
              alpha = 0.4, show.legend = FALSE) +
    scale_fill_gradient(low = "#1a1a2e", high = "#e8e8e8", na.value = NA, guide = "none") +

    # --- NV shelf classes on top, only present ≤200m ---
    new_scale_fill() +
    geom_tile(data = nv_df, aes(x = x, y = y, fill = classname)) +
    scale_fill_manual(
      name   = "Benthic ecosystem",
      values = present_colours[level_order],
      breaks = level_order,
      guide  = nv_guide
    ) +

    new_scale_fill() +
    geom_sf(data = aus, fill = "seashell2", colour = "grey80", linewidth = 0.1) +

    new_scale_fill() +
    geom_sf(data = terrnp, aes(fill = TYPE), colour = NA) +
    scale_fill_manual(
      values = c("National Park"  = "#c4cea6",
                 "Nature Reserve" = "#e4d0bb"),
      name   = "Terrestrial Parks",
      guide  = if (show_legend) guide_legend(order = 2, ncol = 1, title.position = "top") else "none"
    ) +

    geom_sf(data = marine_parks_amp, fill = NA, colour = alpha("grey40", 0.6), linewidth = 0.3) +

    coord_sf(xlim = plot_limits[1:2], ylim = plot_limits[3:4], crs = 4326, expand = FALSE) +
    scale_x_continuous(breaks = x_breaks) +
    scale_y_continuous(breaks = y_breaks) +
    labs(x = NULL, y = NULL, title = title) +
    theme_minimal() +
    theme(
      legend.key.size  = unit(0.45, "cm"),
      legend.text      = element_text(size = 8),
      legend.title     = element_text(size = 9, face = "bold"),
      legend.position  = if (show_legend) "right" else "none",
      legend.box       = "vertical",
      panel.grid       = element_blank(),
      panel.background = element_rect(fill = "white", colour = NA),
      plot.background  = element_rect(fill = "white", colour = NA),
      axis.text        = element_text(size = 10, colour = "grey40"),
      axis.ticks       = element_line(colour = "grey60"),
      plot.title       = if (!is.null(title)) element_text(face = "bold", size = 14, hjust = 0.5)
      else                 element_blank(),
      plot.margin      = margin(t = 0, r = 0, b = 0, l = 0)
    )

  return(p)
}


# ==============================================================================
# 6. FIGURE 2: North Kimberley (worked example — repeat per park)
# ==============================================================================
# NOTE: these limits are a placeholder bounding box only — swap in the real
# extent for each park (check against the shapefile/QGIS) before running.
# check_ratio() helps you tune width/height so panels aren't stretched.

north_kimberley_limits <- c(123.0, 128.0, -15.0, -11.0)
check_ratio(north_kimberley_limits)

north_kimberley_hs <- naturalvalues_map_hillshade_north(
  plot_limits = north_kimberley_limits,
  show_legend = FALSE,
  break_step  = 0.5
)

# Legend built the same way as the network-scale one, just restricted to the
# shelf classes actually present in this park's extent
nk_ext <- ext(north_kimberley_limits[1], north_kimberley_limits[2],
              north_kimberley_limits[3], north_kimberley_limits[4])
nk_nv_crop <- crop(naturalvalues_clipped, nk_ext)
nk_nv_df   <- as.data.frame(nk_nv_crop, xy = TRUE, na.rm = TRUE)
colnames(nk_nv_df)[3] <- "value"
nk_nv_df$classname <- nv_lookup[as.character(nk_nv_df$value)]
nk_nv_df <- dplyr::filter(nk_nv_df, !is.na(classname), classname %in% shelf_classes)

nk_level_order <- names(nv_lookup)[names(nv_lookup) %in% as.character(nk_nv_df$value)]
nk_level_order <- unname(nv_lookup[nk_level_order])

legend_nk <- ggplot(data.frame(x = 1, y = 1,
                               classname = factor(nk_level_order, levels = nk_level_order)),
                    aes(x = x, y = y, fill = classname)) +
  geom_tile() +
  scale_fill_manual(
    name   = "Benthic habitat",
    values = hab_colours_original[nk_level_order],
    breaks = nk_level_order,
    guide  = guide_legend(ncol = 4, direction = "horizontal",
                          title.position = "top", title.hjust = 0)
  ) +
  theme_void() +
  theme(
    legend.key.size = unit(0.6, "cm"),
    legend.text     = element_text(size = 12),
    legend.title    = element_text(size = 13, face = "bold"),
    legend.position = "bottom"
  )

figure_north_kimberley <- cowplot::plot_grid(
  north_kimberley_hs,
  cowplot::get_legend(legend_nk),
  ncol        = 1,
  rel_heights = c(1, 0.2),
  align       = "v",
  axis        = "t"
) +
  theme(plot.background = element_rect(fill = "white", colour = NA),
        plot.margin     = margin(t = 2, r = 15, b = 15, l = 5))

ggsave(paste(paste0("plots/", park, "/spatial/benthic_habitat/", name),
             "north-kimberley-natural-values.png", sep = "-"),
       plot   = figure_north_kimberley,
       dpi    = 600,
       width  = 9,
       height = 8,
       bg     = "white")

# ==============================================================================
# End of script
# ==============================================================================
