###
# Project: NESP 5.6 Project - North-west Network Report
# Data:    New bathymetry data (2024), marine park shapefiles, terrestrial
#          parks and aus outline
# Task:    Create network-scale bathymetry map
# Author:  Annika Leunig
# Date:    July 2026
# Outputs: 1. North-west network bathymetry map
###

# Table of contents
#     1.  Set up and load libraries
#     2.  Load spatial files
#     3.  Hillshade
#     4.  Define colour ramp
#     5.  FIGURE 1: North-west Network

# ==============================================================================
# 1. SET UP AND LOAD
# ==============================================================================

# Clear the environment
rm(list = ls())

# Set the study name
name <- "north-west"
park <- "network"

# Load libraries
library(sf)
library(terra)
library(tidyverse)
library(tidyterra)
library(ggnewscale)
library(RColorBrewer)

# Set cropping extent (matches the north-west network KEF/SST/natural-values scripts)
e <- ext(106, 133, -28, -11)

# Progress bar for raster operations
terraOptions(progress = 3)
sf_use_s2(TRUE)

# ==============================================================================
# 2. LOAD SPATIAL FILES
# ==============================================================================
# Terrestrial parks using CAPAD
terrnp <- st_read("data/north-west network/spatial/shapefiles/Collaborative_Australian_Protected_Areas_Database_(CAPAD)_2024_-_Terrestrial__.shp") %>%
  dplyr::filter(TYPE %in% c("Nature Reserve", "National Park"))

# Aus Outline
aus <- st_read("data/north-west network/spatial/shapefiles/STE_2021_AUST_GDA2020.shp") %>%
  st_make_valid()

# Marine parks — Commonwealth AMPs + WA state marine parks (same list as the
# north-west network KEF script's network_map() filter)
marine_parks <- st_read("data/north-west network/spatial/shapefiles/nw-network-australia_marine-parks-all.shp") %>%
  dplyr::filter(name %in% c(# Commonwealth AMPs (North-west Network)
    "Argo-Rowley Terrace", "Ashmore Reef", "Carnarvon Canyon", "Cartier Island",
    "Dampier", "Eighty Mile Beach", "Gascoyne", "Kimberley", "Mermaid Reef",
    "Montebello", "Ningaloo", "Roebuck", "Shark Bay",
    # WA state marine parks (Gascoyne-Pilbara-Kimberley)
    "Hamelin Pool", "Muiron Islands", "Barrow Island", "Thevenard Island",
    "Montebello Islands", "Yawuru Nagulagun / Roebuck Bay", "Yawuru", # IPA
    "Nyangumarta Warrarn", # IPA
    "Bardi Jawi Gaarra", "North Kimberley", "Mayala",
    "Lalang-gaddam", "Rowley Shoals", "Scott Reef"))

# Bathymetry layer
bathy <- rast("data/north-west network/spatial/rasters/AusBathyTopo__Australia__2024_250m_MSL_cog.tif") %>%
  crop(e)

# ==============================================================================
# 3. CREATE HILLSHADE
# ==============================================================================

make_hillshade <- function(bathy_rast) {
  slope  <- terrain(bathy_rast, v = "slope",  unit = "radians")
  aspect <- terrain(bathy_rast, v = "aspect", unit = "radians")
  shade(slope, aspect, angle = 40, direction = 270)
}

hill <- make_hillshade(bathy)

# ==============================================================================
# 4. DEFINE COLOUR RAMP
# ==============================================================================

# Standard bathymetric blue ramp — dark = deep, light = shallow (RColorBrewer
# "Blues", reversed so deepest water gets the darkest blue)
bathy_cols <- RColorBrewer::brewer.pal(9, "Blues") %>% rev()

hill_scale <- scale_fill_gradient(
  low      = "#1a1a2e",
  high     = "#a0a0a0",
  na.value = NA,
  guide    = "none"
)

# ==============================================================================
# 5. FIGURE 1: North-west Network
# ==============================================================================
# ── Set up ────────────────────────────────────────────────────────────────────
names(bathy) <- "depth"
names(hill)  <- "hillshade"

# Shared plot extent (matches the north-west network KEF/SST/natural-values
# scripts' plot_limits — tighter than the crop extent `e` above)
xlim_shared <- c(109, 130)
ylim_shared <- c(-26.5, -12.5)

# ── Bathymetry panel ───────────────────────────────────────────────────────────
p_bathy <- ggplot() +
  # Hillshade first (bottom layer)
  geom_spatraster(data = hill, aes(fill = hillshade),
                  alpha = 0.55, show.legend = FALSE) +
  hill_scale +
  new_scale_fill() +
  # Bathymetry second
  geom_spatraster(data = bathy, aes(fill = depth),
                  alpha = 0.65) +
  scale_fill_gradientn(
    colours  = bathy_cols,
    limits   = c(-7000, 0),
    na.value = NA,
    name     = "Depth (m)",
    guide    = guide_colorbar(
      barwidth       = 10,
      barheight      = 0.5,
      title.position = "top",
      title.hjust    = 0.5,
      title.theme    = element_text(size = 9, face = "plain"),
      label.theme    = element_text(size = 8, face = "plain")
    )
  ) +
  # Australia landmass
  geom_sf(data = aus, fill = "seashell2", colour = "grey80", linewidth = 0.1) +
  new_scale_fill() +
  # Terrestrial parks
  geom_sf(data = terrnp, aes(fill = TYPE), colour = NA, alpha = 0.8) +
  scale_fill_manual(
    values = c("National Park" = "#c4cea6", "Nature Reserve" = "#e4d0bb"),
    guide  = "none"
  ) +
  # Marine parks on top — no fill, white boundary
  geom_sf(data = marine_parks,
          fill      = NA,
          colour    = alpha("white", 0.3),
          linewidth = 0.5) +
  # Map outline
  annotate("rect",
           xmin = xlim_shared[1], xmax = xlim_shared[2],
           ymin = ylim_shared[1], ymax = ylim_shared[2],
           colour = "grey80", fill = NA, linewidth = 0.4) +
  coord_sf(xlim = xlim_shared, ylim = ylim_shared, expand = FALSE) +
  theme_void() +
  theme(
    legend.position   = "bottom",
    legend.direction  = "horizontal",
    legend.margin     = margin(0, 0, 0, 0),
    legend.box.margin = margin(-10, 0, 0, 0),
    legend.title      = element_text(size = 9,  face = "plain"),
    legend.text       = element_text(size = 8,  face = "plain"),
    plot.background   = element_rect(fill = NA, colour = NA),
    panel.border      = element_blank(),
    plot.margin       = margin(2, 2, 2, 2)
  )

# ── Save ──────────────────────────────────────────────────────────────────────
# Only unhash below if folder doesn't exist yet
# dir.create(paste0("plots/", park, "/spatial/bathymetry/"), recursive = TRUE, showWarnings = FALSE)

ggsave(
  filename = paste0("plots/", park, "/spatial/bathymetry/", name, "-network-bathy-panel.png"),
  plot     = p_bathy,
  dpi      = 800,
  width    = 6,
  height   = 7,
  bg       = "white"
)

# ==============================================================================
# End of script
# ==============================================================================
