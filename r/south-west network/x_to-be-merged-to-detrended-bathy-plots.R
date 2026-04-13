###
# Project: NESP 4.20 - Marine Park Dashboard reporting
# Data:    Marine parks, old and new bathymetry data (2009 & 2024)
# Task:    1x2 panel plot - bathymetry + detrended bathymetry (network extent)
# Author:  Annika Leunig
# Date:    June 2026
###

# Clear the environment
rm(list = ls())

# Set the study name
name <- "south-west"
park <- "network"

# Load libraries
library(sf)
library(terra)
library(stars)
library(starsExtra)
library(tidyverse)
library(tidyterra)
library(patchwork)
library(ggnewscale)

# Set cropping extent
e <- ext(108.0, 138.0, -40.0, -23.0)

# Progress bar for raster operations
terraOptions(progress = 3)

sf_use_s2(TRUE)

# ── Load spatial files ────────────────────────────────────────────────────────

terrnp <- st_read("data/south-west network/spatial/shapefiles/Legislated_Lands_and_Waters_DBCA_011.shp") %>%
  dplyr::filter(leg_catego %in% c("Nature Reserve", "National Park"))

aus <- st_read("data/south-west network/spatial/shapefiles/STE_2021_AUST_GDA2020.shp") %>%
  st_make_valid()

marine_parks <- st_read("data/south-west network/spatial/shapefiles/south-and-western-australia_marine-parks-all.shp") %>%
  dplyr::filter(name %in% c("Abrolhos", "Abrolhos Islands", "Bremer", "Eastern Recherche",
                            "Ngari Capes", "Geographe", "South-west Corner",
                            "Great Australian Bight", "Jurien", "Murat", "Jurien Bay",
                            "Perth Canyon", "Southern Kangaroo Island", "Twilight",
                            "Two Rocks", "Western Eyre", "Western Kangaroo Island",
                            "Nuyts Archipelgo", "Thorny Passage", "Sir Joseph Banks Group",
                            "Investigator", "West coast Bays", "Southern Spencer Gulf",
                            "Upper Spencer Gulf", "Cottesloe Reef", "Rottnest",
                            "Shoalwater Islands"))

# ── Load bathymetry ───────────────────────────────────────────────────────────

old_full_bathy <- rast("data/south-west network/spatial/rasters/ausbath_09_v4") %>%
  crop(e)

old_bathy <- old_full_bathy %>%
  clamp(upper = 0, lower = -250, values = FALSE) %>%
  trim()

# ── Hillshade ─────────────────────────────────────────────────────────────────

make_hillshade <- function(bathy_rast) {
  slope  <- terrain(bathy_rast, v = "slope",  unit = "radians")
  aspect <- terrain(bathy_rast, v = "aspect", unit = "radians")
  shade(slope, aspect, angle = 40, direction = 270)
}

old_hill <- make_hillshade(old_full_bathy)

# ── Detrended bathymetry ──────────────────────────────────────────────────────

old_zstar <- st_as_stars(old_bathy)
old_detre <- detrend(old_zstar, parallel = 8)
old_detre <- as(object = old_detre, Class = "SpatRaster")
names(old_detre) <- c("geoscience_detrended", "lineartrend")

old_detre_layer <- old_detre[["geoscience_detrended"]]

# ── Shared colour scales ──────────────────────────────────────────────────────

bathy_cols <- c("#090d1f", "#090d1f", "#090d1f", "#121a3d",
                "#121a3d", "#121a3d", "#121a3d", "#1a2860",
                "#1a2860", "#1e3870", "#1e3870", "#1e3870",
                "#244e88", "#244e88", "#2e66a0", "#2e66a0",
                "#3d80b8", "#5aa0c8", "#7dbece", "#7dbece",
                "#a2d4a8", "#a2d4a8", "#b8d878", "#b8d878",
                "#b8d878", "#d0e050", "#e8f040", "#d4b060", "#c8a080")

hill_scale <- scale_fill_gradient(
  low      = "#1a1a2e",
  high     = "#a0a0a0",
  na.value = NA,
  guide    = "none"
)

# ── Plot 1: Bathymetry ────────────────────────────────────────────────────────

names(old_full_bathy) <- "depth"
names(old_hill)       <- "hillshade"

# Shared plot extent — both panels locked to this
xlim_shared <- c(108.0, 138.0)
ylim_shared <- c(-40.0, -23.0)

p_bathy <- ggplot() +
  # Hillshade first (bottom layer)
  geom_spatraster(data = old_hill, aes(fill = hillshade),
                  alpha = 0.55, show.legend = FALSE) +
  hill_scale +
  new_scale_fill() +
  # Bathymetry second
  geom_spatraster(data = old_full_bathy, aes(fill = depth),
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
  geom_sf(data = terrnp, aes(fill = leg_catego), colour = NA, alpha = 0.8) +
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
    legend.title      = element_text(size = 9,  face = "plain"),
    legend.text       = element_text(size = 8,  face = "plain"),
    plot.background   = element_rect(fill = NA, colour = NA),
    panel.border      = element_blank()
  )

# ── Plot 2: Detrended bathymetry ──────────────────────────────────────────────

names(old_detre_layer) <- "detrended"

p_detre <- ggplot() +
  geom_spatraster(data = old_detre_layer, aes(fill = detrended)) +
  scale_fill_viridis_c(
    option    = "magma",
    na.value  = NA,
    name      = "Detrended bathymetry",
    direction = 1,
    begin     = 0.15,
    end       = 0.9,
    limits    = c(-200, 50),
    oob       = scales::squish,
    guide     = guide_colorbar(
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
  geom_sf(data = terrnp, aes(fill = leg_catego), colour = NA, alpha = 0.8) +
  scale_fill_manual(
    values = c("National Park" = "#c4cea6", "Nature Reserve" = "#e4d0bb"),
    guide  = "none"
  ) +
  # Marine parks
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
    legend.title      = element_text(size = 9,  face = "plain"),
    legend.text       = element_text(size = 8,  face = "plain"),
    plot.background   = element_rect(fill = NA, colour = NA),
    panel.border      = element_blank()
  )

# ── Combine panels ────────────────────────────────────────────────────────────

p_combined <- p_bathy + p_detre +
  plot_layout(ncol = 2)

# print(p_combined)

# ── Save ──────────────────────────────────────────────────────────────────────
# Only unhash if folder doesn't exist yet
# dir.create(paste0("plots/", park, "/spatial/bathymetry/"), recursive = TRUE, showWarnings = FALSE)

ggsave(
  filename = paste0("plots/", park, "/spatial/bathymetry/", name, "-network-bathy-detrended-panel.png"),
  plot     = p_combined,
  dpi      = 800,
  width    = 12,
  height   = 4,
  bg       = "white"
)

