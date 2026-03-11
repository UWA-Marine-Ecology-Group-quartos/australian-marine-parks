###
# Project: NESP 4.20 - Marine Park Dashboard reporting
# Data:    Marine parks, old and new bathymetry data (2009 & 2024)
# Task:    Format spatial covariates, extract covariates for each sampling location
# Author:  Annika Leunig
# Date:    June 2026
###

# Clear the environment
rm(list = ls())

# Set the study name
name <- "south-west"
park <- "network"

# Set cropping extent - larger than most zoomed out plot (all of aus for this one)
e <- ext(108.0, 138.0, -40.0, -23.0)


# Load libraries
library(sf)
library(terra)
library(stars)
library(starsExtra)
library(tidyverse)
library(tidyterra)
library(patchwork)
library(RNetCDF)
library(rerddap)
library(ggnewscale)
library(metR)

# Load in progress barfor raster operations
terraOptions(progress = 3)

# Load necessary spatial files
sf_use_s2(T)

# For the terrestrial bit
terrnp <- st_read("data/south-west network/spatial/shapefiles/Legislated_Lands_and_Waters_DBCA_011.shp") %>%  # Terrestrial reserves
  dplyr::filter(leg_catego %in% c("Nature Reserve", "National Park"))
#plot(terrnp["leg_catego"])

terr_fills <- scale_fill_manual(values = c("National Park" = "#c4cea6",          # Set the colours for terrestrial parks
                                           "Nature Reserve" = "#e4d0bb"),
                                name = "Terrestrial Parks")

# Australian outline and state and commonwealth marine parks
aus    <- st_read("data/south-west network/spatial/shapefiles/STE_2021_AUST_GDA2020.shp") %>%
  st_make_valid()

# Load marine parks
capad <- st_read("data/south-west network/spatial/shapefiles/Collaborative_Australian_Protected_Areas_Database_(CAPAD)_2022_-_Marine.shp")

# All australian marine parks - for inset plotting
aus_marine_parks <- st_read("data/south-west network/spatial/shapefiles/south-and-western-australia_marine-parks-all.shp")

# Filter to just south west corner
marine_parks <- st_read("data/south-west network/spatial/shapefiles/south-and-western-australia_marine-parks-all.shp") %>%
  dplyr::filter(name %in% c("Abrolhos", "Abrolhos Islands", "Bremer", "Eastern Recherche", "Ngari Capes", "Geographe",
                            "South-west Corner","Great Australian Bight", "Jurien","Murat", "Jurien Bay", "Perth Canyon",
                            "Southern Kangaroo Island", "Twilight", "Two Rocks", "Western Eyre", "Western Kangaroo Island",
                            "Nuyts Archipelgo", "Thorny Passage", "Sir Joseph Banks Group", "Investigator", "West coast Bays",
                            "Southern Spencer Gulf", "Upper Spencer Gulf", "Cottesloe Reef", "Rottnest", "Shoalwater Islands")) %>%
  glimpse()

# Load the old bathymetry data (GA 250m resolution)
# old raster layers are in ESRI grid format, so you will need to pont R to the folder the .adf files are stored in
old_full_bathy <- rast("data/south-west network/spatial/rasters/ausbath_09_v4") %>%
  crop(e)

old_bathy <- old_full_bathy %>%
  clamp(upper = 0, lower = -250, values = F) %>%
  trim()
plot(old_bathy)

# Load the new bathymetry data (GA 250m resolution)
new_full_bathy <- rast("data/south-west network/spatial/rasters/AusBathyTopo__Australia__2024_250m_MSL_cog.tif") %>%
  crop(e)

new_bathy <- new_full_bathy %>%
  clamp(upper = 0, lower = -250, values = F) %>%
  trim()
plot(new_bathy)

# Create detrended bathymetry for 2009 bathy
old_zstar <- st_as_stars(old_bathy)
old_detre <- detrend(old_zstar, parallel = 8)
old_detre <- as(object = old_detre, Class = "SpatRaster")
names(old_detre) <- c("geoscience_detrended", "lineartrend")

# Create detrended bathymetry for 2024 bathy
new_zstar <- st_as_stars(new_bathy)
new_detre <- detrend(new_zstar, parallel = 8)
new_detre <- as(object = new_detre, Class = "SpatRaster")
names(new_detre) <- c("geoscience_detrended", "lineartrend")

# PLOTTING Bathymetry and marine parks
# Create hillsahde
make_hillshade <- function(bathy_rast) {
  slope   <- terrain(bathy_rast, v = "slope",   unit = "radians")
  aspect  <- terrain(bathy_rast, v = "aspect",  unit = "radians")
  shade(slope, aspect, angle = 40, direction = 270)
}

old_hill <- make_hillshade(old_full_bathy)
new_hill <- make_hillshade(new_full_bathy)



bathy_cols <- c("#1a1530", "#1a1530", "#1a1530", "#2a2050",
                "#2a2050", "#2a2050", "#2a2050", "#352860",
                "#352860", "#304878", "#304878", "#304878",
                "#285a8a", "#285a8a", "#3878a0", "#3878a0",
                "#5898b0", "#78b8c8", "#98ccc0",
                "#98ccc0", "#b8d898", "#b8d898", "#c8cc70",
                "#c8cc70", "#c8cc70", "#d8e464",  "#EEfC5E", "#c08060", "#b89070")


bathy_scale <- scale_fill_gradientn(
  colours  = bathy_cols,
  limits   = c(-7000, 0),
  na.value = NA,
  name     = "Depth (m)",
  guide    = guide_colorbar(barwidth = 0.5, barheight = 8)
)



hill_scale <- scale_fill_gradient(
  low  = "#1a1a2e",
  high = "#a0a0a0",   # ← darker grey instead of #e8e8e8
  na.value = NA,
  guide = "none"
)
# 1. Bathymetry map
make_bathy_map <- function(bathy_rast, hill_rast) {

  names(bathy_rast) <- "depth"
  names(hill_rast)  <- "hillshade"

  e <- ext(bathy_rast)

  ggplot() +
    geom_spatraster(data = hill_rast, aes(fill = hillshade),
                    alpha = 0.55, show.legend = FALSE) +
    hill_scale +
    new_scale_fill() +
    geom_spatraster(data = bathy_rast, aes(fill = depth),
                    alpha = 0.65, show.legend = FALSE) +
    bathy_scale +
    geom_sf(data = marine_parks,
            fill      = "#2d2b8f",
            colour    = "#3d3aaa",
            linewidth = 0.08,
            alpha     = 0.35) +
    geom_sf(data = aus, fill = "seashell2", colour = "grey80", linewidth = 0.1) +
    new_scale_fill() +
    geom_sf(data = terrnp, aes(fill = leg_catego), colour = NA, alpha = 0.8) +
    scale_fill_manual(values = c("National Park" = "#c4cea6",
                                 "Nature Reserve" = "#e4d0bb"),
                      name = "Terrestrial Parks",
                      guide = "none") +
    coord_sf(xlim = c(e[1], e[2]), ylim = c(e[3], e[4]), expand = FALSE) +
    theme_void()
}

# Plot old bathymetry and save
p_old <- make_bathy_map(old_full_bathy, old_hill)
print(p_old)

dir.create(paste0('plots/', park, '/spatial/bathymetry/'), recursive = TRUE, showWarnings = TRUE)
ggsave(paste(paste0('plots/', park, '/spatial/bathymetry/', name), 'old-bathymetry-plot.png',
             sep = "-"),
       plot = p_old,
       dpi = 600, width = 12, height = 6, bg = "white")


# Plot new bathymetry and save
p_new <- make_bathy_map(new_full_bathy, new_hill)
print(p_new)

ggsave(paste(paste0('plots/', park, '/spatial/bathymetry/', name), 'new-bathymetry-plot.png',
             sep = "-"),
       plot = p_new,
       dpi = 600, width = 12, height = 6, bg = "white")


# detrended DRAFT - in progress
old_detre_layer <- old_detre[["geoscience_detrended"]]
new_detre_layer <- new_detre[["geoscience_detrended"]]

make_detrend_map <- function(detre_rast, bathy_contour, title_str) {

  names(detre_rast) <- "detrended"

  ggplot() +
    geom_spatraster(data = detre_rast) +
    scale_fill_viridis_c(
      option    = "magma",
      na.value  = NA,
      name      = "Detrended\nbathymetry",
      direction = 1,
      begin     = 0.15,   # ← slightly darker at 0 than before (was 0.1)
      end       = 0.9,
      limits    = c(-200, 50),
      oob       = scales::squish
    ) +
    # If you want to add contour lines
    geom_spatraster_contour(data = bathy_contour,
                            breaks = c(-30, -70, -200),
                            colour = "grey75",
                            linewidth = 0.3,
                            show.legend = FALSE) +
    geom_sf(data = aus, fill = "seashell2", colour = "grey80", linewidth = 0.1) +
    new_scale_fill() +
    geom_sf(data = terrnp, aes(fill = leg_catego), colour = NA, alpha = 0.8) +
    scale_fill_manual(values = c("National Park" = "#c4cea6",
                                 "Nature Reserve" = "#e4d0bb"),
                      name = "Terrestrial Parks",
                      guide = "none") +
    coord_sf(xlim = c(108, 138), ylim = c(-39, -24), expand = FALSE) +
    labs(x = "Longitude", y = "Latitude") +
    scale_y_continuous(breaks = c(-24, -28, -32, -36)) +
    theme_minimal() +
    theme(
      legend.position   = "right",
      legend.key.height = unit(1.5, "cm"),
      legend.title      = element_text(size = 15),
      axis.title        = element_text(size = 14),
      axis.text         = element_text(size = 12),
      panel.grid.major  = element_line(colour = "grey70", linewidth = 0.3),
      panel.grid.minor  = element_blank(),
      panel.background  = element_rect(fill = NA, colour = NA)
    )
}

# Usage
p_detre_old <- make_detrend_map(old_detre_layer, old_bathy, "Detrended Bathymetry 2009")
p_detre_new <- make_detrend_map(new_detre_layer, new_bathy, "Detrended Bathymetry 2024")

print(p_detre_old)
ggsave(paste(paste0('plots/', park, '/spatial/bathymetry/', name), 'old-detrended-bathymetry-plot.png',
             sep = "-"),
       plot = p_detre_old,
       dpi = 600, width = 12, height = 6, bg = "white")

print(p_detre_new)
ggsave(paste(paste0('plots/', park, '/spatial/bathymetry/', name), 'new-detrended-bathymetry-plot.png',
             sep = "-"),
       plot = p_detre_new,
       dpi = 600, width = 12, height = 6, bg = "white")


# Detrended bathymetry - geographe zoom in
# cut down all rasters to save time
# Crop to Geographe extent first
# Detrend specifically for Geographe extent
e_geo <- ext(114.8, 116.0, -33.8, -33.25)

old_bathy_geo <- crop(old_full_bathy, e_geo) %>%
  clamp(upper = 0, lower = -250, values = F) %>%
  trim()

new_bathy_geo <- crop(new_full_bathy, e_geo) %>%
  clamp(upper = 0, lower = -250, values = F) %>%
  trim()

# Detrend on the local extent
old_zstar_geo <- st_as_stars(old_bathy_geo)
old_detre_geo <- detrend(old_zstar_geo, parallel = 8)
old_detre_geo <- as(object = old_detre_geo, Class = "SpatRaster")
names(old_detre_geo) <- c("geoscience_detrended", "lineartrend")
plot(old_detre_geo[[1]])

new_zstar_geo <- st_as_stars(new_bathy_geo)
new_detre_geo <- detrend(new_zstar_geo, parallel = 8)
new_detre_geo <- as(object = new_detre_geo, Class = "SpatRaster")
names(new_detre_geo) <- c("geoscience_detrended", "lineartrend")
plot(new_detre_geo[[1]])

# Extract just detrended layer
old_detre_geo_layer <- old_detre_geo[["geoscience_detrended"]]
new_detre_geo_layer <- new_detre_geo[["geoscience_detrended"]]

make_detrend_map_zoom <- function(detre_rast, bathy_contour, title_str,
                                  xlim, ylim) {

  names(detre_rast) <- "detrended"

  bathy_df <- as.data.frame(bathy_contour, xy = TRUE)
  names(bathy_df)[3] <- "depth"

  contour_labels <- do.call(rbind, lapply(c(-30, -70, -200), function(lvl) {
    bathy_wide <- bathy_df %>%
      tidyr::pivot_wider(names_from = x, values_from = depth)
    x_vals <- sort(unique(bathy_df$x))
    y_vals <- sort(unique(bathy_df$y))
    z_mat  <- as.matrix(bathy_wide[, -1])
    cl <- contourLines(x = x_vals, y = y_vals, z = z_mat, levels = lvl)
    if (length(cl) == 0) return(NULL)

    # Crop to map extent
    longest_in_extent <- do.call(rbind, lapply(cl, function(line) {
      data.frame(x = line$x, y = line$y)
    })) %>%
      dplyr::filter(x >= xlim[1], x <= xlim[2],
                    y >= ylim[1], y <= ylim[2])

    if (nrow(longest_in_extent) == 0) return(NULL)
    # Default midpoint of longest line within extent
    mid <- nrow(longest_in_extent) %/% 2
    data.frame(x = longest_in_extent$x[mid], y = longest_in_extent$y[mid], level = lvl)
  }))

  ggplot() +
    geom_spatraster(data = detre_rast, maxcell = 5e6) +
    scale_fill_viridis_c(
      option    = "rocket",
      na.value  = NA,
      name      = "Detrended\nbathymetry",
      direction = 1,
      begin     = 0.15,
      end       = 0.9,
      limits    = c(-10, 30),
      oob       = scales::squish
    ) +
    geom_spatraster_contour(data = bathy_contour,
                            breaks = c(-30, -70, -200),
                            colour = "grey85",
                            linewidth = 0.3,
                            maxcell = 5e6,
                            show.legend = FALSE) +
    geom_text(data = contour_labels,
              aes(x = x, y = y, label = level),
              colour = "grey70",
              size = 3.5,
              fontface = "bold") +
    geom_sf(data = aus, fill = "seashell2", colour = "grey80", linewidth = 0.1) +
    new_scale_fill() +
    geom_sf(data = terrnp, aes(fill = leg_catego), colour = NA, alpha = 0.8) +
    scale_fill_manual(values = c("National Park" = "#c4cea6",
                                 "Nature Reserve" = "#e4d0bb"),
                      name = "Terrestrial Parks",
                      guide = "none") +
    coord_sf(xlim = xlim, ylim = ylim, expand = FALSE) +   # ← dynamic
    labs(x = "Longitude", y = "Latitude") +
    theme_minimal() +
    theme(
      legend.position   = "right",
      legend.key.height = unit(1.5, "cm"),
      legend.title      = element_text(size = 14),
      axis.title        = element_text(size = 14),
      axis.text         = element_text(size = 12),
      panel.grid.major  = element_line(colour = "grey70", linewidth = 0.3),
      panel.grid.minor  = element_blank(),
      panel.background  = element_rect(fill = NA, colour = NA)
    )
}

# Geographe zoom
p_detre_old_geo <- make_detrend_map_zoom(old_detre_geo_layer, old_bathy_geo,
                                         "Geographe 2009",
                                         xlim = c(114.9, 115.75),
                                         ylim = c(-33.7, -33.25))

p_detre_new_geo <- make_detrend_map_zoom(new_detre_geo_layer, new_bathy_geo,
                                           "Perth Canyon 2009",
                                           xlim = c(114.5, 116.0),
                                           ylim = c(-33.6, -33.3))
#2009
print(p_detre_old_geo)
ggsave(paste(paste0('plots/', park, '/spatial/bathymetry/', name), 'old-geographe-detrended-bathymetry-plot.png',
             sep = "-"),
       plot = p_detre_old_geo,   #
       dpi = 600, width = 12, height = 6, bg = "white")

#2024
print(p_detre_new_geo)
ggsave(paste(paste0('plots/', park, '/spatial/bathymetry/', name), 'new-geographe-detrended-bathymetry-plot.png',
             sep = "-"),
       plot = p_detre_new_geo,   #
       dpi = 600, width = 12, height = 6, bg = "white")

# South-West Corner zoom
# crop to swc extent
e_swc <- ext(110.0, 116.5, -34.5, -33.4)

old_bathy_swc <- crop(old_full_bathy, e_swc) %>%
  clamp(upper = 0, values = F) %>%
  trim()

new_bathy_swc <- crop(new_full_bathy, e_swc) %>%
  clamp(upper = 0, values = F) %>%
  trim()

# Detrend on local extent
old_zstar_swc <- st_as_stars(old_bathy_swc)
old_detre_swc <- detrend(old_zstar_swc, parallel = 8)
old_detre_swc <- as(object = old_detre_swc, Class = "SpatRaster")
names(old_detre_swc) <- c("geoscience_detrended", "lineartrend")

new_zstar_swc <- st_as_stars(new_bathy_swc)
new_detre_swc <- detrend(new_zstar_swc, parallel = 8)
new_detre_swc <- as(object = new_detre_swc, Class = "SpatRaster")
names(new_detre_swc) <- c("geoscience_detrended", "lineartrend")

old_detre_swc_layer <- old_detre_swc[["geoscience_detrended"]]
new_detre_swc_layer <- new_detre_swc[["geoscience_detrended"]]

p_detre_old_swc <- make_detrend_map_zoom(old_detre_swc_layer, old_bathy_swc,
                                         "SWC 2009",
                                         xlim = c(114.2, 116),
                                         ylim = c(-34.5, -33.4))

p_detre_new_swc <- make_detrend_map_zoom(new_detre_swc_layer, new_bathy_swc,
                                         "SWC 2024",
                                         xlim = c(114.2, 116),
                                         ylim = c(-34.5, -33.4))

print(p_detre_old_swc)
ggsave(paste(paste0('plots/', park, '/spatial/bathymetry/', name), 'old-swc-detrended-bathymetry-plot.png',
             sep = "-"),
       plot = p_detre_old_swc,
       dpi = 600, width = 12, height = 6, bg = "white")

print(p_detre_new_swc)
ggsave(paste(paste0('plots/', park, '/spatial/bathymetry/', name), 'new-swc-detrended-bathymetry-plot.png',
             sep = "-"),
       plot = p_detre_new_swc,
       dpi = 600, width = 12, height = 6, bg = "white")
