###
# Project: NESP 4.20 - Marine Park Dashboard reporting
# Data:    Marine Park monitoring data syntheses, oceanographic data, marine park boundary files
# Task:    Create pre-modelling figures for marine park reporting
# Author:  Claude Spencer
# Date:    June 2024
###

# Table of contents
# 1. Overall location plot (including State and Commonwealth Marine Parks)
# 2. Sampling location plot
# 3. Key Ecological Features
# 4. Historical Sea Levels
# 5. Bathymetry cross section

# Clear your environment
rm(list = ls())

# Set the study name and marine park name (for folder structure)
name <- "DampierAMP"
park <- "dampier"

# Load libraries
library(tidyverse)
library(sf)
library(rnaturalearth)
library(metR)
library(patchwork)
library(terra)
library(tidyterra)
library(ggnewscale)
library(CheckEM)
library(geosphere)

# Load functions
file.sources = list.files(pattern = "*.R", path = "functions/", full.names = T)
sapply(file.sources, source, .GlobalEnv)

# Set cropping extent - larger than most zoomed out plot
e <- ext(116.7, 117.7,-20.919, -20)

# Load necessary spatial files
sf_use_s2(T)
# Australian outline and state and commonwealth marine parks
aus    <- st_read("data/south-west network/spatial/shapefiles/STE_2021_AUST_GDA2020.shp") %>%
  st_make_valid()
ausc <- st_crop(aus, e)

# Load marine parks
# aus_marine_parks <- st_read("data/spatial/shapefiles/Collaborative_Australian_Protected_Areas_Database_(CAPAD)_2022_-_Marine.shp")

# All australian marine parks - for inset plotting
aus_marine_parks <- st_read("data/south-west network/spatial/shapefiles/western-australia_marine-parks-all.shp")

marine_parks <- st_read("data/south-west network/spatial/shapefiles/western-australia_marine-parks-all.shp") %>%
  dplyr::filter(name %in% c("Dampier")) %>%
  glimpse()

# Australian Marine Parks only (for separate ggplot legends)
marine_parks_amp <- marine_parks %>%
  dplyr::filter(epbc %in% "Commonwealth")

# State Marine Parks only (for separate ggplot legends)
marine_parks_state <- marine_parks %>%
  dplyr::filter(epbc %in% "State")

# Terrestrial parks
terrnp <- st_read("data/south-west network/spatial/shapefiles/Legislated_Lands_and_Waters_DBCA_011.shp") %>%  # Terrestrial reserves
  dplyr::filter(leg_catego %in% c("Nature Reserve", "National Park"))
plot(terrnp["leg_catego"])

terr_fills <- scale_fill_manual(values = c("National Park" = "#c4cea6",          # Set the colours for terrestrial parks
                                           "Nature Reserve" = "#e4d0bb"),
                                name = "Terrestrial Parks")

# Key Ecological Features
# This shapefile has added columns in QGIS for hex colour code and abbreviated names
kef <- st_read("data/south-west network/spatial/shapefiles/AU_DOEE_KEF_2015.shp") %>%
  CheckEM::clean_names() %>%
  st_make_valid() %>%
  st_crop(e) %>%
  arrange(desc(area_km2)) %>%
  glimpse()
unique(kef$abbrv) # None in Dampier

# Coastal waters limit
cwatr <- st_read("data/south-west network/spatial/shapefiles/amb_coastal_waters_limit.shp") %>%
  st_make_valid() %>%
  st_crop(e)

# Bathymetry data
bathy <- rast("data/south-west network/spatial/rasters/Australian_Bathymetry_and_Topography_2023_250m_MSL_cog.tif") %>%
  crop(e) %>%
  clamp(upper = 0, values = F)
names(bathy) <- "Depth"
plot(bathy)

bathdf <- as.data.frame(bathy, xy = T)

# Create marine park colours and fills (scale_fill_manual)
# amp_fills <- amp_marine_park_fills(marine_parks)
# state_fills <- state_marine_park_fills(marine_parks)
# amp_cols <- amp_marine_park_cols(marine_parks)
# state_cols <- state_marine_park_fills(marine_parks)

# Load Port Walcott shipping zones
channel <- st_read("data/dampier/spatial/shapefiles/port-walcott_shipping-channel.shp") %>%
  summarise(geometry = st_union(geometry)) %>%
  dplyr::mutate(Infrastructure = "Shipping channel") %>%
  glimpse()

spoil <- st_read("data/dampier/spatial/shapefiles/port-walcott_spoil-grounds.shp") %>%
  summarise(geometry = st_union(geometry)) %>%
  dplyr::mutate(Infrastructure = "Spoil ground") %>%
  glimpse()

infrastructure <- bind_rows(channel, spoil)

# 1. Location overview plot
# Set plot inputs
plot_limits = c(116.779, 117.544, -20.738, -20.282) # Extent of the main plot
study_limits = c(116.86, 117.4,-20.51, -20.3) # Extent of sampling
annotation_labels = data.frame(x = c(117.1935), # Labels for annotation e.g. nearby towns
                               y = c(-20.6287),
                               label = c("Point Samson"))

# Custom function for this - need to add new sf layers
location_plot <- function(plot_limits, study_limits, annotation_labels) {
  # 1. Location overview plot - includes parks zones and an aus inset
  require(tidyverse)
  require(tidyterra)
  require(patchwork)
  require(ggpattern)

  p1 <- ggplot() +
    geom_spatraster_contour_filled(data = bathy,
                                   breaks = c(0, -30, -70, -200, - 700, -2000 , -4000, -6000),
                                   colour = NA, show.legend = F) +
    # scale_fill_grey(start = 1, end = 0.5, guide = "none") +
    scale_fill_manual(values = c("#FFFFFF", "#EFEFEF", "#DEDEDE", "#CCCCCC", "#B6B6B6", "#9E9E9E", "#808080")) +
    new_scale_fill() +
    geom_spatraster_contour(data = bathy,
                            breaks = c(-30, -70, -200, - 700, -2000 , -4000, -6000), colour = "white",
                            alpha = 3/5, linewidth = 0.1, show.legend = F) +
    geom_sf(data = ausc, fill = "seashell2", colour = "grey80", linewidth = 0.1) +
    geom_sf(data = terrnp, aes(fill = leg_catego), colour = NA, alpha = 0.8) +
    terr_fills +
    new_scale_fill() +
    geom_sf(data = marine_parks_state, aes(fill = zone), colour = NA, alpha = 0.4) +
    scale_fill_manual(name = "State Marine Parks", guide = "legend",
                      values = with(marine_parks_state, setNames(colour, zone))) +
    new_scale_fill() +
    geom_sf(data = marine_parks_amp, aes(fill = zone), colour = NA, alpha = 0.8) +
    scale_fill_manual(name = "Australian Marine Parks", guide = "legend",
                      values = with(marine_parks_amp, setNames(colour, zone))) +
    new_scale_fill() +
    geom_sf(data = cwatr, colour = "firebrick", alpha = 1, linewidth = 0.4, lineend = "round") +
    geom_sf_pattern(data = infrastructure, aes(pattern = Infrastructure, pattern_fill = Infrastructure, colour = Infrastructure), alpha = 0.7,
                    pattern_density = 0.8, pattern_size = 0.2, pattern_spacing = 0.005) +
    scale_colour_manual(values = c("#4A4E69", "#C9ADA7")) +
    scale_pattern_fill_manual(values = c("#4A4E69", "#C9ADA7")) +
    scale_pattern_manual(values = c("stripe", "crosshatch")) +
    # scale_fill_manual(values = c("Shipping channel" = "",
    #                              "Spoil ground" = )) +
    new_scale_fill() +
    labs(x = NULL, y = NULL) +
    annotate("text", x = annotation_labels$x,
             y = annotation_labels$y,
             label = annotation_labels$label, size = 1.65,
             fontface = "italic") +
    annotate("rect", xmin = study_limits[1], xmax = study_limits[2], ymin = study_limits[3], ymax = study_limits[4],
             fill = NA, colour = "goldenrod2", linewidth = 0.4) +
    coord_sf(xlim = c(plot_limits[1], plot_limits[2]), ylim = c(plot_limits[3], plot_limits[4]), crs = 4326) +
    theme_minimal()

  # inset map
  p1.1 <- ggplot(data = aus) +
    geom_sf(fill = "seashell1", colour = "grey90", linewidth = 0.05, alpha = 4/5) +
    geom_sf(data = aus_marine_parks, alpha = 5/6, colour = "grey85", linewidth = 0.02) +
    coord_sf(xlim = c(110, 125), ylim = c(-37, -13)) + # This is constant for all plots - its just a map of WA
    annotate("rect", xmin = plot_limits[1], xmax = plot_limits[2], ymin = plot_limits[3], ymax = plot_limits[4],   # Change here
             colour = "grey25", fill = "white", alpha = 1/5, linewidth = 0.2) +
    theme_bw() +
    theme(axis.text = element_blank(),
          axis.ticks = element_blank(),
          panel.grid.major = element_blank(),
          panel.border = element_rect(colour = "grey70"))

  p1.1 + p1
}
# Create plot
location_plot(plot_limits,
              study_limits,
              annotation_labels)
# Save plot
ggsave(paste(paste0('plots/', park, '/spatial/', name) , 'broad-site-plot.png',
             sep = "-"), dpi = 600, width = 8, height = 3.5, bg = "white")

# 2. Site level overview - with sampling point locations
metadata <- readRDS(paste0("data/", park, "/tidy/", name, "_metadata-bathymetry-derivatives.rds")) %>%
  st_as_sf(coords = c("longitude_dd", "latitude_dd"), crs = 4326) %>%
  dplyr::mutate(method = case_when(str_detect(campaignid, "BRUV") ~ "BRUV",
                                   str_detect(campaignid, "BOSS") ~ "BOSS")) %>%
  glimpse()

# Set plot inputs
site_limits = c(116.779, 117.544, -20.738, -20.282) # For Dampier match it to the first plot
# Create plot
site_plot(site_limits, annotation_labels)
# Save plot
ggsave(filename = paste(paste0('plots/', park, '/spatial/', name) , 'sampling-locations.png',
                        sep = "-"), units = "in", dpi = 600,
       bg = "white",
       width = 8, height = 3.5)

# 3. Key Ecological Features
# Create plot
# kef_plot(plot_limits, annotation_labels)
# Save plot
# ggsave(filename = paste(paste0('plots/', park, '/spatial/', name) , 'key-ecological-features.png',
#                         sep = "-"), units = "in", dpi = 600,
#        bg = "white",
#        width = 8, height = 6)

# 4. Historical sea levels
# Set coastline fills
depth_fills <- scale_fill_manual(values = c("#f9ddb1","#ee9f27", "#dc6601"),
                                 labels = c("9-10 Ka", "15-17 Ka", "20-30 Ka"),
                                 name = "Coastline age")
# Create plot
sealevel_plot(plot_limits, annotation_labels)
# Save plot
ggsave(filename = paste(paste0('plots/', park, '/spatial/', name) , 'old-sea-levels.png',
                        sep = "-"), units = "in", dpi = 600,
       bg = "white",
       width = 8, height = 5)

# 5. Bathymetry cross sections
# Create data
bath_df1 <- dem_cross_section(116.7475, 116.9888, -20.6993, -20.2273, maxdist = 10)
# Set plot inputs
crosssection_labels = data.frame(x = c(-5, -10, -16), # Labels for annotation
                                 y = c(110, 80, 20),
                                 label = c("Burrup Peninsula", "Dolphin Island", "Legendre Island"))

segment_offset <- 5 # Length of the segment
label_offset <- segment_offset + 2 # Distance from end of segment to label
# Create plot
crosssection_plot(crosssection_labels, label_offset, segment_offset)
# Save plot
ggsave(filename = paste(paste0('plots/', park, '/spatial/', name) , 'bathymetry-cross-section.png',
                        sep = "-"), units = "in", dpi = 600,
       bg = "white",
       width = 8, height = 4)
