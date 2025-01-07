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
  dplyr::filter(epbc %in% "Commonwealth") %>%
  arrange(zone)

# State Marine Parks only (for separate ggplot legends)
marine_parks_state <- marine_parks %>%
  dplyr::filter(epbc %in% "State")

# Make shapefile for the wreck exclusion area
kunmunya <- data.frame(x = 117.213333, y = -20.4301667, zone = "Sanctuary Zone", colour = "#bfd054") %>%
  st_as_sf(coords = c("x", "y"), crs = 4326) %>%
  st_transform(9473) %>%
  st_buffer(dist = 500)
plot(kunmunya)

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

wrecks <- data.frame(x = c(117.042948333243, 117.213089999945),
                     y = c(-20.3212583329681, -20.4297716665203),
                     label = "Shipwreck",
                     wreck = c("Glenbank", "Kunmunuya & Samson II")) %>%
  st_as_sf(coords = c("x", "y"), crs = 4326)

# 1. Location overview plot
# Set plot inputs
plot_limits = c(116.779, 117.544, -20.738, -20.282) # Extent of the main plot
study_limits = c(116.86, 117.4,-20.51, -20.3) # Extent of sampling
annotation_labels = data.frame(x = c(117.1935, 116.8763, 116.8333), # Labels for annotation e.g. nearby towns
                               y = c(-20.6287, -20.3854, -20.5583),
                               label = c("Point\nSamson", "Legendre\nIsland", "Burrup\nPeninsula"))

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
    geom_sf(data = kunmunya, aes(fill = zone), colour = NA, alpha = 0.4) +
    scale_fill_manual(name = "Closed Waters", guide = "legend",
                      values = with(kunmunya, setNames(colour, zone))) +
    new_scale_fill() +
    geom_sf(data = cwatr, colour = "firebrick", alpha = 1, linewidth = 0.4, lineend = "round") +
    geom_sf(data = wrecks, aes(colour = wreck), shape = 9) +
    scale_colour_manual(values = c("#073B4C", "#118AB2"), name = "Shipwreck") +
    new_scale_colour() +
    geom_sf_pattern(data = infrastructure, aes(pattern = Infrastructure, pattern_fill = Infrastructure, colour = Infrastructure), alpha = 0.7,
                    pattern_density = 0.8, pattern_size = 0.2, pattern_spacing = 0.005, pattern_colour = "grey80") +
    scale_colour_manual(values = c("#F35B04", "#D90429")) +
    scale_pattern_fill_manual(values = c("#F35B04", "#D90429")) +
    scale_pattern_manual(values = c("stripe", "crosshatch")) +
    # scale_fill_manual(values = c("Shipping channel" = "",
    #                              "Spoil ground" = )) +
    # new_scale_fill() +
    # new_scale("pattern_fill") +
    # new_scale("pattern") +
    # new_scale_colour() +
    # annotate(geom = "point", x = c(117.042948333243, 117.213089999945), # Glenbank, Dive Wreck
    #          y = c(-20.3212583329681, -20.4297716665203), shape = 9) +
    labs(x = NULL, y = NULL) +
    annotate("rect", xmin = study_limits[1], xmax = study_limits[2], ymin = study_limits[3], ymax = study_limits[4],
             fill = NA, colour = "goldenrod2", linewidth = 0.4) +
    annotate("text", x = annotation_labels$x,
             y = annotation_labels$y,
             label = annotation_labels$label, size = 1.65,
             fontface = "italic") +
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
              annotation_labels) +
  theme(text = element_text(size = 7),
        legend.key.size = unit(0.5, "cm"))
# Save plot
ggsave(paste(paste0('plots/', park, '/spatial/', name) , 'broad-site-plot.png',
             sep = "-"), dpi = 600, width = 8, height = 3.5, bg = "white")
# ggsave(paste(paste0('plots/', park, '/spatial/', name) , 'broad-site-plot.png',
#              sep = "-"), dpi = 600, width = 8, height = 6, bg = "white")

# 2. Site level overview - with sampling point locations
metadata <- readRDS(paste0("data/", park, "/tidy/", name, "_metadata-bathymetry-derivatives.rds")) %>%
  st_as_sf(coords = c("longitude_dd", "latitude_dd"), crs = 4326) %>%
  dplyr::mutate(method = case_when(str_detect(campaignid, "BRUV") ~ "BRUV",
                                   str_detect(campaignid, "BOSS") ~ "BOSS")) %>%
  glimpse()

# Set plot inputs
site_limits = c(116.779, 117.544, -20.738, -20.282) # For Dampier match it to the first plot

site_plot <- function(site_limits, # Tighter zoom for this plot
                      annotation_labels) {
  ggplot() +
    geom_spatraster_contour_filled(data = bathy,
                                   breaks = c(0, -30, -70, -200, -700, -2000, -4000, -10000), alpha = 4/5) +
    # scale_fill_grey(start = 1, end = 0.5 , guide = "none") +
    scale_fill_manual(values = c("#FFFFFF", "#EFEFEF", "#DEDEDE", "#CCCCCC", "#B6B6B6", "#9E9E9E", "#808080"),
                      guide = "none") +
    geom_sf(data = ausc, fill = "seashell2", colour = "grey80", size = 0.1) +
    new_scale_fill() +
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
    geom_sf(data = kunmunya, aes(fill = zone), colour = NA, alpha = 0.4) +
    scale_fill_manual(name = "Closed Waters", guide = "legend",
                      values = with(kunmunya, setNames(colour, zone))) +
    new_scale_fill() +
    labs(x = NULL, y = NULL) +
    new_scale_fill() +
    geom_sf(data = cwatr, colour = "firebrick", alpha = 1, size = 0.2, lineend = "round") +
    geom_sf_pattern(data = infrastructure, aes(pattern = Infrastructure, pattern_fill = Infrastructure, colour = Infrastructure), alpha = 0.7,
                    pattern_density = 0.8, pattern_size = 0.2, pattern_spacing = 0.005, pattern_colour = "grey80") +
    scale_colour_manual(values = c("Shipping channel" = "#F35B04", "Spoil ground" = "#D90429"), name = "Infrastructure") +
    scale_pattern_fill_manual(values = c("Shipping channel" = "#F35B04", "Spoil ground" = "#D90429"), name = "Infrastructure") +
    scale_pattern_manual(values = c("Shipping channel" = "stripe", "Spoil ground" = "crosshatch"), name = "Infrastructure") +
    new_scale_colour() +
    # new_scale("pattern_fill") +
    # new_scale("pattern") +
    geom_sf(data = metadata, alpha = 1, shape = 4, size = 0.8, aes(colour = method)) +
    scale_colour_manual(values = c("BRUV" = "#E1BE6A",
                                   "BOSS" = "#40B0A6"),
                        name = "Method") +
    annotate("text", x = annotation_labels$x,
             y = annotation_labels$y,
             label = annotation_labels$label, size = 1.65,
             fontface = "italic") +
    coord_sf(xlim = c(site_limits[1], site_limits[2]), ylim = c(site_limits[3], site_limits[4]), crs = 4326) +
    theme_minimal() +
    theme(panel.grid = element_blank())
}

# Create plot
site_plot(site_limits, annotation_labels) +
  theme(text = element_text(size = 7),
        legend.key.size = unit(0.4, "cm"))
# Save plot
ggsave(filename = paste(paste0('plots/', park, '/spatial/', name) , 'sampling-locations.png',
                        sep = "-"), units = "in", dpi = 600,
       bg = "white",
       width = 8, height = 3.8)

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

sealevel_plot <- function(plot_limits, annotation_labels) {
  ggplot() +
    # geom_spatraster(data = clamp(bathy, upper = -50, values = F)) +
    # scale_fill_gradient2(low = "royalblue4", mid = "lightskyblue1", high = "white", name = "Depth (m)",
    #                      na.value = "#f9ddb1") +
    # new_scale_fill() +
    geom_spatraster_contour_filled(data = bathy,
                                   breaks = c(0, -40, -70, -125)) +
    depth_fills +
    new_scale_fill() +
    geom_sf(data = marine_parks_amp, aes(colour = zone), fill = NA, alpha = 0.8, linewidth = 1) +
    scale_colour_manual(name = "Australian Marine Parks", guide = "legend",
                      values = with(marine_parks_amp, setNames(colour, zone))) +
    new_scale_fill() +
    geom_sf(data = ausc, fill = "seashell2", colour = "grey62", size = 0.2) +
    geom_sf(data = terrnp, aes(fill = leg_catego), alpha = 4/5, colour = NA, show.legend = F) +
    terr_fills +
    new_scale_fill() +
    annotate("text", x = annotation_labels$x,
             y = annotation_labels$y,
             label = annotation_labels$label, size = 1.65,
             fontface = "italic") +
    coord_sf(xlim = c(plot_limits[1], plot_limits[2]), ylim = c(plot_limits[3], plot_limits[4]), crs = 4326) +
    labs(x = "Longitude", y = "Latitude") +
    theme_minimal()
}
# Create plot
sealevel_plot(plot_limits, annotation_labels) +
  theme(panel.background = element_rect(fill = "#f9ddb1", colour = NA))
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
