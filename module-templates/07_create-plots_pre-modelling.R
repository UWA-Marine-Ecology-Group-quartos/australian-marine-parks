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

# Set the study name
name <- "GeographeAMP"

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

# Set cropping extent - larger than most zoomed out plot
e <- ext(114.2, 115.8,-34.7, -33.1)

# Load necessary spatial files
sf_use_s2(T)
# Australian outline and state and commonwealth marine parks
aus    <- st_read("data/south-west network/spatial/shapefiles/STE_2021_AUST_GDA2020.shp") %>%
  st_make_valid()
ausc <- st_crop(aus, e)

# Load marine parks
# aus_marine_parks <- st_read("data/spatial/shapefiles/Collaborative_Australian_Protected_Areas_Database_(CAPAD)_2022_-_Marine.shp")
aus_marine_parks <- st_read("data/south-west network/spatial/shapefiles/Collaborative_Australian_Protected_Areas_Database_(CAPAD)_2022_-_Marine.shp")

marine_parks <- st_read("data/south-west network/spatial/shapefiles/Collaborative_Australian_Protected_Areas_Database_(CAPAD)_2022_-_Marine.shp") %>%
  CheckEM::clean_names() %>%
  dplyr::filter(name %in% c("South-west Corner", "Geographe", "Ngari Capes")) %>% # Filter to speed up plotting
  # dplyr::mutate(zone_type = str_replace_all(zone_type, " \\s*\\([^\\)]+\\)", "")) %>%
  dplyr::mutate(zone = case_when(
    str_detect(pattern = "Sanctuary", string = zone_type) ~ "Sanctuary Zone",
    str_detect(pattern = "IUCN II", string = zone_type) ~ "National Park Zone",
    str_detect(pattern = "National Park", string = zone_type) ~ "National Park Zone",
    str_detect(pattern = "Recreational|Recreation", string = zone_type) ~ "Recreational Use Zone",
    str_detect(pattern = "Habitat Protection", string = zone_type) ~ "Habitat Protection Zone",
    str_detect(pattern = "Special Purpose", string = zone_type) ~ "Special Purpose Zone",
    str_detect(pattern = "Multiple Use", string = zone_type) ~ "Multiple Use Zone",
    str_detect(pattern = "General", string = zone_type) ~ "General Use Zone")) %>%
  glimpse()
marine_parks_amp <- marine_parks %>%
  dplyr::filter(type %in% "Australian Marine Park")
marine_parks_state <- marine_parks %>%
  dplyr::filter(type %in% "Marine Park")

# Terrestrial parks
terrnp <- st_read("data/south-west network/spatial/shapefiles/Legislated_Lands_and_Waters_DBCA_011.shp") %>%  # Terrestrial reserves
  dplyr::filter(leg_catego %in% c("Nature Reserve", "National Park"))
plot(terrnp["leg_catego"])

terr_fills <- scale_fill_manual(values = c("National Park" = "#c4cea6",          # Set the colours for terrestrial parks
                                           "Nature Reserve" = "#e4d0bb"),
                                name = "Terrestrial Parks")

# Key Ecological Features
kef <- st_read("data/south-west network/spatial/shapefiles/AU_DOEE_KEF_2015.shp") %>%
  CheckEM::clean_names() %>%
  st_make_valid() %>%
  st_crop(e) %>%
  dplyr::mutate(name = case_when(name %in% "Commonwealth marine environment within and adjacent to Geographe Bay" ~ "Geographe Bay",
                                 name %in% "Ancient coastline at 90-120m depth" ~ "Ancient coastline",
                                 .default = name)) %>% # Shorten names - manually choose these
  dplyr::mutate(name = factor(name, levels = c("Western rock lobster",
                                               "Geographe Bay",
                                               "Cape Mentelle upwelling",
                                               "Ancient coastline")),
                order = case_when(name %in% "Western rock lobster" ~ 1,
                                  name %in% "Geographe Bay" ~ 2,
                                  name %in% "Cape Mentelle upwelling" ~ 3,
                                  name %in% "Ancient coastline" ~ 4)) %>%
  arrange(order) %>%
  glimpse()
unique(kef$name)

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

amp_marine_park_fills <- function(data) {
  amp_cols_all <- c("National Park Zone" = "#7bbc63",
                    "Habitat Protection Zone" = "#fff8a3",
                    "Multiple Use Zone" = "#b9e6fb",
                    "Recreational Use Zone" = "#ffb36b",
                    "Sanctuary Zone" = "#f7c0d8",
                    "Special Purpose Zone" = "#6daff4")

  scale_fill_manual(values = amp_cols_all[unique(data$zone)],
                                 name = "Australian Marine Parks")
}

amp_fills <- amp_marine_park_fills(marine_parks)

state_marine_park_fills <- function(data) {
  state_cols_all <- c("Sanctuary Zone" = "#bfd054",
                      "Habitat Protection Zone" = "#fffbcc",
                      "General Use Zone" = "#bddde1",
                      "Recreational Use Zone" = "#f4e952",
                      "Special Purpose Zone" = "#c5bcc9")

  scale_fill_manual(values = state_cols_all[unique(data$zone)],
                                   name = "State Marine Parks")
}

state_fills <- state_marine_park_fills(marine_parks)

amp_marine_park_cols <- function(data) {
  amp_cols_all <- c("National Park Zone" = "#7bbc63",
                    "Habitat Protection Zone" = "#fff8a3",
                    "Multiple Use Zone" = "#b9e6fb",
                    "Recreational Use Zone" = "#ffb36b",
                    "Sanctuary Zone" = "#f7c0d8",
                    "Special Purpose Zone" = "#6daff4")

  scale_colour_manual(values = amp_cols_all[unique(data$zone)],
                    name = "Australian Marine Parks")
}

amp_cols <- amp_marine_park_cols(marine_parks)

state_marine_park_cols <- function(data) {
  state_cols_all <- c("Sanctuary Zone" = "#bfd054",
                      "Habitat Protection Zone" = "#fffbcc",
                      "General Use Zone" = "#bddde1",
                      "Recreational Use Zone" = "#f4e952",
                      "Special Purpose Zone" = "#c5bcc9")

  scale_colour_manual(values = state_cols_all[unique(data$zone)],
                    name = "State Marine Parks")
}

state_cols <- state_marine_park_fills(marine_parks)

# amp_cols_all <- c("National Park Zone" = "#7bbc63",
#                   "Habitat Protection Zone" = "#fff8a3",
#                   "Multiple Use Zone" = "#b9e6fb",
#                   "Recreational Use Zone" = "#ffb36b",
#                   "Sanctuary Zone" = "#f7c0d8",
#                   "Special Purpose Zone" = "#6daff4") # Will I need to add back in Mining exclusion?
#
# state_cols_all <- c("Sanctuary Zone" = "#bfd054",
#                     "Habitat Protection Zone" = "#fffbcc",
#                     "General Use Zone" = "#bddde1",
#                     "Recreational Use Zone" = "#f4e952",
#                     "Special Purpose Zone" = "#c5bcc9")
#
# amp_cols <- scale_colour_manual(values = amp_cols_all[unique(marine_parks_amp$zone)],
#                                 name = "Australian Marine Parks")
# amp_fills <- scale_fill_manual(values = amp_cols_all[unique(marine_parks_amp$zone)],
#                                name = "Australian Marine Parks")
#
# state_cols <- scale_colour_manual(values = state_cols_all[unique(marine_parks_state$zone)],
#                                name = "State Marine Parks")
# state_fills <- scale_fill_manual(values = state_cols_all[unique(marine_parks_state$zone)],
#                                   name = "State Marine Parks")

# 1. Location overview plot - includes parks zones and an aus inset

# p1 <- ggplot() +
#   geom_spatraster_contour_filled(data = bathy,
#                       breaks = c(0, -30, -70, -200, - 700, -2000 , -4000, -6000), colour = NA, show.legend = F) +
#   scale_fill_grey(start = 1, end = 0.5, guide = "none") +
#   new_scale_fill() +
#   geom_spatraster_contour(data = bathy,
#                breaks = c(-30, -70, -200, - 700, -2000 , -4000, -6000), colour = "white",
#                alpha = 3/5, linewidth = 0.1, show.legend = F) +
#   geom_sf(data = ausc, fill = "seashell2", colour = "grey80", size = 0.1) +
#   geom_sf(data = terrnp, aes(fill = leg_catego), colour = NA, alpha = 0.8) +
#   terr_fills +
#   new_scale_fill() +
#   geom_sf(data = marine_parks_state, aes(fill = zone), colour = NA, alpha = 0.4) +
#   state_fills +
#   new_scale_fill() +
#   geom_sf(data = marine_parks_amp, aes(fill = zone), colour = NA, alpha = 0.8) +
#   amp_fills +
#   new_scale_fill() +
#   geom_sf(data = cwatr, colour = "firebrick", alpha = 1, linewidth = 0.4, lineend = "round") +
#   labs(x = NULL, y = NULL) +
#   annotate("point", x = c(115.6409, 115.3473, 115.1074, 115.0630, 115.1573),
#            y = c(-33.3270,-33.6516, -33.6177, -33.9535, -34.3110), size = 1, shape = 4) +
#   annotate("text", x = c(115.6409 - 0.08, 115.3473 + 0.09, 115.1074 - 0.11, 115.0630 + 0.13, 115.1573 - 0.07),
#            y = c(-33.3270,-33.65, -33.6177, -33.9535, -34.3110),
#            label = c("Bunbury", "Busselton", "Dunsborough", "Margaret River", "Augusta"), size = 1.65,
#            fontface = "italic") +
#   annotate("rect", xmin = 114.88, xmax = 115.67, ymin = -33.67, ymax = -33.3,
#            fill = "white", colour = "goldenrod2", alpha = 0, size = 0.4) +
#   coord_sf(xlim = c(114.4, 115.67), ylim = c(-33.3, -34.6), crs = 4326) +
#   theme_minimal()
#
# # inset map
# p1.1 <- ggplot(data = aus) +
#   geom_sf(fill = "seashell1", colour = "grey90", linewidth = 0.05, alpha = 4/5) +
#   geom_sf(data = aus_marine_parks, alpha = 5/6, colour = "grey85", linewidth = 0.02) +
#   coord_sf(xlim = c(110, 125), ylim = c(-37, -13)) + # This is constant for all plots - its just a map of WA
#   annotate("rect", xmin = 115.0, xmax = 115.67, ymin = -33.3, ymax = -33.65,   # Change here
#            colour = "grey25", fill = "white", alpha = 1/5, size = 0.2) +
#   theme_bw() +
#   theme(axis.text = element_blank(),
#         axis.ticks = element_blank(),
#         panel.grid.major = element_blank(),
#         panel.border = element_rect(colour = "grey70"))
#
# p1.1 + p1

location_plot <- function(plot_limits, study_limits, annotation_labels) {
  # 1. Location overview plot - includes parks zones and an aus inset
  require(tidyverse)
  require(tidyterra)
  require(patchwork)

  p1 <- ggplot() +
    geom_spatraster_contour_filled(data = bathy,
                                   breaks = c(0, -30, -70, -200, - 700, -2000 , -4000, -6000),
                                   colour = NA, show.legend = F) +
    scale_fill_grey(start = 1, end = 0.5, guide = "none") +
    new_scale_fill() +
    geom_spatraster_contour(data = bathy,
                            breaks = c(-30, -70, -200, - 700, -2000 , -4000, -6000), colour = "white",
                            alpha = 3/5, linewidth = 0.1, show.legend = F) +
    geom_sf(data = ausc, fill = "seashell2", colour = "grey80", size = 0.1) +
    geom_sf(data = terrnp, aes(fill = leg_catego), colour = NA, alpha = 0.8) +
    terr_fills +
    new_scale_fill() +
    geom_sf(data = marine_parks_state, aes(fill = zone), colour = NA, alpha = 0.4) +
    state_fills +
    new_scale_fill() +
    geom_sf(data = marine_parks_amp, aes(fill = zone), colour = NA, alpha = 0.8) +
    amp_fills +
    new_scale_fill() +
    geom_sf(data = cwatr, colour = "firebrick", alpha = 1, linewidth = 0.4, lineend = "round") +
    labs(x = NULL, y = NULL) +
    annotate("point", x = annotation_points$x,
             y = annotation_points$y, size = 1, shape = 4) +
    annotate("text", x = annotation_labels$x,
             y = annotation_labels$y,
             label = annotation_labels$label, size = 1.65,
             fontface = "italic") +
    annotate("rect", xmin = study_limits[1], xmax = study_limits[2], ymin = study_limits[3], ymax = study_limits[4],
             fill = NA, colour = "goldenrod2", size = 0.4) +
    coord_sf(xlim = c(plot_limits[1], plot_limits[2]), ylim = c(plot_limits[3], plot_limits[4]), crs = 4326) +
    theme_minimal()

  # inset map
  p1.1 <- ggplot(data = aus) +
    geom_sf(fill = "seashell1", colour = "grey90", linewidth = 0.05, alpha = 4/5) +
    geom_sf(data = aus_marine_parks, alpha = 5/6, colour = "grey85", linewidth = 0.02) +
    coord_sf(xlim = c(110, 125), ylim = c(-37, -13)) + # This is constant for all plots - its just a map of WA
    annotate("rect", xmin = plot_limits[1], xmax = plot_limits[2], ymin = plot_limits[3], ymax = plot_limits[4],   # Change here
             colour = "grey25", fill = "white", alpha = 1/5, size = 0.2) +
    theme_bw() +
    theme(axis.text = element_blank(),
          axis.ticks = element_blank(),
          panel.grid.major = element_blank(),
          panel.border = element_rect(colour = "grey70"))

  p1.1 + p1
}

# t <- list(x = c(114), y = c(-36), label = c("Geographe"))
# t$x

library(googlesheets4)
testdat <- read_sheet("https://docs.google.com/spreadsheets/d/1wycMSb8ykriU458sqx5FIKkDlkWIK7Uv58ySSapi8Kc/edit?usp=sharing",
                      sheet = "spatial_variables")

plot_limits = c(114.4, 115.67, -33.3, -34.6)
study_limits = c(114.88, 115.67,-33.3, -33.67)
annotation_labels = data.frame(x = c(115.6409, 115.3473, 115.1074, 115.0630, 115.1573),
                               y = c(-33.3270,-33.65, -33.6177, -33.9535, -34.3110),
                               label = c("Bunbury", "Busselton", "Dunsborough", "Margaret River", "Augusta"))

location_plot(plot_limits,
              study_limits,
              annotation_labels)

ggsave(paste(paste0('plots/geographe/spatial/', name) , 'broad-site-plot.png',
             sep = "-"), dpi = 600, width = 8, height = 5, bg = "white")

# 2. Site zoom plot - including sampling points
metadata <- readRDS(paste0("data/geographe/tidy/", name, "_metadata-bathymetry-derivatives.rds")) %>%
  st_as_sf(coords = c("longitude_dd", "latitude_dd"), crs = 4326) %>%
  glimpse()

site_limits = c(115.0, 115.67, -33.3, -33.65)

site_plot <- function(site_limits, # Tighter zoom for this plot
                      annotation_labels) {
  ggplot() +
    geom_spatraster_contour_filled(data = bathy,
                                   breaks = c(0, -30, -70, -200, -700, -2000, -4000, -10000), alpha = 4/5) +
    scale_fill_grey(start = 1, end = 0.5 , guide = "none") +
    geom_sf(data = ausc, fill = "seashell2", colour = "grey80", size = 0.1) +
    new_scale_fill() +
    geom_sf(data = terrnp, aes(fill = leg_catego), colour = NA, alpha = 0.8) +
    terr_fills +
    new_scale_fill() +
    geom_sf(data = marine_parks_state, aes(fill = zone), colour = NA, alpha = 0.4) +
    state_fills +
    new_scale_fill() +
    geom_sf(data = marine_parks_amp, aes(fill = zone), colour = NA, alpha = 0.8) +
    amp_fills +
    new_scale_fill() +
    labs(x = NULL, y = NULL) +
    new_scale_fill() +
    geom_sf(data = cwatr, colour = "firebrick", alpha = 1, size = 0.2, lineend = "round") +
    geom_sf(data = metadata, alpha = 1, shape = 10, size = 0.8, colour = "indianred4") +
    annotate("text", x = annotation_labels$x,
             y = annotation_labels$y,
             label = annotation_labels$label, size = 1.65,
             fontface = "italic") +
    coord_sf(xlim = c(site_limits[1], site_limits[2]), ylim = c(site_limits[3], site_limits[4]), crs = 4326) +
    theme_minimal() +
    theme(panel.grid = element_blank())
}

site_plot(site_limits, annotation_labels)

# p2 <- ggplot() +
#   geom_spatraster_contour_filled(data = bathy,
#                       breaks = c(0, -30, -70, -200, -700, -2000, -4000, -10000), alpha = 4/5) +
#   scale_fill_grey(start = 1, end = 0.5 , guide = "none") +
#   geom_sf(data = ausc, fill = "seashell2", colour = "grey80", size = 0.1) +
#   new_scale_fill() +
#   geom_sf(data = terrnp, aes(fill = leg_catego), colour = NA, alpha = 0.8) +
#   terr_fills +
#   new_scale_fill() +
#   geom_sf(data = marine_parks_state, aes(fill = zone), colour = NA, alpha = 0.4) +
#   state_fills +
#   new_scale_fill() +
#   geom_sf(data = marine_parks_amp, aes(fill = zone), colour = NA, alpha = 0.8) +
#   amp_fills +
#   new_scale_fill() +
#   labs(x = NULL, y = NULL) +
#   new_scale_fill() +
#   geom_sf(data = cwatr, colour = "firebrick", alpha = 1, size = 0.2, lineend = "round") +
#   geom_sf(data = metadata, alpha = 1, shape = 10, size = 0.8, colour = "indianred4") +
#   annotate("point", x = c(115.6409, 115.3473, 115.1074, 115.0630, 115.1573),
#            y = c(-33.3270,-33.6516, -33.6177, -33.9535, -34.3110), size = 1, shape = 4) +
#   annotate("text", x = c(115.6409 - 0.025, 115.3473 + 0.03, 115.1074 - 0.035),
#            y = c(-33.3270,-33.65, -33.6177),
#            label = c("Bunbury", "Busselton", "Dunsborough"), size = 1.65,
#            fontface = "italic") +
#   coord_sf(xlim = c(115.0, 115.67), ylim = c(-33.3, -33.65), crs = 4326) +
#   theme_minimal() +
#   theme(panel.grid = element_blank())

ggsave(filename = paste(paste0('plots/geographe/spatial/', name) , 'sampling-locations.png',
                        sep = "-"), plot = p2, units = "in", dpi = 600,
       bg = "white",
       width = 8, height = 4)

# 3. Key Ecological Features
unique(kef$name)
levels(kef$name)
kef_fills <- scale_fill_manual(values = c("Geographe Bay" = "#004949",
                                          "Cape Mentelle upwelling" = "#920000",
                                          "Ancient coastline" = "#FF6DB6",
                                          "Western rock lobster" = "#6DB6FF"),
                               name = "Key Ecological Features")

kef_plot <- function(plot_limits, annotation_labels) {
  ggplot() +
    geom_sf(data = ausc, fill = "seashell2", colour = "grey80", size = 0.1) +
    geom_sf(data = terrnp, aes(fill = leg_catego), alpha = 4/5, colour = NA, show.legend = F) +
    labs(fill = "Terrestrial Managed Areas") +
    terr_fills +
    new_scale_fill() +
    geom_sf(data = kef, aes(fill = name), alpha = 0.7, color = NA) +
    kef_fills +
    new_scale_fill() +
    geom_sf(data = terrnp, aes(fill = leg_catego), colour = NA, alpha = 0.8, show.legend = F) +
    terr_fills +
    new_scale_fill() +
    geom_sf(data = marine_parks_state, aes(fill = zone), colour = NA) +
    state_fills +
    new_scale_colour() +
    geom_sf(data = marine_parks_amp, aes(colour = zone), fill = NA, linewidth = 0.4, alpha = 0.3) +
    amp_cols +
    new_scale_colour() +
    geom_sf(data = cwatr, colour = "firebrick", alpha = 1, linewidth = 0.4, lineend = "round") +
    labs(x = NULL, y = NULL,  fill = "Key Ecological Features") +
    annotate("text", x = annotation_labels$x,
             y = annotation_labels$y,
             label = annotation_labels$label, size = 1.65,
             fontface = "italic") +
    coord_sf(xlim = c(plot_limits[1], plot_limits[2]), ylim = c(plot_limits[3], plot_limits[4]), crs = 4326) +
    theme_minimal() +
    theme(panel.grid = element_blank())
}

# p3 <- ggplot() +
#   geom_sf(data = ausc, fill = "seashell2", colour = "grey80", size = 0.1) +
#   geom_sf(data = terrnp, aes(fill = leg_catego), alpha = 4/5, colour = NA, show.legend = F) +
#   labs(fill = "Terrestrial Managed Areas") +
#   terr_fills +
#   new_scale_fill() +
#   geom_sf(data = kef, aes(fill = name), alpha = 0.7, color = NA) +
#   kef_fills +
#   new_scale_fill() +
#   geom_sf(data = terrnp, aes(fill = leg_catego), colour = NA, alpha = 0.8, show.legend = F) +
#   terr_fills +
#   new_scale_fill() +
#   geom_sf(data = marine_parks_state, aes(fill = zone), colour = NA) +
#   state_fills +
#   new_scale_colour() +
#   geom_sf(data = marine_parks_amp, aes(colour = zone), fill = NA, linewidth = 0.4, alpha = 0.3) +
#   amp_cols +
#   new_scale_colour() +
#   geom_sf(data = cwatr, colour = "firebrick", alpha = 1, linewidth = 0.4, lineend = "round") +
#   labs(x = NULL, y = NULL,  fill = "Key Ecological Features") +
#   annotate("point", x = c(115.6409, 115.3473, 115.1074, 115.0630, 115.1573),
#            y = c(-33.3270,-33.6516, -33.6177, -33.9535, -34.3110), size = 1, shape = 4) +
#   annotate("text", x = c(115.6409 - 0.06, 115.3473 + 0.07, 115.1074 - 0.09, 115.0630 + 0.09, 115.1573 - 0.06),
#            y = c(-33.3270,-33.65, -33.6177, -33.9535, -34.3110),
#            label = c("Bunbury", "Busselton", "Dunsborough", "Margaret River", "Augusta"), size = 2,
#            fontface = "italic") +
#   coord_sf(xlim = c(114.4, 115.67), ylim = c(-33.3, -34.6), crs = 4326) +
#   theme_minimal()+
#   theme(panel.grid = element_blank())

ggsave(filename = paste(paste0('plots/geographe/spatial/', name) , 'key-ecological-features.png',
                        sep = "-"), plot = p3, units = "in", dpi = 600,
       bg = "white",
       width = 8, height = 6)

# 4. Old sea level map (p4)
depth_fills <- scale_fill_manual(values = c("#f9ddb1","#ee9f27", "#dc6601"),
                                 labels = c("9-10 Ka", "15-17 Ka", "20-30 Ka"),
                                 name = "Coastline age")

# build basic plot elements

p4 <- ggplot() +
  geom_spatraster(data = clamp(bathy, upper = -50, values = F)) +
  scale_fill_gradient2(low = "royalblue4", mid = "lightskyblue1", high = "white", name = "Depth (m)",
                       na.value = "#f9ddb1") +
  new_scale_fill() +
  geom_spatraster_contour_filled(data = bathy,
                      breaks = c(0, -40, -70, -125)) +
  depth_fills +
  new_scale_fill() +
  geom_sf(data = ausc, fill = "seashell2", colour = "grey62", size = 0.2) +
  # new_scale_fill() +
  geom_sf(data = terrnp, aes(fill = leg_catego), alpha = 4/5, colour = NA, show.legend = F) +
  terr_fills +
  new_scale_fill() +
  annotate("point", x = c(115.6409, 115.3473, 115.1074, 115.0630, 115.1573),
           y = c(-33.3270,-33.6516, -33.6177, -33.9535, -34.3110), size = 1, shape = 4) +
  annotate("text", x = c(115.6409 - 0.06, 115.3473 + 0.07, 115.1074 - 0.09, 115.0630 + 0.09, 115.1573 - 0.06),
           y = c(-33.3270,-33.65, -33.6177, -33.9535, -34.3110),
           label = c("Bunbury", "Busselton", "Dunsborough", "Margaret River", "Augusta"), size = 2,
           fontface = "italic") +
  # geom_sf(data = marine.parks %>% dplyr::filter(ZONE_TYPE %in% "National Park Zone"),
  #         fill = NA, colour = "#7bbc63", linewidth = 0.6) +
  # geom_sf(data = cwatr, colour = "firebrick", alpha = 1, linewidth = 0.25, lineend = "round") +
  coord_sf(xlim = c(114.4, 115.67), ylim = c(-33.3, -34.6), crs = 4326) +
  labs(x = "Longitude", y = "Latitude") +
  theme_minimal()
ggsave(filename = paste(paste0('plots/geographe/spatial/', name) , 'old-sea-levels.png',
                        sep = "-"), plot = p4, units = "in", dpi = 600,
       bg = "white",
       width = 8, height = 6)

# 5. Bathymetry cross section
sf_use_s2(T)
points <- data.frame(x = c(115.096, 115.000),
                     y = c(-33.804, -33.105), id = 1)

tran <- sfheaders::sf_linestring(obj = points,
                                 x = "x",
                                 y = "y",
                                 linestring_id = "id")
st_crs(tran) <- 4326
tranv <- vect(tran)

topo <- rast("data/south-west network/spatial/rasters/Australian_Bathymetry_and_Topography_2023_250m_MSL_cog.tif")
names(topo) <- "depth"
batht <- terra::extract(topo, tranv, xy = T, ID = F)

bath_cross <- st_as_sf(x = batht, coords = c("x", "y"), crs = 4326)
plot(bath_cross)

aus <- st_read("data/south-west network/spatial/shapefiles/aus-shapefile-w-investigator-stokes.shp") %>%
  dplyr::filter(FEAT_CODE %in% "mainland") %>%
  st_transform(4326) %>%
  st_union()
ausout <- st_cast(aus, "MULTILINESTRING")
plot(ausout)

calculate_bearing <- function(alat, alon, blat, blon) {

  # Utility functions
  degrees_to_radians <- function(degrees) {
    return(degrees * pi / 180)
  }
  radians_to_degrees <- function(radians) {
    return(radians * 180 / pi)
  }

  delta_lon <- blon - alon
  delta_lat <- blat - alat

  lat1_rad <- degrees_to_radians(alat)
  lat2_rad <- degrees_to_radians(blat)

  bearing <- atan2(sin(delta_lon) * cos(lat2_rad),
                   cos(lat1_rad) * sin(lat2_rad) - sin(lat1_rad) * cos(lat2_rad) * cos(delta_lon))
  bearing <- radians_to_degrees(bearing)

  bearing <- (bearing + 360) %% 360
  return(bearing)
}

bath_sf <- bath_cross %>%
  dplyr::mutate("distance.from.coast" = st_distance(bath_cross, bath_cross$geometry[which.min(st_distance(bath_cross, ausout))]),
                land = lengths(st_intersects(bath_cross, aus)) > 0,
                coast = bath_cross$geometry[which.min(st_distance(bath_cross, ausout))]) %>%
  bind_cols(st_coordinates(.)) %>%
  dplyr::rename(from_longitude = X, from_latitude = Y) %>%
  bind_cols(st_coordinates(.$coast)) %>%
  dplyr::rename(to_longitude = X, to_latitude = Y) %>%
  dplyr::mutate(bearing = calculate_bearing(alon = .$from_longitude,
                                            alat = .$from_latitude,
                                            blon = .$to_longitude,
                                            blat = .$to_latitude)) %>%
  dplyr::mutate(distance.from.coast = ifelse(between(bearing, 50, 150), distance.from.coast * -1, distance.from.coast)) %>%
  glimpse()

bath_df1 <- as.data.frame(bath_sf) %>%
  dplyr::select(-geometry) %>%
  dplyr::mutate(distance.from.coast = as.numeric(distance.from.coast/1000)) %>%
  dplyr::filter(distance.from.coast < 10) %>%
  glimpse()

paleo <- data.frame(depth = c(-118, -94, -63, -41),
                    label = c("20-30 Ka", "15-17 Ka", "12-13 Ka", "9-10 Ka"))

for (i in 1:nrow(paleo)) {
  temp <- bath_df1 %>%
    dplyr::filter(abs(bath_df1$depth - paleo$depth[i]) == min(abs(bath_df1$depth - paleo$depth[i]))) %>%
    dplyr::select(depth, distance.from.coast) %>%
    slice(1)

  if (i == 1) {
    dat <- temp
  }
  else {
    dat <- bind_rows(dat, temp)
  }
}

paleo$distance.from.coast <- dat$distance.from.coast

min_dist1 <- min(bath_df1$distance.from.coast)

p5 <- ggplot() +
  geom_rect(aes(xmin = min_dist1, xmax = 9, ymin =-Inf, ymax = 0), fill = "#12a5db", alpha = 0.5) +
  annotate("segment", x = -5.556, xend = - 5.556, y = 0, yend = -40, colour = "red") +
  geom_line(data = bath_df1, aes(y = depth, x = distance.from.coast)) +
  geom_ribbon(data = bath_df1, aes(ymin = -Inf, ymax = depth, x = distance.from.coast), fill = "tan") +
  theme_classic() +
  scale_x_continuous(expand = c(0,0), limits = c(min_dist1, max(bath_df1$distance.from.coast))) +
  ylim(min(bath_df1$depth), 150) +
  labs(x = "Distance from coast (km)", y = "Elevation (m)") +
  geom_segment(data = paleo, aes(x = distance.from.coast, xend = distance.from.coast + 5,
                                 y = depth, yend = depth), linetype = 2, alpha = 0.5) +
  geom_text(data = paleo, aes(x = distance.from.coast + 7, y = depth, label = label), size = 3) +
  annotate(geom = "text", x = c(x = -35, 3), y = c(-10, 143), label = c("Naturaliste Reefs", "Cape Naturaliste"))

ggsave(filename = paste(paste0('plots/geographe/spatial/', name) , 'bathymetry-cross-section.png',
                        sep = "-"), plot = p5, units = "in", dpi = 600,
       bg = "white",
       width = 8, height = 4)
