###
# Project: NESP 4.20 - Marine Park Dashboard reporting
# Data:    Marine Park, oceanographic data, marine park boundary files
# Task:    Create South west network map
# Author:  Claude Spencer modified by Annika Leunig
# Date:    Feb 2026
###

# Table of contents
# 1. Overall location plot (including State and Commonwealth Marine Parks)

# Clear your environment
rm(list = ls())

# Set the study name and marine park name (for folder structure)
name <- "south-west"
park <- "network"

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

# Set cropping extent - larger than most zoomed out plot (all of aus for this one)
e <- ext(106.0, 145.0, -45.0, -22.0)
# e <- ext(ext(110, 155, -45, -10)) # Inset map uses this extent

# Load necessary spatial files
sf_use_s2(T)
# Australian outline and state and commonwealth marine parks
aus    <- st_read("data/south-west network/spatial/shapefiles/STE_2021_AUST_GDA2020.shp") %>%
  st_make_valid()
ausc <- st_crop(aus, e)

# Load marine parks
capad <- st_read("data/south-west network/spatial/shapefiles/Collaborative_Australian_Protected_Areas_Database_(CAPAD)_2022_-_Marine.shp")

# All australian marine parks - for inset plotting
aus_marine_parks <- st_read("data/south-west network/spatial/shapefiles/south-and-western-australia_marine-parks-all.shp")

# NEED TO CHECK MIGHT BE MISSING SOME
marine_parks <- st_read("data/south-west network/spatial/shapefiles/south-and-western-australia_marine-parks-all.shp") %>%
  dplyr::filter(name %in% c("Abrolhos", "Abrolhos Islands", "Bremer", "Eastern Recherche", "Ngari Capes", "Geographe",
                            "South-west Corner","Great Australian Bight", "Jurien","Murat", "Jurien Bay", "Perth Canyon",
                            "Southern Kangaroo Island", "Twilight", "Two Rocks", "Western Eyre", "Western Kangaroo Island",
                            "Nuyts Archipelgo", "Thorny Passage", "Sir Joseph Banks Group", "Investigator", "West coast Bays",
                            "Southern Spencer Gulf", "Upper Spencer Gulf", "Cottesloe Reef", "Rottnest", "Shoalwater Islands")) %>%
  glimpse()

# Standardize naming between Western and South Australia State marine parks

# Australian Marine Parks only (for separate ggplot legends)
marine_parks_amp <- marine_parks %>%
  dplyr::filter(epbc %in% "Commonwealth")

# State Marine Parks only (for separate ggplot legends)
marine_parks_state <- marine_parks %>%
  dplyr::filter(epbc %in% "State")

marine_parks_state <- marine_parks %>%
  dplyr::filter(epbc %in% "State") %>%
  dplyr::mutate(zone = case_when(
    zone == "Reef Observation Area" ~ "Sanctuary Zone",
    zone == "National Park Zone" ~ "Sanctuary Zone",
    zone == "Habitat Protection Zone" ~ "Recreational Use Zone",
    TRUE ~ zone
  ))

unique(marine_parks_state$zone)

# Terrestrial parks
terrnp <- st_read("data/south-west network/spatial/shapefiles/Legislated_Lands_and_Waters_DBCA_011.shp") %>%  # Terrestrial reserves
  dplyr::filter(leg_catego %in% c("Nature Reserve", "National Park"))
plot(terrnp["leg_catego"])

terr_fills <- scale_fill_manual(values = c("National Park" = "#c4cea6",          # Set the colours for terrestrial parks
                                           "Nature Reserve" = "#e4d0bb"),
                                name = "Terrestrial Parks")


# Coastal waters limit
cwatr <- st_read("data/south-west network/spatial/shapefiles/amb_coastal_waters_limit.shp") %>%
  st_make_valid() %>%
  st_crop(e)

# Bathymetry data
bathy <- rast("data/south-west network/spatial/rasters/AusBathyTopo__Australia__2024_250m_MSL_cog.tif") %>%
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

# 1. Plot map
# Set plot inputs
plot_limits = c(108.0, 138.0, -40.0, -24.0) # Extent of the main plot
study_limits = NULL # Extent of sampling
annotation_labels = NULL

network_map <- function(plot_limits, study_limits, annotation_labels) {
  require(tidyverse)
  require(tidyterra)
  require(patchwork)
  require(cowplot)

  terr_fills_ordered <- scale_fill_manual(values = c("National Park" = "#c4cea6",
                                                     "Nature Reserve" = "#e4d0bb"),
                                          name = "Terrestrial Parks",
                                          guide = guide_legend(order = 2))

  p1 <- ggplot() +
    geom_spatraster_contour_filled(data = bathy,
                                   breaks = c(0, -30, -70, -200, - 700, -2000 , -4000, -6000),
                                   colour = NA, show.legend = F, maxcell = 5e6) +
    scale_fill_manual(values = c("#FFFFFF", "#EFEFEF", "#DEDEDE", "#CCCCCC", "#B6B6B6", "#9E9E9E", "#808080")) +
    new_scale_fill() +
    geom_spatraster_contour(data = bathy,
                            breaks = c(-30, -70, -200, - 700, -2000 , -4000, -6000), colour = "white",
                            alpha = 3/5, linewidth = 0.1, show.legend = F, maxcell= 5e6) +
    geom_sf(data = ausc, fill = "seashell2", colour = "grey80", linewidth = 0.1) +
    geom_sf(data = marine_parks_amp, aes(fill = zone), colour = NA, alpha = 0.8) +
    scale_fill_manual(name = "Australian Marine Parks",
                      guide = guide_legend(order = 1),
                      values = with(marine_parks_amp, setNames(colour, zone)),
                      breaks = c("National Park Zone", "Habitat Protection Zone", "Multiple Use Zone", "Special Purpose Zone")) +
    new_scale_fill() +
    geom_sf(data = terrnp, aes(fill = leg_catego), colour = NA, alpha = 0.8) +
    terr_fills_ordered +
    new_scale_fill() +
    geom_sf(data = marine_parks_state, aes(fill = zone), colour = NA, alpha = 0.6) +
    scale_fill_manual(name = "State Marine Parks",
                      guide = guide_legend(order = 3),
                      values = with(marine_parks_state, setNames(colour, zone)),
                      breaks = c("Sanctuary Zone", "General Use Zone", "Recreational Use Zone", "Special Purpose Zone",
                                 "Other State Marine Park Zone")) +
    new_scale_fill() +
    geom_sf(data = cwatr, colour = "firebrick", alpha = 1, linewidth = 0.1, lineend = "round") +
    labs(x = NULL, y = NULL) +
    {if(!is.null(annotation_labels))
      list(
        geom_point(data = annotation_labels,
                   aes(x = x, y = y),
                   shape = 4, size = 1, stroke = 0.5, colour = "black"),
        geom_text(data = annotation_labels,
                  aes(x = x, y = y, label = label),
                  size = 1.65, fontface = "italic", nudge_y = -0.03)
      )} +
    {if(!is.null(study_limits))
      annotate("rect", xmin = study_limits[1], xmax = study_limits[2],
               ymin = study_limits[3], ymax = study_limits[4],
               fill = NA, colour = "goldenrod2", linewidth = 0.4)} +
    coord_sf(xlim = c(plot_limits[1], plot_limits[2]), ylim = c(plot_limits[3], plot_limits[4]), crs = 4326) +
    theme_minimal() +
    theme(legend.key.size = unit(0.5, "cm"),
          legend.text = element_text(size = 8),
          legend.title = element_text(size = 10),
          legend.position = "bottom",
          legend.box = "horizontal",
          legend.direction = "vertical") +
    guides(fill = guide_legend(ncol = 1))
  # Inset - full australia (ONLY USE IF YOU WANT INSET)
   p1.1 <- ggplot(data = aus) +
    geom_sf(fill = "seashell1", colour = "grey90", linewidth = 0.05, alpha = 4/5) +
    geom_sf(data = capad, alpha = 5/6, colour = "grey85", linewidth = 0.02) +
    coord_sf(xlim = c(105, 160), ylim = c(-48, -8)) +
    annotate("rect", xmin = plot_limits[1], xmax = plot_limits[2], ymin = plot_limits[3], ymax = plot_limits[4],
             colour = "grey25", fill = "white", alpha = 1/5, linewidth = 0.2) +
    theme_bw() +
    theme(axis.text = element_blank(),
          axis.ticks = element_blank(),
          panel.grid.major = element_blank(),
          panel.border = element_rect(colour = "grey70"))

  # Lines below change where the inset is
  ## Top right inset (inside map)
  # p1 + inset_element(p1.1, left = 0.7, bottom = 0.7, right = 1, top = 1)
  #
  # Side by side (inset left, map right)
  # p1.1 + p1
  #
  # Bottom left inset (inside map)
  # p1 + inset_element(p1.1, left = 0, bottom = 0, right = 0.2, top = 0.25)
   legend <- cowplot::get_legend(p1 + theme(
     legend.text = element_text(size = 7),
     legend.title = element_text(size = 8),
     legend.key.size = unit(0.3, "cm")
   ))

   p1_no_legend <- p1 + theme(legend.position = "none",
                              plot.margin = margin(0, 0, 15, 0))

   (p1_no_legend) / (plot_spacer() + p1.1 + legend + plot_spacer() + plot_layout(widths = c(0.139, 0.3, 1, 0.08))) +
     plot_layout(heights = c(4, 1))
}

network_map(plot_limits,
            study_limits,
            annotation_labels)

# Save plot
ggsave(paste(paste0('plots/', park, '/spatial/', name) , 'bottom-inset_network-plot.png',
             sep = "-"), dpi = 600, width = 8, height = 5, bg = "white")


# -END-------------------------------------------------------------------------#
