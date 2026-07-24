###
# Project: North Network Report
# Data:    Terrestrial CAPAD, bathy, marine park CAPAD, coastal waters boundary
# Task:    Create North network map
# Author:  Annika Leunig
# Date:    July2026
# Outputs: 1. North network zones map (overall location plot, with inset and legend)
###

# Table of contents
#     1.  Set up and load data
#     2.  Standardise State marine park zones
#     3.  Plot inputs
#     4.  Map function
#     5.  FIGURE 1: North network zones map

# ==============================================================================
# 1. SET UP AND LOAD DATA
# ==============================================================================

# Clear your environment
rm(list = ls())

# Set the study name and marine park name (for folder structure)
name <- "north"
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
e <- ext(120, 145.0, -20, -5)
# e <- ext(ext(110, 155, -45, -10)) # Inset map uses this extent

# ── Load spatial files ────────────────────────────────────────────────────────
sf_use_s2(T)
# Australian outline and state and commonwealth marine parks
aus    <- st_read("data/north network/spatial/shapefiles/STE_2021_AUST_GDA2020.shp") %>%
  st_make_valid()
ausc <- st_crop(aus, e)

# For inset
capad    <- st_read("data/north network/spatial/shapefiles/Collaborative_Australian_Protected_Areas_Database_(CAPAD)_2022_-_Marine.shp") %>%
  st_make_valid()

#Add terrestrial parks in
terrnp <- st_read("data/north network/spatial/shapefiles/Collaborative_Australian_Protected_Areas_Database_(CAPAD)_2024_-_Terrestrial__.shp") %>%  # Terrestrial reserves
  dplyr::filter(TYPE %in% c("Nature Reserve", "National Park"))

terr_fills <- scale_fill_manual(values = c("National Park" = "#c4cea6",          # Set the colours for terrestrial parks
                                           "Nature Reserve" = "#e4d0bb"),
                                name = "Terrestrial Parks")


# Coastal waters limit
cwatr <- st_read("data/north network/spatial/shapefiles/amb_coastal_waters_limit.shp") %>%
  st_make_valid() %>%
  st_crop(e)

# Bathymetry data
bathy <- rast("data/north network/spatial/rasters/AusBathyTopo__Australia__2024_250m_MSL_cog.tif") %>%
  crop(e) %>%
  clamp(upper = 0, values = F)
names(bathy) <- "Depth"
bathdf <- as.data.frame(bathy, xy = T)

# Extent for cropping MPAs - cuts off western extent at 126
e_mpa <- ext(126, 142.5, -18, -9)

# Filter for N network
marine_parks <- st_read("data/north network/spatial/shapefiles/north-network-australia_marine-parks-all.shp") %>%
  dplyr::filter(name %in% c("Arafura", "Arnhem", "Gulf of Carpentaria", "Joseph Bonaparte Gulf",
                            "Limmen", "Oceanic Shoals", "Wessel", "West Cape York","North Kimberley",
                            "Garig Gunak Barlu", "Limmen Bight", "Eight Mile Creek", "Morning Inlet - Bynoe River",
                            "Staaten-Gilbert", "Nassau River", "Pine River Bay",
                            "Dhimurru", "Thuwathu/Bujimulla", "Anindilyakwa", "Djelk - Stage 2", #IPAs
                            "Crocodile Islands Maringa")) %>% # IPA
  st_crop(e_mpa) %>%
  glimpse()

# ==============================================================================
# 2. STANDARDISE STATE MARINE PARK ZONES
# ==============================================================================
# Australian Marine Parks only (for separate ggplot legends)
marine_parks_amp <- marine_parks %>%
  dplyr::filter(epbc %in% "Commonwealth")

# Indigenous Protected Areas (in state waters) - kept separate from other state zones
ipa_names <- c("Dhimurru", "Thuwathu/Bujimulla", "Anindilyakwa", "Djelk - Stage 2", "Crocodile Islands Maringa")

# State Marine Parks only (for separate ggplot legends)
marine_parks_state <- marine_parks %>%
  dplyr::filter(epbc %in% "State") %>%
  dplyr::mutate(
    zone = case_when(
      name %in% ipa_names ~ "Indigenous Protected Area",
      zone == "Reef Observation Area" ~ "Sanctuary Zone",
      zone == "National Park Zone" ~ "Sanctuary Zone",
      zone == "Habitat Protection Zone" ~ "Recreational Use Zone",
      TRUE ~ zone
    ),
    colour = case_when(
      name %in% ipa_names ~ "#FFD8A8",
      zone == "Other State Marine Park Zone" ~ "#f7d0dc",
      TRUE ~ colour
    )
  )

# check zone names
unique(marine_parks_state$zone)

# ==============================================================================
# 3. PLOT INPUTS
# ==============================================================================
plot_limits = c(126, 142.5, -18, -9) # Extent of the main plot
study_limits = NULL # Extent of sampling
annotation_labels = NULL

# ==============================================================================
# 4. MAP FUNCTION
# ==============================================================================
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
    geom_sf(data = terrnp, aes(fill = TYPE), colour = NA, alpha = 0.8) +
    terr_fills_ordered +
    new_scale_fill() +
    geom_sf(data = marine_parks_state, aes(fill = zone), colour = NA, alpha = 0.6) +
    scale_fill_manual(name = "State and Territory Marine Parks",
                      guide = guide_legend(order = 3),
                      values = with(marine_parks_state, setNames(colour, zone)),
                      breaks = c("Sanctuary Zone", "General Use Zone", "Recreational Use Zone",
                                 "Special Purpose Zone", "Indigenous Protected Area",
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
          legend.direction = "vertical",
          panel.background = element_rect(fill = "white", colour = NA),
          plot.background = element_rect(fill = "white", colour = NA),
          panel.border = element_rect(colour = "grey80", fill = NA, linewidth = 0.5),
          axis.ticks = element_line(colour = "grey80", linewidth = 0.3)) +
    guides(fill = guide_legend(ncol = 1))
  # Inset
  p1.1 <- ggplot(data = aus) +
    geom_sf(fill = "seashell1", colour = "grey90", linewidth = 0.05, alpha = 4/5) +
    geom_sf(data = capad, alpha = 5/6, colour = "grey85", linewidth = 0.02) +
    annotate("rect", xmin = plot_limits[1], xmax = plot_limits[2],
             ymin  = plot_limits[3], ymax = plot_limits[4],
             colour = "grey25", fill = "white", alpha = 1/5, linewidth = 0.2) +
    coord_sf(xlim = c(105, 160), ylim = c(-48, -8)) +
    theme_bw() +
    theme(axis.text        = element_blank(),
          axis.ticks       = element_blank(),
          panel.grid.major = element_blank(),
          panel.border     = element_rect(colour = "grey70"))

  legend <- cowplot::get_legend(p1 + theme(
    legend.text = element_text(size = 7),
    legend.title = element_text(size = 8),
    legend.key.size = unit(0.3, "cm")
  ))

  p1_no_legend <- p1 + theme(legend.position = "none",
                             plot.margin = margin(0, 0, 15, 0))

  (p1_no_legend) / (p1.1 + plot_spacer() + legend + plot_spacer() +
                      plot_layout(widths = c(0.3, 0.02, 1, 0.095))) +
    plot_layout(heights = c(4, 1))
}

# ==============================================================================
# 5. FIGURE 1: NORTH NETWORK ZONES MAP
# ==============================================================================
network_map(plot_limits,
            study_limits,
            annotation_labels)

# Save plot
ggsave(paste(paste0('plots/', park, '/spatial/', name) , 'network_zones.png',
             sep = "-"), dpi = 600, width = 7.5, height = 5, bg = "white")

# ==============================================================================
# End of script
# ==============================================================================
