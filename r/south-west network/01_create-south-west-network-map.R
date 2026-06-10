###
# Project: NESP 5.6 Project - South west Corner Report
# Data:    Marine Park, oceanographic data, marine park boundary files and
#          image legend for bathy network map, cropped to have no title
#          This image can be found and saved from this link:
#          https://geoserver.imas.utas.edu.au/geoserver/seamap/wms?SERVICE=WMS&VERSION=1.1.1&REQUEST=GetLegendGraphic&LAYER=seamap:bathymetry_AMP_grp&FORMAT=image/png
# Task:    Create South west network map
# Author:  Annika Leunig (modified from Claude Spencer's code)
# Date:    Feb 2026
# Outputs: 1. South-west network zones map (overall location plot, with inset and legend)
#          2. South-west Corner zoom-in map
###

# Table of contents
#     1.  Set up and load data
#     2.  Standardise State marine park zones
#     3.  Plot inputs
#     4.  Map function
#     5.  FIGURE 1: South-west network zones map
#     6.  Zoom-in set up and map function
#     7.  FIGURE 2: South-west Corner zoom-in map


# ==============================================================================
# 1. SET UP AND LOAD DATA
# ==============================================================================

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

# ── Load spatial files ────────────────────────────────────────────────────────
sf_use_s2(T)
# Australian outline and state and commonwealth marine parks
aus    <- st_read("data/south-west network/spatial/shapefiles/STE_2021_AUST_GDA2020.shp") %>%
  st_make_valid()
ausc <- st_crop(aus, e)

#Add terrestrial parks in
terrnp <- st_read("data/south-west network/spatial/shapefiles/Collaborative_Australian_Protected_Areas_Database_(CAPAD)_2024_-_Terrestrial__.shp") %>%  # Terrestrial reserves
  dplyr::filter(TYPE %in% c("Nature Reserve", "National Park"))

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
bathdf <- as.data.frame(bathy, xy = T)

# Filter for SWC network
marine_parks <- st_read("data/south-west network/spatial/shapefiles/south-and-western-australia_marine-parks-all.shp") %>%
  dplyr::filter(name %in% c("Abrolhos", "Abrolhos Islands", "Bremer", "Eastern Recherche", "Ngari Capes", "Geographe",
                            "South-west Corner","Great Australian Bight", "Jurien","Murat", "Jurien Bay", "Perth Canyon",
                            "Southern Kangaroo Island", "Twilight", "Two Rocks", "Western Eyre", "Western Kangaroo Island",
                            "Nuyts Archipelgo", "Thorny Passage", "Sir Joseph Banks Group", "Investigator", "West coast Bays",
                            "Southern Spencer Gulf", "Upper Spencer Gulf", "Cottesloe Reef", "Rottnest", "Shoalwater Islands")) %>%
  glimpse()

# ==============================================================================
# 2. STANDARDISE STATE MARINE PARK ZONES
# ==============================================================================
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

# check zone names
unique(marine_parks_state$zone)

# ==============================================================================
# 3. PLOT INPUTS
# ==============================================================================
plot_limits = c(108.0, 138.0, -40.0, -24.0) # Extent of the main plot
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
  # Inset
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

# ==============================================================================
# 5. FIGURE 1: SOUTH-WEST NETWORK ZONES MAP
# ==============================================================================
network_map(plot_limits,
            study_limits,
            annotation_labels)

# Save plot
ggsave(paste(paste0('plots/', park, '/spatial/', name) , 'network_zones.png',
             sep = "-"), dpi = 600, width = 8, height = 5, bg = "white")

# ==============================================================================
# 6. ZOOM-IN SET UP AND MAP FUNCTION
# ==============================================================================
# Zoom in plots
marine_parks_wa <- st_read("data/south-west network/spatial/shapefiles/western-australia_marine-parks-all.shp") %>%
  dplyr::filter(name %in% c(
    "Abrolhos", "Abrolhos Islands",
    "Bremer",
    "Ngari Capes",
    "Geographe",
    "South-west Corner",
    "Jurien", "Jurien Bay",
    "Murat",
    "Perth Canyon",
    "Twilight",
    "Two Rocks",
    "West coast Bays",
    "Cottesloe Reef",
    "Rottnest",
    "Shoalwater Islands"
  ))

marine_parks_amp <- marine_parks_wa %>%
  dplyr::filter(epbc %in% "Commonwealth")

marine_parks_state <- marine_parks %>%
  dplyr::filter(epbc %in% "State") %>%
  dplyr::mutate(zone = case_when(
    zone == "Reef Observation Area"   ~ "Sanctuary Zone",
    zone == "National Park Zone"      ~ "Sanctuary Zone",
    zone == "Habitat Protection Zone" ~ "Recreational Use Zone",
    TRUE                              ~ zone
  )) %>%
  dplyr::mutate(colour = case_when(
    zone == "Other State Marine Park Zone" ~ "#f7d0dc",
    TRUE                                   ~ colour
  ))

network_map_zoomed <- function(plot_limits, study_limits = NULL, annotation_labels = NULL) {
  require(tidyverse)
  require(tidyterra)
  require(patchwork)
  require(cowplot)

  # Dynamically filter marine parks to those within the plot extent
  extent_box <- st_bbox(c(xmin = plot_limits[1], xmax = plot_limits[2],
                          ymin = plot_limits[3], ymax = plot_limits[4]),
                        crs = st_crs(4326)) %>%
    st_as_sfc()

  mp_amp_zoom <- marine_parks_amp %>%
    dplyr::filter(st_intersects(geometry, extent_box, sparse = FALSE)[,1])

  mp_state_zoom <- marine_parks_state %>%
    dplyr::filter(st_intersects(geometry, extent_box, sparse = FALSE)[,1])

  terr_fills_ordered <- scale_fill_manual(values = c("National Park" = "#c4cea6",
                                                     "Nature Reserve" = "#e4d0bb"),
                                          name = "Terrestrial Parks",
                                          guide = guide_legend(order = 2))

  p1 <- ggplot() +
    geom_spatraster_contour_filled(data = bathy,
                                   breaks = c(0, -30, -70, -200, -700, -2000, -4000, -6000),
                                   colour = NA, show.legend = F, maxcell = 5e6) +
    scale_fill_manual(values = c("#FFFFFF", "#EFEFEF", "#DEDEDE", "#CCCCCC", "#B6B6B6", "#9E9E9E", "#808080")) +
    new_scale_fill() +
    geom_spatraster_contour(data = bathy,
                            breaks = c(-30, -70, -200, -700, -2000, -4000, -6000), colour = "white",
                            alpha = 3/5, linewidth = 0.1, show.legend = F, maxcell = 5e6) +
    geom_sf(data = ausc, fill = "seashell2", colour = "grey80", linewidth = 0.1) +
    geom_sf(data = mp_amp_zoom, aes(fill = zone), colour = NA, alpha = 0.8) +
    scale_fill_manual(name = "Australian Marine Parks",
                      guide = guide_legend(order = 1),
                      values = with(mp_amp_zoom, setNames(colour, zone)),
                      breaks = c("National Park Zone", "Habitat Protection Zone", "Multiple Use Zone", "Special Purpose Zone")) +
    new_scale_fill() +
    geom_sf(data = terrnp, aes(fill = TYPE), colour = NA, alpha = 0.8) +
    terr_fills_ordered +
    new_scale_fill() +
    geom_sf(data = mp_state_zoom, aes(fill = zone), colour = NA, alpha = 0.6) +
    scale_fill_manual(name = "State Marine Parks",
                      guide = guide_legend(order = 3),
                      values = c(with(marine_parks_state, setNames(colour, zone))),
                      breaks = c("Sanctuary Zone", "General Use Zone", "Recreational Use Zone", "Special Purpose Zone",
                                 "Other State Marine Park Zone"))+
    new_scale_fill() +
    geom_sf(data = cwatr, colour = "firebrick", alpha = 1, linewidth = 0.1, lineend = "round") +
    labs(x = NULL, y = NULL) +
    {if (!is.null(annotation_labels))
      list(
        geom_point(data = annotation_labels, aes(x = x, y = y),
                   shape = 4, size = 1, stroke = 0.5, colour = "black"),
        geom_text(data = annotation_labels, aes(x = x, y = y, label = label),
                  size = 1.65, fontface = "italic", nudge_y = -0.03)
      )} +
    {if (!is.null(study_limits))
      annotate("rect", xmin = study_limits[1], xmax = study_limits[2],
               ymin = study_limits[3], ymax = study_limits[4],
               fill = NA, colour = "goldenrod2", linewidth = 0.4)} +
    coord_sf(xlim = c(plot_limits[1], plot_limits[2]), ylim = c(plot_limits[3], plot_limits[4]), crs = 4326) +
    theme_minimal() +
    theme(legend.key.size  = unit(0.5, "cm"),
          legend.text      = element_text(size = 8),
          legend.title     = element_text(size = 10),
          legend.position  = "bottom",
          legend.box       = "horizontal",
          legend.direction = "vertical",
          panel.grid       = element_blank(),
          panel.background = element_rect(fill = "white", colour = NA),
          plot.background  = element_rect(fill = "white", colour = NA),
          panel.border     = element_rect(colour = "grey80", fill = NA, linewidth = 0.5),
          axis.ticks       = element_line(colour = "grey80", linewidth = 0.3)) +
    guides(fill = guide_legend(ncol = 1))

  p1.1 <- ggplot(data = aus) +
    geom_sf(fill = "seashell1", colour = "grey90", linewidth = 0.05, alpha = 4/5) +
    geom_sf(data = capad, alpha = 5/6, colour = "grey85", linewidth = 0.02) +
    coord_sf(xlim = c(108, 138), ylim = c(-40, -24)) +
    annotate("rect", xmin = plot_limits[1], xmax = plot_limits[2], ymin = plot_limits[3], ymax = plot_limits[4],
             colour = "grey25", fill = "white", alpha = 1/5, linewidth = 0.2) +
    theme_bw() +
    theme(axis.text        = element_blank(),
          axis.ticks       = element_blank(),
          panel.grid.major = element_blank(),
          panel.border     = element_rect(colour = "grey70"))

  legend <- cowplot::get_legend(p1 + theme(
    legend.text      = element_text(size = 7),
    legend.title     = element_text(size = 8),
    legend.key.size  = unit(0.3, "cm"),
    legend.box       = "horizontal",
    legend.direction = "vertical"
  ))

  p1_no_legend <- p1 + theme(legend.position = "none",
                             plot.margin    = margin(0, 0, 15, 0))

  (p1_no_legend) / (plot_spacer() + p1.1 + legend + plot_spacer() +
                      plot_layout(widths = c(0.139, 0.3, 1, 0.08))) +
    plot_layout(heights = c(4, 1))
}

# ==============================================================================
# 7. FIGURE 2: SOUTH-WEST CORNER ZOOM-IN MAP
# ==============================================================================
network_map_zoomed(plot_limits = c(113.5, 116.4, -34.7857, -33.2643))
ggsave(paste(paste0('plots/', park, '/spatial/', name), 'swc-MPs.png', sep = "-"),
       dpi = 600, width = 8, height = 5, bg = "white")

# ==============================================================================
# End of script
# ==============================================================================
