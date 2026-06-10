###
# Project: NESP 5.6 Project - South west Corner Report
# Data:    Marine parks, key ecological features, terrestrial parks, aus outline
# Task:    Create South-west network KEF map
# Author:  Annika Leunig
# Date:    April 2026
# Outputs: 1. South-west network KEF map
###

# Table of contents
#     1.  Set up and load data
#     2.  Recode and reorder KEF
#     3.  Colour palettes
#     4.  Plot inputs
#     5.  Map function
#     6.  FIGURE 1: South-west network KEF map


# ==============================================================================
# 1. LOAD DATA AND SETUP
# ==============================================================================
# Clear environment
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

# Set cropping extent
e <- ext(106.0, 145.0, -45.0, -22.0)

# Load necessary spatial files
sf_use_s2(T)

aus  <- st_read("data/south-west network/spatial/shapefiles/STE_2021_AUST_GDA2020.shp") %>%
  st_make_valid()
ausc <- st_crop(aus, e)

KEF <- st_read("data/south-west network/spatial/shapefiles/Marine_Key_Ecological_Features.shp") %>%
  st_make_valid() %>%
  st_crop(e)

capad <- st_read("data/south-west network/spatial/shapefiles/Collaborative_Australian_Protected_Areas_Database_(CAPAD)_2022_-_Marine.shp")

marine_parks <- st_read("data/south-west network/spatial/shapefiles/south-and-western-australia_marine-parks-all.shp") %>%
  dplyr::filter(name %in% c("Abrolhos", "Abrolhos Islands", "Bremer", "Eastern Recherche", "Ngari Capes", "Geographe",
                            "South-west Corner", "Great Australian Bight", "Jurien", "Murat", "Jurien Bay", "Perth Canyon",
                            "Southern Kangaroo Island", "Twilight", "Two Rocks", "Western Eyre", "Western Kangaroo Island",
                            "Nuyts Archipelgo", "Thorny Passage", "Sir Joseph Banks Group", "Investigator", "West coast Bays",
                            "Southern Spencer Gulf", "Upper Spencer Gulf", "Cottesloe Reef", "Rottnest", "Shoalwater Islands"))

marine_parks_amp <- marine_parks %>%
  dplyr::filter(epbc %in% "Commonwealth")


terrnp <- st_read("data/south-west network/spatial/shapefiles/Collaborative_Australian_Protected_Areas_Database_(CAPAD)_2024_-_Terrestrial__.shp") %>%  # Terrestrial reserves
  dplyr::filter(TYPE %in% c("Nature Reserve", "National Park"))


terr_fills <- scale_fill_manual(values = c("National Park" = "#c4cea6",          # Set the colours for terrestrial parks
                                           "Nature Reserve" = "#e4d0bb"),
                                name = "Terrestrial Parks")

bathy <- rast("data/south-west network/spatial/rasters/AusBathyTopo__Australia__2024_250m_MSL_cog.tif") %>%
  crop(e) %>%
  clamp(upper = 0, values = F)
names(bathy) <- "Depth"

# ==============================================================================
# 2. RECODE AND REORDER KEF
# ==============================================================================

KEF$NAME <- dplyr::recode(KEF$NAME,
                          "Perth Canyon and adjacent shelf break, and other west coast canyons"                   = "Perth Canyon",
                          "Commonwealth marine environment within and adjacent to the west coast inshore lagoons" = "West coast lagoons",
                          "Commonwealth marine environment within and adjacent to Geographe Bay"                  = "Geographe Bay",
                          "Cape Mentelle upwelling"                                                                = "Cape Mentelle",
                          "Albany Canyons group and adjacent shelf break"                                          = "Albany Canyons",
                          "Commonwealth marine environment surrounding the Recherche Archipelago"                  = "Recherche Archipelago",
                          "Ancient coastline at 90-120m depth"                                                     = "Ancient coastline",
                          "Western demersal slope and associated fish communities"                                 = "Western demersal fish",
                          "Commonwealth marine environment surrounding the Houtman Abrolhos Islands"              = "Abrolhos Islands",
                          "Commonwealth waters adjacent to Ningaloo Reef"                                         = "Ningaloo Reef",
                          "Kangaroo Island Pool, canyons and adjacent shelf break, and Eyre Peninsula upwellings" = "Kangaroo Island & Eyre Peninsula"
)

KEF <- KEF %>%
  mutate(plot_order = case_when(
    NAME == "Kangaroo Island &  Eyre Peninsula"  ~ 1,
    NAME == "Western rock lobster"               ~ 2,
    NAME == "Western demersal fish"              ~ 3,
    TRUE                                         ~ 4
  )) %>%
  arrange(plot_order) %>%
  select(-plot_order)

# ==============================================================================
# 3. COLOUR PALETTES
# ==============================================================================

kef_colours <- c(
  "Ningaloo Reef"                       = "#ffb6c1",
  "Wallaby Saddle"                      = "#8b0000",
  "Western demersal fish"               = "#3368d2",
  "Abrolhos Islands"                    = "#50f540",
  "Perth Canyon"                        = "#65d1d6",
  "Western rock lobster"                = "#add8e6",
  "West coast lagoons"                  = "#1a4a2a",
  "Geographe Bay"                       = "#f0a469",
  "Cape Mentelle"                       = "#8b1a1a",
  "Naturaliste Plateau"                 = "#ffff00",
  "Diamantina Fracture Zone"            = "#6a0dad",
  "Albany Canyons"                      = "#1a3a2a",
  "Recherche Archipelago"               = "#43c137",
  "Ancient coastline"                   = "#ff69b4",
  "Kangaroo Island & Eyre Peninsula"  = "#8b4513",
  "Bonney Coast Upwelling"              = "#4169e1"
)

# ==============================================================================
# 4. PLOT INPUTS
# ==============================================================================

plot_limits     <- c(108.0, 138.0, -40.0, -24.0)
annotation_labels <- NULL

# ==============================================================================
# 5. MAP FUNCTION
# ==============================================================================

network_map <- function(plot_limits, annotation_labels = NULL) {
  require(tidyverse)
  require(tidyterra)
  require(patchwork)
  require(cowplot)

  p1 <- ggplot() +

    # Landmass
    geom_sf(data = ausc, fill = "seashell2", colour = "grey80", linewidth = 0.1) +

    # Key ecological features
    new_scale_fill() +
    geom_sf(data = KEF, aes(fill = NAME), colour = NA, alpha = 1) +
    scale_fill_manual(name   = "Key Ecological Features",
                      guide  = guide_legend(order = 1, ncol = 3,
                                            title.position = "top"),
                      values = kef_colours,
                      limits = c("Ningaloo Reef", "Wallaby Saddle", "Western demersal fish",
                                 "Abrolhos Islands", "Perth Canyon", "Western rock lobster",
                                 "West coast lagoons", "Geographe Bay", "Cape Mentelle",
                                 "Naturaliste Plateau", "Diamantina Fracture Zone", "Albany Canyons",
                                 "Recherche Archipelago", "Ancient coastline",
                                 "Kangaroo Island & Eyre Peninsula", "Bonney Coast Upwelling")) +

    # Terrestrial parks — no legend
    new_scale_fill() +
    geom_sf(data = terrnp, aes(fill = TYPE), colour = NA, alpha = 0.8) +
    scale_fill_manual(values = c("National Park" = "#c4cea6", "Nature Reserve" = "#e4d0bb"),
                      name   = "Terrestrial Parks",
                      guide  = guide_legend(order = 2, ncol = 1,
                                            title.position = "top")) +

    # Marine park boundaries only
    geom_sf(data = marine_parks_amp,   fill = NA, colour = "lightsteelblue3",  linewidth = 0.3, alpha = 0.5) +

    {if (!is.null(annotation_labels))
      list(
        geom_point(data = annotation_labels, aes(x = x, y = y),
                   shape = 4, size = 1, stroke = 0.5, colour = "black"),
        geom_text(data = annotation_labels, aes(x = x, y = y, label = label),
                  size = 1.65, fontface = "italic", nudge_y = -0.03)
      )} +

    coord_sf(xlim = c(plot_limits[1], plot_limits[2]),
             ylim = c(plot_limits[3], plot_limits[4]),
             crs  = 4326) +
    labs(x = NULL, y = NULL) +
    theme_minimal() +
    theme(legend.key.width    = unit(0.35, "cm"),
          legend.key.height   = unit(0.35, "cm"),
          legend.key.spacing.y = unit(0.05, "cm"),
          legend.spacing.y   = unit(0.01, "cm"),
          legend.text        = element_text(size = 8),
          legend.title       = element_text(size = 10),
          legend.position  = "bottom",
          legend.box       = "horizontal",
          legend.direction = "horizontal",
          panel.grid       = element_blank(),
          panel.background = element_rect(fill = "white", colour = NA),
          plot.background  = element_rect(fill = "white", colour = NA),
          panel.border     = element_rect(colour = "grey80", fill = NA, linewidth = 0.5),
          axis.ticks       = element_line(colour = "grey80", linewidth = 0.3))

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
    legend.text      = element_text(size = 7),
    legend.title     = element_text(size = 8),
    legend.key.width  = unit(0.3, "cm"),
    legend.key.height = unit(0.3, "cm"),
    legend.spacing.y  = unit(0.05, "cm"),
    legend.spacing.x  = unit(0.05, "cm"),
    legend.box        = "horizontal"
  ))

  p1_no_legend <- p1 + theme(legend.position = "none",
                             plot.margin    = margin(0, 0, 15, 0))

  (p1_no_legend) / (p1.1 + plot_spacer() + legend + plot_spacer() +
                      plot_layout(widths = c(0.3, 0.02, 1, 0.095))) +
    plot_layout(heights = c(4, 1))
}

# ==============================================================================
# 6. FIGURE 1: South-west network KEF map
# ==============================================================================

network_map(plot_limits)

ggsave(paste(paste0("plots/", park, "/spatial/", name), "KEF-TEST-1.png", sep = "-"),
       dpi = 600, width = 8, height = 6, bg = "white")

# ==============================================================================
# End of script
# ==============================================================================
