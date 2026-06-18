###
# Project: NESP 5.6 Project - South west Corner Report
# Data:    CAPAD 2022 marine parks, Exclusive Economic Zone (Perth Treaty)
#          limits and AusBathyTopo 2024 250 m bathymetry/topography
# Task:    Create Australia-wide marine parks overview (national context) map
# Author:  Annika Leunig (modified from Claude Spencer's code)
# Date:    Feb 2026
# Outputs: 1. Australia-wide marine parks overview map (AMP zones, state marine
#             parks, bathymetry/topography hillshade and external territories)
###

# Table of contents
#     1.  Set up and load data
#     2.  Prepare marine park layers
#     3.  Bathymetry and topography hillshading
#     4.  Plot inputs
#     5.  FIGURE 1: Australia-wide marine parks overview map


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
library(terra)
library(CheckEM)
library(ggpattern)
library(ggnewscale)
library(scales)     # load AFTER terra so scales::rescale() wins
library(tidyterra)

# ── Load spatial files ──────────────────────────────────────────────────────
# CAPAD Australian Marine Parks (already in WGS 84)
# Parenthetical suffixes are stripped from zone_type, e.g.
# "Special Purpose Zone (Trawl)" -> "Special Purpose Zone"
marine.parks <- st_read("data/south-west network/spatial/shapefiles/Collaborative_Australian_Protected_Areas_Database_(CAPAD)_2022_-_Marine.shp") %>%
  clean_names() %>%
  dplyr::mutate(zone_type = str_replace_all(zone_type, "\\s*\\([^\\)]+\\)", "")) %>%
  glimpse()
unique(marine.parks$zone_type)

# Exclusive Economic Zone (Perth Treaty) limits
eez <- st_read("data/south-west network/spatial/shapefiles/Exclusive_Economic_Zone_(Perth_Treaty)_limits.shp") %>%
  glimpse()


# ==============================================================================
# 2. PREPARE MARINE PARK LAYERS
# ==============================================================================

# State marine parks: simplify to sanctuary vs. other state marine park
state.mps <- marine.parks %>%
  dplyr::filter(!epbc %in% "Commonwealth") %>%
  dplyr::mutate(sanctuary = if_else(str_detect(zone_type, "Sanctuary"),
                                    "Sanctuary Zone", "State Marine Park")) %>%
  glimpse()
unique(state.mps$zone_type)

# Australian (Commonwealth) marine parks: keep only the AMP zone vocabulary.
# This both drops the GBR / other-reserve vocabularies and guarantees every
# value present has a matching key in the manual scales below.
# NOTE: because the parentheses were stripped above, the "(Trawl)" and
# "(Mining Exclusion)" stripe keys in the scales never match - they are
# inert and kept only for reference.
amp_zones <- c(
  "Special Purpose Zone",
  "National Park Zone",
  "Habitat Protection Zone",
  "Recreational Use Zone",
  "Multiple Use Zone",
  "Sanctuary Zone"
)


fed.mps <- st_read("data/south-west network/spatial/shapefiles/Collaborative_Australian_Protected_Areas_Database_(CAPAD)_2022_-_Marine.shp") %>%
  clean_names() %>%
  mutate(zone_type = str_replace_all(zone_type, "\\s*\\([^\\)]+\\)", "")) %>%
  filter(epbc == "Commonwealth") %>%
  filter(zone_type %in% amp_zones) %>%
  glimpse()

sort(unique(fed.mps$zone_type))


# ==============================================================================
# 3. BATHYMETRY AND TOPOGRAPHY HILLSHADING
# ==============================================================================
# ── Bathymetry (sea floor only) ─────────────────────────────────────────────
bathy <- rast("data/south-west network/spatial/rasters/AusBathyTopo__Australia__2024_250m_MSL_cog.tif") %>%
  aggregate(fact = 10) %>%
  clamp(upper = 0, values = F)

slope  <- terrain(bathy, "slope", unit = "radians")
aspect <- terrain(bathy, "aspect", unit = "radians")
hillbath <- shade(slope, aspect, 10, 0)
names(hillbath) <- "shades"

# Hillshading needs a greyscale palette mapped to the shade values
pal_greys <- hcl.colors(1000, "Grays")

index <- hillbath %>%
  mutate(index_col = rescale(shades, to = c(1, length(pal_greys)))) %>%
  mutate(index_col = round(index_col)) %>%
  pull(index_col)
vector_colsbathy <- pal_greys[index]

# ── Topography (land only) ──────────────────────────────────────────────────
topo <- rast("data/south-west network/spatial/rasters/AusBathyTopo__Australia__2024_250m_MSL_cog.tif") %>%
  aggregate(fact = 10) %>%
  clamp(lower = 1, values = F)

slope  <- terrain(topo, "slope", unit = "radians")
aspect <- terrain(topo, "aspect", unit = "radians")
hill <- shade(slope, aspect, 30, 270)
names(hill) <- "shades"

index <- hill %>%
  mutate(index_col = rescale(shades, to = c(1, length(pal_greys)))) %>%
  mutate(index_col = round(index_col)) %>%
  pull(index_col)
vector_cols <- pal_greys[index]


# ==============================================================================
# 4. PLOT INPUTS
# ==============================================================================
plot_limits <- c(94.0, 170.0, -48.0, -9.0)

# Capital cities - label placed to one side of each point so the text never
# overlaps the marker. hjust = 0 puts the label to the right, hjust = 1 to the
# left; lab_x is the matching offset position.
cities <- data.frame(
  city  = c("Darwin", "Brisbane", "Sydney", "Canberra", "Adelaide", "Melbourne", "Perth"),
  x     = c(130.8444, 153.0260, 151.2093, 149.1310, 138.6007, 144.9631, 115.8617),
  y     = c(-12.4637, -27.4705, -33.8688, -35.2802, -34.9285, -37.8136, -31.9514),
  hjust = c(0,         0,         0,        0,         0,         1,         1)
)
cities$lab_x <- cities$x + ifelse(cities$hjust == 0, 0.7, -0.7)


# ==============================================================================
# 5. FIGURE 1: AUSTRALIA-WIDE MARINE PARKS OVERVIEW MAP
# ==============================================================================
p1 <- ggplot() +
  geom_spatraster(data = hillbath, fill = vector_colsbathy, maxcell = Inf,
                  alpha = 1) +
  geom_spatraster(data = bathy, show.legend = F, alpha = 0.6) +
  scale_fill_gradientn(colours = c("#061442", "#2b63b5", "#9dc9e1"),
                       values = rescale(c(-6221, -120, 0))) +
  new_scale_fill() +
  geom_sf(data = state.mps, aes(fill = sanctuary), colour = NA) +
  scale_fill_manual(values = c("Sanctuary Zone" = "#bfd054",
                               "State Marine Park" = "grey80"),
                    name = "State Marine Parks",
                    guide = guide_legend(ncol = 1, order = 2)) +
  new_scale_fill() +
  geom_sf(data = fed.mps,
          aes(fill = zone_type),
          colour = NA,
          alpha = 0.7) +
  scale_fill_manual(
    values = c(
      "Special Purpose Zone" = "#6daff4",
      "National Park Zone" = "#7bbc63",
      "Habitat Protection Zone" = "#fff8a3",
      "Recreational Use Zone" = "#ffb36b",
      "Multiple Use Zone" = "#b9e6fb",
      "Sanctuary Zone" = "#f7c0d8"
    ),
    name = "Australian Marine Parks",
    guide = guide_legend(ncol = 2, order = 1)
  ) +
  new_scale_fill() +
  geom_sf(data = eez, colour = "grey20", linetype = 2, fill = NA) +
  geom_spatraster(data = hill, alpha = 1, show.legend = F) +
  scale_fill_gradientn(colors = pal_greys, na.value = NA) +
  new_scale_fill() +
  geom_spatraster(data = topo, show.legend = F) +
  scale_fill_hypso_tint_c(palette = "dem_poster",
                          alpha = 0.6,
                          na.value = "transparent") +
  # ── Capital cities ──
  geom_point(data = cities, aes(x = x, y = y),
             shape = 9, size = 1) +
  geom_text(data = cities, aes(x = lab_x, y = y, label = city, hjust = hjust),
            size = 3) +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        legend.position = "bottom",
        legend.box = "horizontal",
        legend.direction = "vertical",
        legend.key.size = unit(0.3, "cm"),
        legend.key.spacing.y = unit(0.1, "cm"),
        legend.key.spacing.x = unit(0.2, "cm"),
        legend.text = element_text(size = 9),
        legend.title = element_text(size = 10),
        axis.text.x = element_text(size = 8),
        axis.text.y = element_text(size = 8),
        axis.title = element_blank(),
        axis.ticks = element_line(colour = "grey80", linewidth = 0.3),
        legend.margin = margin(0, 0, 0, 0)) +
  labs(x = NULL, y = NULL) +
  coord_sf(xlim = c(plot_limits[1], plot_limits[2]),
           ylim = c(plot_limits[3], plot_limits[4]))

# p1

# Save plot
ggsave(paste(paste0('plots/', park, '/spatial/'), 'australia-overview.png'),
       plot = p1, dpi = 600, width = 8, height = 6, bg = "white")

# ==============================================================================
# End of script
# ==============================================================================
