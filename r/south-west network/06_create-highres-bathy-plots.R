###
# Project: NESP 5.6 - South west corner report
# Data:    Geographe Bay and SWC LiDAR and multibeam, marine park shapefiles
# Task:    Plot high resolution bathymetry data for geographe and SWC
# Author:  Annika Leunig
# Date:    March 2026
# Outputs: 1. Geographe Bay LiDAR map
#          2. South-west Corner LiDAR & multibeam map
#          3. Capel River zoom (Geographe LiDAR + hillshade)
#          4. Gorbiliyup & Blackwood River zoom (SWC LiDAR, multibeam + hillshade)
###

# Table of contents
#     1.  Set up and load data
#     2.  LiDAR map function
#     3.  FIGURE 1: Geographe Bay LiDAR map
#     4.  FIGURE 2: South-west Corner LiDAR & multibeam map
#     5.  Zoom-in set up (high res outline, hillshades, park layers)
#     6.  FIGURE 3: Capel River zoom (Geographe)
#     7.  FIGURE 4: Gorbiliyup & Blackwood River zoom (SWC)


# ==============================================================================
# 1. SET UP AND LOAD DATA
# ==============================================================================
rm(list = ls())

# Set study name
name <- "south-west"
park <- "network"

# Load libraries
library(sf)
library(terra)
library(tidyverse)
library(tidyterra)
library(ggnewscale)

# ── Load spatial files  ───────────────────────────────────────────────────────
sf_use_s2(TRUE)
# Terrestrial parks
terrnp <- st_read("data/south-west network/spatial/shapefiles/Legislated_Lands_and_Waters_DBCA_011.shp") %>%
  dplyr::filter(leg_catego %in% c("Nature Reserve", "National Park"))

# Aus outline
aus <- st_read("data/south-west network/spatial/shapefiles/STE_2021_AUST_GDA2020.shp") %>%
  st_make_valid()

# Marine parks
marine_parks <- st_read("data/south-west network/spatial/shapefiles/south-and-western-australia_marine-parks-all.shp") %>%
  dplyr::filter(name %in% c("Abrolhos", "Abrolhos Islands", "Bremer", "Eastern Recherche", "Ngari Capes", "Geographe",
                            "South-west Corner", "Great Australian Bight", "Jurien", "Murat", "Jurien Bay", "Perth Canyon",
                            "Southern Kangaroo Island", "Twilight", "Two Rocks", "Western Eyre", "Western Kangaroo Island",
                            "Nuyts Archipelgo", "Thorny Passage", "Sir Joseph Banks Group", "Investigator", "West coast Bays",
                            "Southern Spencer Gulf", "Upper Spencer Gulf", "Cottesloe Reef", "Rottnest", "Shoalwater Islands"))


# ── Load rasters and highres bathy data   ─────────────────────────────────────
# LiDAR
lidar_geo_raw  <- rast("data/south-west network/spatial/rasters/Geographe-bay_lidar.tif")
lidar_geo      <- project(lidar_geo_raw, "EPSG:7844", method = "bilinear")

lidar_swc_raw  <- rast("data/south-west network/spatial/rasters/DoT_south-coastal-lidar.tif")
lidar_swc      <- -lidar_swc_raw

# Multibeam
multibeam_raw  <- rast("data/south-west network/spatial/rasters/south-west-corner_merged-multibeam.tiff")
multibeam <- multibeam_raw[["Depth"]]
multibeam      <- project(multibeam, "EPSG:7844", method = "bilinear")
multibeam      <- clamp(multibeam, upper = 0, values = FALSE)
names(multibeam) <- "depth"

# Crop to extents (saves time loading)
e_geo <- ext(114.8, 116.0, -33.8, -33.25)
e_swc <- ext(114.2, 115.3, -34.7, -33.5)

lidar_geo_crop    <- crop(lidar_geo,  e_geo)
lidar_swc_crop    <- crop(lidar_swc,  e_swc)
multibeam_crop    <- crop(multibeam,  e_swc)

# ==============================================================================
# 2. LIDAR MAP FUNCTION
# ==============================================================================
make_lidar_map <- function(lidar_rast, xlim, ylim) {
  lidar_rast <- clamp(lidar_rast, upper = 0, values = FALSE)
  names(lidar_rast) <- "depth"
  ggplot() +
    geom_spatraster(data = lidar_rast, aes(fill = depth)) +
    scale_fill_viridis_c(
      option   = "viridis",
      na.value = NA,
      name     = "Depth (m)",
      limits   = c(-30, -15),
      oob      = scales::squish,
      breaks   = c(-15, -20, -25, -30),
      labels   = c("-15", "-20", "-25", "-30"),
      guide    = guide_colorbar(
        barwidth       = 1.5,
        barheight      = 8,
        title.position = "top",
        ticks          = TRUE,
        order          = 1
      )
    ) +
    geom_sf(data = aus, fill = "seashell2", colour = "grey80", linewidth = 0.1) +
    new_scale_fill() +
    geom_sf(data = terrnp, aes(fill = leg_catego), colour = NA, alpha = 0.8) +
    scale_fill_manual(
      values = c("National Park"  = "#c4cea6",
                 "Nature Reserve" = "#e4d0bb"),
      name   = "Terrestrial Parks",
      guide  = guide_legend(
        order          = 2,
        title.position = "top"
      )
    ) +
    geom_sf(data = marine_parks, fill = NA, colour = "grey30", linewidth = 0.4) +
    coord_sf(xlim = xlim, ylim = ylim, expand = FALSE) +
    labs(x = NULL, y = NULL) +
    theme_minimal() +
    theme(
      legend.position    = "right",
      legend.title       = element_text(size = 14),
      legend.text        = element_text(size = 13),
      axis.title         = element_text(size = 15),
      axis.text          = element_text(size = 13),
      panel.grid.major   = element_blank(),
      panel.grid.minor   = element_blank(),
      panel.background   = element_rect(fill = "white", colour = NA),
      panel.border       = element_rect(fill = NA, colour = "grey60", linewidth = 0.4)
    )
}

# ==============================================================================
# 3. FIGURE 1: GEOGRAPHE BAY LIDAR MAP
# ==============================================================================
p_lidar_geo <- make_lidar_map(lidar_geo_crop,
                              xlim = c(114.9, 115.75),
                              ylim = c(-33.7, -33.25))
print(p_lidar_geo)
ggsave(paste(paste0('plots/', park, '/spatial/bathymetry/', name), 'geographe_lidar.png', sep = "-"),
       plot = p_lidar_geo, dpi = 600, width = 10, height = 6, bg = "white")

# ==============================================================================
# 4. FIGURE 2: SOUTH-WEST CORNER LIDAR & MULTIBEAM MAP
# ==============================================================================
# SWC lidar and multibeam plot has to be plotted seperately to include both layers
# ── Set up and function ───────────────────────────────────────────────────────
lidar_swc_plot <- clamp(lidar_swc_crop, upper = 0, values = FALSE)
names(lidar_swc_plot) <- "depth2"

# Get dynamic limits from both datasets combined
mm_lidar     <- minmax(lidar_swc_plot)
mm_multibeam <- minmax(multibeam_crop)
depth_min    <- floor(min(mm_lidar[1], mm_multibeam[1]))

marine_parks_swc <- marine_parks %>%
  dplyr::filter(name %in% c("South-west Corner", "Ngari Capes")) %>%
  dplyr::mutate(colour = case_when(
    zone == "Special Purpose Zone" ~ "#ffb6c1",
    TRUE ~ colour
  ))

make_swc_map <- function(xlim, ylim) {
  ggplot() +
    geom_spatraster(data = multibeam_crop, aes(fill = depth)) +
    scale_fill_viridis_c(
      option   = "viridis",
      na.value = NA,
      name     = "Depth (m)",
      limits   = c(depth_min, 0),
      oob      = scales::squish,
      guide    = guide_colorbar(
        barwidth       = 1.5,
        barheight      = 8,
        title.position = "top",
        ticks          = TRUE,
        order          = 1
      )
    ) +
    new_scale_fill() +
    geom_spatraster(data = lidar_swc_plot, aes(fill = depth2)) +
    scale_fill_viridis_c(
      option   = "viridis",
      na.value = NA,
      name     = "Depth (m)",
      limits   = c(depth_min, 0),
      oob      = scales::squish,
      guide    = "none"
    ) +
    new_scale_fill() +
    geom_sf(data = marine_parks_swc, aes(fill = zone, colour = zone),
            linewidth = 0.3, alpha = 0.2) +
    scale_fill_manual(values = with(marine_parks_swc, setNames(colour, zone)),
                      name = "Zone",
                      guide = "none") +
    scale_colour_manual(values = with(marine_parks_swc, setNames(colour, zone)),
                        guide = "none") +
    geom_sf(data = aus, fill = "seashell2", colour = "grey80", linewidth = 0.1) +
    new_scale_fill() +
    geom_sf(data = terrnp, aes(fill = leg_catego), colour = NA, alpha = 0.8) +
    scale_fill_manual(
      values = c("National Park"  = "#c4cea6",
                 "Nature Reserve" = "#e4d0bb"),
      name   = "Terrestrial Parks",
      guide  = guide_legend(
        order          = 2,
        title.position = "top"
      )
    ) +
    coord_sf(xlim = xlim, ylim = ylim, expand = FALSE) +
    labs(x = NULL, y = NULL) +
    theme_minimal() +
    theme(
      legend.position    = "right",
      legend.title       = element_text(size = 14),
      legend.text        = element_text(size = 13),
      axis.title         = element_text(size = 15),
      axis.text          = element_text(size = 13),
      panel.grid.major   = element_blank (),
      panel.grid.minor   = element_blank(),
      panel.background   = element_rect(fill = "white", colour = NA),
      panel.border       = element_rect(fill = NA, colour = "grey60", linewidth = 0.4)
    )
}

# ── Call and save ─────────────────────────────────────────────────────────────
p_lidar_swc <- make_swc_map(xlim = c(114.2, 115.8),
                            ylim = c(-34.7, -33.45))

ggsave(paste(paste0('plots/', park, '/spatial/bathymetry/', name), 'corner_lidar-multibeam.png', sep = "-"),
       plot = p_lidar_swc, dpi = 600, width = 10, height = 8, bg = "white")

# ==============================================================================
# 5. ZOOM-IN SET UP (HIGH RES OUTLINE, HILLSHADES, PARK LAYERS)
# ==============================================================================
# ── Load highres outline  ─────────────────────────────────────────────────────
# High resolution Australian outline
e_aus_hr <- ext(113.0, 117.0, -35.7, -32.5)

aus_hr <- st_read("data/south-west network/spatial/shapefiles/AusOutline_HighRes.shp") %>%
  st_make_valid() %>%
  st_crop(st_bbox(c(xmin = 113.0, xmax = 117.0, ymin = -35.5, ymax = -32.5),
                  crs = st_crs(4283)))

# ── Compute Hillshade ─────────────────────────────────────────────────────────
# Geographe
slope_geo  <- terrain(lidar_geo_crop, v = "slope",  unit = "radians")
aspect_geo <- terrain(lidar_geo_crop, v = "aspect", unit = "radians")
hill_geo   <- shade(slope_geo, aspect_geo, angle = 40, direction = 270)
names(hill_geo) <- "hillshade"

# SWC
slope_swc  <- terrain(lidar_swc_crop, v = "slope",  unit = "radians")
aspect_swc <- terrain(lidar_swc_crop, v = "aspect", unit = "radians")
hill_swc   <- shade(slope_swc, aspect_swc, angle = 40, direction = 270)
names(hill_swc) <- "hillshade"

# ── Standardise zones and colours ─────────────────────────────────────────────
# Geographe
marine_parks_cwlth_geo <- marine_parks %>%
  dplyr::filter(epbc == "Commonwealth", name %in% c("Geographe"))

marine_parks_state_geo <- marine_parks %>%
  dplyr::filter(epbc == "State", name %in% c("Geographe", "Ngari Capes")) %>%
  dplyr::mutate(zone = case_when(
    zone == "Reef Observation Area"   ~ "Sanctuary Zone",
    zone == "National Park Zone"      ~ "Sanctuary Zone",
    zone == "Habitat Protection Zone" ~ "Recreational Use Zone",
    TRUE                              ~ zone
  ))

# SWC
marine_parks_cwlth_swc <- marine_parks %>%
  dplyr::filter(epbc == "Commonwealth", name %in% c("South-west Corner"))

marine_parks_state_swc <- marine_parks %>%
  dplyr::filter(epbc == "State", name %in% c("South-west Corner", "Ngari Capes")) %>%
  dplyr::mutate(zone = case_when(
    zone == "Reef Observation Area"   ~ "Sanctuary Zone",
    zone == "National Park Zone"      ~ "Sanctuary Zone",
    zone == "Habitat Protection Zone" ~ "Recreational Use Zone",
    TRUE                              ~ zone
  )) %>%
  dplyr::filter(zone %in% c("Sanctuary Zone", "General Use Zone"))

# ==============================================================================
# 6. FIGURE 3: CAPEL RIVER ZOOM (GEOGRAPHE)
# =============================================================================
# Function
make_geo_zoom_map_hr <- function(xlim, ylim, annotations = NULL) {
  lidar_rast <- clamp(lidar_geo_crop, upper = 0, values = FALSE)
  names(lidar_rast) <- "depth"

  visible_zones <- function(mp) {
    mp %>%
      st_crop(st_bbox(c(xmin = xlim[1], xmax = xlim[2],
                        ymin = ylim[1], ymax = ylim[2]),
                      crs = st_crs(mp))) %>%
      pull(zone) %>% unique()
  }

  p <- ggplot() +
    geom_spatraster(data = hill_geo, aes(fill = hillshade),
                    alpha = 0.9, show.legend = FALSE) +
    scale_fill_gradient(low = "#1a1a2e", high = "#e8e8e8",
                        na.value = NA, guide = "none") +
    new_scale_fill() +
    geom_spatraster(data = lidar_rast, aes(fill = depth), alpha = 0.55) +
    scale_fill_viridis_c(
      option   = "viridis",
      na.value = NA,
      name     = "Depth (m)",
      limits   = c(-30, -15),
      oob      = scales::squish,
      breaks   = c(-15, -20, -25, -30),
      labels   = c("-15", "-20", "-25", "-30"),
      guide    = guide_colorbar(
        order          = 1,
        barwidth       = 1.5,
        barheight      = 8,
        title.position = "top",
        ticks          = TRUE
      )
    ) +
    geom_sf(data = aus_hr, fill = "seashell2", colour = "grey50", linewidth = 0.3) +
    new_scale_fill() +
    new_scale_colour() +
    geom_sf(data = marine_parks_cwlth_geo, aes(fill = zone, colour = zone),
            linewidth = 0.7, alpha = 0.3) +
    scale_fill_manual(values = with(marine_parks_cwlth_geo, setNames(colour, zone)),
                      name   = "Australian Marine Parks",
                      breaks = visible_zones(marine_parks_cwlth_geo),
                      guide  = guide_legend(
                        order        = 2,
                        override.aes = list(alpha = 1, colour = NA))) +
    scale_colour_manual(values = with(marine_parks_cwlth_geo, setNames(colour, zone)),
                        guide  = "none") +
    new_scale_fill() +
    new_scale_colour() +
    geom_sf(data = marine_parks_state_geo, aes(fill = zone, colour = zone),
            linewidth = 0.7, alpha = 0.3) +
    scale_fill_manual(values = with(marine_parks_state_geo, setNames(colour, zone)),
                      name   = "State Marine Parks",
                      breaks = visible_zones(marine_parks_state_geo),
                      guide  = guide_legend(
                        order        = 3,
                        override.aes = list(alpha = 1, colour = NA))) +
    scale_colour_manual(values = with(marine_parks_state_geo, setNames(colour, zone)),
                        guide  = "none") +
    new_scale_fill() +
    geom_sf(data = terrnp, aes(fill = leg_catego), colour = NA, alpha = 0.8) +
    scale_fill_manual(
      values = c("National Park"  = "#c4cea6",
                 "Nature Reserve" = "#e4d0bb"),
      name   = "Terrestrial Parks",
      guide  = guide_legend(
        order          = 4,
        title.position = "top"
      )
    ) +
    {if (!is.null(annotations))
      list(
        geom_segment(data = annotations,
                     aes(x = x, y = y, xend = label_x, yend = label_y),
                     colour = "black", linewidth = 0.4),
        geom_text(data = annotations,
                  aes(x = label_x, y = label_y, label = label, vjust = vjust, hjust = hjust),
                  colour = "black", size = 3)
      )} +
    coord_sf(xlim = xlim, ylim = ylim, expand = FALSE) +
    labs(x = NULL, y = NULL) +
    theme_minimal() +
    theme(
      legend.position    = "right",
      legend.title       = element_text(size = 12),
      legend.text        = element_text(size = 11),
      legend.key.size    = unit(0.5, "cm"),
      axis.title         = element_text(size = 15),
      axis.text          = element_text(size = 13),
      panel.grid.major   = element_blank(),
      panel.grid.minor   = element_blank(),
      panel.background   = element_rect(fill = "white", colour = NA),
      panel.border       = element_rect(fill = NA, colour = "grey60", linewidth = 0.4)
    )
  p
}

# Annotations
capel_annotations <- data.frame(
  label   = c("  Capel cut", " paleo wetland channel"),
  x       = c(115.545,               115.485),
  y       = c(-33.522,               -33.455),
  label_x = c(115.560,               115.50),
  label_y = c(-33.510,               -33.44),
  vjust   = c(-0.5,                    0),
  hjust   = c(0.5,                   0)
)

# ── Call and save ─────────────────────────────────────────────────────────────
p_capel_hr <- make_geo_zoom_map_hr(xlim = c(115.38, 115.6),
                                   ylim = c(-33.57, -33.4),
                                   annotations = capel_annotations)

ggsave(paste(paste0('plots/', park, '/spatial/bathymetry/', name), 'geographe_capel-zoom-lidar.png', sep = "-"),
       plot = p_capel_hr, dpi = 600, width = 8, height = 6, bg = "white")


# ==============================================================================
# 7. FIGURE 4: GORBILIYUP & BLACKWOOD RIVER ZOOM (SWC)
# ==============================================================================
# Function
make_swc_zoom_map_hr <- function(xlim, ylim, annotations = NULL) {

  lidar_rast <- clamp(lidar_swc_crop, upper = 0, values = FALSE)
  names(lidar_rast) <- "depth2"

  visible_zones <- function(mp) {
    bbox_sf <- st_as_sfc(st_bbox(c(xmin = xlim[1], xmax = xlim[2],
                                   ymin = ylim[1], ymax = ylim[2]),
                                 crs = st_crs(mp)))
    mp %>%
      st_filter(bbox_sf, .predicate = st_intersects) %>%
      pull(zone) %>% unique()
  }

  p <- ggplot() +
    geom_spatraster(data = hill_swc, aes(fill = hillshade),
                    alpha = 0.4, show.legend = FALSE) +
    scale_fill_gradient(low = "#1a1a2e", high = "#e8e8e8",
                        na.value = NA, guide = "none") +
    new_scale_fill() +
    geom_spatraster(data = multibeam_crop, aes(fill = depth), alpha = 1) +
    scale_fill_viridis_c(
      option   = "viridis",
      na.value = NA,
      name     = "Depth (m)",
      limits   = c(-100, 0),
      oob      = scales::squish,
      breaks   = c(0, -25, -50, -75, -100),
      labels   = c("0", "-25", "-50", "-75", "-100"),
      guide    = guide_colorbar(
        order          = 1,
        barwidth       = 1.5,
        barheight      = 8,
        title.position = "top",
        ticks          = TRUE
      )
    ) +
    new_scale_fill() +
    geom_spatraster(data = lidar_rast, aes(fill = depth2), alpha = 0.8) +
    scale_fill_viridis_c(
      option   = "viridis",
      na.value = NA,
      limits   = c(-100, 0),
      oob      = scales::squish,
      guide    = "none"
    ) +
    geom_sf(data = aus_hr, fill = "seashell2", colour = "grey30", linewidth = 0.3) +
    new_scale_fill() +
    new_scale_colour() +
    geom_sf(data = marine_parks_cwlth_swc, aes(fill = zone, colour = zone),
            linewidth = 0.7, alpha = 0.3) +
    scale_fill_manual(values = with(marine_parks_cwlth_swc, setNames(colour, zone)),
                      name   = "Australian Marine Parks",
                      breaks = visible_zones(marine_parks_cwlth_swc),
                      guide  = guide_legend(
                        order        = 2,
                        override.aes = list(alpha = 1, colour = NA))) +
    scale_colour_manual(values = with(marine_parks_cwlth_swc, setNames(colour, zone)),
                        guide  = "none") +
    new_scale_fill() +
    new_scale_colour() +
    geom_sf(data = marine_parks_state_swc, aes(fill = zone, colour = zone),
            linewidth = 0.7, alpha = 0.3) +
    scale_fill_manual(values = with(marine_parks_state_swc, setNames(colour, zone)),
                      name   = "State Marine Parks",
                      breaks = visible_zones(marine_parks_state_swc),
                      guide  = guide_legend(
                        order        = 3,
                        override.aes = list(alpha = 1, colour = NA))) +
    scale_colour_manual(values = with(marine_parks_state_swc, setNames(colour, zone)),
                        guide  = "none") +
    new_scale_fill() +
    geom_sf(data = terrnp, aes(fill = leg_catego), colour = NA, alpha = 0.8) +
    scale_fill_manual(
      values = c("National Park"  = "#c4cea6",
                 "Nature Reserve" = "#e4d0bb"),
      name   = "Terrestrial Parks",
      guide  = guide_legend(
        order          = 4,
        title.position = "top"
      )
    ) +
    geom_sf(data = aus_hr, fill = NA, colour = "grey30", linewidth = 0.3) +
    annotate("segment", x = 115.0, y = -34.5, xend = 114.97, yend = -34.48,
             colour = "black", linewidth = 0.4) +
    annotate("text", x = 115.0, y = -34.47, label = "Gorbiliyup",
             colour = "black", size = 3, fontface = "plain", hjust = 1) +
    annotate("segment", x = 115.17, y = -34.24, xend = 115.21, yend = -34.22,
             colour = "black", linewidth = 0.4) +
    annotate("text", x = 115.06, y = -34.24, label = "  Blackwood River",
             colour = "black", size = 3, fontface = "plain", hjust = 0, vjust = 0.5) +
    {if (!is.null(annotations))
      list(
        geom_segment(data = annotations,
                     aes(x = x, y = y, xend = label_x, yend = label_y),
                     colour = "black", linewidth = 0.4),
        geom_text(data = annotations,
                  aes(x = label_x, y = label_y, label = label),
                  colour = "black", size = 3,
                  hjust = 0)
      )} +
    scale_y_continuous(breaks = seq(-34.6, -34.2, by = 0.1)) +
    coord_sf(xlim = xlim, ylim = ylim, expand = FALSE) +
    labs(x = NULL, y = NULL) +
    theme_minimal() +
    theme(
      legend.position    = "right",
      legend.title       = element_text(size = 12),
      legend.text        = element_text(size = 11),
      legend.key.size    = unit(0.5, "cm"),
      axis.title         = element_text(size = 15),
      axis.text          = element_text(size = 13),
      panel.grid.major   = element_blank(),
      panel.grid.minor   = element_blank(),
      panel.background   = element_rect(fill = "white", colour = NA),
      panel.border       = element_rect(fill = NA, colour = "grey60", linewidth = 0.4)
    )
  p
}

# ── Call and save ─────────────────────────────────────────────────────────────
p_gorbiliyup_hr <- make_swc_zoom_map_hr(xlim = c(114.8, 115.4),
                                       ylim = c(-34.65, -34.2))

ggsave(paste(paste0('plots/', park, '/spatial/bathymetry/', name), 'corner_gorbiliyup-zoom-lidar.png', sep = "-"),
       plot = p_gorbiliyup_hr, dpi = 600, width = 9, height = 6, bg = "white")

# ==============================================================================
# End of script
# ==============================================================================
