###
# Project: NESP 4.20 - Marine Park Dashboard reporting
# Data:    Geographe Bay LiDAR
# Task:    Plot Geographe LiDAR using same extent as original script
# Author:  Annika Leunig
# Date:    June 2026
###

rm(list = ls())

# Set study name (same as original)
name <- "south-west"
park <- "network"

# Load libraries
library(sf)
library(terra)
library(tidyverse)
library(tidyterra)
library(ggnewscale)

sf_use_s2(TRUE)

# --- Load spatial context layers (same as original) ---
terrnp <- st_read("data/south-west network/spatial/shapefiles/Legislated_Lands_and_Waters_DBCA_011.shp") %>%
  dplyr::filter(leg_catego %in% c("Nature Reserve", "National Park"))

aus <- st_read("data/south-west network/spatial/shapefiles/STE_2021_AUST_GDA2020.shp") %>%
  st_make_valid()

# --- Load marine parks (add after loading aus and terrnp) ---
marine_parks <- st_read("data/south-west network/spatial/shapefiles/south-and-western-australia_marine-parks-all.shp") %>%
  dplyr::filter(name %in% c("Abrolhos", "Abrolhos Islands", "Bremer", "Eastern Recherche", "Ngari Capes", "Geographe",
                            "South-west Corner", "Great Australian Bight", "Jurien", "Murat", "Jurien Bay", "Perth Canyon",
                            "Southern Kangaroo Island", "Twilight", "Two Rocks", "Western Eyre", "Western Kangaroo Island",
                            "Nuyts Archipelgo", "Thorny Passage", "Sir Joseph Banks Group", "Investigator", "West coast Bays",
                            "Southern Spencer Gulf", "Upper Spencer Gulf", "Cottesloe Reef", "Rottnest", "Shoalwater Islands"))

marine_parks_geo <- marine_parks %>% dplyr::filter(name %in% "Geographe")
marine_parks_swc <- marine_parks %>% dplyr::filter(name %in% "South-west Corner")

# --- Plot function ---
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
        ticks          = TRUE
      )
    ) +
    geom_sf(data = aus, fill = "seashell2", colour = "grey80", linewidth = 0.1) +
    new_scale_fill() +
    geom_sf(data = terrnp, aes(fill = leg_catego), colour = NA, alpha = 0.8) +
    scale_fill_manual(values = c("National Park" = "#c4cea6",
                                 "Nature Reserve" = "#e4d0bb"),
                      name = "Terrestrial Parks",
                      guide = "none") +
    # Geographe marine park - black outline only
    geom_sf(data = marine_parks, fill = NA, colour = "black", linewidth = 0.4) +
    coord_sf(xlim = xlim, ylim = ylim, expand = FALSE) +
    labs(x = "Longitude", y = "Latitude") +
    theme_minimal() +
    theme(
      legend.position    = "right",
      legend.title       = element_text(size = 14),
      legend.text        = element_text(size = 13),
      axis.title         = element_text(size = 15),
      axis.text          = element_text(size = 13),
      panel.grid.major   = element_line(colour = "grey85", linewidth = 0.3),
      panel.grid.minor   = element_blank(),
      panel.background   = element_rect(fill = "white", colour = NA),
      panel.border       = element_rect(fill = NA, colour = "grey40", linewidth = 0.4)
    )
}

# --- Load LiDAR data ---
lidar_geo_raw  <- rast("data/south-west network/spatial/rasters/Geographe-bay_lidar.tif")
lidar_geo      <- project(lidar_geo_raw, "EPSG:7844", method = "bilinear")

lidar_swc_raw  <- rast("data/south-west network/spatial/rasters/DoT_south-coastal-lidar.tif")
lidar_swc      <- -lidar_swc_raw

# --- Load multibeam - first layer only, reproject to match ---
multibeam_raw  <- rast("data/south-west network/spatial/rasters/south-west-corner_merged-multibeam.tiff")
multibeam <- multibeam_raw[["Depth"]]
multibeam      <- project(multibeam, "EPSG:7844", method = "bilinear")
multibeam      <- clamp(multibeam, upper = 0, values = FALSE)
names(multibeam) <- "depth"

# --- Crop to extents ---
e_geo <- ext(114.8, 116.0, -33.8, -33.25)
e_swc <- ext(114.2, 115.3, -34.7, -33.5)

lidar_geo_crop    <- crop(lidar_geo,  e_geo)
lidar_swc_crop    <- crop(lidar_swc,  e_swc)
multibeam_crop    <- crop(multibeam,  e_swc)

# --- Geographe plot via function ---
p_lidar_geo <- make_lidar_map(lidar_geo_crop,
                              xlim = c(114.9, 115.75),
                              ylim = c(-33.7, -33.25))
print(p_lidar_geo)
ggsave(paste(paste0('plots/', park, '/spatial/bathymetry/', name), 'geographe-lidar-plot.png', sep = "-"),
       plot = p_lidar_geo, dpi = 600, width = 10, height = 6, bg = "white")

## Just geographe marine park extent -------------------------------------------------------------------------------#
marine_parks_geo <- marine_parks %>% dplyr::filter(name %in% "Geographe")

marine_parks_geo_reproj <- st_transform(marine_parks_geo, crs(lidar_geo))

lidar_geo_mp <- crop(lidar_geo, vect(marine_parks_geo_reproj)) %>%
  mask(vect(marine_parks_geo_reproj))

# Get extent from the marine park boundary
geo_bbox <- st_bbox(marine_parks_geo)

p_lidar_geo_mp <- make_lidar_map(lidar_geo_mp,
                                 xlim = c(geo_bbox["xmin"], geo_bbox["xmax"]),
                                 ylim = c(geo_bbox["ymin"], geo_bbox["ymax"]))



# Get extent from the marine park boundary with a buffer
geo_bbox <- st_bbox(marine_parks_geo)
buf <- 0.03

lidar_geo_mp_plot <- clamp(lidar_geo_mp, upper = 0, values = FALSE)
names(lidar_geo_mp_plot) <- "depth"

p_lidar_geo_mp <- ggplot() +
  geom_spatraster(data = lidar_geo_mp_plot, aes(fill = depth)) +
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
      ticks          = TRUE
    )
  ) +
  geom_sf(data = marine_parks_geo, fill = NA, colour = "black", linewidth = 0.4) +
  coord_sf(xlim = c(geo_bbox["xmin"] - buf, geo_bbox["xmax"] + buf),
           ylim = c(geo_bbox["ymin"] - buf, geo_bbox["ymax"] + buf),
           expand = FALSE) +
  labs(x = "Longitude", y = "Latitude") +
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

print(p_lidar_geo_mp)

ggsave(paste(paste0('plots/', park, '/spatial/bathymetry/', name), 'geographe-mp-lidar-plot.png', sep = "-"),
       plot = p_lidar_geo_mp, dpi = 600, width = 10, height = 6, bg = "white")

marine_parks %>%
  dplyr::filter(name %in% "Ngari Capes") %>%
  distinct(zone, colour)

# --- SWC plot manually - multibeam under lidar --------------------------------------
lidar_swc_plot <- clamp(lidar_swc_crop, upper = 0, values = FALSE)
names(lidar_swc_plot) <- "depth2"

# Get dynamic limits from both datasets combined
mm_lidar     <- minmax(lidar_swc_plot)
mm_multibeam <- minmax(multibeam_crop)
depth_min    <- floor(min(mm_lidar[1], mm_multibeam[1]))

marine_parks_swc <- marine_parks %>%
  dplyr::filter(name %in% c("South-west Corner", "Ngari Capes")) %>%
  dplyr::mutate(colour = case_when(
    zone == "Special Purpose Zone" ~ "#ffb6c1",  # light pink
    TRUE ~ colour
  ))

make_swc_map <- function(xlim, ylim) {

  ggplot() +
    # Multibeam first (under)
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
        ticks          = TRUE
      )
    ) +
    # LiDAR on top
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
    # Marine parks zone colours
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
    scale_fill_manual(values = c("National Park" = "#c4cea6",
                                 "Nature Reserve" = "#e4d0bb"),
                      name = "Terrestrial Parks",
                      guide = "none") +
    coord_sf(xlim = xlim, ylim = ylim, expand = FALSE) +
    labs(x = "Longitude", y = "Latitude") +
    theme_minimal() +
    theme(
      legend.position    = "right",
      legend.title       = element_text(size = 14),
      legend.text        = element_text(size = 13),
      axis.title         = element_text(size = 15),
      axis.text          = element_text(size = 13),
      panel.grid.major   = element_line(colour = "grey85", linewidth = 0.3),
      panel.grid.minor   = element_blank(),
      panel.background   = element_rect(fill = "white", colour = NA),
      panel.border       = element_rect(fill = NA, colour = "grey60", linewidth = 0.4)
    )
}

p_lidar_swc <- make_swc_map(xlim = c(114.2, 115.8),
                            ylim = c(-34.7, -33.45))
print(p_lidar_swc)
ggsave(paste(paste0('plots/', park, '/spatial/bathymetry/', name), 'swc-lidar-multibeam-plot.png', sep = "-"),
       plot = p_lidar_swc, dpi = 600, width = 8, height = 10, bg = "white")



## Zoom ins on Capel river and blackwood  -----------------------------------------------------------------------#
# --- High resolution Australian outline ---
e_aus_hr <- ext(113.0, 117.0, -35.7, -32.5)  # crop to SWC region on import

aus_hr <- st_read("data/south-west network/spatial/shapefiles/AusOutline_HighRes.shp") %>%
  st_make_valid() %>%
  st_crop(st_bbox(c(xmin = 113.0, xmax = 117.0, ymin = -35.5, ymax = -32.5),
                  crs = st_crs(4283)))

marine_parks <- marine_parks %>%
  dplyr::mutate(colour = case_when(
    zone == "Sanctuary Zone"       ~ "#f5e642",
    TRUE ~ colour
  ))

# --- Compute hillshade from Geographe LiDAR ---
slope_geo  <- terrain(lidar_geo_crop, v = "slope",  unit = "radians")
aspect_geo <- terrain(lidar_geo_crop, v = "aspect", unit = "radians")
hill_geo   <- shade(slope_geo, aspect_geo, angle = 40, direction = 270)
names(hill_geo) <- "hillshade"

# --- Updated Geographe zoom function with high res outline ---
make_geo_zoom_map_hr <- function(xlim, ylim, annotations = NULL) {

  lidar_rast <- clamp(lidar_geo_crop, upper = 0, values = FALSE)
  names(lidar_rast) <- "depth"

  p <- ggplot() +
    # Hillshade base - increased alpha for more visibility
    geom_spatraster(data = hill_geo, aes(fill = hillshade),
                    alpha = 0.9, show.legend = FALSE) +
    scale_fill_gradient(low = "#1a1a2e", high = "#e8e8e8",
                        na.value = NA, guide = "none") +
    # LiDAR on top - slightly more transparent to let hillshade through
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
    geom_sf(data = terrnp, aes(fill = leg_catego), colour = NA, alpha = 0.8) +
    scale_fill_manual(values = c("National Park" = "#c4cea6",
                                 "Nature Reserve" = "#e4d0bb"),
                      name = "Terrestrial Parks",
                      guide = "none") +
    new_scale_fill() +
    geom_sf(data = marine_parks, aes(fill = zone, colour = zone),
            linewidth = 0.7, alpha = 0.3) +
    scale_fill_manual(values = with(marine_parks, setNames(colour, zone)),
                      name = "Marine Parks",
                      breaks = marine_parks %>%
                        st_crop(st_bbox(c(xmin = xlim[1], xmax = xlim[2],
                                          ymin = ylim[1], ymax = ylim[2]),
                                        crs = st_crs(marine_parks))) %>%
                        pull(zone) %>% unique(),
                      guide = guide_legend(
                        order = 2,
                        override.aes = list(alpha = 0.6, colour = NA))) +
    scale_colour_manual(values = with(marine_parks, setNames(colour, zone)),
                        guide = "none") +
    # Optional annotations with leader lines
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
    coord_sf(xlim = xlim, ylim = ylim, expand = FALSE) +
    labs(x = "Longitude", y = "Latitude") +
    theme_minimal() +
    theme(
      legend.position    = "right",
      legend.title       = element_text(size = 14),
      legend.text        = element_text(size = 13),
      axis.title         = element_text(size = 15),
      axis.text          = element_text(size = 13),
      panel.grid.major   = element_line(colour = "grey85", linewidth = 0.3),
      panel.grid.minor   = element_blank(),
      panel.background   = element_rect(fill = "white", colour = NA),
      panel.border       = element_rect(fill = NA, colour = "grey60", linewidth = 0.4)
    )
  p
}

# --- Capel River annotations ---
# x/y = tip of leader line (feature location)
# label_x/label_y = text position
capel_annotations <- data.frame(
  label   = c("Capel River", "Capel River"),
  x       = c(115.545, 115.485),
  y       = c(-33.522, -33.455),
  label_x = c(115.560, 115.50),
  label_y = c(-33.510, -33.44)
)

p_capel_hr <- make_geo_zoom_map_hr(xlim = c(115.38, 115.6),
                                   ylim = c(-33.57, -33.4),
                                   annotations = capel_annotations)

print(p_capel_hr)

ggsave(paste(paste0('plots/', park, '/spatial/bathymetry/', name), 'geographe-capel-zoom-highres-with-labels-plot.png', sep = "-"),
       plot = p_capel_hr, dpi = 600, width = 8, height = 6, bg = "white")


# --------------- Blackwood river zoom ----------------------------------------#
# --- Compute hillshade from SWC LiDAR ---
slope_swc  <- terrain(lidar_swc_crop, v = "slope",  unit = "radians")
aspect_swc <- terrain(lidar_swc_crop, v = "aspect", unit = "radians")
hill_swc   <- shade(slope_swc, aspect_swc, angle = 40, direction = 270)
names(hill_swc) <- "hillshade"

# --- SWC zoom function with hillshade ---
make_swc_zoom_map_hr <- function(xlim, ylim, annotations = NULL) {

  lidar_rast <- clamp(lidar_swc_crop, upper = 0, values = FALSE)
  names(lidar_rast) <- "depth2"

  p <- ggplot() +
    # Hillshade base
    geom_spatraster(data = hill_swc, aes(fill = hillshade),
                    alpha = 0.4, show.legend = FALSE) +
    scale_fill_gradient(low = "#1a1a2e", high = "#e8e8e8",
                        na.value = NA, guide = "none") +
    # Multibeam under LiDAR
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
    # LiDAR on top
    new_scale_fill() +
    geom_spatraster(data = lidar_rast, aes(fill = depth2), alpha = 0.8) +
    scale_fill_viridis_c(
      option   = "viridis",
      na.value = NA,
      limits   = c(-100, 0),
      oob      = scales::squish,
      guide    = "none"
    ) +
    # Australia with fill and outline
    geom_sf(data = aus_hr, fill = "seashell2", colour = "grey30", linewidth = 0.3) +
    new_scale_fill() +
    geom_sf(data = terrnp, aes(fill = leg_catego), colour = NA, alpha = 0.8) +
    scale_fill_manual(values = c("National Park" = "#c4cea6",
                                 "Nature Reserve" = "#e4d0bb"),
                      name = "Terrestrial Parks",
                      guide = "none") +
    # Marine parks outlines only
    new_scale_fill() +
    geom_sf(data = marine_parks_swc, aes(fill = zone, colour = zone),
            linewidth = 0.7, alpha = 0.3) +
    scale_fill_manual(values = with(marine_parks_swc, setNames(colour, zone)),
                      name = "Marine Parks",
                      breaks = marine_parks_swc %>%
                        st_crop(st_bbox(c(xmin = xlim[1], xmax = xlim[2],
                                          ymin = ylim[1], ymax = ylim[2]),
                                        crs = st_crs(marine_parks_swc))) %>%
                        pull(zone) %>% unique(),
                      guide = guide_legend(order = 2,
                                           override.aes = list(alpha = 0.6, colour = NA))) +
    scale_colour_manual(values = with(marine_parks_swc, setNames(colour, zone)),
                        guide = "none") +
    # Australia outline only on very top
    geom_sf(data = aus_hr, fill = NA, colour = "grey30", linewidth = 0.3) +
    # Blackwood River label
    annotate("segment", x = 115.0, y = -34.5, xend = 114.97, yend = -34.48,
             colour = "black", linewidth = 0.4) +
    annotate("text", x = 115.06, y = -34.47, label = "Blackwood River",
             colour = "black", size = 3, fontface = "plain", hjust = 1) +
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
    labs(x = "Longitude", y = "Latitude") +
    theme_minimal() +
    theme(
      legend.position    = "right",
      legend.title       = element_text(size = 14),
      legend.text        = element_text(size = 13),
      axis.title         = element_text(size = 15),
      axis.text          = element_text(size = 13),
      panel.grid.major   = element_line(colour = "grey85", linewidth = 0.3),
      panel.grid.minor   = element_blank(),
      panel.background   = element_rect(fill = "white", colour = NA),
      panel.border       = element_rect(fill = NA, colour = "grey60", linewidth = 0.4)
    )
  p
}

# --- Blackwood zoom ---
p_blackwood_hr <- make_swc_zoom_map_hr(xlim = c(114.8, 115.4),
                                       ylim = c(-34.58, -34.2))
print(p_blackwood_hr)
ggsave(paste(paste0('plots/', park, '/spatial/bathymetry/', name), 'swc-blackwood-zoom-highres-plot.png', sep = "-"),
       plot = p_blackwood_hr, dpi = 600, width = 9, height = 6, bg = "white")
