###
# Project: NESP 4.20 - Marine Park Dashboard reporting
# Data:    Geographe Bay and SWC LiDAR and multibeam, marine park shapefiles
# Task:    Faceted 2x2 bathymetry comparison (2009 vs 2024, Geographe vs SWC)
# Author:  Annika Leunig
# Date:    March 2026
###

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
library(cowplot)

sf_use_s2(TRUE)

# ==============================================================================
# 1. LOAD SPATIAL CONTEXT LAYERS
# ==============================================================================

terrnp <- st_read("data/south-west network/spatial/shapefiles/Legislated_Lands_and_Waters_DBCA_011.shp") %>%
  dplyr::filter(leg_catego %in% c("Nature Reserve", "National Park"))

aus <- st_read("data/south-west network/spatial/shapefiles/STE_2021_AUST_GDA2020.shp") %>%
  st_make_valid()

aus_hr <- st_read("data/south-west network/spatial/shapefiles/AusOutline_HighRes.shp") %>%
  st_make_valid() %>%
  st_crop(st_bbox(c(xmin = 113.0, xmax = 117.0, ymin = -35.5, ymax = -32.5),
                  crs = st_crs(4283)))

marine_parks <- st_read("data/south-west network/spatial/shapefiles/south-and-western-australia_marine-parks-all.shp") %>%
  dplyr::filter(name %in% c("Abrolhos", "Abrolhos Islands", "Bremer", "Eastern Recherche",
                            "Ngari Capes", "Geographe", "South-west Corner",
                            "Great Australian Bight", "Jurien", "Murat", "Jurien Bay",
                            "Perth Canyon", "Southern Kangaroo Island", "Twilight",
                            "Two Rocks", "Western Eyre", "Western Kangaroo Island",
                            "Nuyts Archipelgo", "Thorny Passage", "Sir Joseph Banks Group",
                            "Investigator", "West coast Bays", "Southern Spencer Gulf",
                            "Upper Spencer Gulf", "Cottesloe Reef", "Rottnest",
                            "Shoalwater Islands")) %>%
  dplyr::mutate(colour = case_when(
    zone == "Special Purpose Zone" ~ "#ffb6c1",
    zone == "Sanctuary Zone"       ~ "#f5e642",
    TRUE ~ colour
  ))

marine_parks_swc <- marine_parks %>%
  dplyr::filter(name %in% c("South-west Corner", "Ngari Capes"))

# ==============================================================================
# 2. LOAD AND PREPARE RASTER DATA
# ==============================================================================

# --- Geographe LiDAR (2024) ---
lidar_geo_raw <- rast("data/south-west network/spatial/rasters/Geographe-bay_lidar.tif")
lidar_geo     <- project(lidar_geo_raw, "EPSG:7844", method = "bilinear")
lidar_geo     <- clamp(lidar_geo, upper = 0, values = FALSE)
names(lidar_geo) <- "depth"

# --- Geographe multibeam (2009) ---
geo_multibeam_raw <- rast("data/south-west network/spatial/rasters/GeographeBayMarineFuturesMultibeamDepth_WGS84z50s.tif")
geo_multibeam     <- project(geo_multibeam_raw, "EPSG:7844", method = "bilinear")
geo_multibeam     <- clamp(geo_multibeam, upper = 0, values = FALSE)
names(geo_multibeam) <- "depth"

# --- SWC LiDAR (2024) ---
lidar_swc_raw <- rast("data/south-west network/spatial/rasters/DoT_south-coastal-lidar.tif")
lidar_swc     <- -lidar_swc_raw
lidar_swc     <- clamp(lidar_swc, upper = 0, values = FALSE)
names(lidar_swc) <- "depth"

# --- SWC multibeam (2024) ---
multibeam_raw <- rast("data/south-west network/spatial/rasters/south-west-corner_merged-multibeam.tiff")
multibeam     <- multibeam_raw[["Depth"]]
multibeam     <- project(multibeam, "EPSG:7844", method = "bilinear")
multibeam     <- clamp(multibeam, upper = 0, values = FALSE)
names(multibeam) <- "depth"

# ==============================================================================
# 3. DEFINE EXTENTS
# ------------------------------------------------------------------------------
# Geographe ratio used as reference for both regions so panels facet cleanly.
# Geographe: xlim = c(114.9, 115.75)  -> width  = 0.85 deg
#            ylim = c(-33.7, -33.25)  -> height = 0.45 deg
# aspect ratio (width/height) = 0.85 / 0.45 = ~1.889
#
# SWC extent matched to same aspect ratio:
#   desired width  = 1.6 deg  -> height = 1.6 / 1.889 = ~0.847
#   centre lat     = -34.075  -> ylim = c(-34.499, -33.652) ~ c(-34.5, -33.65)
# ==============================================================================

geo_xlim <- c(114.9,  115.75)
geo_ylim <- c(-33.7,  -33.25)

swc_xlim <- c(114.0,  116.0)
swc_ylim <- c(-34.6,  -33.3)

# Crop rasters to extents
e_geo <- ext(geo_xlim[1], geo_xlim[2], geo_ylim[1], geo_ylim[2])
e_swc <- ext(swc_xlim[1], swc_xlim[2], swc_ylim[1], swc_ylim[2])

lidar_geo_crop    <- crop(lidar_geo,    e_geo)
geo_multibeam_crop <- crop(geo_multibeam, e_geo)
lidar_swc_crop    <- crop(lidar_swc,    e_swc)
multibeam_crop    <- crop(multibeam,    e_swc)

# ==============================================================================
# 4. HILLSHADES
# ==============================================================================

make_hillshade <- function(bathy_rast, altitude = 40, azimuth = 270) {
  slope  <- terrain(bathy_rast, v = "slope",  unit = "radians")
  aspect <- terrain(bathy_rast, v = "aspect", unit = "radians")
  hill   <- shade(slope, aspect, angle = altitude, direction = azimuth, normalize = TRUE)
  names(hill) <- "hillshade"
  hill
}

hill_geo_lidar    <- make_hillshade(lidar_geo_crop)
hill_geo_mb       <- make_hillshade(geo_multibeam_crop)
hill_swc_lidar    <- make_hillshade(lidar_swc_crop)
hill_swc_mb       <- make_hillshade(multibeam_crop)

# ==============================================================================
# 5. COLOUR PALETTES  (matching original script style)
# ==============================================================================

v <- scales::viridis_pal(option = "viridis")(100)

# Geographe: shallow shelf, -30 to -15 m range
bathy_palette_geo <- colorRampPalette(c(
  v[1], v[3], v[6], v[9], v[12], v[15], v[18], v[22], v[26], v[30],
  v[34], v[38], v[42], v[46], v[52], v[58], v[65], v[72], v[79],
  v[86], v[92], v[96], v[100]
))(500)

# SWC: deeper range including multibeam
bathy_palette_swc <- colorRampPalette(c(
  v[1],  v[2],  v[3],  v[4],  v[5],  v[6],  v[7],  v[8],  v[9],  v[10],
  v[11], v[13], v[16], v[20], v[24], v[28], v[32], v[36], v[40], v[44],
  v[48], v[58], v[68], v[76], v[83], v[89], v[94], v[98], v[100]
))(500)

# ==============================================================================
# 6. MPA ZONE FILL/COLOUR SCALES (SWC style — same approach for both regions)
# ==============================================================================

# Helper: build named colour vectors for fill + outline from a marine_parks sf
mpa_colours <- function(mp) {
  setNames(mp$colour, mp$zone)
}

# ==============================================================================
# 7. PANEL PLOT FUNCTION
# ==============================================================================

# All panels share this look:
#   - hillshade base
#   - viridis depth overlay (alpha 0.65)
#   - MPA zones filled + outlined (SWC style)
#   - aus outline
#   - terrnp overlay
#   - NO individual legends (all suppressed; shared legends added via cowplot)

make_panel <- function(depth_rast,        # primary depth raster (cropped)
                       hill_rast,         # hillshade raster (cropped)
                       xlim,
                       ylim,
                       depth_limits,      # e.g. c(-30, -15) for Geo, c(-100, 0) for SWC
                       palette,
                       marine_parks_sf,   # filtered marine parks for this region
                       depth_rast2 = NULL # optional second raster (lidar over multibeam)
) {

  mp_cols <- mpa_colours(marine_parks_sf)

  p <- ggplot() +

    # --- Hillshade base ---
    geom_spatraster(data = hill_rast, aes(fill = hillshade),
                    alpha = 0.4, show.legend = FALSE) +
    scale_fill_gradient(low = "#1a1a2e", high = "#e8e8e8",
                        na.value = NA, guide = "none") +

    # --- Primary depth raster ---
    new_scale_fill() +
    geom_spatraster(data = depth_rast, aes(fill = depth), alpha = 0.8) +
    scale_fill_gradientn(
      colours  = palette,
      limits   = depth_limits,
      oob      = scales::squish,
      na.value = NA,
      guide    = "none"
    )

  # --- Optional second raster (LiDAR draped over multibeam for SWC 2024) ---
  if (!is.null(depth_rast2)) {
    p <- p +
      new_scale_fill() +
      geom_spatraster(data = depth_rast2, aes(fill = depth), alpha = 0.8) +
      scale_fill_gradientn(
        colours  = palette,
        limits   = depth_limits,
        oob      = scales::squish,
        na.value = NA,
        guide    = "none"
      )
  }

  p <- p +

    # --- MPA zones (SWC style: filled + outlined, low alpha) ---
    new_scale_fill() +
    geom_sf(data = marine_parks_sf,
            aes(fill = zone, colour = zone),
            linewidth = 0.35, alpha = 0.25) +
    scale_fill_manual(values   = mp_cols, guide = "none") +
    scale_colour_manual(values = mp_cols, guide = "none") +

    # --- Australia outline ---
    geom_sf(data = aus_hr, fill = "seashell2", colour = "grey30", linewidth = 0.25) +

    # --- Terrestrial parks ---
    new_scale_fill() +
    geom_sf(data = terrnp, aes(fill = leg_catego), colour = NA, alpha = 0.8) +
    scale_fill_manual(
      values = c("National Park"  = "#c4cea6",
                 "Nature Reserve" = "#e4d0bb"),
      guide  = "none"
    ) +

    coord_sf(xlim = xlim, ylim = ylim, expand = FALSE) +
    labs(x = NULL, y = NULL) +
    theme_minimal() +
    theme(
      axis.text        = element_text(size = 8, colour = "grey40"),
      axis.ticks       = element_line(colour = "grey60"),
      panel.grid       = element_blank(),
      panel.background = element_rect(fill = "white", colour = NA),
      plot.background  = element_rect(fill = "white", colour = NA),
      panel.border     = element_rect(fill = NA, colour = "grey60", linewidth = 0.4),
      plot.margin      = margin(2, 2, 2, 2)
    )

  p
}

# ==============================================================================
# 8. BUILD THE FOUR PANELS
# ==============================================================================

# Top-left:  Geographe 2009 (multibeam only)
p_geo_2009 <- make_panel(
  depth_rast     = geo_multibeam_crop,
  hill_rast      = hill_geo_mb,
  xlim           = geo_xlim,
  ylim           = geo_ylim,
  depth_limits   = c(-30, -15),
  palette        = bathy_palette_geo,
  marine_parks_sf = marine_parks %>% dplyr::filter(name %in% "Geographe")
)

# Top-right: Geographe 2024 (LiDAR)
p_geo_2024 <- make_panel(
  depth_rast      = lidar_geo_crop,
  hill_rast       = hill_geo_lidar,
  xlim            = geo_xlim,
  ylim            = geo_ylim,
  depth_limits    = c(-30, -15),
  palette         = bathy_palette_geo,
  marine_parks_sf = marine_parks %>% dplyr::filter(name %in% "Geographe")
)

# Bottom-left: SWC 2009 (context only — no raster, just MPA zones + land)
# Built manually so we can omit depth raster and hillshade entirely
marine_parks_swc_filtered <- marine_parks %>%
  dplyr::filter(name %in% c("South-west Corner", "Ngari Capes"))
mp_cols_swc <- mpa_colours(marine_parks_swc_filtered)

p_swc_2009 <- ggplot() +

  new_scale_fill() +
  geom_sf(data = marine_parks_swc_filtered,
          aes(fill = zone, colour = zone),
          linewidth = 0.35, alpha = 0.25) +
  scale_fill_manual(values   = mp_cols_swc, guide = "none") +
  scale_colour_manual(values = mp_cols_swc, guide = "none") +

  geom_sf(data = aus_hr, fill = "seashell2", colour = "grey30", linewidth = 0.25) +

  new_scale_fill() +
  geom_sf(data = terrnp, aes(fill = leg_catego), colour = NA, alpha = 0.8) +
  scale_fill_manual(
    values = c("National Park"  = "#c4cea6",
               "Nature Reserve" = "#e4d0bb"),
    guide  = "none"
  ) +

  coord_sf(xlim = swc_xlim, ylim = swc_ylim, expand = FALSE) +
  labs(x = NULL, y = NULL) +
  theme_minimal() +
  theme(
    axis.text        = element_text(size = 8, colour = "grey40"),
    axis.ticks       = element_line(colour = "grey60"),
    panel.grid       = element_blank(),
    panel.background = element_rect(fill = "white", colour = NA),
    plot.background  = element_rect(fill = "white", colour = NA),
    panel.border     = element_rect(fill = NA, colour = "grey60", linewidth = 0.4),
    plot.margin      = margin(2, 2, 2, 2)
  )

# Bottom-right: SWC 2024 (multibeam + LiDAR draped on top)
p_swc_2024 <- make_panel(
  depth_rast      = multibeam_crop,
  hill_rast       = hill_swc_mb,
  xlim            = swc_xlim,
  ylim            = swc_ylim,
  depth_limits    = c(-100, 0),
  palette         = bathy_palette_swc,
  marine_parks_sf = marine_parks_swc_filtered,
  depth_rast2     = lidar_swc_crop     # LiDAR draped over multibeam
)

# ==============================================================================
# 9. STANDALONE LEGEND HELPERS  (cowplot approach from faceting script)
# ==============================================================================

make_bathy_legend <- function(depth_limits, depth_breaks, palette, title = "Depth (m)") {
  dummy <- data.frame(
    x     = 1,
    y     = seq(depth_limits[1], depth_limits[2], length.out = 200),
    depth = seq(depth_limits[1], depth_limits[2], length.out = 200)
  )
  p_leg <- ggplot(dummy, aes(x = x, y = y, fill = depth)) +
    geom_tile() +
    scale_fill_gradientn(
      colours  = palette,
      limits   = depth_limits,
      name     = title,
      breaks   = depth_breaks,
      labels   = as.character(depth_breaks),
      guide    = guide_colorbar(
        barwidth       = 1.2,
        barheight      = 6,
        title.position = "top",
        ticks          = TRUE
      )
    ) +
    theme_void() +
    theme(
      legend.position = "right",
      legend.title    = element_text(size = 10),
      legend.text     = element_text(size = 9)
    )
  cowplot::get_legend(p_leg)
}

# Geographe depth legend
legend_geo <- make_bathy_legend(
  depth_limits = c(-30, -15),
  depth_breaks = c(-15, -20, -25, -30),
  palette      = bathy_palette_geo
)

# SWC depth legend
legend_swc <- make_bathy_legend(
  depth_limits = c(-100, 0),
  depth_breaks = c(0, -25, -50, -75, -100),
  palette      = bathy_palette_swc
)

# MPA zone legends — split into Commonwealth and State, built from visible zones
relevant_parks <- marine_parks %>%
  dplyr::filter(name %in% c("Geographe", "South-west Corner", "Ngari Capes"))

cwlth_zones <- relevant_parks %>%
  dplyr::filter(epbc == "Commonwealth") %>%
  dplyr::distinct(zone, colour) %>%
  dplyr::arrange(zone)

state_zones <- relevant_parks %>%
  dplyr::filter(epbc == "State") %>%
  dplyr::distinct(zone, colour) %>%
  dplyr::arrange(zone)

make_mpa_legend <- function(zones_df, title) {
  leg_df <- data.frame(
    x    = 1, y = seq_len(nrow(zones_df)),
    zone = factor(zones_df$zone, levels = zones_df$zone)
  )
  p <- ggplot(leg_df, aes(x = x, y = y, fill = zone)) +
    geom_tile() +
    scale_fill_manual(
      name   = title,
      values = setNames(zones_df$colour, zones_df$zone),
      guide  = guide_legend(
        direction      = "horizontal",
        title.position = "top",
        title.hjust    = 0.5,
        nrow           = 2
      )
    ) +
    theme_void() +
    theme(
      legend.position = "bottom",
      legend.title    = element_text(size = 11, face = "bold"),
      legend.text     = element_text(size = 10),
      legend.key.size = unit(0.45, "cm")
    )
  cowplot::get_legend(p)
}

legend_cwlth_mpa <- make_mpa_legend(cwlth_zones, "Commonwealth Marine Park Zones")
legend_state_mpa <- make_mpa_legend(state_zones,  "State Marine Park Zones")

# Terrestrial parks legend — single column (vertical)
tp_df <- data.frame(
  x  = 1, y = 1,
  tp = factor(c("National Park", "Nature Reserve"),
              levels = c("National Park", "Nature Reserve"))
)
p_tp <- ggplot(tp_df, aes(x = x, y = y, fill = tp)) +
  geom_tile() +
  scale_fill_manual(
    name   = "Terrestrial Parks",
    values = c("National Park" = "#c4cea6", "Nature Reserve" = "#e4d0bb"),
    guide  = guide_legend(
      direction      = "vertical",
      title.position = "top",
      title.hjust    = 0,
      ncol           = 1
    )
  ) +
  theme_void() +
  theme(
    legend.position = "bottom",
    legend.title    = element_text(size = 11, face = "bold"),
    legend.text     = element_text(size = 10),
    legend.key.size = unit(0.45, "cm")
  )
terrp_legend <- cowplot::get_legend(p_tp)

# Combined bottom legend row: Commonwealth | State | Terrestrial
bottom_legend <- cowplot::plot_grid(
  legend_cwlth_mpa,
  legend_state_mpa,
  terrp_legend,
  nrow       = 1,
  rel_widths = c(1.4, 1.0, 0.5)
)

# ==============================================================================
# 10. ASSEMBLE FACETED FIGURE  (cowplot, matching the faceting script style)
# ==============================================================================

# Column headers
title_2009 <- ggdraw() + draw_label("2009", fontface = "bold", size = 16, hjust = 0.5)
title_2024 <- ggdraw() + draw_label("2024", fontface = "bold", size = 16, hjust = 0.5)

title_row <- cowplot::plot_grid(
  NULL, title_2009, NULL, title_2024,
  nrow = 1, rel_widths = c(0.05, 1, 0.03, 1)
)

# Row labels
label_geo <- ggdraw() + draw_label("Geographe",    fontface = "plain", size = 13, angle = 90)
label_swc <- ggdraw() + draw_label("South-west\nCorner", fontface = "plain", size = 13, angle = 90)

# Depth legend column: Geo legend on top row, SWC legend on bottom row
depth_legends <- cowplot::plot_grid(
  legend_geo,
  legend_swc,
  ncol        = 1,
  rel_heights = c(1, 1)
)

# Map rows
row_geo <- cowplot::plot_grid(
  label_geo, p_geo_2009, NULL, p_geo_2024,
  nrow = 1, rel_widths = c(0.05, 1, 0.03, 1),
  align = "h", axis = "tb"
)

row_swc <- cowplot::plot_grid(
  label_swc, p_swc_2009, NULL, p_swc_2024,
  nrow = 1, rel_widths = c(0.05, 1, 0.03, 1),
  align = "h", axis = "tb"
)

# Stack title + rows
maps_grid <- cowplot::plot_grid(
  title_row,
  row_geo,
  row_swc,
  ncol        = 1,
  rel_heights = c(0.05, 1, 1)
)

# Attach depth legend column on the right
maps_with_legends <- cowplot::plot_grid(
  maps_grid,
  depth_legends,
  nrow       = 1,
  rel_widths = c(1, 0.07)
)

# Attach shared MPA + terrestrial legends at bottom
figure_final <- cowplot::plot_grid(
  maps_with_legends,
  bottom_legend,
  ncol        = 1,
  rel_heights = c(1, 0.10)
) +
  theme(
    plot.background = element_rect(fill = "white", colour = NA),
    plot.margin     = margin(t = 5, r = 5, b = 5, l = 5)
  )

# ==============================================================================
# 11. SAVE
# ==============================================================================

ggsave(
  paste(paste0("plots/", park, "/spatial/bathymetry/", name),
        "lidar-multibeam-facet-comparison.png", sep = "-"),
  plot   = figure_final,
  dpi    = 600,
  width  = 16,
  height = 11,
  bg     = "white"
)
