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
  st_crop(st_bbox(c(xmin = 113.0, xmax = 126.0, ymin = -35.5, ymax = -32.5),
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
                            "Shoalwater Islands"))

marine_parks_swc <- marine_parks %>%
  dplyr::filter(name %in% c("South-west Corner", "Ngari Capes"))

# ==============================================================================
# 2. LOAD AND PREPARE RASTER DATA
# ==============================================================================

# --- Background bathymetry for grey hillshade context ---
bg_bathy_raw <- rast("data/south-west network/spatial/rasters/ausbath_09_v4") %>%
  crop(ext(113.0, 126.0, -35.5, -32.5)) %>%
  clamp(upper = 0, values = FALSE)

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
# Geographe: xlim = c(114.9, 115.75) -> width  = 0.85 deg
#            ylim = c(-33.7, -33.25) -> height = 0.45 deg
#            aspect ratio (width/height) = 0.85 / 0.45 = 1.889
#
# SWC matched to same aspect ratio:
#   height = 34.6 - 33.3 = 1.3 deg
#   width  = 1.3 * 1.889 = 2.456 deg
#   centre = 115.0 -> xlim = c(113.77, 116.23)
# ==============================================================================

geo_xlim <- c(114.9,   115.75)
geo_ylim <- c(-33.7,   -33.25)

swc_xlim <- c(113.77,  116.23)
swc_ylim <- c(-34.6,   -33.3)

e_geo <- ext(geo_xlim[1], geo_xlim[2], geo_ylim[1], geo_ylim[2])
e_swc <- ext(swc_xlim[1], swc_xlim[2], swc_ylim[1], swc_ylim[2])

lidar_geo_crop     <- crop(lidar_geo,     e_geo)
geo_multibeam_crop <- crop(geo_multibeam, e_geo)
lidar_swc_crop     <- crop(lidar_swc,     e_swc)
multibeam_crop     <- crop(multibeam,     e_swc)
lidar_geo_swc_crop <- crop(lidar_geo,     e_swc)

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

# Background hillshades from ausbath_09 (full coverage)
hill_bg_geo <- make_hillshade(crop(bg_bathy_raw, e_geo))
hill_bg_swc <- make_hillshade(crop(bg_bathy_raw, e_swc))

# Highres hillshades from survey rasters
hill_geo_lidar <- make_hillshade(lidar_geo_crop)
hill_geo_mb    <- make_hillshade(geo_multibeam_crop)
hill_swc_lidar <- make_hillshade(lidar_swc_crop)
hill_swc_mb    <- make_hillshade(multibeam_crop)

# ==============================================================================
# 5. COLOUR PALETTES
# ==============================================================================

v <- scales::viridis_pal(option = "viridis")(100)

bathy_palette_geo <- colorRampPalette(c(
  v[1], v[3], v[6], v[9], v[12], v[15], v[18], v[22], v[26], v[30],
  v[34], v[38], v[42], v[46], v[52], v[58], v[65], v[72], v[79],
  v[86], v[92], v[96], v[100]
))(500)

bathy_palette_swc <- colorRampPalette(c(
  v[1],  v[2],  v[3],  v[4],  v[5],  v[6],  v[7],  v[8],  v[9],  v[10],
  v[11], v[13], v[16], v[20], v[24], v[28], v[32], v[36], v[40], v[44],
  v[48], v[58], v[68], v[76], v[83], v[89], v[94], v[98], v[100]
))(500)

# ==============================================================================
# 6. PANEL PLOT FUNCTION
# ==============================================================================

make_panel <- function(depth_rast,
                       hill_rast,
                       hill_bg,
                       xlim,
                       ylim,
                       depth_limits,
                       palette,
                       marine_parks_sf,
                       depth_rast2 = NULL,
                       depth_rast3 = NULL) {

  p <- ggplot() +

    # --- Grey background hillshade (ausbath_09, full coverage) ---
    geom_spatraster(data = hill_bg, aes(fill = hillshade),
                    alpha = 0.45, show.legend = FALSE) +
    scale_fill_gradient(low      = "#1a1a2e",
                        high     = "#ffffff",
                        na.value = NA,
                        guide    = "none") +

    # --- Primary depth raster (coloured, no alpha) ---
    new_scale_fill() +
    geom_spatraster(data = depth_rast, aes(fill = depth), alpha = 1) +
    scale_fill_gradientn(
      colours  = palette,
      limits   = depth_limits,
      oob      = scales::squish,
      na.value = NA,
      guide    = "none"
    ) +

    # --- Highres hillshade overlay on coloured bathy ---
    new_scale_fill() +
    geom_spatraster(data = hill_rast, aes(fill = hillshade),
                    alpha = 0.3, show.legend = FALSE) +
    scale_fill_gradient(low      = "#000000",
                        high     = "#ffffff",
                        na.value = NA,
                        guide    = "none")

  # --- Optional second raster ---
  if (!is.null(depth_rast2)) {
    p <- p +
      new_scale_fill() +
      geom_spatraster(data = depth_rast2, aes(fill = depth), alpha = 1) +
      scale_fill_gradientn(
        colours  = palette,
        limits   = depth_limits,
        oob      = scales::squish,
        na.value = NA,
        guide    = "none"
      )
  }

  # --- Optional third raster ---
  if (!is.null(depth_rast3)) {
    p <- p +
      new_scale_fill() +
      geom_spatraster(data = depth_rast3, aes(fill = depth), alpha = 1) +
      scale_fill_gradientn(
        colours  = palette,
        limits   = depth_limits,
        oob      = scales::squish,
        na.value = NA,
        guide    = "none"
      )
  }

  p <- p +

    # --- MPA boundaries: no fill, white outline, alpha 0.3 ---
    geom_sf(data      = marine_parks_sf,
            fill      = NA,
            colour    = alpha("white", 0.4),
            linewidth = 0.4) +

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
# 7. BUILD THE FOUR PANELS
# ==============================================================================

p_geo_2009 <- make_panel(
  depth_rast      = geo_multibeam_crop,
  hill_rast       = hill_geo_mb,
  hill_bg         = hill_bg_geo,
  xlim            = geo_xlim,
  ylim            = geo_ylim,
  depth_limits    = c(-30, -15),
  palette         = bathy_palette_geo,
  marine_parks_sf = marine_parks %>% dplyr::filter(name %in% "Geographe")
)

p_geo_2024 <- make_panel(
  depth_rast      = lidar_geo_crop,
  hill_rast       = hill_geo_lidar,
  hill_bg         = hill_bg_geo,
  xlim            = geo_xlim,
  ylim            = geo_ylim,
  depth_limits    = c(-30, -15),
  palette         = bathy_palette_geo,
  marine_parks_sf = marine_parks %>% dplyr::filter(name %in% "Geographe")
)

marine_parks_swc_filtered <- marine_parks %>%
  dplyr::filter(name %in% c("South-west Corner", "Ngari Capes"))

p_swc_2009 <- ggplot() +

  # --- Grey background hillshade ---
  geom_spatraster(data = hill_bg_swc, aes(fill = hillshade),
                  alpha = 0.45, show.legend = FALSE) +
  scale_fill_gradient(low      = "#1a1a2e",
                      high     = "#ffffff",
                      na.value = NA,
                      guide    = "none") +

  # --- MPA boundaries ---
  new_scale_fill() +
  geom_sf(data      = marine_parks_swc_filtered,
          fill      = NA,
          colour    = alpha("white", 0.4),
          linewidth = 0.4) +

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

p_swc_2024 <- make_panel(
  depth_rast      = multibeam_crop,
  hill_rast       = hill_swc_mb,
  hill_bg         = hill_bg_swc,
  xlim            = swc_xlim,
  ylim            = swc_ylim,
  depth_limits    = c(-100, 0),
  palette         = bathy_palette_swc,
  marine_parks_sf = marine_parks_swc_filtered,
  depth_rast2     = lidar_swc_crop,
  depth_rast3     = lidar_geo_swc_crop
)

# ==============================================================================
# 8. LEGENDS
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
      legend.title    = element_text(size = 10, face = "plain"),
      legend.text     = element_text(size = 9,  face = "plain")
    )
  cowplot::get_legend(p_leg)
}

legend_geo <- make_bathy_legend(
  depth_limits = c(-30, -15),
  depth_breaks = c(-15, -20, -25, -30),
  palette      = bathy_palette_geo
)

legend_swc <- make_bathy_legend(
  depth_limits = c(-100, 0),
  depth_breaks = c(0, -25, -50, -75, -100),
  palette      = bathy_palette_swc
)

# Terrestrial parks legend only — 2 columns, no bold
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
      direction      = "horizontal",
      title.position = "top",
      title.hjust    = 0.5,
      ncol           = 2
    )
  ) +
  theme_void() +
  theme(
    legend.position = "bottom",
    legend.title    = element_text(size = 10, face = "plain"),
    legend.text     = element_text(size = 9,  face = "plain"),
    legend.key.size = unit(0.45, "cm")
  )

bottom_legend <- cowplot::get_legend(p_tp)

# ==============================================================================
# 9. ASSEMBLE FIGURE
# ==============================================================================

title_2009 <- ggdraw() + draw_label("2009", fontface = "bold", size = 16, hjust = 0.5)
title_2024 <- ggdraw() + draw_label("2024", fontface = "bold", size = 16, hjust = 0.5)

title_row <- cowplot::plot_grid(
  NULL, title_2009, NULL, title_2024,
  nrow = 1, rel_widths = c(0.05, 1, 0.03, 1)
)

label_geo <- ggdraw() + draw_label("Geographe",          fontface = "plain", size = 13, angle = 90)
label_swc <- ggdraw() + draw_label("South-west\nCorner", fontface = "plain", size = 13, angle = 90)

depth_legends <- cowplot::plot_grid(
  legend_geo,
  legend_swc,
  ncol        = 1,
  rel_heights = c(1, 1)
)

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

maps_grid <- cowplot::plot_grid(
  title_row,
  row_geo,
  row_swc,
  ncol        = 1,
  rel_heights = c(0.05, 1, 1)
)

maps_with_legends <- cowplot::plot_grid(
  maps_grid,
  depth_legends,
  nrow       = 1,
  rel_widths = c(1, 0.07)
)

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
# 10. SAVE
# ==============================================================================

ggsave(
  paste(paste0("plots/", park, "/spatial/bathymetry/", name),
        "lidar-multibeam-facet-comparison.png", sep = "-"),
  plot   = figure_final,
  dpi    = 600,
  width  = 13.5,
  height = 8.5,
  bg     = "white"
)


# ==============================================================================
# 11. EASTERN EXTENTS — SWC EASTERN ARM & EASTERN RECHERCHE
# ==============================================================================

# --- Load DoT LiDAR uncropped (covers eastern areas) ---
lidar_east_raw <- rast("data/south-west network/spatial/rasters/DoT_south-coastal-lidar.tif")
lidar_east     <- -lidar_east_raw
lidar_east     <- clamp(lidar_east, upper = 0, values = FALSE)
names(lidar_east) <- "depth"

# --- SWC east bathy  ---
swc_east_bathy     <- rast("data/south-west network/spatial/rasters/UWA-EGS_AU038423-Wudjari-r50-SB-FP_rev1.tiff")
swc_east_bathy     <- project(swc_east_bathy, "EPSG:7844", method = "bilinear")
swc_east_bathy     <- -swc_east_bathy
swc_east_bathy     <- clamp(swc_east_bathy, upper = 0, values = FALSE)
names(swc_east_bathy) <- "depth"

# --- Define extents ---
swc_east_xlim <- c(120.6, 121.4)
swc_east_ylim <- c(-34.15, -33.75)

er_xlim <- c(122.8, 124.8)
er_ylim <- c(-34.5, -33.5)

e_swc_east <- ext(swc_east_xlim[1], swc_east_xlim[2], swc_east_ylim[1], swc_east_ylim[2])
e_er       <- ext(er_xlim[1],       er_xlim[2],       er_ylim[1],       er_ylim[2])

# --- Crop rasters to each extent ---
lidar_swc_east_crop  <- crop(lidar_east,     e_swc_east)
lidar_er_crop        <- crop(lidar_east,     e_er)
swc_east_bathy_crop <- crop(swc_east_bathy, e_swc_east)

# --- Background hillshades ---
hill_bg_swc_east <- make_hillshade(crop(bg_bathy_raw, e_swc_east))
hill_bg_er       <- make_hillshade(crop(bg_bathy_raw, e_er))

# --- Highres hillshades ---
hill_swc_east    <- make_hillshade(lidar_swc_east_crop)
hill_er          <- make_hillshade(lidar_er_crop)

# --- Marine parks filtered for eastern areas ---
marine_parks_swc_east <- marine_parks %>%
  dplyr::filter(name %in% c("South-west Corner"))

marine_parks_er <- marine_parks %>%
  dplyr::filter(name %in% c("Eastern Recherche"))

# --- Colour palettes ---
bathy_palette_swc_east <- bathy_palette_swc_east_full
bathy_palette_er       <- bathy_palette_swc

# --- 2009 panels (empty — no survey coverage) ---
p_swc_east_2009 <- ggplot() +
  geom_spatraster(data = hill_bg_swc_east, aes(fill = hillshade),
                  alpha = 0.45, show.legend = FALSE) +
  scale_fill_gradient(low      = "#1a1a2e",
                      high     = "#ffffff",
                      na.value = NA,
                      guide    = "none") +
  new_scale_fill() +
  geom_sf(data      = marine_parks_swc_east,
          fill      = NA,
          colour    = alpha("white", 0.3),
          linewidth = 0.35) +
  geom_sf(data = aus_hr, fill = "seashell2", colour = "grey30", linewidth = 0.25) +
  new_scale_fill() +
  geom_sf(data = terrnp, aes(fill = leg_catego), colour = NA, alpha = 0.8) +
  scale_fill_manual(
    values = c("National Park"  = "#c4cea6",
               "Nature Reserve" = "#e4d0bb"),
    guide  = "none"
  ) +
  coord_sf(xlim = swc_east_xlim, ylim = swc_east_ylim, expand = FALSE) +
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

p_er_2009 <- ggplot() +
  geom_spatraster(data = hill_bg_er, aes(fill = hillshade),
                  alpha = 0.45, show.legend = FALSE) +
  scale_fill_gradient(low      = "#1a1a2e",
                      high     = "#ffffff",
                      na.value = NA,
                      guide    = "none") +
  new_scale_fill() +
  geom_sf(data      = marine_parks_er,
          fill      = NA,
          colour    = alpha("white", 0.3),
          linewidth = 0.35) +
  geom_sf(data = aus_hr, fill = "seashell2", colour = "grey30", linewidth = 0.25) +
  new_scale_fill() +
  geom_sf(data = terrnp, aes(fill = leg_catego), colour = NA, alpha = 0.8) +
  scale_fill_manual(
    values = c("National Park"  = "#c4cea6",
               "Nature Reserve" = "#e4d0bb"),
    guide  = "none"
  ) +
  coord_sf(xlim = er_xlim, ylim = er_ylim, expand = FALSE) +
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

# --- 2024 panels ---
p_swc_east_2024 <- make_panel(
  depth_rast      = lidar_swc_east_crop,
  hill_rast       = hill_swc_east,
  hill_bg         = hill_bg_swc_east,
  xlim            = swc_east_xlim,
  ylim            = swc_east_ylim,
  depth_limits    = c(-78, 0),
  palette         = bathy_palette_swc_east,
  marine_parks_sf = marine_parks_swc_east,
  depth_rast2     = swc_east_bathy_crop
)

p_er_2024 <- make_panel(
  depth_rast      = lidar_er_crop,
  hill_rast       = hill_er,
  hill_bg         = hill_bg_er,
  xlim            = er_xlim,
  ylim            = er_ylim,
  depth_limits    = c(-50, 0),
  palette         = bathy_palette_er,
  marine_parks_sf = marine_parks_er
)

# --- Legends ---
legend_swc_east <- make_bathy_legend(
  depth_limits = c(-78, 0),
  depth_breaks = c(0, -20, -40, -60, -78),
  palette      = bathy_palette_swc_east
)

legend_er <- make_bathy_legend(
  depth_limits = c(-50, 0),
  depth_breaks = c(0, -10, -20, -30, -40, -50),
  palette      = bathy_palette_er
)

# --- Assemble figure ---
title_row_east <- cowplot::plot_grid(
  NULL, title_2009, NULL, title_2024,
  nrow = 1, rel_widths = c(0.05, 1, 0.03, 1)
)

label_swc_east <- ggdraw() + draw_label("SWC\nEastern Arm", fontface = "plain", size = 13, angle = 90)
label_er       <- ggdraw() + draw_label("Eastern\nRecherche",  fontface = "plain", size = 13, angle = 90)

depth_legends_east <- cowplot::plot_grid(
  legend_swc_east,
  legend_er,
  ncol        = 1,
  rel_heights = c(1, 1)
)

row_swc_east <- cowplot::plot_grid(
  label_swc_east, p_swc_east_2009, NULL, p_swc_east_2024,
  nrow = 1, rel_widths = c(0.05, 1, 0.03, 1),
  align = "h", axis = "tb"
)

row_er <- cowplot::plot_grid(
  label_er, p_er_2009, NULL, p_er_2024,
  nrow = 1, rel_widths = c(0.05, 1, 0.03, 1),
  align = "h", axis = "tb"
)

maps_grid_east <- cowplot::plot_grid(
  title_row_east,
  row_swc_east,
  row_er,
  ncol        = 1,
  rel_heights = c(0.05, 1, 1)
)

maps_with_legends_east <- cowplot::plot_grid(
  maps_grid_east,
  depth_legends_east,
  nrow       = 1,
  rel_widths = c(1, 0.07)
)

figure_east <- cowplot::plot_grid(
  maps_with_legends_east,
  bottom_legend,
  ncol        = 1,
  rel_heights = c(1, 0.10)
) +
  theme(
    plot.background = element_rect(fill = "white", colour = NA),
    plot.margin     = margin(t = 5, r = 5, b = 5, l = 5)
  )

# --- Save ---
ggsave(
  paste(paste0("plots/", park, "/spatial/bathymetry/", name),
        "lidar-eastern-facet-comparison-TEST.png", sep = "-"),
  plot   = figure_east,
  dpi    = 600,
  width  = 16,
  height = 9.63,
  bg     = "white"
)


# ==============================================================================
# 12. SWC EASTERN ARM ONLY — viridis full depth ramp, 2009 vs 2024
# ==============================================================================

bathy_palette_swc_east_full <- colorRampPalette(c(
  v[1], v[16], v[30], v[44], v[58], v[72], v[83], v[92], v[100]
))(500)

legend_swc_east_full <- make_bathy_legend(
  depth_limits = c(-78, 0),
  depth_breaks = c(0, -20, -40, -60, -78),
  palette      = bathy_palette_swc_east_full
)

p_swc_east_2009_12 <- p_swc_east_2009 +
  theme(
    axis.text.x = element_text(size = 9, colour = "grey40"),
    axis.text.y = element_text(size = 9, colour = "grey40"),
    plot.margin = margin(5, 15, 5, 15)
  ) +
  scale_x_continuous(breaks = c(120.6, 120.8, 121.0, 121.2, 121.4)) +
  scale_y_continuous(breaks = c(-34.15, -34.0, -33.9, -33.75))

p_swc_east_2024_full <- make_panel(
  depth_rast      = lidar_swc_east_crop,
  hill_rast       = hill_swc_east,
  hill_bg         = hill_bg_swc_east,
  xlim            = swc_east_xlim,
  ylim            = swc_east_ylim,
  depth_limits    = c(-78, 0),
  palette         = bathy_palette_swc_east_full,
  marine_parks_sf = marine_parks_swc_east,
  depth_rast2     = swc_east_bathy_crop
) +
  theme(
    axis.text.x = element_text(size = 9, colour = "grey40"),
    axis.text.y = element_text(size = 9, colour = "grey40"),
    plot.margin = margin(5, 15, 5, 15)
  ) +
  scale_x_continuous(breaks = c(120.6, 120.8, 121.0, 121.2, 121.4)) +
  scale_y_continuous(breaks = c(-34.15, -34.0, -33.9, -33.75))

row_swc_east_full <- cowplot::plot_grid(
  p_swc_east_2009_12, NULL, p_swc_east_2024_full,
  nrow = 1, rel_widths = c(1, 0.03, 1),
  align = "h", axis = "tb"
)

maps_grid_swc_east <- cowplot::plot_grid(
  title_row,
  row_swc_east_full,
  ncol        = 1,
  rel_heights = c(0.05, 1)
)

maps_with_legends_swc_east <- cowplot::plot_grid(
  maps_grid_swc_east,
  legend_swc_east_full,
  nrow       = 1,
  rel_widths = c(1, 0.07)
)

figure_swc_east <- cowplot::plot_grid(
  maps_with_legends_swc_east,
  bottom_legend,
  ncol        = 1,
  rel_heights = c(1, 0.10)
) +
  theme(
    plot.background = element_rect(fill = "white", colour = NA),
    plot.margin     = margin(t = 5, r = 5, b = 5, l = 5)
  )

ggsave(
  paste(paste0("plots/", park, "/spatial/bathymetry/", name),
        "swc-eastern-arm-full-ramp.png", sep = "-"),
  plot   = figure_swc_east,
  dpi    = 600,
  width  = 16,
  height = 5.5,
  bg     = "white"
)
