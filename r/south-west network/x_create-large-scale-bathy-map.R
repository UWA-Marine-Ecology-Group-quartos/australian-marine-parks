###
# Project: NESP 4.20 - Marine Park Dashboard reporting
# Data:    AusBathy, marine parks, bioregions, marine regions, FHPA
# Task:    WA context map with bathymetry, hillshade, bioregions, sanctuary zones
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
# 1. SET RASTER EXTENT
# ==============================================================================

e <- ext(106.0, 124.0, -39.0, -23.0)

# ==============================================================================
# 2. LOAD SPATIAL DATA
# ==============================================================================

aus <- st_read("data/south-west network/spatial/shapefiles/STE_2021_AUST_GDA2020.shp") %>%
  st_make_valid() %>%
  st_transform(4326)

terrnp <- st_read("data/south-west network/spatial/shapefiles/Legislated_Lands_and_Waters_DBCA_011.shp") %>%
  dplyr::filter(leg_catego %in% c("Nature Reserve", "National Park")) %>%
  st_transform(4326)

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
  st_transform(4326)

mp_state_sanctuary <- marine_parks %>%
  dplyr::filter(epbc == "State",
                zone %in% c("Reef Observation Area", "Sanctuary Zone"))
mp_cwlth_sanctuary <- marine_parks %>%
  dplyr::filter(epbc == "Commonwealth",
                zone %in% c("National Park Zone"))

# ==============================================================================
# 3. LOAD AND PREPARE RASTERS
# ==============================================================================

bathy <- rast("data/south-west network/spatial/rasters/AusBathyTopo__Australia__2024_250m_MSL_cog.tif") %>%
  project("EPSG:4326", method = "bilinear") %>%
  crop(e)

bathy_shelf        <- clamp(bathy, lower = -200, upper = 0, values = FALSE)
names(bathy_shelf) <- "depth"
bathy_df           <- as.data.frame(bathy_shelf, xy = TRUE, na.rm = TRUE)
colnames(bathy_df)[3] <- "depth"

slope  <- terrain(bathy, v = "slope",  unit = "radians")
aspect <- terrain(bathy, v = "aspect", unit = "radians")
hs     <- shade(slope, aspect, angle = 35, direction = 315, normalize = TRUE)
names(hs) <- "hillshade"
hs_df     <- as.data.frame(hs, xy = TRUE, na.rm = TRUE)
colnames(hs_df)[3] <- "hillshade"

# ==============================================================================
# 4. COLOUR PALETTES
# ==============================================================================

v <- scales::viridis_pal(option = "viridis")(100)

bathy_palette <- colorRampPalette(c(
  v[1],
  v[3],
  v[6],
  v[9],
  v[12],
  v[15],
  v[18],
  v[22],
  v[26],
  v[30],
  v[34],
  v[38],
  v[42],
  v[46],
  v[52],
  v[58],
  v[65],
  v[72],
  v[79],
  v[86],
  v[92],
  v[96],
  v[100]
))(500)



# ==============================================================================
# 5. MAP FUNCTION
# ==============================================================================

make_bathy_map <- function(plot_limits,
                           show_legend = TRUE,
                           label_size  = 3.5,
                           palette     = bathy_palette) {

  legend_pos <- if (show_legend) "right" else "none"

  p <- ggplot() +

    geom_tile(data = bathy_df, aes(x = x, y = y, fill = depth)) +
    scale_fill_gradientn(
      colours  = palette,
      limits   = c(-200, 0),
      na.value = NA,
      name     = "Depth (m)",
      breaks   = c(0, -50, -100, -150, -200),
      labels   = c("0", "-50", "-100", "-150", "-200"),
      guide    = if (show_legend) guide_colorbar(
        barwidth       = 1.2,
        barheight      = 8,
        title.position = "top",
        ticks          = TRUE,
        order          = 1
      ) else "none"
    ) +

    new_scale_fill() +
    geom_tile(data = hs_df, aes(x = x, y = y, fill = hillshade),
              alpha = 0.3, show.legend = FALSE) +
    scale_fill_gradient(low      = "grey20",
                        high     = "#e8e8e8",
                        na.value = NA,
                        guide    = "none") +

    geom_contour(data = bathy_df, aes(x = x, y = y, z = depth),
                 breaks    = c(-120, -60),
                 colour    = "white",
                 alpha     = 0.35,
                 linewidth = 0.25) +

    geom_sf(data = aus, fill = "seashell2", colour = "grey50", linewidth = 0.25) +

    new_scale_fill() +
    geom_sf(data = terrnp, aes(fill = leg_catego), colour = NA, alpha = 0.8) +
    scale_fill_manual(
      values = c("National Park"  = "#c4cea6",
                 "Nature Reserve" = "#e4d0bb"),
      name   = "Terrestrial Parks",
      guide  = if (show_legend) guide_legend(
        order          = 4,
        title.position = "top",
        override.aes   = list(alpha = 0.8)
      ) else "none"
    ) +

    # State no-take zones
    new_scale_fill() +
    geom_sf(data = mp_state_sanctuary, aes(fill = zone),
            colour = NA, linewidth = 0.15, alpha = 0.75) +
    scale_fill_manual(
      name   = "State",
      values = c("Sanctuary Zone"        = "#fc887c",
                 "Reef Observation Area" = "#ff4430"),
      labels = c("Reef Observation Area" = "Reef Observation Areas (no line fishing)",
                 "Sanctuary Zone"        = "Sanctuary Zones (no fishing)"),
      guide  = if (show_legend) guide_legend(
        order          = 2,
        title.position = "top",
        override.aes   = list(alpha = 0.75)
      ) else "none"
    ) +

    # Commonwealth no-take zones
    new_scale_fill() +
    geom_sf(data = mp_cwlth_sanctuary, aes(fill = zone),
            colour = NA, linewidth = 0.15, alpha = 0.75) +
    scale_fill_manual(
      name   = "Commonwealth",
      values = c("National Park Zone" = "#ffc8c2"),
      labels = c("National Park Zone" = "National Parks Zones (no fishing)"),
      guide  = if (show_legend) guide_legend(
        order          = 3,
        title.position = "top",
        override.aes   = list(alpha = 0.75)
      ) else "none"
    ) +

    coord_sf(xlim = plot_limits[1:2], ylim = plot_limits[3:4],
             crs = 4326, expand = FALSE) +
    labs(x = NULL, y = NULL) +
    theme_minimal() +
    theme(
      legend.key.size  = unit(0.5, "cm"),
      legend.text      = element_text(size = 9),
      legend.title     = element_text(size = 10),
      legend.position  = legend_pos,
      legend.box       = "vertical",
      legend.spacing.y = unit(0.3, "cm"),
      panel.grid       = element_blank(),
      panel.background = element_rect(fill = "white", colour = NA),
      plot.background  = element_rect(fill = "white", colour = NA),
      axis.text        = element_text(size = 9,  colour = "grey40"),
      axis.ticks       = element_line(colour = "grey60"),
      panel.border     = element_rect(fill = NA, colour = "grey60", linewidth = 0.4),
      plot.margin      = margin(t = 5, r = 5, b = 5, l = 5)
    )

  return(p)
}

make_bathy_map_inset <- function(plot_limits,
                                 show_legend = TRUE,
                                 label_size  = 3.5,
                                 palette     = bathy_palette) {

  p <- ggplot() +

    geom_tile(data = bathy_df, aes(x = x, y = y, fill = depth)) +
    scale_fill_gradientn(
      colours  = palette,
      limits   = c(-200, 0),
      na.value = NA,
      name     = "Depth (m)",
      breaks   = c(0, -50, -100, -150, -200),
      labels   = c("0", "-50", "-100", "-150", "-200"),
      guide    = if (show_legend) guide_colorbar(
        barwidth       = 1.2,
        barheight      = 8,
        title.position = "top",
        ticks          = TRUE,
        order          = 1
      ) else "none"
    ) +

    new_scale_fill() +
    geom_tile(data = hs_df, aes(x = x, y = y, fill = hillshade),
              alpha = 0.3, show.legend = FALSE) +
    scale_fill_gradient(low      = "grey20",
                        high     = "#e8e8e8",
                        na.value = NA,
                        guide    = "none") +

    geom_contour(data = bathy_df, aes(x = x, y = y, z = depth),
                 breaks    = c(-120, -60),
                 colour    = "white",
                 alpha     = 0.35,
                 linewidth = 0.25) +

    geom_sf(data = aus, fill = "seashell2", colour = "grey50", linewidth = 0.25) +

    # Terrestrial parks drawn BEFORE the white box so the box sits on top
    new_scale_fill() +
    geom_sf(data = terrnp, aes(fill = leg_catego), colour = NA, alpha = 0.8) +
    scale_fill_manual(
      values = c("National Park"  = "#c4cea6",
                 "Nature Reserve" = "#e4d0bb"),
      name   = "Terrestrial Parks",
      guide  = if (show_legend) guide_legend(
        order          = 4,
        title.position = "top",
        override.aes   = list(alpha = 0.8)
      ) else "none"
    ) +

    # White background polygon — drawn AFTER terrnp so it covers it in the legend area
    annotate("rect",
             xmin      = 117.7, xmax = 121.9,
             ymin      = -30.1, ymax = -24.1,
             fill      = alpha("white", 0.85),
             colour    = "grey70",
             linewidth = 0.3) +

    # Marine park zones drawn AFTER the white box so they show on top of it
    new_scale_fill() +
    geom_sf(data = mp_state_sanctuary, aes(fill = zone),
            colour = NA, linewidth = 0.15, alpha = 0.75) +
    scale_fill_manual(
      name   = "State",
      values = c("Sanctuary Zone"        = "#fc887c",
                 "Reef Observation Area" = "#ff4430"),
      labels = c("Reef Observation Area" = "Reef Observation Areas (no line fishing)",
                 "Sanctuary Zone"        = "Sanctuary Zones (no fishing)"),
      guide  = if (show_legend) guide_legend(
        order          = 2,
        title.position = "top",
        override.aes   = list(alpha = 0.75)
      ) else "none"
    ) +

    new_scale_fill() +
    geom_sf(data = mp_cwlth_sanctuary, aes(fill = zone),
            colour = NA, linewidth = 0.15, alpha = 0.75) +
    scale_fill_manual(
      name   = "Commonwealth",
      values = c("National Park Zone" = "#ffc8c2"),
      labels = c("National Park Zone" = "National Parks Zones (no fishing)"),
      guide  = if (show_legend) guide_legend(
        order          = 3,
        title.position = "top",
        override.aes   = list(alpha = 0.75)
      ) else "none"
    ) +

    coord_sf(xlim = plot_limits[1:2], ylim = plot_limits[3:4],
             crs = 4326, expand = FALSE) +
    labs(x = NULL, y = NULL) +
    theme_minimal() +
    theme(
      legend.key.size        = unit(0.5, "cm"),
      legend.key             = element_blank(),
      legend.text            = element_text(size = 9),
      legend.title           = element_text(size = 10),
      legend.position        = if (show_legend) "inside" else "none",
      legend.position.inside = c(0.995, 0.98),
      legend.justification   = c("right", "top"),
      legend.box             = "vertical",
      legend.spacing.y       = unit(0.3, "cm"),
      legend.background      = element_blank(),
      legend.box.background  = element_blank(),
      legend.box.margin      = margin(0, 0, 0, 0),
      legend.margin          = margin(t = 5, r = 6, b = 5, l = 6),
      panel.grid             = element_blank(),
      panel.background       = element_rect(fill = "white", colour = NA),
      plot.background        = element_rect(fill = "white", colour = NA),
      axis.text              = element_text(size = 9,  colour = "grey40"),
      axis.ticks             = element_line(colour = "grey60"),
      panel.border           = element_rect(fill = NA, colour = "grey60", linewidth = 0.4),
      plot.margin            = margin(t = 5, r = 5, b = 5, l = 5)
    )

  return(p)
}

# ==============================================================================
# 6. CREATE OUTPUT FOLDER
# ==============================================================================

out_dir <- paste0("plots/", park, "/spatial/notake_bathy_overview_maps")

# ==============================================================================
# 7. WA OVERVIEW
# ==============================================================================

ggsave(file.path(out_dir, paste0(name, "-wa-overview-virdis-TEST.png")),
       plot = make_bathy_map(plot_limits     = c(108.0, 122.0, -38.0, -24.0),
                             show_legend     = TRUE,
                             palette         = bathy_palette),
       dpi = 600, width = 10, height = 11, bg = "white")

ggsave(file.path(out_dir, paste0(name, "-wa-overview-virdis-INSE-TEST.png")),
       plot = make_bathy_map_inset(plot_limits     = c(108.0, 122.0, -38.0, -24.0),
                             show_legend     = TRUE,
                             palette         = bathy_palette),
       dpi = 600, width = 10, height = 11, bg = "white")


# ==============================================================================
# 8. SOUTH-WEST CORNER
# ==============================================================================

ggsave(file.path(out_dir, paste0(name, "-swc-virdis-TEST.png")),
       plot = make_bathy_map(plot_limits     = c(114.2, 116.2, -34.6, -33.4),
                             show_legend     = TRUE,
                             palette         = bathy_palette),
       dpi = 600, width = 10, height = 6, bg = "white")

# ==============================================================================
# 9. JURIEN BAY
# ==============================================================================

ggsave(file.path(out_dir, paste0(name, "-jurien-virdis-TEST.png")),
       plot = make_bathy_map(plot_limits     = c(114.5, 115.4, -30.8, -30.0),
                             show_legend     = FALSE,
                             palette         = bathy_palette),
       dpi = 600, width = 10, height = 8, bg = "white")


# ==============================================================================
# 10. ROCKINGHAM
# ==============================================================================

ggsave(file.path(out_dir, paste0(name, "-rockingham-virdis-TEST.png")),
       plot = make_bathy_map(plot_limits     = c(115.2, 116.0, -32.6, -31.8),
                             show_legend     = FALSE,
                             palette         = bathy_palette),
       dpi = 600, width = 10, height = 8, bg = "white")

# ==============================================================================
# 10. Abrolhos
# ==============================================================================

ggsave(file.path(out_dir, paste0(name, "-abrolhos-virdis-TEST.png")),
       plot = make_bathy_map(plot_limits = c(113.3, 114.3, -29.2, -28.2),
                             show_legend     = FALSE,
                             palette         = bathy_palette),
       dpi = 600, width = 10, height = 8, bg = "white")



# ==============================================================================
# GASCOYNE — SEPARATE RASTER LOAD & CROP
# ==============================================================================

e_gasc <- ext(108.0, 122.0, -30.0, -16.0)

bathy_gasc <- rast("data/south-west network/spatial/rasters/AusBathyTopo__Australia__2024_250m_MSL_cog.tif") %>%
  project("EPSG:4326", method = "bilinear") %>%
  crop(e_gasc)

bathy_shelf_gasc        <- clamp(bathy_gasc, lower = -200, upper = 0, values = FALSE)
names(bathy_shelf_gasc) <- "depth"
bathy_df_gasc           <- as.data.frame(bathy_shelf_gasc, xy = TRUE, na.rm = TRUE)
colnames(bathy_df_gasc)[3] <- "depth"

slope_gasc  <- terrain(bathy_gasc, v = "slope",  unit = "radians")
aspect_gasc <- terrain(bathy_gasc, v = "aspect", unit = "radians")
hs_gasc     <- shade(slope_gasc, aspect_gasc, angle = 35, direction = 315, normalize = TRUE)
names(hs_gasc) <- "hillshade"
hs_df_gasc     <- as.data.frame(hs_gasc, xy = TRUE, na.rm = TRUE)
colnames(hs_df_gasc)[3] <- "hillshade"

# ==============================================================================
# GASCOYNE — MARINE PARKS
# (edit park names to match what is in your shapefile for the Gascoyne region)
# ==============================================================================

marine_parks_gasc <- st_read("data/south-west network/spatial/shapefiles/south-and-western-australia_marine-parks-all.shp") %>%
  dplyr::filter(name %in% c("Ningaloo", "Shark Bay", "Gascoyne",
                            "Abrolhos", "Abrolhos Islands",
                            "Murat", "Two Rocks", "West coast Bays")) %>%
  st_transform(4326)

mp_state_sanctuary_gasc <- marine_parks_gasc %>%
  dplyr::filter(epbc == "State",
                zone %in% c("Reef Observation Area", "Sanctuary Zone"))

mp_cwlth_sanctuary_gasc <- marine_parks_gasc %>%
  dplyr::filter(epbc == "Commonwealth",
                zone %in% c("National Park Zone"))

# ==============================================================================
# GASCOYNE — MAP FUNCTION
# Uses _gasc data objects and dynamic legend box position
# ==============================================================================

make_bathy_map_inset_gasc <- function(plot_limits,
                                 show_legend = TRUE,
                                 label_size  = 3.5,
                                 palette     = bathy_palette) {

  p <- ggplot() +

    geom_tile(data = bathy_df_gasc, aes(x = x, y = y, fill = depth)) +
    scale_fill_gradientn(
      colours  = palette,
      limits   = c(-200, 0),
      na.value = NA,
      name     = "Depth (m)",
      breaks   = c(0, -50, -100, -150, -200),
      labels   = c("0", "-50", "-100", "-150", "-200"),
      guide    = if (show_legend) guide_colorbar(
        barwidth       = 1.2,
        barheight      = 8,
        title.position = "top",
        ticks          = TRUE,
        order          = 1
      ) else "none"
    ) +

    new_scale_fill() +
    geom_tile(data = hs_df_gasc, aes(x = x, y = y, fill = hillshade),
              alpha = 0.3, show.legend = FALSE) +
    scale_fill_gradient(low      = "grey20",
                        high     = "#e8e8e8",
                        na.value = NA,
                        guide    = "none") +

    geom_contour(data = bathy_df, aes(x = x, y = y, z = depth),
                 breaks    = c(-120, -60),
                 colour    = "white",
                 alpha     = 0.35,
                 linewidth = 0.25) +

    geom_sf(data = aus, fill = "seashell2", colour = "grey50", linewidth = 0.25) +

    # Terrestrial parks drawn BEFORE the white box so the box sits on top
    new_scale_fill() +
    geom_sf(data = terrnp, aes(fill = leg_catego), colour = NA, alpha = 0.8) +
    scale_fill_manual(
      values = c("National Park"  = "#c4cea6",
                 "Nature Reserve" = "#e4d0bb"),
      name   = "Terrestrial Parks",
      guide  = if (show_legend) guide_legend(
        order          = 4,
        title.position = "top",
        override.aes   = list(alpha = 0.8)
      ) else "none"
    ) +

    # White background polygon — drawn AFTER terrnp so it covers it in the legend area
    annotate("rect",
             xmin      = 117.7, xmax = 121.9,
             ymin      = -22.7, ymax = -16.1,
             fill      = "white",
             colour    = "grey70",
             linewidth = 0.3) +

    # Marine park zones drawn AFTER the white box so they show on top of it
    new_scale_fill() +
    geom_sf(data = mp_state_sanctuary_gasc, aes(fill = zone),
            colour = NA, linewidth = 0.15, alpha = 0.75) +
    scale_fill_manual(
      name   = "State",
      values = c("Sanctuary Zone"        = "#fc887c",
                 "Reef Observation Area" = "#ff4430"),
      labels = c("Reef Observation Area" = "Reef Observation Areas (no line fishing)",
                 "Sanctuary Zone"        = "Sanctuary Zones (no fishing)"),
      guide  = if (show_legend) guide_legend(
        order          = 2,
        title.position = "top",
        override.aes   = list(alpha = 0.75)
      ) else "none"
    ) +

    new_scale_fill() +
    geom_sf(data = mp_cwlth_sanctuary_gasc, aes(fill = zone),
            colour = NA, linewidth = 0.15, alpha = 0.75) +
    scale_fill_manual(
      name   = "Commonwealth",
      values = c("National Park Zone" = "#ffc8c2"),
      labels = c("National Park Zone" = "National Parks Zones (no fishing)"),
      guide  = if (show_legend) guide_legend(
        order          = 3,
        title.position = "top",
        override.aes   = list(alpha = 0.75)
      ) else "none"
    ) +

    coord_sf(xlim = plot_limits[1:2], ylim = plot_limits[3:4],
             crs = 4326, expand = FALSE) +
    labs(x = NULL, y = NULL) +
    theme_minimal() +
    theme(
      legend.key.size        = unit(0.5, "cm"),
      legend.key             = element_blank(),
      legend.text            = element_text(size = 9),
      legend.title           = element_text(size = 10),
      legend.position        = if (show_legend) "inside" else "none",
      legend.position.inside = c(0.995, 0.98),
      legend.justification   = c("right", "top"),
      legend.box             = "vertical",
      legend.spacing.y       = unit(0.3, "cm"),
      legend.background      = element_blank(),
      legend.box.background  = element_blank(),
      legend.box.margin      = margin(0, 0, 0, 0),
      legend.margin          = margin(t = 5, r = 6, b = 5, l = 6),
      panel.grid             = element_blank(),
      panel.background       = element_rect(fill = "white", colour = NA),
      plot.background        = element_rect(fill = "white", colour = NA),
      axis.text              = element_text(size = 9,  colour = "grey40"),
      axis.ticks             = element_line(colour = "grey60"),
      panel.border           = element_rect(fill = NA, colour = "grey60", linewidth = 0.4),
      plot.margin            = margin(t = 5, r = 5, b = 5, l = 5)
    )

  return(p)
}

# ==============================================================================
# GASCOYNE MAP 1 — same extent as WA overview, shifted north
# Matches c(108.0, 122.0, -38.0, -24.0) but moved up by ~6 degrees
# ==============================================================================

ggsave(file.path(out_dir, paste0(name, "-gascoyne-virdis-INSET-TEST.png")),
       plot = make_bathy_map_inset_gasc(plot_limits     = c(108.0, 122.0, -30.0, -16.0),
                                   show_legend     = TRUE,
                                   palette         = bathy_palette),
       dpi = 600, width = 10, height = 11, bg = "white")


# ==============================================================================
# GASCOYNE MAP 2 — zoomed in on Gascoyne marine region
# ==============================================================================

ggsave(file.path(out_dir, "gascoyne-zoomed-viridis.png"),
       plot = make_bathy_map_inset_gasc(
         plot_limits = c(112.0, 116.5, -26.5, -22.0),
         show_legend = TRUE,
         palette     = bathy_palette
       ),
       dpi = 600, width = 10, height = 9, bg = "white")

