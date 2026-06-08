###
# Project: NESP 4.20 - Marine Park Dashboard reporting
# Data:    Marine Park, oceanographic data, marine park boundary files and
#          image legend for bathy network map, cropped to have no title
#          This image can be found and saved from this link:
#          https://geoserver.imas.utas.edu.au/geoserver/seamap/wms?SERVICE=WMS&VERSION=1.1.1&REQUEST=GetLegendGraphic&LAYER=seamap:bathymetry_AMP_grp&FORMAT=image/png
# Task:    Create South west network WMS bathymetry maps (full network + zoom-ins)
# Author:  Annika
# Date:    May 2026
###

# Table of contents
# 1.   Setup - libraries, names, spatial data
# 1.5  Save WMS rasters for all extents
# 2.   network_map_with_legend()   - full network map with WMS bathy + image legend
# 3.   network_map_wms_zoomed()    - zoom-in panels with WMS bathy, left-hand legend


####################### DO NOT OVERWRITE EXISTING PNG IF ALREADY THERE ###############################
# Clear your environment
rm(list = ls())

# ==============================================================================
# 1. Setup
# ==============================================================================

# Set the study name and marine park name (for folder structure)
name <- "south-west"
park <- "network"

# Load libraries
library(tidyverse)
library(sf)
library(rnaturalearth)
library(patchwork)
library(terra)
library(tidyterra)
library(ggnewscale)
library(CheckEM)
library(cowplot)
library(png)
library(grid)
library(gridExtra)
library(stars)
library(scales)

# Set cropping extent
e <- ext(106.0, 145.0, -45.0, -22.0)

sf_use_s2(TRUE)

# Australian state outlines
aus <- st_read("data/south-west network/spatial/shapefiles/STE_2021_AUST_GDA2020.shp") %>%
  st_make_valid()
ausc <- st_crop(aus, e)

# CAPAD - Commonwealth marine parks (for inset)
capad <- st_read("data/south-west network/spatial/shapefiles/Collaborative_Australian_Protected_Areas_Database_(CAPAD)_2022_-_Marine.shp")

# All marine parks (state + commonwealth) filtered to SWC network
marine_parks <- st_read("data/south-west network/spatial/shapefiles/south-and-western-australia_marine-parks-all.shp") %>%
  dplyr::filter(name %in% c(
    "Abrolhos", "Abrolhos Islands", "Bremer", "Eastern Recherche",
    "Ngari Capes", "Geographe", "South-west Corner", "Great Australian Bight",
    "Jurien", "Murat", "Jurien Bay", "Perth Canyon",
    "Southern Kangaroo Island", "Twilight", "Two Rocks", "Western Eyre",
    "Western Kangaroo Island", "Nuyts Archipelgo", "Thorny Passage",
    "Sir Joseph Banks Group", "Investigator", "West coast Bays",
    "Southern Spencer Gulf", "Upper Spencer Gulf", "Cottesloe Reef",
    "Rottnest", "Shoalwater Islands"
  ))

# Australian Marine Parks only
marine_parks_amp <- marine_parks %>%
  dplyr::filter(epbc %in% "Commonwealth")

# State Marine Parks only - standardise zone naming and colours
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

# Terrestrial parks
terrnp <- st_read("data/south-west network/spatial/shapefiles/Collaborative_Australian_Protected_Areas_Database_(CAPAD)_2024_-_Terrestrial__.shp") %>%
  dplyr::filter(TYPE %in% c("Nature Reserve", "National Park"))

# Coastal waters limit
cwatr <- st_read("data/south-west network/spatial/shapefiles/amb_coastal_waters_limit.shp") %>%
  st_make_valid() %>%
  st_crop(e)

# Bathymetry raster
bathy <- rast("data/south-west network/spatial/rasters/AusBathyTopo__Australia__2024_250m_MSL_cog.tif") %>%
  crop(e) %>%
  clamp(upper = 0, values = FALSE)
names(bathy) <- "Depth"


# ==============================================================================
# Helper: filter marine parks to a plot extent
# ==============================================================================

filter_to_extent <- function(mp, plot_limits) {
  extent_box <- st_bbox(
    c(xmin = plot_limits[1], xmax = plot_limits[2],
      ymin = plot_limits[3], ymax = plot_limits[4]),
    crs = st_crs(4326)
  ) %>% st_as_sfc()
  mp %>% dplyr::filter(st_intersects(geometry, extent_box, sparse = FALSE)[, 1])
}


# ==============================================================================
# Helper: download and prepare WMS raster for a given extent
# ==============================================================================

get_wms_raster <- function(plot_limits, width_px = 2000, height_px = 1067, expand = 1.2) {

  # Expand the request extent
  lon_range <- plot_limits[2] - plot_limits[1]
  lat_range <- plot_limits[4] - plot_limits[3]
  lon_pad   <- (lon_range * expand - lon_range) / 2
  lat_pad   <- (lat_range * expand - lat_range) / 2

  # bbox order: xmin, ymin, xmax, ymax
  bbox_exp <- c(
    plot_limits[1] - lon_pad,  # xmin
    plot_limits[3] - lat_pad,  # ymin
    plot_limits[2] + lon_pad,  # xmax
    plot_limits[4] + lat_pad   # ymax
  )

  wms_url <- paste0(
    "https://geoserver.imas.utas.edu.au/geoserver/seamap/wms",
    "?SERVICE=WMS&VERSION=1.1.1&REQUEST=GetMap",
    "&LAYERS=seamap:bathymetry_AMP_grp",
    "&STYLES=&SRS=EPSG:4326",
    "&BBOX=", paste(bbox_exp, collapse = ","),
    "&WIDTH=", width_px,
    "&HEIGHT=", height_px,
    "&FORMAT=image/png&TRANSPARENT=TRUE"
  )

  tmp_wms <- tempfile(fileext = ".png")
  options(timeout = 1200)
  attempt <- 0
  success <- FALSE
  while (attempt < 3 & !success) {
    attempt <- attempt + 1
    tryCatch({
      download.file(wms_url, destfile = tmp_wms, mode = "wb", quiet = FALSE)
      success <- TRUE
    }, error = function(e) {
      message("Attempt ", attempt, " failed: ", e$message)
      Sys.sleep(5)
    })
  }
  if (!success) stop("WMS download failed after 3 attempts")

  amp_img <- rast(tmp_wms)
  amp_img <- flip(amp_img, direction = "vertical")
  # Georeference to expanded extent (terra ext() order: xmin, xmax, ymin, ymax)
  ext(amp_img) <- ext(bbox_exp[1], bbox_exp[3], bbox_exp[2], bbox_exp[4])
  crs(amp_img) <- "EPSG:4326"

  # Clip back to original plot extent
  amp_img <- crop(amp_img, ext(plot_limits[1], plot_limits[2], plot_limits[3], plot_limits[4]))

  # Mask white pixels
  white_mask <- (amp_img[[1]] == 255 & amp_img[[2]] == 255 & amp_img[[3]] == 255)
  amp_img[white_mask] <- NA

  amp_img
}


# ==============================================================================
# 1.5  Download and save WMS rasters for all extents
# ==============================================================================
# Saves cropped WMS GeoTIFF tiles to:
#   data/south-west network/spatial/rasters/AMP/
# Files are only downloaded if they do not already exist.

raster_out_dir <- "data/south-west network/spatial/rasters/AMP"
dir.create(raster_out_dir, recursive = TRUE, showWarnings = FALSE)

save_wms_raster <- function(name, limits, width_px = 2000, height_px = 1067) {
  out_path <- file.path(raster_out_dir, paste0(name, "_wms_bathy.tif"))
  if (file.exists(out_path)) {
    message("Skipping ", name, " - already exists"); return(invisible(NULL))
  }
  message("Downloading: ", name)
  r <- get_wms_raster(limits, width_px = width_px, height_px = height_px)
  writeRaster(r, out_path, overwrite = FALSE)
  message("Saved -> ", out_path)
}

# Run these ONE at a time!
save_wms_raster("network",            c(108.0, 138.0,    -40.0,    -24.0))
save_wms_raster("abrolhos",           c(108.5, 116.1,    -30.0,    -24.2))
save_wms_raster("bremer",             c(119.3, 120.3,    -35.3,    -33.9), 1000, 1000)
save_wms_raster("eastern-recherche",  c(123.2, 124.4,    -34.9,    -33.5), 1200, 1200)
save_wms_raster("geographe",          c(114.8, 115.7,    -33.7,    -33.2), 1600,  800)
save_wms_raster("great-aus-bight",    c(128.7, 132.5,    -33.6,    -31.3), 1600,  900)
save_wms_raster("jurien",             c(114.2, 115.5,    -31.0,    -30.0), 1600,  800)
save_wms_raster("kangaroo-island",    c(136.0, 137.85,   -36.5,    -35.5), 1600, 1067)
save_wms_raster("murat-western-eyre", c(132.45, 135.5,   -35.4,    -31.9), 1400, 1200)
save_wms_raster("rottnest-canyon",    c(113.8, 115.8,    -32.8,    -31.3), 1600, 1067)
save_wms_raster("swc-east",           c(120.35, 122.2,   -35.5,    -33.7), 1400, 1200)
save_wms_raster("swc-west",           c(113.5, 116.4,    -34.7857, -33.2643), 1600, 900)
save_wms_raster("two-rocks",          c(114.7, 116.0,    -32.0,    -31.3), 1600,  900)


# ==============================================================================
# 2. Full network map with WMS bathymetry + image depth legend
# ==============================================================================

network_map_with_legend <- function(plot_limits,
                                    study_limits      = NULL,
                                    annotation_labels = NULL) {

  # --- WMS image ---
  amp_img <- get_wms_raster(plot_limits, width_px = 4000, expand = 1.5)

  # --- Depth legend image ---
  legend_img  <- png::readPNG("data/south-west network/spatial/rasters/depth-legend-2.png")
  img_h_px    <- nrow(legend_img)
  img_w_px    <- ncol(legend_img)
  plot_h_in   <- 6 * (4 / 5) * 0.70
  legend_w_in <- plot_h_in * (img_w_px / img_h_px)

  legend_grob <- grid::rasterGrob(
    legend_img, interpolate = TRUE,
    x      = unit(0.5, "npc"),
    y      = unit(1,   "npc"),
    width  = unit(legend_w_in, "inches"),
    height = unit(plot_h_in,   "inches"),
    just   = c("centre", "top")
  )

  title_grob <- grid::textGrob(
    "Composite bathymetry \nmosaics per Australian \nMarine Park (AMP)\n \n    Depth (m)",
    x    = unit(0, "npc"),
    y    = unit(0.5, "npc"),
    just = c("left", "centre"),
    gp   = grid::gpar(fontsize = 8, fontface = "plain")
  )

  right_grob <- gridExtra::arrangeGrob(
    title_grob,
    legend_grob,
    nrow    = 2,
    heights = grid::unit(c(0.18, 0.82), "npc")
  )

  # --- State MPs clipped to extent ---
  mp_state_zoom <- filter_to_extent(marine_parks_state, plot_limits) %>%
    dplyr::mutate(colour = case_when(
      zone == "Sanctuary Zone"               ~ "#b3de69",
      zone == "General Use Zone"             ~ "#80cdc1",
      zone == "Recreational Use Zone"        ~ "#fee08b",
      zone == "Special Purpose Zone"         ~ "#9970ab",
      zone == "Other State Marine Park Zone" ~ "#f7d0dc",
      TRUE ~ colour
    ))

  terr_fills_ordered <- scale_fill_manual(
    values = c("National Park" = "#c4cea6", "Nature Reserve" = "#e4d0bb"),
    name   = "Terrestrial Parks",
    guide  = guide_legend(order = 2, ncol = 1)
  )

  # --- Main map ---
  p1 <- ggplot() +
    geom_spatraster_contour_filled(data = bathy,
                                   breaks = c(0, -30, -70, -200, -700, -2000, -4000, -6000),
                                   colour = NA, show.legend = FALSE, maxcell = 5e6) +
    scale_fill_manual(values = c("#FFFFFF", "#EFEFEF", "#DEDEDE", "#CCCCCC",
                                 "#B6B6B6", "#9E9E9E", "#808080")) +
    new_scale_fill() +
    geom_spatraster_contour(data = bathy,
                            breaks = c(-30, -70, -200, -700, -2000, -4000, -6000),
                            colour = "white", alpha = 3/5, linewidth = 0.1,
                            show.legend = FALSE, maxcell = 5e6) +
    geom_sf(data = ausc, fill = "seashell2", colour = "grey80", linewidth = 0.1) +
    geom_spatraster_rgb(data = amp_img, alpha = 1) +
    geom_sf(data = terrnp, aes(fill = TYPE), colour = NA, alpha = 0.8) +
    terr_fills_ordered +
    new_scale_fill() +
    geom_sf(data = mp_state_zoom, aes(fill = zone), colour = NA, alpha = 0.6) +
    scale_fill_manual(
      name   = "State Marine Parks",
      guide  = guide_legend(order = 3, ncol = 1),
      values = with(mp_state_zoom, setNames(colour, zone)),
      breaks = c("Sanctuary Zone", "General Use Zone", "Recreational Use Zone",
                 "Special Purpose Zone", "Other State Marine Park Zone")
    ) +
    new_scale_fill() +
    geom_sf(data = cwatr, colour = "firebrick", alpha = 1,
            linewidth = 0.1, lineend = "round") +
    labs(x = NULL, y = NULL) +
    {if (!is.null(annotation_labels))
      list(
        geom_point(data = annotation_labels, aes(x = x, y = y),
                   shape = 4, size = 1, stroke = 0.5, colour = "black"),
        geom_text(data = annotation_labels, aes(x = x, y = y, label = label),
                  size = 1.65, fontface = "italic", nudge_y = -0.03)
      )} +
    {if (!is.null(study_limits))
      annotate("rect",
               xmin = study_limits[1], xmax = study_limits[2],
               ymin = study_limits[3], ymax = study_limits[4],
               fill = NA, colour = "goldenrod2", linewidth = 0.4)} +
    coord_sf(xlim = c(plot_limits[1], plot_limits[2]),
             ylim = c(plot_limits[3], plot_limits[4]), crs = 4326) +
    theme_minimal() +
    theme(legend.key.size  = unit(0.4, "cm"),
          legend.text      = element_text(size = 7),
          legend.title     = element_text(size = 8),
          legend.position  = "bottom",
          legend.box       = "horizontal",
          legend.direction = "vertical",
          panel.grid       = element_blank(),
          panel.background = element_rect(fill = "white", colour = NA),
          plot.background  = element_rect(fill = "white", colour = NA),
          panel.border     = element_rect(colour = "grey80", fill = NA, linewidth = 0.5),
          axis.ticks       = element_line(colour = "grey80", linewidth = 0.3))

  # --- State / terrestrial legend ---
  p1_other_legend <- cowplot::get_legend(p1 + theme(
    legend.text      = element_text(size = 7),
    legend.title     = element_text(size = 8),
    legend.key.size  = unit(0.3, "cm"),
    legend.position  = "bottom",
    legend.box       = "horizontal",
    legend.direction = "vertical"
  ))

  # --- Inset ---
  p1.1 <- ggplot(data = aus) +
    geom_sf(fill = "seashell1", colour = "grey90", linewidth = 0.05, alpha = 4/5) +
    geom_sf(data = capad, alpha = 5/6, colour = "grey85", linewidth = 0.02) +
    coord_sf(xlim = c(105, 160), ylim = c(-48, -8)) +
    annotate("rect",
             xmin = plot_limits[1], xmax = plot_limits[2],
             ymin = plot_limits[3], ymax = plot_limits[4],
             colour = "grey25", fill = "white", alpha = 1/5, linewidth = 0.2) +
    theme_bw() +
    theme(axis.text        = element_blank(),
          axis.ticks       = element_blank(),
          panel.grid.major = element_blank(),
          panel.border     = element_rect(colour = "grey70"))

  # --- Assemble ---
  p1_no_legend <- p1 + theme(legend.position = "none",
                             plot.margin    = margin(0, 0, 15, 0))

  right_panel      <- wrap_elements(right_grob)
  right_panel_w_in <- 1.8

  map_row    <- p1_no_legend + right_panel +
    plot_layout(widths = c(4, right_panel_w_in / (11 - right_panel_w_in) * 4))

  bottom_row <- p1.1 + wrap_elements(p1_other_legend) + plot_spacer() +
    plot_layout(widths = c(0.28, 1, 0.5))

  map_row / bottom_row +
    plot_layout(heights = c(4, 1))
}

# Run and save full network map
p_network <- network_map_with_legend(plot_limits = c(108.0, 138.0, -40.0, -24.0))

ggsave(paste(paste0("plots/", park, "/spatial/AMP_bathy/", name), "network_AMP-bathy-plot.png", sep = "-"),
       plot = p_network, dpi = 600, width = 8.5, height = 6, bg = "white")


# ==============================================================================
# 3. Zoom-in panels with WMS bathymetry - left-hand legend, no image legend
# ==============================================================================

network_map_wms_zoomed <- function(plot_limits,
                                   inset_xlim = c(108, 138),
                                   inset_ylim = c(-40, -24),
                                   show_inset = TRUE,
                                   save_name  = NULL,
                                   width      = 10,
                                   height     = 6) {

  # --- WMS image for this extent ---
  amp_img <- get_wms_raster(plot_limits)

  # --- Filter parks to extent ---
  mp_amp_zoom   <- filter_to_extent(marine_parks_amp,   plot_limits)
  mp_state_zoom <- filter_to_extent(marine_parks_state, plot_limits) %>%
    dplyr::mutate(colour = case_when(
      zone == "Sanctuary Zone"               ~ "#b3de69",
      zone == "General Use Zone"             ~ "#80cdc1",
      zone == "Recreational Use Zone"        ~ "#fee08b",
      zone == "Special Purpose Zone"         ~ "#9970ab",
      zone == "Other State Marine Park Zone" ~ "#f7d0dc",
      TRUE ~ colour
    ))

  terr_fills_ordered <- scale_fill_manual(
    values = c("National Park" = "#c4cea6", "Nature Reserve" = "#e4d0bb"),
    name   = "Terrestrial Parks",
    guide  = guide_legend(order = 2, ncol = 1)
  )

  # --- Main map panel ---
  p_map <- ggplot() +
    geom_spatraster_contour_filled(data = bathy,
                                   breaks = c(0, -30, -70, -200, -700, -2000, -4000, -6000),
                                   colour = NA, show.legend = FALSE, maxcell = 5e6) +
    scale_fill_manual(values = c("#FFFFFF", "#EFEFEF", "#DEDEDE", "#CCCCCC",
                                 "#B6B6B6", "#9E9E9E", "#808080")) +
    new_scale_fill() +
    geom_spatraster_contour(data = bathy,
                            breaks = c(-30, -70, -200, -700, -2000, -4000, -6000),
                            colour = "white", alpha = 3/5, linewidth = 0.1,
                            show.legend = FALSE, maxcell = 5e6) +
    geom_sf(data = ausc, fill = "seashell2", colour = "grey80", linewidth = 0.1) +
    geom_spatraster_rgb(data = amp_img, alpha = 1) +
    geom_sf(data = terrnp, aes(fill = TYPE), colour = NA, alpha = 0.8) +
    terr_fills_ordered +
    new_scale_fill() +
    geom_sf(data = mp_state_zoom, aes(fill = zone), colour = NA, alpha = 0.6) +
    scale_fill_manual(
      name   = "State Marine Parks",
      guide  = guide_legend(order = 3, ncol = 1),
      values = with(mp_state_zoom, setNames(colour, zone)),
      breaks = c("Sanctuary Zone", "General Use Zone", "Recreational Use Zone",
                 "Special Purpose Zone", "Other State Marine Park Zone")
    ) +
    new_scale_fill() +
    geom_sf(data = cwatr, colour = "firebrick", alpha = 1,
            linewidth = 0.1, lineend = "round") +
    labs(x = NULL, y = NULL) +
    coord_sf(xlim = c(plot_limits[1], plot_limits[2]),
             ylim = c(plot_limits[3], plot_limits[4]), crs = 4326) +
    theme_minimal() +
    theme(legend.key.size  = unit(0.5, "cm"),
          legend.text      = element_text(size = 9),
          legend.title     = element_text(size = 11),
          legend.position  = "left",
          legend.box       = "vertical",
          legend.direction = "vertical",
          panel.grid       = element_blank(),
          panel.background = element_rect(fill = "white", colour = NA),
          plot.background  = element_rect(fill = "white", colour = NA),
          panel.border     = element_rect(colour = "grey80", fill = NA, linewidth = 0.5),
          axis.ticks       = element_line(colour = "grey80", linewidth = 0.3)) +
    guides(fill = guide_legend(ncol = 1))

  # --- Extract legend ---
  legend_single <- cowplot::get_legend(p_map + theme(
    legend.position  = "left",
    legend.box       = "vertical",
    legend.direction = "vertical",
    legend.text      = element_text(size = 9),
    legend.title     = element_text(size = 11),
    legend.key.size  = unit(0.5, "cm"),
    legend.spacing.y = unit(0.2, "cm")
  ))

  # --- Inset ---
  if (show_inset) {
    p_inset <- ggplot(data = aus) +
      geom_sf(fill = "seashell1", colour = "grey90", linewidth = 0.05, alpha = 4/5) +
      geom_sf(data = capad, alpha = 5/6, colour = "grey85", linewidth = 0.02) +
      annotate("rect",
               xmin = plot_limits[1], xmax = plot_limits[2],
               ymin = plot_limits[3], ymax = plot_limits[4],
               colour = "grey25", fill = "white", alpha = 1/5, linewidth = 0.3) +
      coord_sf(xlim = inset_xlim, ylim = inset_ylim) +
      theme_bw() +
      theme(axis.text        = element_blank(),
            axis.ticks       = element_blank(),
            panel.grid.major = element_blank(),
            panel.border     = element_rect(colour = "grey70"))

    left_col <- cowplot::plot_grid(
      legend_single,
      NULL,
      p_inset,
      ncol        = 1,
      rel_heights = c(1, 0.1, 0.45)
    )
  } else {
    left_col <- cowplot::plot_grid(legend_single, ncol = 1)
  }

  # --- Assemble ---
  p_map_nl <- p_map + theme(legend.position = "none",
                            plot.margin    = margin(0, 0, 0, 15))

  fig <- cowplot::plot_grid(
    left_col,
    p_map_nl,
    nrow       = 1,
    rel_widths = c(0.32, 1)
  ) +
    theme(plot.background = element_rect(fill = "white", colour = NA),
          plot.margin     = margin(5, 5, 5, 5))

  print(fig)

  if (!is.null(save_name)) {
    dir.create(paste0("plots/", park, "/spatial/AMP_bathy/"), recursive = TRUE, showWarnings = FALSE)
    ggsave(
      paste(paste0("plots/", park, "/spatial/AMP_bathy/", name), paste0(save_name, ".png"), sep = "-"),
      plot   = fig,
      dpi    = 300,
      width  = width,
      height = height,
      bg     = "white"
    )
  }

  return(invisible(fig))
}


# ==============================================================================
# Run zoom-in plots
# ==============================================================================
# Run these ONE at a time! - or else they render very weirdly

# Abrolhos
network_map_wms_zoomed(
  plot_limits = c(108.5, 116.1, -30.0, -24.2),
  inset_xlim  = c(108.0, 138.0),
  inset_ylim  = c(-40.0, -24.0),
  show_inset  = TRUE,
  save_name   = "abrolhos-AMP-bathy",
  width       = 10,
  height      = 6
)

# Bremer Bay
network_map_wms_zoomed(
  plot_limits = c(119.3, 120.3, -35.3, -33.9),
  inset_xlim  = c(108, 138),
  inset_ylim  = c(-40, -24),
  show_inset  = TRUE,
  save_name   = "bremer-AMP-bathy",
  width       = 8,
  height      = 8
)

# Eastern Recherche
network_map_wms_zoomed(
  plot_limits = c(123.2, 124.4, -34.9, -33.5),
  inset_xlim  = c(108, 138),
  inset_ylim  = c(-40, -24),
  show_inset  = TRUE,
  save_name   = "eastern-recherche-AMP-bathy",
  width       = 8,
  height      = 8
)

# Geographe
network_map_wms_zoomed(
  plot_limits = c(114.8, 115.7, -33.7, -33.2),
  inset_xlim  = c(108.0, 138.0),
  inset_ylim  = c(-40.0, -24.0),
  show_inset  = TRUE,
  save_name   = "geographe-AMP-bathy",
  width       = 10,
  height      = 6
)

# Great Australian Bight
network_map_wms_zoomed(
  plot_limits = c(128.7, 132.5, -33.6, -31.3),
  inset_xlim  = c(108, 138),
  inset_ylim  = c(-40, -24),
  show_inset  = TRUE,
  save_name   = "great-aus-bight-AMP-bathy",
  width       = 9,
  height      = 5
)

# Jurien Bay
network_map_wms_zoomed(
  plot_limits = c(114.2, 115.5, -31.0, -30.0),
  inset_xlim  = c(108, 138),
  inset_ylim  = c(-40, -24),
  show_inset  = TRUE,
  save_name   = "jurien-AMP-bathy",
  width       = 8,
  height      = 5
)

# Kangaroo Island
network_map_wms_zoomed(
  plot_limits = c(136.0, 137.85, -36.5, -35.5),
  inset_xlim  = c(108, 138),
  inset_ylim  = c(-40, -24),
  show_inset  = TRUE,
  save_name   = "kangaroo-island-AMP-bathy",
  width       = 9,
  height      = 6
)

# Murat and Western Eyre
network_map_wms_zoomed(
  plot_limits = c(132.45, 135.5, -35.4, -31.9),
  inset_xlim  = c(108, 138),
  inset_ylim  = c(-40, -24),
  show_inset  = TRUE,
  save_name   = "murat-western-eyre-AMP-bathy",
  width       = 8,
  height      = 7
)

# Rottnest Island Canyon
network_map_wms_zoomed(
  plot_limits = c(113.8, 115.8, -32.8, -31.3),
  inset_xlim  = c(108, 138),
  inset_ylim  = c(-40, -24),
  show_inset  = TRUE,
  save_name   = "rottnest-canyon-AMP-bathy",
  width       = 10,
  height      = 6
)

# SWC eastern arm
network_map_wms_zoomed(
  plot_limits = c(120.35, 122.2, -35.5, -33.7),
  inset_xlim  = c(108, 138),
  inset_ylim  = c(-40, -24),
  show_inset  = TRUE,
  save_name   = "swc-east-AMP-bathy",
  width       = 8,
  height      = 6
)

# SWC western arm
network_map_wms_zoomed(
  plot_limits = c(113.5, 116.4, -34.7857, -33.2643),
  inset_xlim  = c(108, 138),
  inset_ylim  = c(-40, -24),
  show_inset  = TRUE,
  save_name   = "swc-west-AMP-bathy",
  width       = 8,
  height      = 5
)

# Two Rocks
network_map_wms_zoomed(
  plot_limits = c(114.7, 116.0, -32.0, -31.3),
  inset_xlim  = c(108, 138),
  inset_ylim  = c(-40, -24),
  show_inset  = TRUE,
  save_name   = "tworocks-AMP-bathy",
  width       = 9,
  height      = 5
)
