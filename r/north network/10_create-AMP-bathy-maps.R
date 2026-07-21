###
# Project: NESP 4.20 - Marine Park Dashboard reporting
# Data:    IMAS WMS (MERI greyscale + AMP composite bathymetry), aus outline,
#          CAPAD marine (inset), terrestrial parks (CAPAD), coastal waters
#          limit, AMP zone boundaries (WFS), saved depth-legend image.
# Task:    Plot AMP mosaics on network and individual MP extents
# Author:  Annika Leunig
# Date:    July 2026
# Outputs: 1. North network AMP bathymetry map
#          2. Individual park zoom-in AMP bathymetry maps (Arafura, Arnhem,
#             Gulf of Carpentaria, Joseph Bonaparte Gulf, Limmen, North Kimberley,
#             Oceanic Shoals, West Cape York, Wessel)
###

# Table of contents
#     1.  Set up and load data
#     2.  WMS download functions
#     3.  Network figure builder (fixed Aus-extent inset)
#     4.  FIGURE 1: NORTH NETWORK MAP (assemble and save)
#     5.  Zoom-in map function (legend on left)
#     6.  FIGURES 2-10: Individual park zoom-ins (assemble and save)


# ==============================================================================
# 1. SET UP AND LOAD DATA
# ==============================================================================
# Clear environment
rm(list = ls())

# Set study name
name <- "north"
park <- "network"

# Load libraries
library(sf)
library(ggplot2)
library(dplyr)
library(png)
library(grid)
library(gridExtra)
library(patchwork)
library(cowplot)

sf::sf_use_s2(FALSE)
options(timeout = 1200)

# ── Paths (adjust if your tree differs) ───────────────────────────────────────
shp_dir          <- "data/north network/spatial/shapefiles/"
depth_legend_png <- "data/north network/spatial/rasters/depth-legend.png"

# ── Map extents (xmin, ymin, xmax, ymax) ──────────────────────────────────────
bbox_network <- c(xmin = 120,    ymin = -21,   xmax = 148,   ymax = -8)

north_inset_xlim <- c(unname(bbox_network["xmin"]), unname(bbox_network["xmax"]))
north_inset_ylim <- c(unname(bbox_network["ymin"]), unname(bbox_network["ymax"]))

# Cropping box for all layers
crop_box <- sf::st_as_sfc(sf::st_bbox(
  c(xmin = 117, ymin = -24, xmax = 151, ymax = -5), crs = 4326
))

# ── Load spatial files ────────────────────────────────────────────────────────
# Australia outline
aus <- sf::st_read(paste0(shp_dir, "STE_2021_AUST_GDA2020.shp"), quiet = TRUE) %>%
  sf::st_make_valid() %>%
  sf::st_transform(4326)

# CAPAD commonwealth marine parks - for the inset only
capad <- sf::st_read(
  paste0(shp_dir, "Collaborative_Australian_Protected_Areas_Database_(CAPAD)_2022_-_Marine.shp"),
  quiet = TRUE
) %>%
  sf::st_make_valid() %>%
  sf::st_transform(4326)

# Terrestrial parks
# NB: this shapefile is only stored under the south-west network folder and is
# shared across networks, hence the different path from the layers above.
terrnp <- sf::st_read(
  paste0("data/south-west network/spatial/shapefiles/",
         "Collaborative_Australian_Protected_Areas_Database_(CAPAD)_2024_-_Terrestrial__.shp"),
  quiet = TRUE
) %>%
  sf::st_make_valid() %>%
  sf::st_transform(4326) %>%
  dplyr::filter(TYPE %in% c("National Park", "Nature Reserve"))

# Coastal waters limit
cwatr <- sf::st_read(paste0(shp_dir, "amb_coastal_waters_limit.shp"), quiet = TRUE) %>%
  sf::st_make_valid() %>%
  sf::st_transform(4326)
cwatr <- suppressWarnings(sf::st_intersection(cwatr, crop_box))

# AMP zone boundaries (WFS)
wfs_url <- paste0(
  "https://geoserver.imas.utas.edu.au/geoserver/seamap/ows?",
  "service=WFS",
  "&version=1.0.0",
  "&request=GetFeature",
  "&typeName=SeamapAus_BOUNDARIES_AMP_ZONE",
  "&outputFormat=application/json"
)

amp <- try(sf::st_read(wfs_url, quiet = TRUE), silent = TRUE)
if (inherits(amp, "sf")) amp <- sf::st_transform(amp, 4326)

# ==============================================================================
# 2. WMS DOWNLOAD FUNCTIONS
# ==============================================================================
# ── MERI greyscale base ───────────────────────────────────────────────────────
get_meri_grey <- function(bbox) {
  meri_url <- paste0(
    "https://geoserver.imas.utas.edu.au/geoserver/wms?",
    "SERVICE=WMS",
    "&VERSION=1.1.1",
    "&REQUEST=GetMap",
    "&LAYERS=seamap:Aus_bathy_grid_MERI",
    "&STYLES=Aus_bathy_grid_MERI_greyscale",
    "&SRS=EPSG:4326",
    "&BBOX=", paste(bbox, collapse = ","),
    "&WIDTH=3000",
    "&HEIGHT=2000",
    "&FORMAT=image/png",
    "&TRANSPARENT=TRUE"
  )

  meri_tmp <- tempfile(fileext = ".png")
  download.file(meri_url, meri_tmp, mode = "wb")

  png::readPNG(meri_tmp)
}

# ── AMP composite bathymetry ──────────────────────────────────────────────────
get_amp_bathy <- function(bbox) {
  bathy_url <- paste0(
    "https://geoserver.imas.utas.edu.au/geoserver/wms?",
    "?SERVICE=WMS",        # For some reason this only works for me when WMS? is doubled up -AL
    "&VERSION=1.1.1",
    "&REQUEST=GetMap",
    "&LAYERS=seamap:bathymetry_AMP_grp",
    "&STYLES=",
    "&SRS=EPSG:4326",
    "&BBOX=", paste(bbox, collapse = ","),
    "&WIDTH=3000",
    "&HEIGHT=1800",
    "&FORMAT=image/png",
    "&TRANSPARENT=TRUE"
  )

  bth_tmp <- tempfile(fileext = ".png")
  download.file(bathy_url, bth_tmp, mode = "wb")

  png::readPNG(bth_tmp)
}

# ── Helper: thinned axis breaks (keeps every 2nd break at a fixed step) ───────
thin_breaks <- function(limits, step = 0.2) {
  b <- seq(from = floor(min(limits)   / step) * step,
           to   = ceiling(max(limits) / step) * step,
           by   = step)
  b[seq(1, length(b), by = 2)]
}


# ==============================================================================
# 3. NETWORK FIGURE BUILDER (FIXED AUS-EXTENT INSET)
# ==============================================================================
amp_bathy_map <- function(bbox, meri_img, bath_img, x_breaks = NULL, y_breaks = NULL) {

  xmin <- unname(bbox["xmin"]); xmax <- unname(bbox["xmax"])
  ymin <- unname(bbox["ymin"]); ymax <- unname(bbox["ymax"])

  if (is.null(x_breaks)) x_breaks <- pretty(c(xmin, xmax), n = 4)
  if (is.null(y_breaks)) y_breaks <- pretty(c(ymin, ymax), n = 4)

  terr_fills_ordered <- scale_fill_manual(
    values = c("National Park" = "#c4cea6", "Nature Reserve" = "#e4d0bb"),
    name   = "Terrestrial Parks",
    guide  = guide_legend(order = 2, ncol = 1)
  )

  # Dpeth legend
  legend_img  <- png::readPNG(depth_legend_png)
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
    "Composite bathymetry \nmosaics per Australian \nMarine Park (AMP)\n",
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

  # Main map MERI bathy and AMP mosaics
  p1 <- ggplot() +

    # MERI greyscale base
    annotation_raster(meri_img, xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax) +

    # AMP composite bathymetry
    annotation_raster(bath_img, xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax) +

    # Land
    geom_sf(data = aus, fill = "seashell2", colour = "grey80", linewidth = 0.1) +

    # Terrestrial parks
    geom_sf(data = terrnp, aes(fill = TYPE), colour = NA, alpha = 0.8) +
    terr_fills_ordered +

    # Coastal waters limit
    geom_sf(data = cwatr, fill = NA, colour = "firebrick",
            linewidth = 0.2, lineend = "round") +

    # AMP zone boundaries
    {if (inherits(amp, "sf"))
      geom_sf(data = amp, fill = NA, colour = "black", linewidth = 0.15)} +

    coord_sf(xlim = c(xmin, xmax), ylim = c(ymin, ymax), crs = 4326, expand = FALSE) +
    scale_x_continuous(
      name = NULL, breaks = x_breaks,
      labels = function(x) paste0(x, "°E")
    ) +
    scale_y_continuous(
      name = NULL, breaks = y_breaks,
      labels = function(y) paste0(abs(y), "°S")
    ) +
    theme_bw() +
    theme(
      panel.background = element_rect(fill = "white"),
      panel.border     = element_rect(colour = "grey70", fill = NA, linewidth = 0.3),
      panel.grid.major = element_line(colour = "grey85", linewidth = 0.3),
      axis.text        = element_text(size = 10),
      legend.key.size  = unit(0.4, "cm"),
      legend.text      = element_text(size = 7),
      legend.title     = element_text(size = 8),
      legend.position  = "bottom",
      legend.box       = "horizontal",
      legend.direction = "vertical"
    )

  # TPs legend
  p1_other_legend <- cowplot::get_legend(p1 + theme(
    legend.text      = element_text(size = 7),
    legend.title     = element_text(size = 8),
    legend.key.size  = unit(0.3, "cm"),
    legend.position  = "bottom",
    legend.box       = "horizontal",
    legend.direction = "vertical"
  ))

  # Inset
  p1.1 <- ggplot(data = aus) +
    geom_sf(fill = "seashell1", colour = "grey90", linewidth = 0.05, alpha = 4/5) +
    geom_sf(data = capad, alpha = 5/6, colour = "grey85", linewidth = 0.02) +
    coord_sf(xlim = c(105, 160), ylim = c(-48, -8)) +
    annotate("rect",
             xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax,
             colour = "grey25", fill = "white", alpha = 1/5, linewidth = 0.2) +
    theme_bw() +
    theme(axis.text        = element_blank(),
          axis.ticks       = element_blank(),
          panel.grid.major = element_blank(),
          panel.border     = element_rect(colour = "grey70"))

  # Assembly
  p1_no_legend <- p1 + theme(legend.position = "none",
                             plot.margin     = margin(0, 0, 15, 0))

  right_panel      <- wrap_elements(right_grob)
  right_panel_w_in <- 1.8

  map_row <- p1_no_legend + right_panel +
    plot_layout(widths = c(4, right_panel_w_in / (11 - right_panel_w_in) * 4))

  bottom_row <- p1.1 + wrap_elements(p1_other_legend) + plot_spacer() +
    plot_layout(widths = c(0.28, 1, 0.5))

  map_row / bottom_row +
    plot_layout(heights = c(4, 1))
}

# ==============================================================================
# 4. FIGURE 1: NORTH NETWORK MAP (assemble and save)
# ==============================================================================
# SAve function
build_and_save <- function(bbox, save_name, width, height,
                           x_breaks = NULL, y_breaks = NULL) {
  meri_img <- get_meri_grey(bbox)
  bath_img <- get_amp_bathy(bbox)
  fig <- amp_bathy_map(bbox, meri_img, bath_img, x_breaks, y_breaks)
  ggsave(paste(paste0("plots/", park, "/spatial/AMP_bathy/", name),
               paste0(save_name, ".png"), sep = "-"),
         plot = fig, dpi = 600, width = width, height = height, bg = "white")
  invisible(fig)
}

# Save
build_and_save(bbox_network,
               "network_AMP-bathy-plot",  width = 10, height = 5.5,
               x_breaks = seq(120, 145, by = 5),
               y_breaks = seq(-20, -10, by = 5))

# ==============================================================================
# 5. ZOOM-IN MAP FUNCTION (LEGEND ON LEFT)
# ==============================================================================
network_map_wms_zoomed <- function(
    plot_limits,
    inset_xlim = c(120, 148),
    inset_ylim = c(-21, -8),
    show_inset = TRUE,
    thin_lon_breaks = FALSE,   # thin the longitude axis breaks (useful for wide/short extents)
    break_step      = 0.2,     # spacing used by thin_breaks() before thinning; only used when thin_lon_breaks = TRUE
    save_name  = NULL,
    width      = 10,
    height     = 6
) {

  bbox <- c(
    xmin = plot_limits[1],
    ymin = plot_limits[3],
    xmax = plot_limits[2],
    ymax = plot_limits[4]
  )

  xmin <- bbox["xmin"]
  xmax <- bbox["xmax"]
  ymin <- bbox["ymin"]
  ymax <- bbox["ymax"]

  # Thinned lon breaks for wide/short extents, ggplot default breaks otherwise
  x_breaks <- if (thin_lon_breaks) {
    thin_breaks(c(unname(xmin), unname(xmax)), step = break_step)
  } else {
    pretty(c(unname(xmin), unname(xmax)), n = 5)
  }

  # Download rasters
  meri_img <- get_meri_grey(bbox)
  bath_img <- get_amp_bathy(bbox)

  # TPs
  terr_fills_ordered <- scale_fill_manual(
    values = c(
      "National Park" = "#c4cea6",
      "Nature Reserve" = "#e4d0bb"
    ),
    name = "Terrestrial Parks",
    guide = guide_legend(order = 2, ncol = 1)
  )

  # main map
  p_map <- ggplot() +

    # MERI bathy greyscale
    annotation_raster(
      meri_img,
      xmin = xmin,
      xmax = xmax,
      ymin = ymin,
      ymax = ymax
    ) +

    # AMP mosaics
    annotation_raster(
      bath_img,
      xmin = xmin,
      xmax = xmax,
      ymin = ymin,
      ymax = ymax
    ) +

    # Land
    geom_sf(
      data = aus,
      fill = "seashell2",
      colour = "grey80",
      linewidth = 0.1
    ) +

    # TPs
    geom_sf(
      data = terrnp,
      aes(fill = TYPE),
      colour = NA,
      alpha = 0.8
    ) +

    terr_fills_ordered +

    # Coastal waters
    geom_sf(
      data = cwatr,
      fill = NA,
      colour = "firebrick",
      linewidth = 0.15,
      lineend = "round"
    ) +

    # AMP zone boundaries
    {
      if (exists("amp") && inherits(amp, "sf"))
        geom_sf(
          data = amp,
          fill = NA,
          colour = "black",
          linewidth = 0.15
        )
    } +

    coord_sf(
      xlim = c(xmin, xmax),
      ylim = c(ymin, ymax),
      crs = 4326,
      expand = FALSE
    ) +

    scale_x_continuous(breaks = x_breaks) +   # thinned or default lon breaks

    labs(x = NULL, y = NULL) +

    theme_minimal() +

    theme(
      legend.key.size = unit(0.5, "cm"),
      legend.text = element_text(size = 9),
      legend.title = element_text(size = 11),
      legend.position = "left",
      legend.box = "vertical",
      legend.direction = "vertical",
      panel.grid = element_blank(),
      panel.background = element_rect(
        fill = "white",
        colour = NA
      ),
      plot.background = element_rect(
        fill = "white",
        colour = NA
      ),
      panel.border = element_rect(
        colour = "grey80",
        fill = NA,
        linewidth = 0.5
      ),
      axis.ticks = element_line(
        colour = "grey80",
        linewidth = 0.3
      )
    ) +

    guides(fill = guide_legend(ncol = 1))

  # Extract legend
  legend_single <- cowplot::get_legend(
    p_map +
      theme(
        legend.position = "left",
        legend.box = "vertical",
        legend.direction = "vertical",
        legend.key.size = unit(0.5, "cm"),
        legend.spacing.y = unit(0.2, "cm")
      )
  )

  # Inset map
  if (show_inset) {

    p_inset <- ggplot(data = aus) +

      geom_sf(
        fill = "seashell1",
        colour = "grey90",
        linewidth = 0.05,
        alpha = 0.8
      ) +

      geom_sf(
        data = capad,
        colour = "grey85",
        linewidth = 0.02,
        alpha = 0.8
      ) +

      annotate(
        "rect",
        xmin = xmin,
        xmax = xmax,
        ymin = ymin,
        ymax = ymax,
        colour = "grey25",
        fill = "white",
        alpha = 0.2,
        linewidth = 0.3
      ) +

      coord_sf(
        xlim = inset_xlim,
        ylim = inset_ylim
      ) +

      theme_bw() +

      theme(
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid.major = element_blank(),
        panel.border = element_rect(colour = "grey70")
      )

    left_col <- cowplot::plot_grid(
      legend_single,
      NULL,
      p_inset,
      ncol = 1,
      rel_heights = c(1, 0.1, 0.45)
    )

  } else {

    left_col <- cowplot::plot_grid(
      legend_single,
      ncol = 1
    )

  }

  # Assembly
  p_map_nl <- p_map +
    theme(
      legend.position = "none",
      plot.margin = margin(0, 0, 0, 15)
    )

  fig <- cowplot::plot_grid(
    left_col,
    p_map_nl,
    nrow = 1,
    rel_widths = c(0.32, 1)
  ) +
    theme(
      plot.background = element_rect(
        fill = "white",
        colour = NA
      ),
      plot.margin = margin(5, 5, 5, 5)
    )

  # Save
  if (!is.null(save_name)) {

    ggsave(paste(paste0("plots/",park,"/spatial/AMP_bathy/", name),
                 paste0(save_name, ".png"),sep = "-"),
           plot = fig, dpi = 600, width = width, height = height, bg = "white")
  }

  invisible(fig)
}

# ==============================================================================
# 6. FIGURES 2-10: INDIVIDUAL PARK ZOOM-INS (assemble and save)
# ==============================================================================
# NB: plot_limits below are rough estimates only (I don't have your AMP
# shapefile extents to hand) — check each against your `amp` layer, e.g.
#   st_bbox(amp %>% filter(NAME == "Arafura"))
# and adjust before running. Format is c(xmin, xmax, ymin, ymax).

# ── Arafura ───────────────────────────────────────────────────────────────────
network_map_wms_zoomed(
  plot_limits = c(130.0, 136.5, -8.5, -11.5),  # TODO: verify extent
  save_name   = "arafura_AMP-bathy-plot",
  width       = 9,
  height      = 6,
  inset_xlim  = north_inset_xlim,
  inset_ylim  = north_inset_ylim
)

# ── Arnhem ────────────────────────────────────────────────────────────────────
network_map_wms_zoomed(
  plot_limits = c(133.0, 137.5, -10.0, -12.5),  # TODO: verify extent
  save_name   = "arnhem_AMP-bathy-plot",
  width       = 8,
  height      = 5.5,
  inset_xlim  = north_inset_xlim,
  inset_ylim  = north_inset_ylim
)

# ── Gulf of Carpentaria ───────────────────────────────────────────────────────
network_map_wms_zoomed(
  plot_limits = c(139.0, 142.5, -13.5, -17.5),  # TODO: verify extent
  save_name   = "gulf-of-carpentaria_AMP-bathy-plot",
  width       = 8,
  height      = 7,
  inset_xlim  = north_inset_xlim,
  inset_ylim  = north_inset_ylim
)

# ── Joseph Bonaparte Gulf ─────────────────────────────────────────────────────
network_map_wms_zoomed(
  plot_limits = c(126.5, 130.5, -12.5, -15.5),  # TODO: verify extent
  save_name   = "joseph-bonaparte-gulf_AMP-bathy-plot",
  width       = 8,
  height      = 6.5,
  inset_xlim  = north_inset_xlim,
  inset_ylim  = north_inset_ylim
)

# ── Limmen ────────────────────────────────────────────────────────────────────
network_map_wms_zoomed(
  plot_limits = c(135.0, 137.5, -13.5, -16.0),  # TODO: verify extent
  save_name   = "limmen_AMP-bathy-plot",
  width       = 7.5,
  height      = 6.5,
  inset_xlim  = north_inset_xlim,
  inset_ylim  = north_inset_ylim
)

# ── North Kimberley ───────────────────────────────────────────────────────────
network_map_wms_zoomed(
  plot_limits = c(122.5, 127.0, -12.5, -15.5),  # TODO: verify extent
  save_name   = "north-kimberley_AMP-bathy-plot",
  width       = 8.5,
  height      = 6.5,
  inset_xlim  = north_inset_xlim,
  inset_ylim  = north_inset_ylim
)

# ── Oceanic Shoals ────────────────────────────────────────────────────────────
network_map_wms_zoomed(
  plot_limits = c(122.5, 130.5, -8.5, -13.5),  # TODO: verify extent (large park)
  save_name   = "oceanic-shoals_AMP-bathy-plot",
  width       = 9,
  height      = 7,
  inset_xlim  = north_inset_xlim,
  inset_ylim  = north_inset_ylim
)

# ── West Cape York ────────────────────────────────────────────────────────────
network_map_wms_zoomed(
  plot_limits = c(140.5, 142.5, -10.5, -15.0),  # TODO: verify extent
  save_name   = "west-cape-york_AMP-bathy-plot",
  thin_lon_breaks = TRUE,
  break_step      = 0.5,
  width       = 5.5,
  height      = 8,
  inset_xlim  = north_inset_xlim,
  inset_ylim  = north_inset_ylim
)

# ── Wessel ────────────────────────────────────────────────────────────────────
network_map_wms_zoomed(
  plot_limits = c(135.5, 137.5, -10.5, -12.5),  # TODO: verify extent
  save_name   = "wessel_AMP-bathy-plot",
  width       = 7,
  height      = 6,
  inset_xlim  = north_inset_xlim,
  inset_ylim  = north_inset_ylim
)

# ==============================================================================
# End of script
# ==============================================================================
