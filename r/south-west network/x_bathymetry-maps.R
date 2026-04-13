###
# Project: NESP 4.20 - Marine Park Dashboard reporting
# Data:    Marine parks, old and new bathymetry data (2009 & 2024), terrestrial
#          parks and aus outline
# Task:    2009 vs 2024 Bathymetry maps
# Author:  Annika Leunig
# Date:    June 2026
###

# Clear the environment
rm(list = ls())

# Set the study name
name <- "south-west"
park <- "network"


# Load libraries
library(sf)
library(terra)
library(stars)
library(starsExtra)
library(tidyverse)
library(tidyterra)
library(patchwork)
library(RNetCDF)
library(rerddap)
library(ggnewscale)
library(metR)
library(cowplot)

terraOptions(progress = 3)
sf_use_s2(T)

# Set cropping extent
e <- ext(108.0, 138.0, -40.0, -23.0)

# Load all spatial files
terrnp <- st_read("data/south-west network/spatial/shapefiles/Legislated_Lands_and_Waters_DBCA_011.shp") %>%
  dplyr::filter(leg_catego %in% c("Nature Reserve", "National Park"))

terr_fills <- scale_fill_manual(values = c("National Park"  = "#c4cea6",
                                           "Nature Reserve" = "#e4d0bb"),
                                name = "Terrestrial Parks")

aus <- st_read("data/south-west network/spatial/shapefiles/STE_2021_AUST_GDA2020.shp") %>%
  st_make_valid()

capad <- st_read("data/south-west network/spatial/shapefiles/Collaborative_Australian_Protected_Areas_Database_(CAPAD)_2022_-_Marine.shp")

aus_marine_parks <- st_read("data/south-west network/spatial/shapefiles/south-and-western-australia_marine-parks-all.shp")

marine_parks <- st_read("data/south-west network/spatial/shapefiles/south-and-western-australia_marine-parks-all.shp") %>%
  dplyr::filter(name %in% c("Abrolhos", "Abrolhos Islands", "Bremer", "Eastern Recherche", "Ngari Capes", "Geographe",
                            "South-west Corner", "Great Australian Bight", "Jurien", "Murat", "Jurien Bay", "Perth Canyon",
                            "Southern Kangaroo Island", "Twilight", "Two Rocks", "Western Eyre", "Western Kangaroo Island",
                            "Nuyts Archipelgo", "Thorny Passage", "Sir Joseph Banks Group", "Investigator", "West coast Bays",
                            "Southern Spencer Gulf", "Upper Spencer Gulf", "Cottesloe Reef", "Rottnest", "Shoalwater Islands"))


# Load bathymetry files (2009 and 2024)
old_full_bathy <- rast("data/south-west network/spatial/rasters/ausbath_09_v4") %>%
  crop(e)

old_bathy <- old_full_bathy %>%
  clamp(upper = 0, lower = -250, values = F) %>%
  trim()

new_full_bathy <- rast("data/south-west network/spatial/rasters/AusBathyTopo__Australia__2024_250m_MSL_cog.tif") %>%
  crop(e)

new_bathy <- new_full_bathy %>%
  clamp(upper = 0, lower = -250, values = F) %>%
  trim()


# Define extents
geographe_limits <- c(114.9, 115.7, -33.8,    -33.369)
tworocks_limits  <- c(114.7, 116.0, -32.0,    -31.3)
swc_limits       <- c(114.2, 116.2, -34.65, -33.3)

# Create function helpers (makes for a cleaner plot)
thin_breaks <- function(limits, step = 0.2) {
  b <- seq(from = floor(min(limits)   / step) * step,
           to   = ceiling(max(limits) / step) * step,
           by   = step)
  b[seq(1, length(b), by = 2)]
}

# Create Colour ramps
v <- scales::viridis_pal(option = "viridis")(100)

bathy_palette_geo <- colorRampPalette(c(
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

bathy_palette_even <- scales::viridis_pal(option = "viridis")(8)

bathy_palette_swc <- colorRampPalette(c(
  v[1],    # dark purple  — -200 m
  v[2],
  v[3],
  v[4],
  v[5],
  v[6],
  v[7],
  v[8],
  v[9],
  v[10],
  v[11],
  v[13],
  v[16],
  v[20],
  v[24],
  v[28],
  v[32],
  v[36],
  v[40],
  v[44],
  v[48],   # teal         — ~-100 m
  v[58],
  v[68],
  v[76],
  v[83],
  v[89],
  v[94],
  v[98],
  v[100]   # bright yellow — 0 m
))(500)

# Create and compute hillshade
make_hillshade <- function(bathy_rast, altitude = 35, azimuth = 270) {
  slope  <- terrain(bathy_rast, v = "slope",  unit = "radians")
  aspect <- terrain(bathy_rast, v = "aspect", unit = "radians")
  shade(slope, aspect, angle = altitude, direction = azimuth, normalize = TRUE)
}

hill_old <- make_hillshade(old_full_bathy)
hill_new <- make_hillshade(new_full_bathy)

# Rotated polygon (so that it can be a diagonal box)
rotated_rect <- function(cx, cy, width, height, angle_deg, xlim, ylim, plot_width, plot_height) {
  # Compute the pixel-to-degree scaling ratio so the shape is square on screen
  x_range    <- diff(xlim)
  y_range    <- diff(abs(ylim))
  aspect     <- (y_range / x_range) * (plot_width / plot_height)

  angle_rad  <- angle_deg * pi / 180
  dx         <- c(-width/2,  width/2,  width/2, -width/2)
  dy         <- c(-height/2, -height/2, height/2,  height/2)

  # Scale dy by aspect before rotating, then unscale after
  x <- cx + dx * cos(angle_rad) - (dy / aspect) * sin(angle_rad)
  y <- cy + (dx * sin(angle_rad) + (dy / aspect) * cos(angle_rad)) * aspect

  list(x = x, y = y)
}

# Define plot function
make_bathy_panel <- function(bathy_rast,
                             hill_rast,
                             xlim,
                             ylim,
                             depth_limits  = c(-300, 0),
                             depth_breaks  = c(0, -25, -50, -75, -100, -125, -150, -200, -250, -300),
                             palette       = bathy_palette_even,
                             clip_to_limit = FALSE,
                             highlight_box = NULL,
                             title         = NULL,
                             break_step    = 0.1) {

  ext_plot    <- ext(xlim[1], xlim[2], ylim[1], ylim[2])
  bathy_crop  <- crop(bathy_rast, ext_plot)

  bathy_clamp <- clamp(bathy_crop,
                       lower  = depth_limits[1],
                       upper  = 0,
                       values = !clip_to_limit)
  names(bathy_clamp) <- "depth"

  hill_crop        <- crop(hill_rast, ext_plot)
  names(hill_crop) <- "hillshade"

  x_breaks <- thin_breaks(xlim, step = break_step)
  y_breaks <- thin_breaks(abs(ylim), step = break_step) * -1

  oob_fn <- if (clip_to_limit) scales::censor else scales::squish

  p <- ggplot() +
    geom_spatraster(data = hill_crop, aes(fill = hillshade),
                    alpha = 0.85, show.legend = FALSE) +
    scale_fill_gradient(low = "#1a1a2e", high = "#e8e8e8",
                        na.value = NA, guide = "none") +
    new_scale_fill() +
    geom_spatraster(data = bathy_clamp, aes(fill = depth), alpha = 0.65) +
    scale_fill_gradientn(
      colours  = palette,
      limits   = depth_limits,
      oob      = oob_fn,
      na.value = "white",
      name     = "Depth (m)",
      breaks   = depth_breaks,
      labels   = as.character(depth_breaks),
      guide    = "none"
    ) +
    geom_sf(data = aus, fill = "seashell2", colour = "grey50", linewidth = 0.25) +
    new_scale_fill() +
    geom_sf(data = terrnp, aes(fill = leg_catego), colour = NA, alpha = 0.8) +
    scale_fill_manual(
      values = c("National Park"  = "#c4cea6",
                 "Nature Reserve" = "#e4d0bb"),
      name   = "Terrestrial Parks",
      guide  = "none"
    ) +
    geom_sf(data = marine_parks,
            fill      = NA,
            colour    = alpha("white", 0.3),
            linewidth = 0.4) +
    coord_sf(xlim = xlim, ylim = ylim, expand = FALSE) +
    scale_x_continuous(breaks = x_breaks) +
    scale_y_continuous(breaks = y_breaks)

  if (!is.null(highlight_box)) {
    p <- p +
      annotate("rect",
               xmin      = highlight_box$xmin,
               xmax      = highlight_box$xmax,
               ymin      = highlight_box$ymin,
               ymax      = highlight_box$ymax,
               fill      = NA,
               colour    = "orange",
               linewidth = 0.8)
  }

  p <- p +
    labs(x = NULL, y = NULL, title = title) +
    theme_minimal() +
    theme(
      plot.title       = element_blank(),
      axis.text        = element_text(size = 8, colour = "grey40"),
      axis.ticks       = element_line(colour = "grey60"),
      panel.grid       = element_blank(),
      panel.background = element_rect(fill = "white", colour = NA),
      plot.background  = element_rect(fill = "white", colour = NA),
      panel.border     = element_rect(fill = NA, colour = "grey50", linewidth = 0.4),
      legend.position  = "right",
      legend.title     = element_text(size = 10),
      legend.text      = element_text(size = 9),
      plot.margin      = margin(2, 2, 2, 2)
    )

  return(p)
}

# Bathy stand alone legend function
make_bathy_legend <- function(depth_limits = c(-300, 0),
                              depth_breaks = c(0, -25, -50, -75, -100, -125, -150, -200, -250, -300),
                              palette      = bathy_palette_even) {
  dummy <- data.frame(
    x     = 1,
    y     = seq(depth_limits[1], depth_limits[2], length.out = 100),
    depth = seq(depth_limits[1], depth_limits[2], length.out = 100)
  )

  p_leg <- ggplot(dummy, aes(x = x, y = y, fill = depth)) +
    geom_tile() +
    scale_fill_gradientn(
      colours  = palette,
      limits   = depth_limits,
      name     = "Depth (m)",
      breaks   = depth_breaks,
      labels   = as.character(depth_breaks),
      guide    = guide_colorbar(
        barwidth       = 1.2,
        barheight      = 7,
        title.position = "top",
        ticks          = TRUE
      )
    ) +
    theme_void() +
    theme(
      legend.position = "right",
      legend.title    = element_text(size = 11),
      legend.text     = element_text(size = 10)
    )

  cowplot::get_legend(p_leg)
}

# Terrestrial parks legend function (combined)
make_terrp_legend <- function() {
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
        title.hjust    = 0.5
      )
    ) +
    theme_void() +
    theme(
      legend.position  = "bottom",
      legend.title     = element_text(size = 12),
      legend.text      = element_text(size = 11),
      legend.key.size  = unit(0.5, "cm")
    )

  cowplot::get_legend(p_tp)
}


# PLOT FUNCTIONS
# TWO ROCKS AND GEOGRAPHE 2009 v 2024 FACETED PLOTS
p_tr_old <- make_bathy_panel(old_full_bathy, hill_old,
                             xlim          = tworocks_limits[1:2],
                             ylim          = tworocks_limits[3:4],
                             depth_limits  = c(-200, 0),
                             depth_breaks  = c(0, -50, -100, -150, -200),
                             palette       = bathy_palette_even,
                             clip_to_limit = TRUE,
                             highlight_box = list(xmin = 115.09, xmax = 115.42,
                                                  ymin = -31.72, ymax = -31.42),
                             break_step    = 0.1)

p_tr_new <- make_bathy_panel(new_full_bathy, hill_new,
                             xlim          = tworocks_limits[1:2],
                             ylim          = tworocks_limits[3:4],
                             depth_limits  = c(-200, 0),
                             depth_breaks  = c(0, -50, -100, -150, -200),
                             palette       = bathy_palette_even,
                             clip_to_limit = TRUE,
                             highlight_box = list(xmin = 115.09, xmax = 115.42,
                                                  ymin = -31.72, ymax = -31.42),
                             break_step    = 0.1)

p_geo_old <- make_bathy_panel(old_full_bathy, hill_old,
                              xlim          = geographe_limits[1:2],
                              ylim          = geographe_limits[3:4],
                              depth_limits  = c(-50, 0),
                              depth_breaks  = c(0, -10, -20, -30, -40, -50),
                              palette       = bathy_palette_geo,
                              clip_to_limit = FALSE,
                              break_step    = 0.1)

p_geo_new <- make_bathy_panel(new_full_bathy, hill_new,
                              xlim          = geographe_limits[1:2],
                              ylim          = geographe_limits[3:4],
                              depth_limits  = c(-50, 0),
                              depth_breaks  = c(0, -10, -20, -30, -40, -50),
                              palette       = bathy_palette_geo,
                              clip_to_limit = FALSE,
                              break_step    = 0.1)


geo_box <- rotated_rect(cx        = 115.445, cy = -33.505,
                        width     = 0.09,
                        height    = 0.18,
                        angle_deg = 40,
                        xlim      = geographe_limits[1:2],
                        ylim      = geographe_limits[3:4],
                        plot_width  = 15 * (1 / 2.09),
                        plot_height = 10 * (1 / 2.06) * (1 / 1.06)) #adjust plot width and height to make polygon
                                                                    # look like rectangle and not uneven

p_geo_old <- p_geo_old +
  annotate("polygon",
           x         = geo_box$x,
           y         = geo_box$y,
           fill      = NA,
           colour    = "orange",
           linewidth = 0.8)

p_geo_new <- p_geo_new +
  annotate("polygon",
           x         = geo_box$x,
           y         = geo_box$y,
           fill      = NA,
           colour    = "orange",
           linewidth = 0.8)

# Facet the plots and make them look pretty
# column and row labels
title_2009 <- ggdraw() +
  draw_label("2009", fontface = "bold", size = 18, hjust = 0.5)
title_2024 <- ggdraw() +
  draw_label("2024", fontface = "bold", size = 18, hjust = 0.5)

title_row <- cowplot::plot_grid(
  NULL, title_2009, NULL, title_2024,
  nrow = 1, rel_widths = c(0.06, 1, 0.03, 1)
)

label_tr  <- ggdraw() + draw_label("Two Rocks", fontface = "plain", size = 14, angle = 90)
label_geo <- ggdraw() + draw_label("Geographe", fontface = "plain", size = 14, angle = 90)

# depth scales
legend_tr <- make_bathy_legend(
  depth_limits = c(-200, 0),
  depth_breaks = c(0, -50, -100, -150, -200),
  palette      = bathy_palette_even
)

legend_geo <- make_bathy_legend(
  depth_limits = c(-50, 0),
  depth_breaks = c(0, -10, -20, -30, -40, -50),
  palette      = bathy_palette_geo
)

depth_legends <- cowplot::plot_grid(
  legend_tr,
  legend_geo,
  ncol        = 1,
  rel_heights = c(1, 1)
)

# Assemble the faceted plot
row_tr <- cowplot::plot_grid(
  label_tr, p_tr_old, NULL, p_tr_new,
  nrow = 1, rel_widths = c(0.06, 1, 0.03, 1),
  align = "h", axis = "tb"
)

row_geo <- cowplot::plot_grid(
  label_geo, p_geo_old, NULL, p_geo_new,
  nrow = 1, rel_widths = c(0.06, 1, 0.03, 1),
  align = "h", axis = "tb"
)

maps_grid <- cowplot::plot_grid(
  title_row,
  row_tr,
  row_geo,
  ncol        = 1,
  rel_heights = c(0.06, 1, 1)
)

maps_with_legends <- cowplot::plot_grid(
  maps_grid,
  depth_legends,
  nrow       = 1,
  rel_widths = c(1, 0.08)
)

terrp_legend <- make_terrp_legend()

figure1 <- cowplot::plot_grid(
  maps_with_legends,
  terrp_legend,
  ncol        = 1,
  rel_heights = c(1, 0.06)
) +
  theme(plot.background = element_rect(fill = "white", colour = NA),
        plot.margin     = margin(t = 5, r = 5, b = 5, l = 5))

# Save plot to file
ggsave(paste(paste0("plots/", park, "/spatial/bathymetry/", name),
             "geographe-tworocks-bathy-comparison.png", sep = "-"),
       plot   = figure1,
       dpi    = 600,
       width  = 15,
       height = 10,
       bg     = "white")



# SWC 2009 v 2024 FACETED PLOTS
# Similar steps asabove, just without two rows

p_swc_old <- make_bathy_panel(old_full_bathy, hill_old,
                              xlim          = swc_limits[1:2],
                              ylim          = swc_limits[3:4],
                              depth_limits  = c(-200, 0),
                              depth_breaks  = c(0, -50, -100, -150, -200),
                              palette       = bathy_palette_swc,
                              clip_to_limit = TRUE,
                              break_step    = 0.2)

p_swc_new <- make_bathy_panel(new_full_bathy, hill_new,
                              xlim          = swc_limits[1:2],
                              ylim          = swc_limits[3:4],
                              depth_limits  = c(-200, 0),
                              depth_breaks  = c(0, -50, -100, -150, -200),
                              palette       = bathy_palette_swc,
                              clip_to_limit = TRUE,
                              break_step    = 0.2)

legend_swc <- make_bathy_legend(
  depth_limits = c(-200, 0),
  depth_breaks = c(0, -50, -100, -150, -200),
  palette      = bathy_palette_swc
)

title_row_swc <- cowplot::plot_grid(
  title_2009, NULL, title_2024,
  nrow = 1, rel_widths = c(1, 0.03, 1)
) + theme(plot.margin = margin(0, 0, -10, 0))

row_swc <- cowplot::plot_grid(
  p_swc_old, NULL, p_swc_new,
  nrow = 1, rel_widths = c(1, 0.03, 1),
  align = "h", axis = "tb"
)

maps_grid_swc <- cowplot::plot_grid(
  title_row_swc,
  row_swc,
  ncol        = 1,
  rel_heights = c(0.01, 1)
)

maps_with_legends_swc <- cowplot::plot_grid(
  maps_grid_swc,
  legend_swc,
  nrow       = 1,
  rel_widths = c(1, 0.08)
)

figure2 <- cowplot::plot_grid(
  maps_with_legends_swc,
  terrp_legend,
  ncol        = 1,
  rel_heights = c(1, 0.06)
) +
  theme(plot.background = element_rect(fill = "white", colour = NA),
        plot.margin     = margin(t = 5, r = 5, b = 5, l = 5))

# Save output
ggsave(paste(paste0("plots/", park, "/spatial/bathymetry/", name),
             "swc-bathy-comparison.png", sep = "-"),
       plot   = figure2,
       dpi    = 600,
       width  = 14,
       height = 6,
       bg     = "white")


