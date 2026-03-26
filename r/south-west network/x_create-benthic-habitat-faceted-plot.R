###
# Project: NESP 4.20 - Marine Park Dashboard reporting
# Data:    Natural values ecosystems, predicted reef, marine park shapefiles,
#          terrestrial parks and aus outline
# Task:    Creating benthic habitat maps
# Author:  Annika Leunig
# Date:    March 2026
###

# Clear environment
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
library(cowplot)

# Load in shapefiles
# set extent
e <- ext(106.0, 140.0, -45.0, -22.0)

# Aus outline
aus <- st_read("data/south-west network/spatial/shapefiles/STE_2021_AUST_GDA2020.shp") %>%
  st_make_valid()

# SWC Marine parks
marine_parks <- st_read("data/south-west network/spatial/shapefiles/south-and-western-australia_marine-parks-all.shp") %>%
  dplyr::filter(name %in% c("Abrolhos", "Abrolhos Islands", "Bremer", "Eastern Recherche", "Ngari Capes", "Geographe",
                            "South-west Corner","Great Australian Bight", "Jurien","Murat", "Jurien Bay", "Perth Canyon",
                            "Southern Kangaroo Island", "Twilight", "Two Rocks", "Western Eyre", "Western Kangaroo Island",
                            "Nuyts Archipelgo", "Thorny Passage", "Sir Joseph Banks Group", "Investigator", "West coast Bays",
                            "Southern Spencer Gulf", "Upper Spencer Gulf", "Cottesloe Reef", "Rottnest", "Shoalwater Islands")) %>%
  glimpse()

# Terrestrial parks for mapping
terrnp <- st_read("data/south-west network/spatial/shapefiles/Legislated_Lands_and_Waters_DBCA_011.shp") %>%
  dplyr::filter(leg_catego %in% c("Nature Reserve", "National Park"))

# Natural values ecosystem
naturalvalues <- rast("data/south-west network/spatial/rasters/ecosystem-types-27class-naland.tif") %>%
  crop(e)

# Predicted Habitat
predictedhabitat <- rast("data/south-west network/spatial/rasters/binomial_preds_reef_range_multi.tif") %>%
  crop(e)

# Bathymetry data to clip to 250m shelf
bathy <- rast("data/south-west network/spatial/rasters/AusBathyTopo__Australia__2024_250m_MSL_cog.tif") %>%
  crop(e)

# Match CRS
target_crs <- "EPSG:4326"

if (!same.crs(naturalvalues, target_crs))    naturalvalues    <- project(naturalvalues,    target_crs, method = "near")
if (!same.crs(predictedhabitat, target_crs)) predictedhabitat <- project(predictedhabitat, target_crs, method = "near")
if (!same.crs(bathy, target_crs))            bathy            <- project(bathy,            target_crs, method = "bilinear")
if (st_crs(aus)          != st_crs(4326)) aus          <- st_transform(aus,          4326)
if (st_crs(terrnp)       != st_crs(4326)) terrnp       <- st_transform(terrnp,       4326)
if (st_crs(marine_parks) != st_crs(4326)) marine_parks <- st_transform(marine_parks, 4326)


# Assign natural values names and colours
nv_lookup <- c(
  "1"  = "Shelf unvegetated sediments",
  "2"  = "Upper slope sediments",
  "3"  = "Mid slope sediments",
  "4"  = "Lower slope reef and sediments",
  "5"  = "Abyssal reef and sediments",
  "6"  = "Seamount sediments",
  "7"  = "Shelf incising canyons",
  "8"  = "Oceanic shallow coral reefs",
  "9"  = "Shelf vegetated sediments",
  "10" = "Shallow coral reefs",
  "11" = "Shallow rocky reefs",
  "12" = "Mesophotic coral reefs",
  "13" = "Mesophotic rocky reefs",
  "14" = "Oceanic mesophotic coral reefs",
  "15" = "Rariphotic shelf reefs",
  "16" = "Upper slope reefs",
  "17" = "Mid slope reefs",
  "18" = "Seamount reefs"
)

hab_colours <- c(
  "Shelf unvegetated sediments"      = "cornsilk1",
  "Upper slope sediments"            = "wheat1",
  "Mid slope sediments"              = "navajowhite1",
  "Lower slope reef and sediments"   = "lightsteelblue2",
  "Abyssal reef and sediments"       = "slategrey",
  "Seamount sediments"               = "rosybrown2",
  "Shelf incising canyons"           = "grey50",
  "Oceanic shallow coral reefs"      = "lightsalmon",
  "Shelf vegetated sediments"        = "seagreen3",
  "Shallow coral reefs"              = "coral2",
  "Shallow rocky reefs"              = "darkgoldenrod1",
  "Mesophotic coral reefs"           = "plum2",
  "Mesophotic rocky reefs"           = "khaki4",
  "Oceanic mesophotic coral reefs"   = "burlywood3",
  "Rariphotic shelf reefs"           = "steelblue3",
  "Upper slope reefs"                = "indianred3",
  "Mid slope reefs"                  = "palevioletred3",
  "Seamount reefs"                   = "mediumpurple3"
)

# Clip to 250 (for faster loading for zoomed in plots)

mask_250 <- ifel(bathy >= -250, 1, NA)
mask_250_resamp <- resample(mask_250, naturalvalues, method = "near")
naturalvalues_clipped <- mask(naturalvalues, mask_250_resamp)

# Create helper functions (for cleaner faceted plots)
check_ratio <- function(l) {
  mean_lat <- (l[3] + l[4]) / 2
  cos_lat  <- cos(mean_lat * pi / 180)
  rendered <- (l[2] - l[1]) / (l[4] - l[3]) * cos_lat
  cat(sprintf("w: %.4f  h: %.4f  raw_ratio: %.3f  rendered_ratio: %.3f\n",
              l[2]-l[1], l[4]-l[3], (l[2]-l[1])/(l[4]-l[3]), rendered))
}

thin_breaks <- function(limits, step = 0.2) {
  b <- seq(from = floor(min(limits)   / step) * step,
           to   = ceiling(max(limits) / step) * step,
           by   = step)
  b[seq(1, length(b), by = 2)]
}

# Plot functions
# FUNCTION 1: Solid background colour
naturalvalues_map_dynamic <- function(plot_limits,
                                      use_clipped     = TRUE,
                                      show_predicted  = FALSE,
                                      predicted_alpha = 1,
                                      ocean_colour    = "cornsilk1",
                                      show_legend     = TRUE,
                                      y_label         = NULL,
                                      title           = NULL,
                                      break_step      = 0.2) {

  require(tidyverse); require(tidyterra); require(ggnewscale)

  ext_plot  <- ext(plot_limits[1], plot_limits[2], plot_limits[3], plot_limits[4])
  nv_source <- if (use_clipped) naturalvalues_clipped else naturalvalues
  nv_crop   <- crop(nv_source, ext_plot)

  nv_df <- as.data.frame(nv_crop, xy = TRUE, na.rm = TRUE)
  colnames(nv_df)[3] <- "value"
  nv_df$classname <- nv_lookup[as.character(nv_df$value)]
  nv_df <- dplyr::filter(nv_df, !is.na(classname))

  present_classes <- unique(nv_df$classname)
  present_colours <- hab_colours[names(hab_colours) %in% present_classes]

  nv_guide <- if (show_legend) guide_legend(order = 1, ncol = 1) else "none"

  x_breaks <- thin_breaks(plot_limits[1:2], step = break_step)
  y_breaks <- thin_breaks(abs(plot_limits[3:4]), step = break_step) * -1

  p <- ggplot() +

    geom_tile(data = nv_df, aes(x = x, y = y, fill = classname)) +
    scale_fill_manual(
      name   = "Benthic ecosystem",
      values = present_colours,
      breaks = names(present_colours),
      guide  = nv_guide
    ) +

    new_scale_fill() +
    geom_sf(data = aus, fill = "seashell2", colour = "grey80", linewidth = 0.1) +

    new_scale_fill() +
    geom_sf(data = terrnp, aes(fill = leg_catego), colour = NA) +
    scale_fill_manual(
      values = c("National Park"  = "#c4cea6",
                 "Nature Reserve" = "#e4d0bb"),
      name   = "Terrestrial Parks",
      guide  = if (show_legend) guide_legend(order = 4) else "none"
    )

  if (show_predicted) {
    ph_crop   <- crop(predictedhabitat, ext_plot)
    ph_masked <- mask(ph_crop, resample(mask_250, ph_crop, method = "near"))

    ph_df <- as.data.frame(ph_masked, xy = TRUE, na.rm = TRUE)
    colnames(ph_df)[3] <- "prob"
    ph_df <- dplyr::filter(ph_df, prob > 0)

    ph_guide <- if (show_legend) guide_colourbar(order = 3) else "none"

    p <- p +
      new_scale_fill() +
      geom_tile(data = ph_df, aes(x = x, y = y, fill = prob),
                alpha = predicted_alpha) +
      scale_fill_gradient2(
        name     = "Predicted reef\nhabitat (probability)",
        low      = "#fff1e6",
        mid      = "#a87448",
        high     = "#301703",
        midpoint = 0.5,
        na.value = NA,
        guide    = ph_guide
      )
  }

  p <- p +
    geom_sf(data = marine_parks, fill = NA, colour = "black", linewidth = 0.2)

  p <- p +
    coord_sf(xlim   = plot_limits[1:2],
             ylim   = plot_limits[3:4],
             crs    = 4326,
             expand = FALSE) +
    scale_x_continuous(breaks = x_breaks) +
    scale_y_continuous(breaks = y_breaks) +
    labs(x = NULL, y = y_label, title = title) +
    theme_minimal() +
    theme(
      legend.key.size  = unit(0.45, "cm"),
      legend.text      = element_text(size = 8),
      legend.title     = element_text(size = 9, face = "bold"),
      legend.position  = if (show_legend) "right" else "none",
      legend.box       = "vertical",
      panel.grid       = element_blank(),
      panel.background = element_rect(fill = ocean_colour, colour = NA),
      plot.background  = element_rect(fill = "white",      colour = NA),
      axis.text        = element_text(size = 10, colour = "grey40"),
      axis.ticks       = element_line(colour = "grey60"),
      axis.title.y     = if (!is.null(y_label)) element_text(face = "bold", size = 12, margin = margin(r = 11))
      else                   element_blank(),
      plot.title       = if (!is.null(title)) element_text(face = "bold", size = 14, hjust = 0.5)
      else                 element_blank(),
      plot.margin = margin(t = 0, r = 0, b = 0, l = 0)
    )

  return(p)
}

# FUNCTION 2: White gridline background
naturalvalues_map_white <- function(plot_limits,
                                    use_clipped            = TRUE,
                                    show_predicted         = FALSE,
                                    predicted_alpha        = 0.6,
                                    grid_colour            = "grey80",
                                    grid_lwd               = 0.3,
                                    show_graticule_labels  = TRUE,
                                    show_legend            = TRUE) {

  require(tidyverse); require(tidyterra); require(ggnewscale)

  ext_plot  <- ext(plot_limits[1], plot_limits[2], plot_limits[3], plot_limits[4])
  nv_source <- if (use_clipped) naturalvalues_clipped else naturalvalues
  nv_crop   <- crop(nv_source, ext_plot)

  nv_df <- as.data.frame(nv_crop, xy = TRUE, na.rm = TRUE)
  colnames(nv_df)[3] <- "value"
  nv_df$classname <- nv_lookup[as.character(nv_df$value)]
  nv_df <- dplyr::filter(nv_df, !is.na(classname))

  present_classes <- unique(nv_df$classname)
  present_colours <- hab_colours[names(hab_colours) %in% present_classes]

  nv_guide <- if (show_legend) guide_legend(order = 1, ncol = 1) else "none"

  x_breaks <- thin_breaks(plot_limits[1:2])
  y_breaks <- thin_breaks(abs(plot_limits[3:4])) * -1

  p <- ggplot() +

    geom_tile(data = nv_df, aes(x = x, y = y, fill = classname)) +
    scale_fill_manual(
      name   = "Benthic ecosystem",
      values = present_colours,
      breaks = names(present_colours),
      guide  = nv_guide
    ) +

    new_scale_fill() +
    geom_sf(data = aus, fill = "seashell2", colour = "grey70", linewidth = 0.15) +

    new_scale_fill() +
    geom_sf(data = terrnp, aes(fill = leg_catego), colour = NA) +
    scale_fill_manual(
      values = c("National Park"  = "#c4cea6",
                 "Nature Reserve" = "#e4d0bb"),
      name   = "Terrestrial Parks",
      guide  = if (show_legend) guide_legend(order = 4) else "none"
    )

    geom_sf(data = marine_parks, fill = NA, colour = "grey50", linewidth = 0.25)

  if (show_predicted) {
    ph_crop   <- crop(predictedhabitat, ext_plot)
    ph_masked <- mask(ph_crop, resample(mask_250, ph_crop, method = "near"))

    ph_df <- as.data.frame(ph_masked, xy = TRUE, na.rm = TRUE)
    colnames(ph_df)[3] <- "prob"
    ph_df <- dplyr::filter(ph_df, prob > 0)

    ph_guide <- if (show_legend) guide_colourbar(order = 3) else "none"

    p <- p +
      new_scale_fill() +
      geom_tile(data = ph_df, aes(x = x, y = y, fill = prob),
                alpha = predicted_alpha) +
      scale_fill_gradient2(
        name     = "Predicted reef\nhabitat (probability)",
        low      = "#fff1e6",
        mid      = "#a87448",
        high     = "#301703",
        midpoint = 0.5,
        na.value = NA,
        guide    = ph_guide
      )
  }

  p <- p +
    coord_sf(xlim   = plot_limits[1:2],
             ylim   = plot_limits[3:4],
             crs    = 4326,
             expand = FALSE) +
    scale_x_continuous(breaks = x_breaks) +
    scale_y_continuous(breaks = y_breaks) +
    labs(x = NULL, y = NULL) +
    theme_minimal() +
    theme(
      legend.key.size   = unit(0.45, "cm"),
      legend.text       = element_text(size = 8),
      legend.title      = element_text(size = 9, face = "bold"),
      legend.position   = if (show_legend) "right" else "none",
      legend.box        = "vertical",
      panel.background  = element_rect(fill = "white", colour = NA),
      plot.background   = element_rect(fill = "white", colour = NA),
      panel.grid.major  = element_line(colour = grid_colour,
                                       linewidth = grid_lwd,
                                       linetype  = "solid"),
      panel.grid.minor  = element_blank(),
      axis.text         = if (show_graticule_labels) element_text(size = 10, colour = "grey40")
      else                       element_blank(),
      axis.ticks        = element_line(colour = "grey60")
    )

  return(p)
}

# FUNCTION 3: Bathymetry background
naturalvalues_map_hillshade <- function(plot_limits,
                                        use_clipped     = TRUE,
                                        show_predicted  = FALSE,
                                        predicted_alpha = 0.6,
                                        nv_alpha        = 0.55,
                                        hs_altitude     = 35,
                                        hs_azimuth      = 315,
                                        bathy_palette   = c("#04134a","#0a3272",
                                                            "#1a5fa0","#4b9ec9",
                                                            "#a8d4e6","#daeef7"),
                                        bathy_limits    = c(-6000, 0),
                                        show_legend     = TRUE) {

  require(tidyverse); require(tidyterra); require(ggnewscale); require(terra)

  ext_plot <- ext(plot_limits[1], plot_limits[2], plot_limits[3], plot_limits[4])

  bathy_crop    <- crop(bathy, ext_plot)
  bathy_clamped <- clamp(bathy_crop, lower = bathy_limits[1], upper = bathy_limits[2], values = TRUE)
  bathy_df      <- as.data.frame(bathy_clamped, xy = TRUE, na.rm = TRUE)
  colnames(bathy_df)[3] <- "depth"

  slope  <- terrain(bathy_crop, v = "slope",  unit = "radians")
  aspect <- terrain(bathy_crop, v = "aspect", unit = "radians")
  hs     <- shade(slope, aspect, angle = hs_altitude, direction = hs_azimuth, normalize = TRUE)
  hs_df  <- as.data.frame(hs, xy = TRUE, na.rm = TRUE)
  colnames(hs_df)[3] <- "hs"

  nv_source <- if (use_clipped) naturalvalues_clipped else naturalvalues
  nv_crop   <- crop(nv_source, ext_plot)
  nv_df     <- as.data.frame(nv_crop, xy = TRUE, na.rm = TRUE)
  colnames(nv_df)[3] <- "value"
  nv_df$classname <- nv_lookup[as.character(nv_df$value)]
  nv_df <- dplyr::filter(nv_df, !is.na(classname))

  present_classes <- unique(nv_df$classname)
  present_colours <- hab_colours[names(hab_colours) %in% present_classes]

  bathy_guide <- if (show_legend) guide_colourbar(order = 2,
                                                  barwidth  = unit(0.4, "cm"),
                                                  barheight = unit(3, "cm")) else "none"
  nv_guide    <- if (show_legend) guide_legend(order = 1, ncol = 1) else "none"

  p <- ggplot() +

    geom_tile(data = bathy_df, aes(x = x, y = y, fill = depth)) +
    scale_fill_gradientn(
      name     = "Depth (m)",
      colours  = bathy_palette,
      limits   = bathy_limits,
      na.value = "white",
      guide    = bathy_guide
    ) +

    new_scale_fill() +
    geom_tile(data = hs_df, aes(x = x, y = y, fill = hs), alpha = 0.45) +
    scale_fill_gradient(low = "black", high = "white", na.value = NA, guide = "none")

  if (nv_alpha > 0 && nrow(nv_df) > 0) {
    p <- p +
      new_scale_fill() +
      geom_tile(data = nv_df, aes(x = x, y = y, fill = classname), alpha = 1) +
      scale_fill_manual(
        name   = "Benthic ecosystem",
        values = present_colours,
        breaks = names(present_colours),
        guide  = nv_guide
      )
  }

  if (show_predicted) {
    ph_crop   <- crop(predictedhabitat, ext_plot)
    ph_masked <- mask(ph_crop, resample(mask_250, ph_crop, method = "near"))
    ph_df     <- as.data.frame(ph_masked, xy = TRUE, na.rm = TRUE)
    colnames(ph_df)[3] <- "prob"
    ph_df <- dplyr::filter(ph_df, prob > 0)

    ph_guide <- if (show_legend) guide_colourbar(order = 3) else "none"

    p <- p +
      new_scale_fill() +
      geom_tile(data = ph_df, aes(x = x, y = y, fill = prob), alpha = predicted_alpha) +
      scale_fill_gradient2(
        name     = "Predicted reef\nhabitat (probability)",
        low      = "#fff1e6", mid = "#a87448", high = "#301703",
        midpoint = 0.5, na.value = NA, guide = ph_guide
      )
  }

  p <- p +
    new_scale_fill() +
    geom_sf(data = aus, fill = "seashell2", colour = "grey80", linewidth = 0.1) +
    new_scale_fill() +
    geom_sf(data = terrnp, aes(fill = leg_catego), colour = NA) +
    scale_fill_manual(
      values = c("National Park"  = "#c4cea6",
                 "Nature Reserve" = "#e4d0bb"),
      name   = "Terrestrial Parks",
      guide  = if (show_legend) guide_legend(order = 4) else "none"
    ) +
    geom_sf(data = marine_parks, fill = NA, colour = "black", linewidth = 0.25) +
    coord_sf(xlim = plot_limits[1:2], ylim = plot_limits[3:4], crs = 4326, expand = FALSE) +
    labs(x = NULL, y = NULL) +
    theme_minimal() +
    theme(
      legend.key.size  = unit(0.45, "cm"),
      legend.text      = element_text(size = 8, colour = "grey20"),
      legend.title     = element_text(size = 9, face = "bold", colour = "grey10"),
      legend.position  = if (show_legend) "right" else "none",
      legend.box       = "vertical",
      panel.grid       = element_blank(),
      panel.background = element_rect(fill = "#04134a", colour = NA),
      plot.background  = element_rect(fill = "white",   colour = NA)
    )

  return(p)
}

# FUNCTION 4: Categorised predicted reef overlay
naturalvalues_map_reef_classified <- function(plot_limits,
                                              reef_threshold  = 0.5,
                                              depth_breaks    = c(shallow    = -30,
                                                                  mesophotic = -70,
                                                                  rariphotic = -200),
                                              ocean_colour    = "cornsilk1",
                                              show_legend     = TRUE,
                                              title           = NULL,
                                              break_step =0.2) {

  require(tidyverse); require(terra); require(sf); require(ggnewscale)

  ext_plot <- ext(plot_limits[1], plot_limits[2], plot_limits[3], plot_limits[4])

  # ── 1. Crop rasters ────────────────────────────────────────────────────────
  bathy_crop <- crop(bathy,                 ext_plot)
  ph_crop    <- crop(predictedhabitat,      ext_plot)
  nv_crop    <- crop(naturalvalues_clipped, ext_plot)
  ph_resamp  <- resample(ph_crop, bathy_crop, method = "bilinear")

  # ── 2. Classified reef data frame ─────────────────────────────────────────
  bathy_single <- bathy_crop[[1]]; names(bathy_single) <- "depth"
  ph_single    <- ph_resamp[[1]];  names(ph_single)    <- "prob"

  df <- as.data.frame(c(bathy_single, ph_single), xy = TRUE, na.rm = FALSE) %>%
    mutate(
      reef_class = case_when(
        !is.na(prob) & prob >= reef_threshold &
          depth >= depth_breaks["shallow"] & depth <= 0
        ~ "Shallow rocky reefs",

        !is.na(prob) & prob >= reef_threshold &
          depth >= depth_breaks["mesophotic"] & depth < depth_breaks["shallow"]
        ~ "Mesophotic reefs",

        !is.na(prob) & prob >= reef_threshold &
          depth >= depth_breaks["rariphotic"] & depth < depth_breaks["mesophotic"]
        ~ "Rariphotic reefs",           # merged class

        TRUE ~ "Shelf unvegetated sediments"
      )
    ) %>%
    filter(!is.na(depth))

  # ── 3. Natural values overlay ──────────────────────────────────────────────
  nv_df <- as.data.frame(nv_crop, xy = TRUE, na.rm = TRUE)
  colnames(nv_df)[3] <- "value"

  nv_df <- nv_df %>%
    filter(value != 1) %>%                           # drop shelf unvegetated
    mutate(
      nv_class = case_when(
        value %in% c(12, 13) ~ "Mesophotic reefs",   # merge mesophotic
        value == 15          ~ "Rariphotic reefs",    # merge rariphotic
        value == 10          ~ "Shallow rocky reefs", #Merge shallow coral reefs
        TRUE                  ~ nv_lookup[as.character(value)]
      )
    ) %>%
    filter(!is.na(nv_class))

  # ── 4. Colour palette ─────────────────────────────────────────────────────
  reef_colours <- c(
    "Shelf unvegetated sediments" = "cornsilk1",
    "Shallow rocky reefs"         = "darkgoldenrod1",
    "Mesophotic reefs"            = "khaki4",
    "Rariphotic reefs"            = "steelblue3"
  )

  nv_overlay_colours <- c(
    hab_colours[!names(hab_colours) %in% c("Shelf unvegetated sediments",
                                           "Mesophotic coral reefs",
                                           "Mesophotic rocky reefs",
                                           "Rariphotic shelf reefs")],
    "Mesophotic reefs" = "khaki4",
    "Rariphotic reefs" = "steelblue3"
  )

  all_colours <- c(reef_colours,
                   nv_overlay_colours[!names(nv_overlay_colours) %in% names(reef_colours)])

  present_reef <- unique(df$reef_class)
  present_nv   <- unique(nv_df$nv_class)
  present_all  <- names(all_colours)[names(all_colours) %in% c(present_reef, present_nv)]

  level_order <- c(
    "Shelf unvegetated sediments",
    "Shelf vegetated sediments",
    "Shallow rocky reefs",
    "Mesophotic reefs",
    "Rariphotic reefs",
    "Upper slope reefs",
    "Upper slope sediments",
    # remaining — shown only if present
    "Oceanic shallow coral reefs",
    "Oceanic mesophotic coral reefs",
    "Mid slope reefs",
    "Lower slope reef and sediments",
    "Abyssal reef and sediments",
    "Seamount reefs",
    "Seamount sediments",
    "Shelf incising canyons",
    "Mid slope sediments"
  )
  level_order <- level_order[level_order %in% present_all]

  df$reef_class  <- factor(df$reef_class,  levels = level_order)
  nv_df$nv_class <- factor(nv_df$nv_class, levels = level_order)

  x_breaks <- thin_breaks(plot_limits[1:2], step = break_step)
  y_breaks <- thin_breaks(abs(plot_limits[3:4]), step = break_step) * -1

  # ── 5. Build plot ──────────────────────────────────────────────────────────
  legend_guide <- if (show_legend) guide_legend(order = 1, ncol = 1) else "none"

  p <- ggplot() +

    geom_tile(data = df, aes(x = x, y = y, fill = reef_class)) +
    scale_fill_manual(
      name   = paste0("Habitat class\n(prob ≥ ", reef_threshold, ")"),
      values = all_colours[names(all_colours) %in% present_all],
      breaks = level_order,
      guide  = legend_guide,
      drop   = TRUE
    ) +

    new_scale_fill() +
    geom_tile(data = nv_df, aes(x = x, y = y, fill = nv_class)) +
    scale_fill_manual(
      name   = "Natural values overlay",
      values = all_colours[names(all_colours) %in% present_nv],
      breaks = level_order[level_order %in% present_nv],
      guide  = legend_guide,
      drop   = TRUE
    ) +

    new_scale_fill() +
    geom_sf(data = aus, fill = "seashell2", colour = "grey80", linewidth = 0.1) +

    new_scale_fill() +
    geom_sf(data = terrnp, aes(fill = leg_catego), colour = NA) +
    scale_fill_manual(
      values = c("National Park"  = "#c4cea6",
                 "Nature Reserve" = "#e4d0bb"),
      name   = "Terrestrial Parks",
      guide  = if (show_legend) guide_legend(order = 4) else "none"
    ) +

    geom_sf(data = marine_parks, fill = NA, colour = "black", linewidth = 0.2) +

    coord_sf(xlim = plot_limits[1:2], ylim = plot_limits[3:4], crs = 4326, expand = FALSE) +
    scale_x_continuous(breaks = x_breaks) +
    scale_y_continuous(breaks = y_breaks) +
    labs(x = NULL, y = NULL, title = title) +
    theme_minimal() +
    theme(
      legend.key.size  = unit(0.45, "cm"),
      legend.text      = element_text(size = 8),
      legend.title     = element_text(size = 9, face = "bold"),
      legend.position  = if (show_legend) "right" else "none",
      legend.box       = "vertical",
      panel.grid       = element_blank(),
      panel.background = element_rect(fill = ocean_colour, colour = NA),
      plot.background  = element_rect(fill = "white",      colour = NA),
      axis.text        = element_text(size = 10, colour = "grey40"),
      axis.ticks       = element_line(colour = "grey60"),
      plot.title       = if (!is.null(title)) element_text(face = "bold", size = 14, hjust = 0.5)
      else                 element_blank(),
      plot.margin = margin(t = 0, r = 0, b = 0, l = 0)
    )

  return(p)
}

# FUNCTION 5: Network-scale classified reef map with white MPs
naturalvalues_map_network_classified <- function(plot_limits,
                                                 reef_threshold = 0.5,
                                                 depth_breaks   = c(shallow    = -30,
                                                                    mesophotic = -70,
                                                                    rariphotic = -200),
                                                 ocean_colour   = "#2b3a4a",
                                                 break_step     = 2.0,
                                                 title          = NULL) {

  require(tidyverse); require(terra); require(sf); require(ggnewscale)

  ext_plot <- ext(plot_limits[1], plot_limits[2], plot_limits[3], plot_limits[4])

  # ── 1. Crop rasters ──────────────────────────────────────────────────────
  bathy_crop <- crop(bathy,          ext_plot)
  ph_crop    <- crop(predictedhabitat, ext_plot)
  nv_crop    <- crop(naturalvalues,  ext_plot)   # <- was naturalvalues_clipped
  ph_resamp  <- resample(ph_crop, bathy_crop, method = "bilinear")
  ph_resamp  <- mask(ph_resamp, resample(mask_250, bathy_crop, method = "near"))

  # ── 2. Natural values base layer ─────────────────────────────────────────
  nv_df <- as.data.frame(nv_crop, xy = TRUE, na.rm = TRUE)
  colnames(nv_df)[3] <- "value"

  nv_df <- nv_df %>%
    mutate(
      nv_class = case_when(
        value == 1                   ~ "Shelf unvegetated sediments",
        value %in% c(8, 10, 11)     ~ "Shallow reefs",
        value %in% c(12, 13, 14)    ~ "Mesophotic reefs",
        value == 15                  ~ "Rariphotic reefs",
        value == 16                  ~ "Upper slope reefs",
        value == 17                  ~ "Mid slope reefs",
        value == 18                  ~ "Seamount reefs",
        value == 9                   ~ "Shelf vegetated sediments",
        value == 2                   ~ "Upper slope sediments",
        value == 3                   ~ "Mid slope sediments",
        value == 4                   ~ "Lower slope reef and sediments",
        value == 5                   ~ "Abyssal reef and sediments",
        value == 6                   ~ "Seamount sediments",
        value == 7                   ~ "Shelf incising canyons",
        TRUE                          ~ NA_character_
      )
    ) %>%
    filter(!is.na(nv_class))

  # ── 3. Predicted reef classified layer (non-sediment classes only) ────────
  bathy_single <- bathy_crop[[1]]; names(bathy_single) <- "depth"
  ph_single    <- ph_resamp[[1]];  names(ph_single)    <- "prob"

  df_reef <- as.data.frame(c(bathy_single, ph_single), xy = TRUE, na.rm = FALSE) %>%
    mutate(
      reef_class = case_when(
        !is.na(prob) & prob >= reef_threshold &
          depth >= depth_breaks["shallow"] & depth <= 0
        ~ "Shallow reefs",

        !is.na(prob) & prob >= reef_threshold &
          depth >= depth_breaks["mesophotic"] & depth < depth_breaks["shallow"]
        ~ "Mesophotic reefs",

        !is.na(prob) & prob >= reef_threshold &
          depth >= depth_breaks["rariphotic"] & depth < depth_breaks["mesophotic"]
        ~ "Rariphotic reefs",

        TRUE ~ NA_character_     # sediments dropped — ocean_colour shows through
      )
    ) %>%
    filter(!is.na(depth), !is.na(reef_class))   # only plot reef pixels

  # ── 4. Colour palette ────────────────────────────────────────────────────
  all_colours <- c(
    "Shelf unvegetated sediments"    = "cornsilk1",
    "Shelf vegetated sediments"      = "seagreen3",
    "Shallow reefs"                  = "darkgoldenrod1",
    "Mesophotic reefs"               = "khaki4",
    "Rariphotic reefs"               = "steelblue3",
    "Upper slope reefs"              = "indianred3",
    "Mid slope reefs"                = "palevioletred3",
    "Seamount reefs"                 = "mediumpurple3",
    "Upper slope sediments"          = "wheat1",
    "Mid slope sediments"            = "navajowhite1",
    "Lower slope reef and sediments" = "lightsteelblue2",
    "Abyssal reef and sediments"     = "slategrey",
    "Seamount sediments"             = "rosybrown2",
    "Shelf incising canyons"         = "grey50"
  )

  present_nv   <- unique(nv_df$nv_class)
  present_reef <- unique(df_reef$reef_class)
  present_all  <- names(all_colours)[names(all_colours) %in% c(present_nv, present_reef)]

  level_order <- c(
    "Shelf unvegetated sediments",
    "Shelf vegetated sediments",
    "Shallow reefs",
    "Mesophotic reefs",
    "Rariphotic reefs",
    "Upper slope reefs",
    "Upper slope sediments",
    "Mid slope reefs",
    "Lower slope reef and sediments",
    "Abyssal reef and sediments",
    "Seamount reefs",
    "Seamount sediments",
    "Shelf incising canyons",
    "Mid slope sediments"
  )
  level_order <- level_order[level_order %in% present_all]

  nv_df$nv_class     <- factor(nv_df$nv_class,     levels = level_order)
  df_reef$reef_class <- factor(df_reef$reef_class, levels = level_order)

  x_breaks <- thin_breaks(plot_limits[1:2], step = break_step)
  y_breaks <- thin_breaks(abs(plot_limits[3:4]), step = break_step) * -1

  # ── 5. Build plot — NV base, predicted reef on top ───────────────────────
  p <- ggplot() +

    # Layer 1: natural values (all classes including sediments)
    geom_tile(data = nv_df, aes(x = x, y = y, fill = nv_class)) +
    scale_fill_manual(
      values = all_colours[names(all_colours) %in% present_nv],
      breaks = level_order[level_order %in% present_nv],
      guide  = "none"
    ) +

    # Layer 2: predicted reef classes on top (sediments excluded)
    new_scale_fill() +
    geom_tile(data = df_reef, aes(x = x, y = y, fill = reef_class)) +
    scale_fill_manual(
      values = all_colours[names(all_colours) %in% present_reef],
      breaks = level_order[level_order %in% present_reef],
      guide  = "none"
    ) +

    # Layer 3: land + terrestrial parks
    new_scale_fill() +
    geom_sf(data = aus, fill = "seashell2", colour = "grey80", linewidth = 0.1) +

    new_scale_fill() +
    geom_sf(data = terrnp, aes(fill = leg_catego), colour = NA) +
    scale_fill_manual(
      values = c("National Park"  = "#c4cea6",
                 "Nature Reserve" = "#e4d0bb"),
      guide  = "none"
    ) +

    # Layer 4: marine park boundaries
    geom_sf(data = marine_parks, fill = alpha("grey80", 0.3), colour = "white", linewidth = 0.2) +

    coord_sf(xlim = plot_limits[1:2], ylim = plot_limits[3:4], crs = 4326, expand = FALSE) +
    scale_x_continuous(breaks = x_breaks) +
    scale_y_continuous(breaks = y_breaks) +
    labs(x = NULL, y = NULL, title = title) +
    theme_minimal() +
    theme(
      legend.position  = "none",
      panel.grid       = element_blank(),
      panel.background = element_rect(fill = ocean_colour, colour = NA),
      plot.background  = element_rect(fill = "white",      colour = NA),
      axis.text        = element_text(size = 10, colour = "grey40"),
      axis.ticks       = element_line(colour = "grey60"),
      plot.title       = if (!is.null(title)) element_text(face = "bold", size = 14, hjust = 0.5)
      else                 element_blank(),
      plot.margin      = margin(t = 0, r = 0, b = 0, l = 0)
    )

  return(p)
}

# FUNCTION 6: Network-scale natural values map with white MPs
naturalvalues_map__network_dynamic <- function(plot_limits,
                                               use_clipped     = TRUE,
                                               show_predicted  = FALSE,
                                               predicted_alpha = 1,
                                               ocean_colour    = "cornsilk1",
                                               show_legend     = TRUE,
                                               y_label         = NULL,
                                               title           = NULL,
                                               break_step      = 0.2) {

  require(tidyverse); require(tidyterra); require(ggnewscale)

  ext_plot  <- ext(plot_limits[1], plot_limits[2], plot_limits[3], plot_limits[4])
  nv_source <- if (use_clipped) naturalvalues_clipped else naturalvalues
  nv_crop   <- crop(nv_source, ext_plot)

  nv_df <- as.data.frame(nv_crop, xy = TRUE, na.rm = TRUE)
  colnames(nv_df)[3] <- "value"
  nv_df$classname <- nv_lookup[as.character(nv_df$value)]
  nv_df <- dplyr::filter(nv_df, !is.na(classname))

  present_classes <- unique(nv_df$classname)
  present_colours <- hab_colours[names(hab_colours) %in% present_classes]

  nv_guide <- if (show_legend) guide_legend(order = 1, ncol = 1) else "none"

  x_breaks <- thin_breaks(plot_limits[1:2], step = break_step)
  y_breaks <- thin_breaks(abs(plot_limits[3:4]), step = break_step) * -1

  p <- ggplot() +

    geom_tile(data = nv_df, aes(x = x, y = y, fill = classname)) +
    scale_fill_manual(
      name   = "Benthic ecosystem",
      values = present_colours,
      breaks = names(present_colours),
      guide  = nv_guide
    ) +

    new_scale_fill() +
    geom_sf(data = aus, fill = "seashell2", colour = "grey80", linewidth = 0.1) +

    new_scale_fill() +
    geom_sf(data = terrnp, aes(fill = leg_catego), colour = NA) +
    scale_fill_manual(
      values = c("National Park"  = "#c4cea6",
                 "Nature Reserve" = "#e4d0bb"),
      name   = "Terrestrial Parks",
      guide  = if (show_legend) guide_legend(order = 4) else "none"
    )

  if (show_predicted) {
    ph_crop   <- crop(predictedhabitat, ext_plot)
    ph_masked <- mask(ph_crop, resample(mask_250, ph_crop, method = "near"))

    ph_df <- as.data.frame(ph_masked, xy = TRUE, na.rm = TRUE)
    colnames(ph_df)[3] <- "prob"
    ph_df <- dplyr::filter(ph_df, prob > 0)

    ph_guide <- if (show_legend) guide_colourbar(order = 3) else "none"

    p <- p +
      new_scale_fill() +
      geom_tile(data = ph_df, aes(x = x, y = y, fill = prob),
                alpha = predicted_alpha) +
      scale_fill_gradient2(
        name     = "Predicted reef\nhabitat (probability)",
        low      = "#fff1e6",
        mid      = "#a87448",
        high     = "#301703",
        midpoint = 0.5,
        na.value = NA,
        guide    = ph_guide
      )
  }

  p <- p +
    geom_sf(data = marine_parks, fill = alpha("grey80", 0.3), colour = "white", linewidth = 0.2)

  p <- p +
    coord_sf(xlim   = plot_limits[1:2],
             ylim   = plot_limits[3:4],
             crs    = 4326,
             expand = FALSE) +
    scale_x_continuous(breaks = x_breaks) +
    scale_y_continuous(breaks = y_breaks) +
    labs(x = NULL, y = y_label, title = title) +
    theme_minimal() +
    theme(
      legend.key.size  = unit(0.45, "cm"),
      legend.text      = element_text(size = 8),
      legend.title     = element_text(size = 9, face = "bold"),
      legend.position  = if (show_legend) "right" else "none",
      legend.box       = "vertical",
      panel.grid       = element_blank(),
      panel.background = element_rect(fill = ocean_colour, colour = NA),
      plot.background  = element_rect(fill = "white",      colour = NA),
      axis.text        = element_text(size = 10, colour = "grey40"),
      axis.ticks       = element_line(colour = "grey60"),
      axis.title.y     = if (!is.null(y_label)) element_text(face = "bold", size = 12, margin = margin(r = 11))
      else                   element_blank(),
      plot.title       = if (!is.null(title)) element_text(face = "bold", size = 14, hjust = 0.5)
      else                 element_blank(),
      plot.margin = margin(t = 0, r = 0, b = 0, l = 0)
    )

  return(p)
}

# Create shared legend builder function
build_network_legend <- function(park_extents,
                                 reef_threshold = 0.5,
                                 depth_breaks   = c(shallow    = -30,
                                                    mesophotic = -70,
                                                    rariphotic = -200),
                                 ncol_legend    = 4) {

  all_present <- character(0)

  for (limits in park_extents) {

    ext_plot <- ext(limits[1], limits[2], limits[3], limits[4])

    # Natural values classes
    nv_crop <- crop(naturalvalues_clipped, ext_plot)
    nv_df   <- as.data.frame(nv_crop, xy = TRUE, na.rm = TRUE)
    colnames(nv_df)[3] <- "value"
    nv_df$classname <- nv_lookup[as.character(nv_df$value)]
    nv_df <- dplyr::filter(nv_df, !is.na(classname))
    all_present <- union(all_present, unique(nv_df$classname))

    # Reef classified classes
    bathy_crop <- crop(bathy,            ext_plot)
    ph_crop    <- crop(predictedhabitat, ext_plot)
    ph_resamp  <- resample(ph_crop, bathy_crop, method = "bilinear")

    bathy_single <- bathy_crop[[1]]; names(bathy_single) <- "depth"
    ph_single    <- ph_resamp[[1]];  names(ph_single)    <- "prob"

    df_reef <- as.data.frame(c(bathy_single, ph_single), xy = TRUE, na.rm = FALSE) %>%
      mutate(
        reef_class = case_when(
          !is.na(prob) & prob >= reef_threshold &
            depth >= depth_breaks["shallow"] & depth <= 0
          ~ "Shallow rocky reefs",

          !is.na(prob) & prob >= reef_threshold &
            depth >= depth_breaks["mesophotic"] & depth < depth_breaks["shallow"]
          ~ "Mesophotic reefs",

          !is.na(prob) & prob >= reef_threshold &
            depth >= depth_breaks["rariphotic"] & depth < depth_breaks["mesophotic"]
          ~ "Rariphotic reefs",          # merged class

          TRUE ~ "Shelf unvegetated sediments"
        )
      ) %>%
      filter(!is.na(depth))
    all_present <- union(all_present, unique(df_reef$reef_class))

    # Natural values overlay classes (with merging)
    nv_overlay_df <- as.data.frame(nv_crop, xy = TRUE, na.rm = TRUE)
    colnames(nv_overlay_df)[3] <- "value"
    nv_overlay_df <- nv_overlay_df %>%
      filter(value != 1) %>%
      mutate(
        nv_class = case_when(
          value %in% c(12, 13) ~ "Mesophotic reefs",
          value == 15          ~ "Rariphotic reefs",
          value == 10          ~ "Shallow rocky reefs",
          TRUE                  ~ nv_lookup[as.character(value)]
        )
      ) %>%
      filter(!is.na(nv_class))
    all_present <- union(all_present, unique(nv_overlay_df$nv_class))
  }

  # ── Colour lookup ──────────────────────────────────────────────────────────
  reef_colours <- c(
    "Shelf unvegetated sediments" = "cornsilk1",
    "Shallow rocky reefs"         = "darkgoldenrod1",
    "Mesophotic reefs"            = "khaki4",
    "Rariphotic reefs"            = "steelblue3"
  )

  nv_all_colours <- c(
    hab_colours[!names(hab_colours) %in% c("Shelf unvegetated sediments",
                                           "Mesophotic rocky reefs",
                                           "Rariphotic shelf reefs")],
    "Mesophotic reefs" = "khaki4",
    "Rariphotic reefs" = "steelblue3"
  )

  all_colours     <- c(reef_colours,
                       nv_all_colours[!names(nv_all_colours) %in% names(reef_colours)])
  present_colours <- all_colours[names(all_colours) %in% all_present]

  # ── Legend order ───────────────────────────────────────────────────────────
  level_order <- c(
    "Shelf unvegetated sediments",
    "Shelf vegetated sediments",
    "Shallow coral reefs",
    "Shallow rocky reefs",
    "Mesophotic coral reefs",
    "Mesophotic reefs",
    "Rariphotic reefs",
    "Upper slope reefs",
    "Upper slope sediments",
    # remaining — shown only if present
    "Oceanic shallow coral reefs",
    "Oceanic mesophotic coral reefs",
    "Mid slope reefs",
    "Lower slope reef and sediments",
    "Abyssal reef and sediments",
    "Seamount reefs",
    "Seamount sediments",
    "Shelf incising canyons",
    "Mid slope sediments"
  )
  level_order <- level_order[level_order %in% names(present_colours)]

  # ── Benthic legend ────────────────────────────────────────────────────────
  dummy_df <- data.frame(
    x         = 1,
    y         = 1,
    classname = factor(level_order, levels = level_order)
  )

  legend_benthic <- ggplot(dummy_df, aes(x = x, y = y, fill = classname)) +
    geom_tile() +
    scale_fill_manual(
      name   = "Benthic habitat",
      values = present_colours[level_order],
      breaks = level_order,
      guide  = guide_legend(
        ncol           = ncol_legend,
        direction      = "horizontal",
        title.position = "top",
        title.hjust    = 0
      )
    ) +
    theme_void() +
    theme(
      legend.key.size  = unit(0.6, "cm"),
      legend.text      = element_text(size = 15),
      legend.title     = element_text(size = 16, face = "bold"),
      legend.position  = "bottom"
    )

  # ── Terrestrial parks legend ──────────────────────────────────────────────
  tp_df <- data.frame(
    x  = 1, y = 1,
    tp = factor(c("National Park", "Nature Reserve"),
                levels = c("National Park", "Nature Reserve"))
  )

  legend_tp <- ggplot(tp_df, aes(x = x, y = y, fill = tp)) +
    geom_tile() +
    scale_fill_manual(
      name   = "Terrestrial Parks",
      values = c("National Park" = "#c4cea6", "Nature Reserve" = "#e4d0bb"),
      guide  = guide_legend(
        ncol           = 1,
        direction      = "vertical",
        title.position = "top",
        title.hjust    = 0
      )
    ) +
    theme_void() +
    theme(
      legend.key.size  = unit(0.6, "cm"),
      legend.text      = element_text(size = 15),
      legend.title     = element_text(size = 16, face = "bold"),
      legend.position  = "bottom",
      legend.key.spacing.y = unit(0.2, "cm")
    )

  cowplot::plot_grid(
    cowplot::get_legend(legend_benthic),
    cowplot::get_legend(legend_tp),
    nrow       = 1,
    rel_widths = c(4, 1.1)
  )
}

# CREATING PLOTS
# plot extents
geographe_limits <- c(114.4, 115.9, -33.8957, -33.1043)
tworocks_limits  <- c(114.7, 116.0, -32.0,    -31.3)
swc_limits       <- c(113.5, 116.4, -34.7857, -33.2643)

check_ratio(geographe_limits)
check_ratio(tworocks_limits)
check_ratio(swc_limits)


# FIGURE 1: Two rocks and Geographe bay
# Call functions
tworocks_2018  <- naturalvalues_map_dynamic(plot_limits    = tworocks_limits,
                                            show_predicted = FALSE,
                                            ocean_colour   = "cornsilk1",
                                            show_legend    = FALSE,
                                            break_step = 0.1)

p_tworocks     <- naturalvalues_map_reef_classified(plot_limits    = tworocks_limits,
                                                    reef_threshold = 0.5,
                                                    ocean_colour   = "cornsilk1",
                                                    show_legend    = FALSE,
                                                    break_step = 0.1)

geographe_2018 <- naturalvalues_map_dynamic(plot_limits    = geographe_limits,
                                            show_predicted = FALSE,
                                            ocean_colour   = "cornsilk1",
                                            show_legend    = FALSE,
                                            break_step = 0.1)

p_geographe    <- naturalvalues_map_reef_classified(plot_limits    = geographe_limits,
                                                    reef_threshold = 0.5,
                                                    ocean_colour   = "cornsilk1",
                                                    show_legend    = FALSE,
                                                    break_step = 0.1)


legend_fig1 <- build_network_legend(
  park_extents   = list(tworocks = tworocks_limits, geographe = geographe_limits),
  reef_threshold = 0.5,
  ncol_legend    = 4
)

#Assemble plot together
label_tworocks  <- ggdraw() + draw_label("Two Rocks",  size = 16, angle = 90)
label_geographe <- ggdraw() + draw_label("Geographe",  size = 16, angle = 90)

row_tworocks <- cowplot::plot_grid(
  label_tworocks, tworocks_2018, NULL, p_tworocks,
  nrow = 1, rel_widths = c(0.08, 1, 0.03, 1)
) + theme(plot.margin = margin(0, 0, 0, 0))

row_geographe <- cowplot::plot_grid(
  label_geographe, geographe_2018, NULL, p_geographe,
  nrow = 1, rel_widths = c(0.08, 1, 0.03, 1)
) + theme(plot.margin = margin(0, 0, 0, 0))

title_2018 <- ggdraw() + draw_label("2018", fontface = "bold", size = 20)
title_2025 <- ggdraw() + draw_label("2025", fontface = "bold", size = 20)
title_row_fig1 <- cowplot::plot_grid(
  NULL, title_2018, NULL, title_2025,
  nrow = 1, rel_widths = c(0.08, 1, 0.03, 1)
) + theme(plot.margin = margin(0, 0, 0, 0))

maps_fig1 <- cowplot::plot_grid(
  title_row_fig1,
  row_tworocks,
  row_geographe,
  ncol        = 1,
  rel_heights = c(0.06, 1, 1)
)

figure1 <- cowplot::plot_grid(
  maps_fig1,
  legend_fig1,
  ncol        = 1,
  rel_heights = c(1, 0.14)
) +
  theme(plot.background = element_rect(fill = "white", colour = NA),
        plot.margin = margin(t = 5, r = 15, b = 5, l = 5))

# Save output
ggsave(paste(paste0('plots/', park, '/spatial/benthic_habitat/', name),
             'tworocks-geographe-benthic-habitats.png', sep = "-"),
       plot   = figure1,
       dpi    = 600,
       width  = 15,
       height = 11,
       bg     = "white")


# FIGURE 2: Southwest corner only
# Call functions
swc_2018    <- naturalvalues_map_dynamic(plot_limits    = swc_limits,
                                         show_predicted = FALSE,
                                         ocean_colour   = "cornsilk1",
                                         show_legend    = FALSE)

p_southwest <- naturalvalues_map_reef_classified(plot_limits    = swc_limits,
                                                 reef_threshold = 0.5,
                                                 ocean_colour   = "cornsilk1",
                                                 show_legend    = FALSE)

legend_fig2 <- build_network_legend(
  park_extents   = list(swc = swc_limits),
  reef_threshold = 0.5,
  ncol_legend    = 4
)

# Assemble plots
label_swc <- ggdraw() + draw_label("South-west Corner", size = 16, angle = 90)

row_swc <- cowplot::plot_grid(
  label_swc, swc_2018, NULL, p_southwest,
  nrow = 1, rel_widths = c(0.08, 1, 0.03, 1)
) + theme(plot.margin = margin(0, 0, 0, 0))

title_row_fig2 <- cowplot::plot_grid(
  NULL, title_2018, NULL, title_2025,
  nrow = 1, rel_widths = c(0.08, 1, 0.03, 1)
) + theme(plot.margin = margin(0, 0, 0, 0))

maps_fig2 <- cowplot::plot_grid(
  title_row_fig2,
  row_swc,
  ncol        = 1,
  rel_heights = c(0.06, 1)
)

figure2 <- cowplot::plot_grid(
  maps_fig2,
  legend_fig2,
  ncol        = 1,
  rel_heights = c(1, 0.14),
  align      = "v",
  axis       = "t"
) +
  theme(plot.background = element_rect(fill = "white", colour = NA),
        plot.margin = margin(t = 2, r = 15, b = 15, l = 5))

# svae output
ggsave(paste(paste0('plots/', park, '/spatial/benthic_habitat/', name),
             'corner-benthic-habitats-withTP.png', sep = "-"),
       plot   = figure2,
       dpi    = 600,
       width  = 15,
       height = 6.5,
       bg     = "white")


# FIGURE 3: SW network full extent
# Network extent
network_limits <- c(108.0, 138.0, -42.0, -24.0)

# Call functions
# use this to test if you like the look of map before committing to facetting the plots
# as network maps can take a while to load if lots of raster layers added
p_network <- naturalvalues_map_network_classified(
  plot_limits    = network_limits,
  reef_threshold = 0.5,
  ocean_colour   = "#2b3a4a",
  break_step     = 2.0
)

#  FIGURE 3: SW Network map
# Call functions
p_network_nv <- naturalvalues_map__network_dynamic(
  plot_limits    = network_limits,
  use_clipped    = FALSE,          # full depth range
  show_predicted = FALSE,
  ocean_colour   = "#2b3a4a",
  show_legend    = FALSE,
  break_step     = 2.0
)

p_network_reef <- naturalvalues_map_network_classified(
  plot_limits    = network_limits,
  reef_threshold = 0.5,
  ocean_colour   = "#2b3a4a",
  break_step     = 2.0
)


# Assemble
network_legend <- build_network_legend(
  park_extents   = list(network = network_limits),
  reef_threshold = 0.5,
  ncol_legend    = 4
)

title_2018_net <- ggdraw() + draw_label("2018", fontface = "bold", size = 20)
title_2025_net <- ggdraw() + draw_label("2025", fontface = "bold", size = 20)

title_row_fig3 <- cowplot::plot_grid(
  title_2018_net, NULL, title_2025_net,
  nrow = 1, rel_widths = c(1, 0.03, 1)
) + theme(plot.margin = margin(0, 0, -15, 0))

row_network <- cowplot::plot_grid(
  p_network_nv, NULL, p_network_reef,
  nrow = 1, rel_widths = c(1, 0.03, 1)
) + theme(plot.margin = margin(0, 0, 0, 0))

maps_fig3 <- cowplot::plot_grid(
  title_row_fig3,
  row_network,
  ncol        = 1,
  rel_heights = c(0.02, 1)
)

figure3 <- cowplot::plot_grid(
  maps_fig3,
  network_legend,
  ncol        = 1,
  rel_heights = c(1, 0.14),
  align       = "v",
  axis        = "t"
) +
  theme(plot.background = element_rect(fill = "white", colour = NA),
        plot.margin = margin(t = 2, r = 15, b = 15, l = 5))

# Save output
ggsave(paste(paste0('plots/', park, '/spatial/benthic_habitat/', name),
             'network-benthic-habitats.png', sep = "-"),
       plot   = figure3,
       dpi    = 600,
       width  = 15,
       height = 7.5,
       bg     = "white")
