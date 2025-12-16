###
# Project: NESP 4.20 - Marine Park Dashboard reporting
# Data:    Fish data synthesis & habitat models derived from FSSgam
# Task:    Create post-modelling fish figures for marine park reporting
# Author:  Claude Spencer
# Date:    June 2024
###

# Clear your environment
rm(list = ls())

# Set the study name
name <- "GeographeAMP"
park <- "geographe"

# Load libraries
library(tidyverse)
library(terra)
library(sf)
library(ggplot2)
library(ggnewscale)
library(scales)
library(viridis)
library(patchwork)
library(tidyterra)
library(png)
library(lwgeom)

# Load functions
file.sources = list.files(pattern = "*.R", path = "functions/", full.names = T)
sapply(file.sources, source, .GlobalEnv)

# Set cropping extent - larger than most zoomed out plot
e <- ext(114.2, 115.8, -34.7, -33.1)

# Load necessary spatial files
sf_use_s2(FALSE)  # Switch off spatial geometry for cropping

# Australian outline and state and commonwealth marine parks
marine_parks <- st_read("data/south-west network/spatial/shapefiles/western-australia_marine-parks-all.shp") %>%
  dplyr::filter(name %in% c("Ngari Capes", "Geographe", "South-west Corner")) %>%
  glimpse()
plot(marine_parks["zone"])

marine_parks_amp <- marine_parks %>% dplyr::filter(epbc %in% "Commonwealth")
marine_parks_state <- marine_parks %>% dplyr::filter(epbc %in% "State")

# Australian outline
aus <- st_read("data/south-west network/spatial/shapefiles/aus-shapefile-w-investigator-stokes.shp")
ausc <- st_crop(aus, e)

# Spatial predictions limits
prediction_limits <- c(115.0539, 115.5539, -33.64861, -33.35361)

# ------------------------------------------------------------
# PLOTS: loop years (mirrors habitat Script 08)
# ------------------------------------------------------------
pred.years <- c(2014L, 2024L)

for (pred_year in pred.years) {

  print(pred_year)

  # Read year-specific predictions
  dat <- readRDS(paste0("output/model-output/", park, "/fish/",
                        name, "_predicted-fish_", pred_year, ".rds"))

  # Ensure SpatRaster + CRS (fixes the unused crs arg error)
  if (!inherits(dat, "SpatRaster")) dat <- terra::rast(dat)
  terra::crs(dat) <- "EPSG:4326"

  plot(dat)

  fishmetric_plot(prediction_limits, dat = dat, year = pred_year)

  ggsave(paste0("plots/", park, "/fish/", name,
                "_individual-predictions_", pred_year, ".png"),
         width = 9, height = 5, dpi = 300, units = "in", bg = "white")
}

# ------------------------------------------------------------
# CONTROL DATA: mirrors habitat Script 08 (combine years on plots)
# ------------------------------------------------------------

pred.years <- c(2014L, 2024L)

# Create the data (returns a list per year: shallow/meso/rari)
control_all <- purrr::map(pred.years, \(yy) {

  dat_yy <- readRDS(paste0("output/model-output/", park, "/fish/",
                           name, "_predicted-fish_", yy, ".rds"))
  if (!inherits(dat_yy, "SpatRaster")) dat_yy <- terra::rast(dat_yy)
  terra::crs(dat_yy) <- "EPSG:4326"

  controldata_fish(dat = dat_yy, year = yy, amp_abbrv = "GMP", state_abbrv = "NCMP")
})

# Bind years together per depth band (so year is combined on plots)
park_dat.shallow <- purrr::map_dfr(control_all, "shallow")
park_dat.meso    <- purrr::map_dfr(control_all, "meso")
park_dat.rari    <- purrr::map_dfr(control_all, "rari")

# Shallow plot (both years together)
p_shallow <- controlplot_fish(data = park_dat.shallow, amp_abbrv = "GMP", state_abbrv = "NCMP",
                              title = "Shallow (0 - 30 m)")
ggsave(paste0("plots/", park, "/fish/", name, "_shallow-control-plots.png"),
       plot = p_shallow, height = 9, width = 8, dpi = 300, units = "in", bg = "white")

# Mesophotic plot (both years together)
p_meso <- controlplot_fish(data = park_dat.meso, amp_abbrv = "GMP", state_abbrv = "NCMP",
                           title = "Mesophotic (30 - 70 m)")
ggsave(paste0("plots/", park, "/fish/", name, "_mesophotic-control-plots.png"),
       plot = p_meso, height = 9, width = 8, dpi = 300, units = "in", bg = "white")

# Optional rariphotic:
# p_rari <- controlplot_fish(data = park_dat.rari, amp_abbrv = "GMP", state_abbrv = "NCMP",
#                            title = "Rariphotic (70 - 200 m)")
# ggsave(paste0("plots/", park, "/fish/", name, "_rariphotic-control-plots.png"),
#        plot = p_rari, height = 9, width = 8, dpi = 300, units = "in", bg = "white")
