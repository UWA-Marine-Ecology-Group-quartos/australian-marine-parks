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
name <- "DampierAMP"
park <- "dampier"

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

# Load functions
file.sources = list.files(pattern = "*.R", path = "functions/", full.names = T)
sapply(file.sources, source, .GlobalEnv)

dat <- readRDS(paste0("output/model-output/", park, "/fish/",
                      name, "_predicted-fish.RDS")) %>%
  rast(crs = "epsg:4326")
plot(dat)

# Set cropping extent - larger than most zoomed out plot
e <- ext(116.7, 117.7,-20.919, -20)

# Load necessary spatial files
sf_use_s2(F)                                                                    # Switch off spatial geometry for cropping
# Australian outline and state and commonwealth marine parks
marine_parks <- st_read("data/south-west network/spatial/shapefiles/western-australia_marine-parks-all.shp") %>%
  dplyr::filter(name %in% c("Dampier")) %>%
  arrange(zone) %>%
  glimpse()
plot(marine_parks["zone"])

marine_parks_amp <- marine_parks %>%
  dplyr::filter(epbc %in% "Commonwealth")
marine_parks_state <- marine_parks %>%
  dplyr::filter(epbc %in% "State")

# Australian outline and state and commonwealth marine parks
aus    <- st_read("data/south-west network/spatial/shapefiles/aus-shapefile-w-investigator-stokes.shp")
ausc <- st_crop(aus, e)

# Spatial predictions

prediction_limits = c(116.779, 117.544, -20.738, -20.282)
fishmetric_plot(prediction_limits)

ggsave(paste0("plots/", park, "/fish/", name, "_individual-predictions.png"),
       width = 9, height = 5, dpi = 300, units = "in", bg = "white")


controldata_fish(year = 2023, amp_abbrv = "DMP", state_abbrv = NA)

controlplot_fish(data = park_dat.shallow, amp_abbrv = "DMP",
                 state_abbrv = NA, title = "Shallow (0 - 30 m)")
ggsave(paste0("plots/", park, "/fish/", name, "_shallow_control-plots.png"),
       height = 7, width = 8, dpi = 300, units = "in", bg = "white")

controlplot_fish(data = park_dat.meso, amp_abbrv = "DMP",
                 state_abbrv = NA, title = "Mesophotic (30 - 70 m)")
ggsave(paste0("plots/", park, "/fish/", name, "_mesophotic_control-plots.png"),
       height = 7, width = 8, dpi = 300, units = "in", bg = "white")
