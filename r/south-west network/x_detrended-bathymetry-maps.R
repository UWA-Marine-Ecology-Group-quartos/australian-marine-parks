###
# Project: NESP 4.20 - Marine Park Dashboard reporting
# Data:    Marine parks, old and new bathymetry data (2009 & 2024)
# Task:    Format spatial covariates, extract covariates for each sampling location
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

# Set the extent of the study
e <- ext(108.0, 138.0, -40.0, -24.0)

# Load the old bathymetry data (GA 250m resolution)
# old raster layers are in ESRI grid format, so you will need to pont R to the folder the .adf files are stored in
old_bathy <- rast("data/south-west network/spatial/rasters/ausbath_09_v4") %>%
  crop(e) %>%
  clamp(upper = 0, lower = -250, values = F) %>%
  trim()
plot(old_bathy)

# Load the new bathymetry data (GA 250m resolution)
new_bathy <- rast("data/south-west network/spatial/rasters/AusBathyTopo__Australia__2024_250m_MSL_cog.tif") %>%
  crop(e) %>%
  clamp(upper = 0, lower = -250, values = F) %>%
  trim()
plot(new_bathy)

# Create detrended bathymetry for 2009 bathy
old_zstar <- st_as_stars(old_bathy)
old_detre <- detrend(old_zstar, parallel = 8)
old_detre <- as(object = old_detre, Class = "SpatRaster")
names(old_detre) <- c("geoscience_detrended", "lineartrend")

# Create detrended bathymetry for 2024 bathy
new_zstar <- st_as_stars(new_bathy)
new_detre <- detrend((old)new_zstar, parallel = 8)
new_detre <- as(object = new_detre, Class = "SpatRaster")
names(new_detre) <- c("geoscience_detrended", "lineartrend")
