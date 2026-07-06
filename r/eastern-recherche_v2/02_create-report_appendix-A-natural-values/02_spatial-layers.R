###
# Project: NESP 4.20 - Marine Park Dashboard reporting
# Data:    Spatial covariates
# Task:    Format spatial covariates, extract covariates for each sampling location
# Author:  Claude Spencer
# Date:    June 2024
###

# Clear the environment
rm(list = ls())

# Set the study name
script_dir <- dirname(
  rstudioapi::getActiveDocumentContext()$path
)

config <- yaml::read_yaml(
  file.path(script_dir, "00_config.yml")
)

name <- config$name
park <- config$park

# Load libraries
library(sf)
library(terra)
library(stars)
library(starsExtra)
library(tidyverse)
library(RNetCDF)
library(rerddap)

# ---- Bathymetry: Cape Pasley to Pollock Reef, 30 m multibeam -----------------
bathy <- rast("data/eastern-recherche_v2/spatial/rasters/CapePasleytoPollockReef_SI1054_epsg3857_Bathymetry_SI51_Depth_30m_2025_cog.tiff")
NAflag(bathy) <- 3.4028234663852886e+38
bathy <- project(bathy, "EPSG:4326", method = "bilinear") %>%
  clamp(upper = 0, lower = -250, values = FALSE) %>%
  trim()
plot(bathy)

# Create terrain metrics (bathymetry derivatives)
preds <- terrain(bathy, neighbors = 8,
                 v = c("aspect", "roughness"),
                 unit = "degrees")
names(preds) <- c("geoscience_aspect", "geoscience_roughness")

# Create detrended bathymetry
zstar <- st_as_stars(bathy)
detre <- detrend(zstar, parallel = 8)
detre <- as(object = detre, Class = "SpatRaster")
names(detre) <- c("geoscience_detrended", "lineartrend")

# Join depth, terrain metrics and detrended bathymetry
preds <- rast(list(bathy, preds, detre[[1]]))
names(preds)[1] <- "geoscience_depth"

# Save the bathymetry derivatives
saveRDS(preds, file = paste0("data/", park, "/spatial/rasters/",
                             name, "_bathymetry-derivatives.rds"))

# Read in the metadata
metadata <- readRDS(paste0("data/", park, "/raw/metadata.RDS")) %>%
  dplyr::select(campaignid, sample, longitude_dd, latitude_dd, status, year) %>%
  glimpse()

# Bind in BOSS metadata (not in GlobalArchive)
boss_meta <- read_csv(paste0("data/", park, "/raw/Salisbury_Investigator_MBH_BOSS_habitat_Metadata.csv")) %>%
  rename_with(tolower) %>%
  filter(location == "Salisbury MBH") %>%
  mutate(
    campaignid   = "2022-11_Salisbury-Investigator_BOSS",
    sample       = str_replace_all(sample, "_", "-"),
    longitude_dd = longitude,
    latitude_dd  = latitude,
    status       = as.factor(status),
    year         = as.factor(year(dmy(date)))
  ) %>%
  dplyr::select(campaignid, sample, longitude_dd, latitude_dd, status, year)

metadata <- bind_rows(metadata, boss_meta)

# Convert metadata to a spatial file and check alignment with bathymetry
metadata_sf <- st_as_sf(metadata, coords = c("longitude_dd", "latitude_dd"), crs = 4326)

# Check that samples align with bathymetry derivatives
plot(preds[[1]])
plot(metadata_sf, add = T)

# Extract bathymetry derivatives at each of the samples
metadata.bathy.derivatives   <- cbind(metadata,
                                      terra::extract(preds, metadata_sf)) %>%
  filter(if_all(c(geoscience_depth, geoscience_aspect, geoscience_roughness, geoscience_detrended),
                ~!is.na(.))) %>% # TODO Removes samples missing bathymetry derivatives - check these!
  dplyr::select(-ID) %>%
  glimpse()

# Save the metadata bathymetry derivatives
dir.create(paste0("data/", park, "/tidy/"), recursive = TRUE, showWarnings = TRUE)
saveRDS(metadata.bathy.derivatives, paste0("data/", park, "/tidy/", name, "_metadata-bathymetry-derivatives.rds"))
