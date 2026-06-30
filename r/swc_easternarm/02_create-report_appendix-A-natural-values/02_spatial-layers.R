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

# Set the extent of the study
e <- ext(120.7, 121.8, -34.6, -33.8)

# TODO Download AusBathyTopo 2024 from https://pid.geoscience.gov.au/dataset/ga/150050
# and save in below folder
# Load the bathymetry data (GA 250m resolution)
bathy <- rast("data/south-west network/spatial/rasters/AusBathyTopo__Australia__2024_250m_MSL_cog.tif") %>%
  crop(e) %>%
  clamp(upper = 0, lower = -250, values = F) %>%
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
dir.create(paste0("data/", park, "/spatial/rasters/"), recursive = TRUE, showWarnings = FALSE)
saveRDS(preds, file = paste0("data/", park, "/spatial/rasters/",
                             name, "_bathymetry-derivatives.rds"))

# Read in the metadata
metadata <- readRDS(paste0("data/", park, "/raw/metadata.RDS")) %>%
  dplyr::select(campaignid, sample, longitude_dd, latitude_dd, status, year) %>%
  glimpse()

# Bind in BOSS metadata (Investigator MBH, not in GlobalArchive) ----
boss_meta <- read_csv(paste0("data/", park, "/raw/Salisbury_Investigator_MBH_BOSS_habitat_Metadata.csv")) %>%
  rename_with(tolower) %>%
  filter(location == "Investigator MBH") %>%
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
extracted <- cbind(metadata,
                   terra::extract(preds, metadata_sf)) %>%
  dplyr::select(-ID)

# Check which samples are missing bathymetry derivatives
missing_bathy <- extracted %>%
  filter(if_any(c(geoscience_depth, geoscience_aspect, geoscience_roughness, geoscience_detrended),
                ~is.na(.)))
message(nrow(missing_bathy), " sample(s) missing bathymetry derivatives and will be dropped:")
print(missing_bathy %>% dplyr::select(campaignid, sample, longitude_dd, latitude_dd))

metadata.bathy.derivatives <- extracted %>%
  filter(if_all(c(geoscience_depth, geoscience_aspect, geoscience_roughness, geoscience_detrended),
                ~!is.na(.))) %>%
  glimpse()

# Save the metadata bathymetry derivatives
dir.create(paste0("data/", park, "/tidy/"), recursive = TRUE, showWarnings = FALSE)
saveRDS(metadata.bathy.derivatives, paste0("data/", park, "/tidy/", name, "_metadata-bathymetry-derivatives.rds"))

