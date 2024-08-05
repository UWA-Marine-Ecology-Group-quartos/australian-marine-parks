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
name <- "GeographeAMP"

# Load libraries
library(sf)
library(terra)
library(stars)
library(starsExtra)
library(tidyverse)
library(tidyterra)
library(patchwork)
library(RNetCDF)
library(ncdf4)

# Set the extent of the study
e <- ext(115.05, 115.558, -33.67, -33.349)

# Load the bathymetry data (GA 250m resolution)
bathy <- rast("data/south-west network/spatial/rasters/Australian_Bathymetry_and_Topography_2023_250m_MSL_cog.tif") %>%
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
saveRDS(preds, file = paste0("data/geographe/spatial/rasters/",
                      name, "_bathymetry-derivatives.rds"))

# Read in the metadata

# metadata <- readRDS(paste0("data/tidy/",
#                            name, "_metadata.rds")) %>%
#   dplyr::mutate(longitude_dd = as.numeric(longitude_dd),
#                 latitude_dd = as.numeric(latitude_dd)) %>%
#   glimpse()

metadata <- read.csv("data/geographe/raw/temp/2007-2014-Geographe-stereo-BRUVs.checked.metadata.csv") %>%
  dplyr::select(campaignid, sample, longitude, latitude, status) %>%
  dplyr::rename(longitude_dd = longitude, latitude_dd = latitude) %>%
  glimpse()

# Convert metadata to a spatial file and check alignment with bathymetry
metadata_sf <- st_as_sf(metadata, coords = c("longitude_dd", "latitude_dd"), crs = 4326)

# Check that samples align with bathymetry derivatives
plot(preds[[1]])
plot(metadata_sf, add = T)

# Extract bathymetry derivatives at each of the samples
metadata.bathy.derivatives   <- cbind(metadata,
                                      terra::extract(preds, metadata_sf)) %>%
  filter_at(vars(geoscience_depth, geoscience_aspect, geoscience_roughness, geoscience_detrended),
            all_vars(!is.na(.))) %>% # Removes samples missing bathymetry derivatives - check these!!
  dplyr::select(-ID) %>%
  glimpse()

# Save the metadata bathymetry derivatives
saveRDS(metadata.bathy.derivatives, paste0("data/geographe/tidy/", name, "_metadata-bathymetry-derivatives.rds"))

gmp <- st_read("data/south-west network/spatial/shapefiles/Collaborative_Australian_Protected_Areas_Database_(CAPAD)_2022_-_Marine.shp") %>%
  dplyr::filter(NAME %in% "Geographe") %>%
  glimpse()
plot(gmp)

# Oceanography/Pressures
# Sea surface temperature
nc_sst <- open.nc("data/geographe/spatial/oceanography/SST.nc", write = TRUE)
print.nc(nc_sst) # shows you all the file details
time_nc <- var.get.nc(nc_sst, 'time')  # NC_CHAR time:units = "days since 1981-01-01 00:00:00" ;
time_nc_sst <- utcal.nc("seconds since 1981-01-01 00:00:00", time_nc, type = "c")
dates_sst <- as.Date(time_nc_sst)
close.nc(nc_sst) # GDAL errors otherwise

rast_sst <- rast("data/geographe/spatial/oceanography/SST.nc",
                 subds = "sea_surface_temperature")
plot(rast_sst)
names(rast_sst) <- dates_sst

gmp_sst <- rast_sst %>%
  mask(gmp)
plot(gmp_sst)

winter_sst_ts <- rast_sst[[names(rast_sst)[str_detect(names(rast_sst), "-06-|-07-|-08-")]]]
winter_sst <- mean(winter_sst_ts, na.rm = T) %>%
  app(fun = function(i) {i - 273.15}) %>% # Convert kelvin to celsius
  mask(gmp) %>%
  trim()
plot(winter_sst)

sst_tsdf <- terra::global(rast_sst, fun = "mean", na.rm = T) %>%
  tibble::rownames_to_column() %>%
  cbind(terra::global(rast_sst, fun = "sd", na.rm = T)) %>%
  dplyr::mutate(temp = mean - 273.15,
                date = date(rowname)) %>% # Convert kelvin to celsius
  dplyr::mutate(season = case_when(month(date) %in% c(3, 4, 5) ~ "Autumn",
                                   month(date) %in% c(6, 7, 8) ~ "Winter",
                                   month(date) %in% c(9, 10, 11) ~ "Spring",
                                   month(date) %in% c(12, 1, 2) ~ "Summer")) %>%
  glimpse()

# Sea Level Anomaly
# nc_sla <- open.nc(paste0("data/geographe/spatial/oceanography/", name, "-SLA.nc"),
#                   write = TRUE)
nc_sla <- open.nc("data/geographe/spatial/oceanography/SLA.nc",
                  write = TRUE)
print.nc(nc_sla) # shows you all the file details
time_nc <- var.get.nc(nc_sla, 'TIME')
time_nc_sla <- utcal.nc("days since 1985-01-01 00:00:00 UTC", time_nc, type = "c")
dates_sla <- as.Date(time_nc_sla)
close.nc(nc_sla)

rast_sla <- terra::rast("data/geographe/spatial/oceanography/SLA.nc",
                        subds = "GSLA")
plot(rast_sla)
names(rast_sla) <- dates_sla

winter_sst_ts <- rast_sst[[names(rast_sst)[str_detect(names(rast_sst), "-06-|-07-|-08-")]]]
winter_sst <- mean(winter_sst_ts, na.rm = T)
plot(winter_sst)
