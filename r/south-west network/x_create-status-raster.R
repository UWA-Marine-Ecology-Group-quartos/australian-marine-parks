rm(list = ls())

library(terra)
library(sf)
library(tidyverse)

parks <- st_read("data/south-west network/spatial/shapefiles/western-australia_marine-parks-all.shp") %>%
  dplyr::mutate(status = if_else(zone %in% c("Sanctuary Zone", "National Park Zone"), 1, 0)) %>%
  glimpse()
plot(parks["status"])

parkv <- vect(parks)

template <- rast("data/south-west network/spatial/rasters/Australian_Bathymetry_and_Topography_2023_250m_MSL_cog.tif") %>%
  crop(ext(parkv)) %>%
  clamp(upper = 0, values = F)
plot(template)

status_rast <- rasterize(parkv, template, field = "status")
status_rast[is.na(status_rast)] <- 0
plot(status_rast)

writeRaster(status_rast, "data/south-west network/spatial/rasters/status_raster.tif",
            overwrite = T)
