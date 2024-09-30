rm(list = ls())

library(terra)
library(sf)

parks <- st_read("data/south-west network/spatial/shapefiles/western-australia_marine-parks-all.shp") %>%
  glimpse()

template <- rast("data/south-west network/spatial/rasters/Australian_Bathymetry_and_Topography_2023_250m_MSL_cog.tif")
plot(template)
