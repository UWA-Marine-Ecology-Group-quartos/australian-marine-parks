library(tidyterra)
library(terra)
library(tidyverse)
library(sf)
library(ggnewscale)

e <- ext(114.402, 115.27, -34.563, -33.942)
e_utm <- ext(261645.39, 340120.53, 6190000, 6210000)

aus <- st_read("data/south-west network/spatial/shapefiles/aus-shapefile-w-investigator-stokes.shp") %>%
  st_crop(e) %>%
  st_transform(32750)

bathy <- rast("data/south-west network/spatial/rasters/SI1031-combined-10m-final-coverage-interim.tiff") %>%
  crop(e_utm)
plot(bathy)

hillshade <- rast("data/south-west network/spatial/rasters/SI1031_hillshade.tif") %>%
  crop(e_utm) %>%
  stretch(minq = 0.2, maxq = 0.98)
plot(hillshade)

ggplot() +
  geom_spatraster(data = hillshade, aes(fill = SI1031_hillshade), show.legend = F, maxcell = Inf) +
  scale_fill_gradient(low = "black", high = "white", na.value = NA) +
  new_scale_fill() +
  geom_spatraster(data = bathy, aes(fill = Depth), alpha = 0.4, show.legend = F, maxcell = Inf) +
  scale_fill_viridis_c(na.value = NA, option = "turbo", direction = 1) +
  new_scale_fill() +
  geom_sf(data = aus) +
  theme_minimal() +
  coord_sf(xlim = c(114.85, 115.1),
           ylim = c(-34.4, -34.25),
           crs = 4326)
ggsave(file = "plots/swc_riverbeds.png", height = 6, width = 8, dpi = 300, bg = "white")

e_utm <- ext(311197, 333053, 6174962, 6188474)

bathy <- rast("data/south-west network/spatial/rasters/SI1031-combined-10m-final-coverage-interim.tiff") %>%
  crop(e_utm)
plot(bathy)

hillshade <- rast("data/south-west network/spatial/rasters/SI1031_hillshade.tif") %>%
  crop(e_utm)
plot(hillshade)

ggplot() +
  geom_spatraster(data = hillshade, aes(fill = SI1031_hillshade), show.legend = F) +
  scale_fill_gradient(low = "black", high = "white", na.value = NA) +
  new_scale_fill() +
  geom_spatraster(data = bathy, aes(fill = Depth), alpha = 0.4, show.legend = F) +
  scale_fill_viridis_c(na.value = NA, option = "turbo", direction = 1) +
  new_scale_fill() +
  geom_sf(data = aus) +
  theme_minimal() +
  coord_sf(xlim = c(114.95, 115.15), ylim = c(-34.43, -34.54), crs = 4326)
ggsave(file = "plots/swc_riverbeds2.png", height = 6, width = 8, dpi = 300, bg = "white")

bathy <- rast("data/south-west network/spatial/rasters/SI1031-combined-10m-final-coverage-interim.tiff")

hillshade <- rast("data/south-west network/spatial/rasters/SI1031_hillshade.tif")

mps <- st_read("data/south-west network/spatial/shapefiles/western-australia_marine-parks-all.shp") %>%
  dplyr::filter(name %in% "South-west Corner") %>%
  glimpse()

ggplot() +
  geom_spatraster(data = hillshade, aes(fill = SI1031_hillshade), show.legend = F) +
  scale_fill_gradient(low = "black", high = "white", na.value = NA) +
  new_scale_fill() +
  geom_spatraster(data = bathy, aes(fill = Depth), alpha = 0.4, show.legend = F) +
  scale_fill_viridis_c(na.value = NA, option = "turbo", direction = 1) +
  new_scale_fill() +
  geom_sf(data = aus) +
  geom_sf(data = mps, aes(colour = zone), fill = NA) +
  scale_colour_manual(name = "Australian Marine Parks", guide = "legend",
                    values = with(mps, setNames(colour, zone))) +
  theme_minimal() +
  coord_sf(xlim = c(114.5, 115.2), ylim = c(-34, -34.7), crs = 4326) +
  annotate(geom = "rect", xmin = c(114.95, 114.85), xmax = c(115.15, 115.1),
           ymin = c(-34.43, -34.4), ymax = c(-34.54, -34.25), fill = NA, colour = "darkgoldenrod")
ggsave(file = "plots/swc_riverbed-locations.png", height = 6, width = 8, dpi = 300, bg = "white")
