rm(list = ls())

library(tidyverse)
library(tidyterra)
library(terra)
library(sf)
library(ggnewscale)
library(patchwork)

# Set cropping extent - larger than most zoomed out plot
e <- ext(116.7, 117.7,-20.919, -20)

# Load necessary spatial files
sf_use_s2(T)
# Australian outline and state and commonwealth marine parks
aus    <- st_read("data/south-west network/spatial/shapefiles/STE_2021_AUST_GDA2020.shp") %>%
  st_make_valid()
ausc <- st_crop(aus, e)

site_limits = c(116.779, 117.544, -20.738, -20.282) # For Dampier match it to the first plot

aus_marine_parks <- st_read("data/south-west network/spatial/shapefiles/western-australia_marine-parks-all.shp")

marine_parks <- st_read("data/south-west network/spatial/shapefiles/western-australia_marine-parks-all.shp") %>%
  dplyr::filter(name %in% c("Dampier")) %>%
  glimpse()

# Australian Marine Parks only (for separate ggplot legends)
marine_parks_amp <- marine_parks %>%
  dplyr::filter(epbc %in% "Commonwealth") %>%
  arrange(zone)

preds <- readRDS("data/dampier/spatial/rasters/DampierAMP_bathymetry-derivatives.rds")

mb <- rast("data/dampier/spatial/rasters/North_West_Shelf_DEM_v2_Bathymetry_2020_30m_MSL_cog.tif") %>%
  project("epsg:4326") %>%
  crop(e) %>%
  clamp(upper = 0, values = F)
plot(mb)
names(mb) <- "mb_depth"

depth <- ggplot() +
  geom_spatraster(data = preds, aes(fill = geoscience_depth)) +
  scale_fill_viridis_c(na.value = NA, option = "viridis", name = "Depth") +
  geom_sf(data = ausc, fill = "seashell2", colour = "grey80", size = 0.1) +
  new_scale_fill() +
  geom_sf(data = marine_parks_amp, aes(colour = zone), fill = NA, alpha = 0.8, linewidth = 1, show.legend = F) +
  scale_colour_manual(name = "Australian Marine Parks", guide = "legend",
                      values = with(marine_parks_amp, setNames(colour, zone))) +
  new_scale_fill() +
  labs(x = NULL, y = NULL) +
  new_scale_fill() +
  coord_sf(xlim = c(site_limits[1], site_limits[2]), ylim = c(site_limits[3], site_limits[4]), crs = 4326) +
  theme_minimal() +
  theme(panel.grid = element_blank())

aspect <- ggplot() +
  geom_spatraster(data = preds, aes(fill = geoscience_aspect)) +
  scale_fill_viridis_c(na.value = NA, option = "inferno", name = "Aspect") +
  geom_sf(data = ausc, fill = "seashell2", colour = "grey80", size = 0.1) +
  new_scale_fill() +
  geom_sf(data = marine_parks_amp, aes(colour = zone), fill = NA, alpha = 0.8, linewidth = 1, show.legend = F) +
  scale_colour_manual(name = "Australian Marine Parks", guide = "legend",
                      values = with(marine_parks_amp, setNames(colour, zone))) +
  new_scale_fill() +
  labs(x = NULL, y = NULL) +
  new_scale_fill() +
  coord_sf(xlim = c(site_limits[1], site_limits[2]), ylim = c(site_limits[3], site_limits[4]), crs = 4326) +
  theme_minimal() +
  theme(panel.grid = element_blank())

roughness <- ggplot() +
  geom_spatraster(data = preds, aes(fill = geoscience_roughness)) +
  scale_fill_viridis_c(na.value = NA, option = "turbo", name = "Roughness") +
  geom_sf(data = ausc, fill = "seashell2", colour = "grey80", size = 0.1) +
  new_scale_fill() +
  geom_sf(data = marine_parks_amp, aes(colour = zone), fill = NA, alpha = 0.8, linewidth = 1, show.legend = F) +
  scale_colour_manual(name = "Australian Marine Parks", guide = "legend",
                      values = with(marine_parks_amp, setNames(colour, zone))) +
  new_scale_fill() +
  labs(x = NULL, y = NULL) +
  new_scale_fill() +
  coord_sf(xlim = c(site_limits[1], site_limits[2]), ylim = c(site_limits[3], site_limits[4]), crs = 4326) +
  theme_minimal() +
  theme(panel.grid = element_blank())

detrended <- ggplot() +
  geom_spatraster(data = preds, aes(fill = geoscience_detrended)) +
  scale_fill_viridis_c(na.value = NA, option = "rocket", name = "Detrended") +
  geom_sf(data = ausc, fill = "seashell2", colour = "grey80", size = 0.1) +
  new_scale_fill() +
  geom_sf(data = marine_parks_amp, aes(colour = zone), fill = NA, alpha = 0.8, linewidth = 1, show.legend = F) +
  scale_colour_manual(name = "Australian Marine Parks", guide = "legend",
                      values = with(marine_parks_amp, setNames(colour, zone))) +
  new_scale_fill() +
  labs(x = NULL, y = NULL) +
  new_scale_fill() +
  coord_sf(xlim = c(site_limits[1], site_limits[2]), ylim = c(site_limits[3], site_limits[4]), crs = 4326) +
  theme_minimal() +
  theme(panel.grid = element_blank())

detrended
ggsave("plots/dampier/spatial/DampierAMP_detrended.png",
       height = 6, width = 11, dpi = 300, bg = "white")

# Combine plots
(depth + aspect)/(roughness + plot_spacer())

ggsave("plots/dampier/spatial/DampierAMP_bathymetry-derivatives.png",
       height = 6, width = 11, dpi = 300, bg = "white")

depth <- ggplot() +
  geom_spatraster(data = preds, aes(fill = geoscience_depth)) +
  scale_fill_viridis_c(na.value = NA, option = "viridis", name = "Depth (250m res)") +
  geom_sf(data = ausc, fill = "seashell2", colour = "grey80", size = 0.1) +
  new_scale_fill() +
  geom_sf(data = marine_parks_amp, aes(colour = zone), fill = NA, alpha = 0.8, linewidth = 1, show.legend = F) +
  scale_colour_manual(name = "Australian Marine Parks", guide = "legend",
                      values = with(marine_parks_amp, setNames(colour, zone))) +
  new_scale_fill() +
  labs(x = NULL, y = NULL) +
  new_scale_fill() +
  coord_sf(xlim = c(site_limits[1], site_limits[2]), ylim = c(site_limits[3], site_limits[4]), crs = 4326) +
  theme_minimal() +
  theme(panel.grid = element_blank())

mb_depth <- ggplot() +
  geom_spatraster(data = mb, aes(fill = mb_depth)) +
  scale_fill_viridis_c(na.value = NA, option = "viridis", name = "Depth (30m res)") +
  geom_sf(data = ausc, fill = "seashell2", colour = "grey80", size = 0.1) +
  new_scale_fill() +
  geom_sf(data = marine_parks_amp, aes(colour = zone), fill = NA, alpha = 0.8, linewidth = 1, show.legend = F) +
  scale_colour_manual(name = "Australian Marine Parks", guide = "legend",
                      values = with(marine_parks_amp, setNames(colour, zone))) +
  new_scale_fill() +
  labs(x = NULL, y = NULL) +
  new_scale_fill() +
  coord_sf(xlim = c(site_limits[1], site_limits[2]), ylim = c(site_limits[3], site_limits[4]), crs = 4326) +
  theme_minimal() +
  theme(panel.grid = element_blank())

depth / mb_depth + plot_annotation(tag_levels = "a") &
  theme(legend.justification = "left")
ggsave("plots/dampier/spatial/DampierAMP_multibeam-comparison.png",
       height = 6, width = 6, dpi = 300, bg = "white")
