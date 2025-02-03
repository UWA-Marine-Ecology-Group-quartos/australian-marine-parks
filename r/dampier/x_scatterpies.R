rm(list = ls())

library(tidyverse)
library(tidyterra)
library(terra)
library(sf)
library(ggnewscale)
library(patchwork)
library(CheckEM)
library(scales)
library(scatterpie)

# Set the study name
name <- "DampierAMP"
park <- "dampier"

# Set cropping extent - larger than most zoomed out plot
e <- ext(116.7, 117.7,-20.919, -20)

site_limits = c(116.779, 117.544, -20.738, -20.282) # For Dampier match it to the first plot

# Load necessary spatial files
sf_use_s2(T)
# Australian outline and state and commonwealth marine parks
aus    <- st_read("data/south-west network/spatial/shapefiles/STE_2021_AUST_GDA2020.shp") %>%
  st_make_valid()
ausc <- st_crop(aus, e)

preds <- readRDS("data/dampier/spatial/rasters/DampierAMP_bathymetry-derivatives.rds")

metadata_bathy_derivatives <- readRDS(paste0("data/", park, "/tidy/", name, "_metadata-bathymetry-derivatives.rds")) %>%
  clean_names() %>%
  glimpse()

habi <- readRDS(paste0("data/", park, "/tidy/", name, "_benthos-count.RDS")) %>%
  left_join(metadata_bathy_derivatives) %>%
  dplyr::filter(!is.na(latitude_dd)) %>% # Check this
  dplyr::arrange(sessile_invertebrates) %>%
  glimpse()

hab_fills <- scale_fill_manual(values = c("sessile_invertebrates" = "plum",
                                          "macroalgae" = "darkgoldenrod4",
                                          # "Seagrass" = "forestgreen",
                                          "rock" = "grey40",
                                          "sand" = "wheat"),
                               name = "Habitat")

ggplot() +
  geom_spatraster(data = preds, aes(fill = geoscience_depth), alpha = 1, maxcell = Inf) +
  scale_fill_gradientn(colours = c("#061442","#014091", "#2b63b5","#6794d6"),
                       values = rescale(c(-50, -15,-8, 0)),
                       na.value = "#A0C3D8", name = "Depth")  +
  new_scale_fill() +
  geom_sf(data = ausc, fill = "seashell2", colour = "grey80", size = 0.1) +
  # geom_point(data = habitat_park, aes(x = longitude, y = latitude),
  #            fill = "white", colour = "white", alpha = 0.1, size = 7, shape = 16) +
  geom_scatterpie(data = habi, aes(x = longitude_dd, y = latitude_dd),
                  cols = c("sand", "sessile_invertebrates", "rock", "macroalgae"),
                  colour = NA, pie_scale = 0.66) +
  hab_fills +
  labs(x = "Longitude", y = "Latitude") +
  coord_sf(xlim = c(site_limits[1], site_limits[2]), ylim = c(site_limits[3], site_limits[4]), crs = 4326) +
  theme_minimal() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
ggsave(filename = "plots/dampier/habitat/DampierAMP_scatterpies.png",
       height = 6, width = 11, dpi = 300, bg = "white")
