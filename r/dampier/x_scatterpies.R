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

# habi <- readRDS(paste0("data/", park, "/tidy/", name, "_benthos-count.RDS")) %>%
#   left_join(metadata_bathy_derivatives) %>%
#   dplyr::filter(!is.na(latitude_dd)) %>% # Check this
#   dplyr::arrange(sessile_invertebrates) %>%
#   glimpse()

# hab_fills <- scale_fill_manual(values = c("sessile_invertebrates" = "plum",
#                                           "macroalgae" = "darkgoldenrod4",
#                                           # "Seagrass" = "forestgreen",
#                                           "rock" = "grey40",
#                                           "sand" = "wheat"),
#                                name = "Habitat")

# hab_fills <- scale_fill_manual(values = c("Sessile invertebrates" = "pink1",
#                                           "Black & Octocorals" = "mediumpurple4",
#                                           "Sponges" = "orangered",
#                                           "Hydroids" = "springgreen4",
#                                           "Macroalgae" = "darkgoldenrod4",
#                                           "Consolidated (hard)" = "grey40",
#                                           "Unconsolidated (soft)" = "wheat"),
#                                name = "Habitat")
#
# ggplot() +
#   geom_spatraster(data = preds, aes(fill = geoscience_depth), alpha = 1, maxcell = Inf) +
#   scale_fill_gradientn(colours = c("#061442","#014091", "#2b63b5","#6794d6"),
#                        values = rescale(c(-50, -15,-8, 0)),
#                        na.value = "#A0C3D8", name = "Depth")  +
#   new_scale_fill() +
#   geom_sf(data = ausc, fill = "seashell2", colour = "grey80", size = 0.1) +
#   # geom_point(data = habitat_park, aes(x = longitude, y = latitude),
#   #            fill = "white", colour = "white", alpha = 0.1, size = 7, shape = 16) +
#   geom_scatterpie(data = habi, aes(x = longitude_dd, y = latitude_dd),
#                   cols = c("sand", "sessile_invertebrates", "rock", "macroalgae"),
#                   colour = NA, pie_scale = 0.66) +
#   hab_fills +
#   labs(x = "Longitude", y = "Latitude") +
#   coord_sf(xlim = c(site_limits[1], site_limits[2]), ylim = c(site_limits[3], site_limits[4]), crs = 4326) +
#   theme_minimal() +
#   theme(panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank())
# ggsave(filename = "plots/dampier/habitat/DampierAMP_scatterpies.png",
#        height = 6, width = 11, dpi = 300, bg = "white")

benthosboss <- readRDS(paste0("data/", park, "/raw/", name, "_BOSS_benthos.RDS")) %>%
  dplyr::rename(sample = period)
benthosbruv <- readRDS(paste0("data/", park, "/raw/", name, "_BRUVs_benthos.RDS")) %>%
  dplyr::rename(sample = opcode)

benthos <- bind_rows(benthosboss, benthosbruv) %>%
  dplyr::select(campaignid, sample, level_2, level_3, count) %>%
  dplyr::mutate(habitat = case_when(level_2 %in% "Substrate" ~ level_3,
                                    level_2 %in% "Sessile invertebrates" ~ level_2,
                                    level_2 %in% "Sponges" ~ level_2,
                                    level_3 %in% "Corals" ~ "Black & Octocorals",
                                    level_2 %in% "Macroalgae" ~ level_2,
                                    level_3 %in% "Hydroids" ~ level_3,
                                    level_3 %in% "True anemones" ~ "Sessile invertebrates",
                                    level_3 %in% "Hydrocorals" ~ "Sessile invertebrates")) %>%
  pivot_wider(names_from = habitat, values_from = count, values_fill = 0) %>%
  left_join(metadata_bathy_derivatives) %>%
  # clean_names() %>%
  dplyr::filter(!is.na(longitude_dd)) %>%
  glimpse()

hab_fills <- scale_fill_manual(values = c("Sessile invertebrates" = "pink1",
                                          "Black & Octocorals" = "mediumpurple4",
                                          "Sponges" = "orangered",
                                          "Hydroids" = "springgreen4",
                                          "Macroalgae" = "darkgoldenrod4",
                                          "Consolidated (hard)" = "grey40",
                                          "Unconsolidated (soft)" = "wheat"),
                               name = "Habitat")

ggplot() +
  geom_spatraster(data = preds, aes(fill = geoscience_depth), alpha = 0.75, maxcell = Inf) +
  scale_fill_gradientn(colours = c("#061442","#014091", "#2b63b5","#6794d6"),
                       values = rescale(c(-50, -15,-8, 0)),
                       na.value = "#6794d6", name = "Depth")  +
  new_scale_fill() +
  geom_sf(data = ausc, fill = "seashell2", colour = "grey80", size = 0.1) +
  geom_sf(data = marine_parks_amp, aes(fill = zone), colour = NA, show.legend = F,
          linewidth = 0.75, alpha = 0.5) +
  scale_fill_manual(name = "Australian Marine Parks",
                      values = with(marine_parks_amp, setNames(colour, zone))) +
  new_scale_fill() +
  geom_scatterpie(data = benthos, aes(x = longitude_dd, y = latitude_dd),
                  cols = c("Unconsolidated (soft)", "Black & Octocorals",
                           "Sessile invertebrates", "Sponges", "Consolidated (hard)",
                           "Macroalgae", "Hydroids"),
                  colour = NA, pie_scale = 0.66) +
  # geom_scatterpie(data = benthos, aes(x = longitude_dd, y = latitude_dd),
  #                 cols = c("unconsolidated_soft", "black_octocorals",
  #                          "sessile_invertebrates", "sponges", "consolidated_hard",
  #                          "macroalgae", "hydroids"),
  #                 colour = NA, pie_scale = 0.66) +
  hab_fills +
  labs(x = "Longitude", y = "Latitude") +
  coord_sf(xlim = c(site_limits[1], site_limits[2]), ylim = c(site_limits[3], site_limits[4]), crs = 4326) +
  theme_minimal() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

ggsave(filename = "plots/dampier/habitat/DampierAMP_scatterpies.png",
       height = 6, width = 11, dpi = 300, bg = "white")
