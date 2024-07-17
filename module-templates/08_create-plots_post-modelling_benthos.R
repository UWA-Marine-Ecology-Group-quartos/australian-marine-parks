###
# Project: NESP 4.20 - Marine Park Dashboard reporting
# Data:    Habitat data synthesis & habitat models derived from FSSgam
# Task:    Create post-modelling habitat figures for marine park reporting
# Author:  Claude Spencer
# Date:    June 2024
###

# Clear your environment
rm(list = ls())

# Set the study name
name <- "GeographeAMP"

# Load libraries
library(tidyverse)
library(terra)
library(sf)
library(ggnewscale)
library(scales)
library(tidyterra)
library(patchwork)

pred_class <- readRDS(paste0("output/model-output/geographe/habitat/", name, "_predicted-habitat.rds")) %>%
  dplyr::mutate(dom_tag = case_when(dom_tag %in% "p_sand.fit" ~ "Sand",
                                    dom_tag %in% "p_inverts.fit" ~ "Sessile invertebrates",
                                    dom_tag %in% "p_rock.fit" ~ "Rock",
                                    dom_tag %in% "p_macro.fit" ~ "Macroalgae",
                                    dom_tag %in% "p_seagrass.fit" ~ "Seagrass")) %>%
  glimpse()

# Assign habitat class colours
unique(pred_class$dom_tag)
hab_fills <- scale_fill_manual(values = c(
  "Sand" = "wheat",
  "Sessile invertebrates" = "plum",
  # "Rock" = "grey40",
  "Macroalgae" = "darkgoldenrod4",
  "Seagrass" = "forestgreen"), name = "Habitat")

# Individual habitat class predictions
ind_class <- pred_class %>%
  pivot_longer(cols = starts_with("p"), names_to = "habitat",
               values_to = "Probability") %>%
  dplyr::mutate(habitat = case_when(habitat %in% "p_inverts.fit" ~ "Sessile invertebrates",
                                    habitat %in% "p_macro.fit" ~ "Macroalgae",
                                    habitat %in% "p_rock.fit" ~ "Rock",
                                    habitat %in% "p_sand.fit" ~ "Sand",
                                    habitat %in% "p_seagrass.fit" ~ "Seagrass",
                                    habitat %in% "p_reef.fit" ~ "Reef")) %>%
  dplyr::filter(!is.na(habitat)) %>%
  dplyr::filter(!habitat %in% "Reef") %>%
  glimpse()
unique(ind_class$habitat)

# Set cropping extent - larger than most zoomed out plot
e <- ext(114.2, 115.8,-34.7, -33.1)

# Load necessary spatial files
# Australian outline and state and commonwealth marine parks
aus    <- st_read("data/south-west network/spatial/shapefiles/aus-shapefile-w-investigator-stokes.shp")
ausc <- st_crop(aus, e)

# Australian outline and state and commonwealth marine parks
marine_parks <- st_read("data/south-west network/spatial/shapefiles/Collaborative_Australian_Protected_Areas_Database_(CAPAD)_2022_-_Marine.shp") %>%
  CheckEM::clean_names() %>%
  dplyr::mutate(zone = case_when(
    str_detect(pattern = "Sanctuary", string = zone_type) ~ "Sanctuary Zone",
    str_detect(pattern = "IUCN II", string = zone_type) ~ "National Park Zone",
    str_detect(pattern = "National Park", string = zone_type) ~ "National Park Zone",
    str_detect(pattern = "Recreational|Recreation", string = zone_type) ~ "Recreational Use Zone",
    str_detect(pattern = "Habitat Protection", string = zone_type) ~ "Habitat Protection Zone",
    str_detect(pattern = "Special Purpose", string = zone_type) ~ "Special Purpose Zone",
    str_detect(pattern = "Multiple Use", string = zone_type) ~ "Multiple Use Zone",
    str_detect(pattern = "General", string = zone_type) ~ "General Use Zone")) %>%
  st_crop(e)
plot(marine_parks["zone"])

marine_parks_amp <- marine_parks %>%
  dplyr::filter(type %in% "Australian Marine Park")
marine_parks_state <- marine_parks %>%
  dplyr::filter(type %in% "Marine Park")

amp_cols <- scale_colour_manual(values = c("National Park Zone" = "#7bbc63",
                                           "Habitat Protection Zone" = "#fff8a3",
                                           "Multiple Use Zone" = "#b9e6fb",
                                           "Recreational Use Zone" = "#ffb36b",
                                           "Sanctuary Zone" = "#f7c0d8",
                                           "Special Purpose Zone" = "#6daff4"),
                                name = "Australian Marine Parks")

state_cols <- scale_colour_manual(values = c("Sanctuary Zone" = "#bfd054",
                                             # "Habitat Protection Zone" = "#fffbcc",
                                             "General Use Zone" = "#bddde1",
                                             "Recreational Use Zone" = "#f4e952",
                                             "Special Purpose Zone" = "#c5bcc9"),
                                  name = "State Marine Parks")

# Normalise the inverse of standard error
pred_plot <- pred_class %>%
  dplyr::mutate(p_sand.alpha     = 1 - (p_sand.se.fit - min(p_sand.se.fit, na.rm = T))/(max(p_sand.se.fit, na.rm = T) - min(p_sand.se.fit, na.rm = T)),
                # p_rock.alpha     = 1 - (p_rock.se.fit - min(p_rock.se.fit))/(max(p_rock.se.fit) - min(p_rock.se.fit)),
                p_macro.alpha    = 1 - (p_macro.se.fit - min(p_macro.se.fit, na.rm = T))/(max(p_macro.se.fit, na.rm = T) - min(p_macro.se.fit, na.rm = T)),
                p_seagrass.alpha = 1 - (p_seagrass.se.fit - min(p_seagrass.se.fit, na.rm = T))/(max(p_seagrass.se.fit, na.rm = T) - min(p_seagrass.se.fit, na.rm = T)),
                p_inverts.alpha  = 1 - (p_inverts.se.fit - min(p_inverts.se.fit, na.rm = T))/(max(p_inverts.se.fit, na.rm = T) - min(p_inverts.se.fit, na.rm = T))) %>%
  glimpse()
summary(pred_plot)

p1 <- ggplot() +
  geom_tile(data = pred_plot, aes(x = x, y = y, fill = p_inverts.alpha, alpha = p_inverts.fit)) +
  scale_alpha_continuous(range = c(0, 1), guide = "none", name = "Sessile invertebrates") +
  scale_fill_gradient(low = "white", high = "deeppink3", name = "Sessile invertebrates", na.value = "transparent") +
  new_scale_fill() +
  new_scale("alpha") +
  geom_tile(data = pred_plot, aes(x = x, y = y, fill = p_sand.alpha, alpha = p_sand.fit)) +
  scale_alpha_continuous(range = c(0, 1), guide = "none", name = "Sand") +
  scale_fill_gradient(low = "white", high = "wheat", name = "Sand", na.value = "transparent") +
  new_scale_fill() +
  new_scale("alpha") +
  # geom_tile(data = pred_plot, aes(x = x, y = y, fill = p_rock.alpha, alpha = p_rock.fit)) +
  # scale_alpha_continuous(range = c(0, 1), guide = "none", name = "Rock") +
  # scale_fill_gradient(low = "white", high = "grey40", name = "Rock", na.value = "transparent") +
  # new_scale_fill() +
  # new_scale("alpha") +
  geom_tile(data = pred_plot, aes(x = x, y = y, fill = p_macro.alpha, alpha = p_macro.fit)) +
  scale_alpha_continuous(range = c(0, 1), guide = "none", name = "Macroalgae") +
  scale_fill_gradient(low = "white", high = "darkorange4", name = "Macroalgae", na.value = "transparent") +
  new_scale_fill() +
  new_scale("alpha") +
  geom_tile(data = pred_plot, aes(x = x, y = y, fill = p_seagrass.alpha, alpha = p_seagrass.fit)) +
  scale_alpha_continuous(range = c(0, 1), guide = "none", name = "Seagrass") +
  scale_fill_gradient(low = "white", high = "forestgreen", name = "Seagrass", na.value = "transparent") +
  new_scale_fill() +
  new_scale("alpha") +
  geom_sf(data = ausc, fill = "seashell2", colour = "black", size = 0.2) +
  labs(x = "", y = "") +
  geom_sf(data = marine_parks_amp, aes(colour = zone), fill = NA, show.legend = F,
          linewidth = 0.75) +
  amp_cols +
  # geom_sf(data = marine.parks, fill = NA, aes(colour = ZONE_TYPE), show.legend = F) +
  # scale_colour_manual(values = c("National Park Zone" = "#7bbc63")) +
  theme_minimal() +
  theme(legend.position = "bottom",
        legend.direction = "horizontal",
        legend.box = "horizontal",
        legend.box.just = "left",
        legend.text = element_text(size = 5),
        legend.title = element_text(size = 7),
        legend.key.size = unit(0.5, "cm"),
        legend.margin = margin(t = -0.1, unit = "cm")
        # text = element_text(size = 6),
        # legend.box.margin = margin(l = -35)
        ) +
  coord_sf(xlim = c(min(pred_class$x), max(pred_class$x)),
           ylim = c(min(pred_class$y), max(pred_class$y)), crs = 4326)
ggsave(filename = paste0("plots/geographe/habitat/", name, "_predicted-dominant-habitat.png"),
       plot = p1, height = 6, width = 8, dpi = 600, units = "in", bg = "white")

# Individual habitat

pred_rast <- rast(pred_class %>% dplyr::select(x, y, p_inverts.fit, p_seagrass.fit, p_macro.fit, p_seagrass.fit, p_sand.fit),
             crs = "epsg:4326")
names(pred_rast) <- c("Sessile invertebrates", "Seagrass", "Macroalgae", "Sand")
plot(pred_rast)

p2 <- ggplot() +
  # geom_raster(data = dplyr::filter(ind_class, !habitat %in% "Reef"),
  #           aes(x, y, fill = Probability)) +
  geom_spatraster(data = pred_rast) +
  scale_fill_gradientn(colours = c("#fde725", "#21918c", "#440154"),
                       na.value = "transparent", name = "Probability") +
  new_scale_fill() +
  geom_sf(data = ausc, fill = "seashell2", colour = "black", size = 0.2) +
  geom_sf(data = marine_parks_amp, aes(colour = zone), fill = NA, show.legend = F,
          linewidth = 0.75) +
  amp_cols +
  coord_sf(xlim = c(min(pred_class$x), max(pred_class$x)),
           ylim = c(min(pred_class$y), max(pred_class$y)), crs = 4326) +
  labs(x = NULL, y = NULL, fill = "Probability",                                    # Labels
       colour = NULL) +
  theme_minimal() +
  facet_wrap(~lyr)
ggsave(filename = paste0("plots/geographe/habitat/", name, "_predicted-individual-habitat.png"),
         plot = p2, height = 5.5, width = 8, dpi = 900, units = "in", bg = "white")
