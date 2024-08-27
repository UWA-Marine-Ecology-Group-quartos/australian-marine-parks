###
# Project: NESP 4.20 - Marine Park Dashboard reporting
# Data:    Fish data synthesis & habitat models derived from FSSgam
# Task:    Create post-modelling fish figures for marine park reporting
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
library(ggplot2)
library(ggnewscale)
library(scales)
library(viridis)
library(patchwork)
library(tidyterra)
library(png)

dat <- readRDS(paste0("output/model-output/geographe/fish/",
                      name, "_predicted-fish.RDS")) %>%
  rast(crs = "epsg:4326")
plot(dat)

# Set cropping extent - larger than most zoomed out plot
e <- ext(114.2, 115.8,-34.7, -33.1)

# Load necessary spatial files
sf_use_s2(F)                                                                    # Switch off spatial geometry for cropping
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

# Spatial predictions
gg_mature <- ggplot() +
  geom_spatraster(data = dat,
                  aes(fill = p_mature.fit)) +
  scale_fill_gradientn(colours = c("#fde725", "#21918c", "#440154"),
                       na.value = "transparent") +
  labs(fill = "> Lm", x = NULL, y = NULL, title = "Large bodied carnivores") +
  new_scale_fill() +
  geom_sf(data = marine_parks_state, aes(colour = zone), fill = NA,
          linewidth = 0.7, show.legend = F) +
  state_cols +
  new_scale_colour() +
  geom_sf(data = ausc, fill = "seashell2", colour = "black", size = 0.2) +
  geom_sf(data = marine_parks_amp, aes(colour = zone), fill = NA,
          linewidth = 0.7, show.legend = F) +
  amp_cols +
  coord_sf(xlim = c(ext(dat)[1], ext(dat)[2]),
           ylim = c(ext(dat)[3], ext(dat)[4]),
           crs = 4326) +
  theme_minimal()

gg_cti <- ggplot() +
  geom_spatraster(data = dat,
                  aes(fill = p_cti.fit)) +
  scale_fill_gradientn(colours = c("#fde725", "#21918c", "#440154"),
                       na.value = "transparent") +
  labs(fill = "CTI", x = NULL, y = NULL) +
  new_scale_fill() +
  geom_sf(data = marine_parks_state, aes(colour = zone), fill = NA,
          linewidth = 0.7, show.legend = F) +
  state_cols +
  new_scale_colour() +
  geom_sf(data = ausc, fill = "seashell2", colour = "black", size = 0.2) +
  geom_sf(data = marine_parks_amp, aes(colour = zone), fill = NA,
          linewidth = 0.7, show.legend = F) +
  amp_cols +
  coord_sf(xlim = c(ext(dat)[1], ext(dat)[2]),
           ylim = c(ext(dat)[3], ext(dat)[4]),
           crs = 4326) +
  theme_minimal()

gg_richness <- ggplot() +
  geom_spatraster(data = dat,
                  aes(fill = p_richness.fit)) +
  scale_fill_gradientn(colours = c("#fde725", "#21918c", "#440154"),
                       na.value = "transparent") +
  labs(fill = "Species \nrichness", x = NULL, y = NULL, title = "Whole assemblage") +
  new_scale_fill() +
  geom_sf(data = marine_parks_state, aes(colour = zone), fill = NA,
          linewidth = 0.7, show.legend = F) +
  state_cols +
  new_scale_colour() +
  geom_sf(data = ausc, fill = "seashell2", colour = "black", size = 0.2) +
  geom_sf(data = marine_parks_amp, aes(colour = zone), fill = NA,
          linewidth = 0.7, show.legend = F) +
  amp_cols +
  coord_sf(xlim = c(ext(dat)[1], ext(dat)[2]),
           ylim = c(ext(dat)[3], ext(dat)[4]),
           crs = 4326) +
  theme_minimal()

gg_pinkies <- ggplot() +
  geom_spatraster(data = dat,
                  aes(fill = p_pinkies.fit)) +
  scale_fill_gradientn(colours = c("#fde725", "#21918c", "#440154"),
                       na.value = "transparent") +
  labs(fill = "< Lm", x = NULL, y = NULL, title = expression("Pink snapper"~italic("(Chrysophrys auratus)"))) +
  new_scale_fill() +
  geom_sf(data = marine_parks_state, aes(colour = zone), fill = NA,
          linewidth = 0.7, show.legend = F) +
  state_cols +
  new_scale_colour() +
  geom_sf(data = ausc, fill = "seashell2", colour = "black", size = 0.2) +
  geom_sf(data = marine_parks_amp, aes(colour = zone), fill = NA,
          linewidth = 0.7, show.legend = F) +
  amp_cols +
  coord_sf(xlim = c(ext(dat)[1], ext(dat)[2]),
           ylim = c(ext(dat)[3], ext(dat)[4]),
           crs = 4326) +
  theme_minimal()

gg_grid <- gg_richness + gg_cti + gg_mature + gg_pinkies +
  plot_layout(ncol = 2, nrow = 2) &
  theme(legend.justification = "left")

png(filename = paste(paste("plots/geographe/fish", name, sep = "/"),
                     "fish-individual_predictions.png", sep = "_"),
    width = 9, height = 5, res = 300, units = "in")                             # Change the dimensions here as necessary
gg_grid
dev.off()

# Temporal predictions
# NEED TO ADD IN SST TO CTI PLOT
preds <- readRDS(paste0("data/geographe/spatial/rasters/", name, "_bathymetry-derivatives.rds")) %>%
  crop(dat)
shallow <- preds[[1]] %>%
  clamp(upper = 0, lower = -30, values = F)
meso <- preds[[1]] %>%
  clamp(upper = -30, lower = -70, values = F)

dat.shallow <- dat %>%
  terra::mask(shallow)
plot(dat.shallow)
dat.meso <- dat %>%
  terra::mask(meso)

# Function for standard error
se <- function(x) sd(x, na.rm = T)/sqrt(length(x[!is.na(x)]))

# Spatial standard error for each marine park zone in the data
# Be careful if you have multiple of the same zone type in dataset!
errors <- terra::extract(dat.shallow, marine_parks_amp) %>%
  dplyr::group_by(ID) %>%
  dplyr::summarise(across(starts_with("p"), se)) %>%
  dplyr::mutate(ID = as.character(ID),
                year = 2014) %>%
  dplyr::rename(cti_se = p_cti.fit, richness_se = p_richness.fit,
                Lm_se = p_mature.fit) %>%
  dplyr::select(ID, year, cti_se, richness_se, Lm_se) %>%
  glimpse()

# Mean metrics for each marine park zone in the data
# Be careful if you have multiple of the same zone type in dataset!
means <- terra::extract(dat.shallow, marine_parks_amp) %>%
  dplyr::group_by(ID) %>%
  dplyr::summarise(across(starts_with("p"), ~mean(.x, na.rm = T))) %>%
  dplyr::mutate(ID = as.character(ID),
                year = 2014) %>%
  dplyr::rename(cti = p_cti.fit, richness = p_richness.fit,
                Lm= p_mature.fit) %>%
  dplyr::select(ID, year, cti, richness, Lm) %>%
  glimpse()

# Join the data back to the zone data by ID
park_dat_shallow <- as.data.frame(marine_parks_amp) %>%
  tibble::rownames_to_column() %>%
  dplyr::rename(ID = rowname) %>%
  left_join(errors) %>%
  left_join(means) %>%
  dplyr::select(zone, year, cti, cti_se, richness, richness_se,
                Lm, Lm_se) %>%
  dplyr::filter(!is.na(Lm)) %>%
  glimpse()

sst <- readRDS(paste0("data/geographe/spatial/oceanography/",
                      name, "_SST_time-series.rds")) %>%
  dplyr::mutate(year = as.numeric(year)) %>%
  group_by(year) %>%
  summarise(sst = mean(sst, na.rm = T), sd = mean(sd, na.rm = T)) %>%
  glimpse()

# plot year by species richness - plus a line for MPA gazetting time ---
gg_sr <- ggplot(data = park_dat_shallow, aes(x = year, y = richness, fill = zone)) +
  geom_errorbar(data = park_dat_shallow, aes(ymin = richness - richness_se,
                                          ymax = richness + richness_se),
                width = 0.8, position = position_dodge(width = 0.6)) +
  geom_point(size = 3, position = position_dodge(width = 0.6),
             stroke = 0.2, color = "black", alpha = 0.8, shape = 21) +
  theme_classic() +
  scale_x_continuous(limits = c(2013, 2024),
                     breaks = c(2013, 2015, 2017, 2019, 2021, 2023)) +
  geom_vline(xintercept = 2018, linetype = "dashed", color = "black", linewidth = 0.5, alpha = 0.5) +
  scale_fill_manual(values = c("Multiple Use Zone" = "#b9e6fb",
                               "Habitat Protection Zone" = "#fff8a3",
                               "National Park Zone" = "#7bbc63",
                               "Special Purpose Zone" = "#6daff4"),
                    name = "Australian Marine Parks") +
  labs(title = "a)", x = "Year", y = "Species richness")
gg_sr

# Greater than Lm carnivores
gg_lm <- ggplot(data = park_dat_shallow,
                   aes(x = year, y = Lm, fill = zone))+
  geom_errorbar(data = park_dat_shallow,
                aes(ymin = Lm - Lm_se, ymax= Lm + Lm_se),
                width = 0.8, position = position_dodge(width = 0.6))+
  geom_point(size = 3, position = position_dodge(width = 0.6),
             stroke = 0.2, color = "black", alpha = 0.8, shape = 21)+
  theme_classic() +
  scale_x_continuous(limits = c(2013, 2024),
                     breaks = c(2013, 2015, 2017, 2019, 2021, 2023)) +
  geom_vline(xintercept = 2018, linetype = "dashed",color = "black",
             linewidth = 0.5,alpha = 0.5)+
  scale_fill_manual(values = c("Multiple Use Zone" = "#b9e6fb",
                               "Habitat Protection Zone" = "#fff8a3",
                               "National Park Zone" = "#7bbc63",
                               "Special Purpose Zone" = "#6daff4"),
                    name = "Australian Marine Parks") +
  labs(title = "b)", x = "Year", y = ">Lm large bodied carnivores")
gg_lm

# plot year by community thermal index - plus a line for MPA gazetting time ---

gg_cti <- ggplot() +

  # SST needs turning back on after it is added to temporal_dat

  geom_line(data = sst, aes(x = year, y = sst))+
  geom_ribbon(data = sst, aes(x = year, y = sst,
                                       ymin = sst - sd, ymax = sst + sd),
              alpha = 0.2) +
  geom_errorbar(data = park_dat_shallow, aes(x = year, y = cti, ymin = cti - cti_se,
                                          ymax = cti + cti_se, fill = zone), # This has a warning but it plots wrong if you remove fill
                width = 0.8, position = position_dodge(width = 0.6))+
  geom_point(data = park_dat_shallow, aes(x = year, y = cti, fill = zone),size = 3,
             stroke = 0.2, color = "black", position = position_dodge(width = 0.6),
             alpha = 0.8, shape = 21)+
  theme_classic() +
  # scale_y_continuous(limits = c(0, 8)) +
  scale_x_continuous(limits = c(2013, 2024),
                     breaks = c(2013, 2015, 2017, 2019, 2021, 2023)) +
  geom_vline(xintercept = 2018, linetype = "dashed", color = "black",
             size = 0.5, alpha = 0.5) +
  scale_fill_manual(values = c("Multiple Use Zone" = "#b9e6fb",
                               "Habitat Protection Zone" = "#fff8a3",
                               "National Park Zone" = "#7bbc63",
                               "Special Purpose Zone" = "#6daff4"),
                    name = "Australian Marine Parks") +
  labs(title = "c)", x = "Year", y = "Community Temperature Index")
gg_cti


plot_grid <- gg_sr / gg_lm / gg_cti + plot_layout(guides = 'collect') +
  plot_annotation(title = 'Shallow (0 - 30 m)')
plot_grid

# Save out plot
ggsave(paste0("plots/geographe/fish/", name, "_shallow_control-plots.png"), plot_grid,
       height = 7, width = 8, dpi = 300, units = "in")

# Mesophotic (30 - 70 m)
errors <- terra::extract(dat.meso, marine_parks_amp) %>%
  dplyr::group_by(ID) %>%
  dplyr::summarise(across(starts_with("p"), se)) %>%
  dplyr::mutate(ID = as.character(ID),
                year = 2014) %>%
  dplyr::rename(cti_se = p_cti.fit, richness_se = p_richness.fit,
                Lm_se = p_mature.fit) %>%
  dplyr::select(ID, year, cti_se, richness_se, Lm_se) %>%
  glimpse()

# Mean metrics for each marine park zone in the data
# Be careful if you have multiple of the same zone type in dataset!
means <- terra::extract(dat.meso, marine_parks_amp) %>%
  dplyr::group_by(ID) %>%
  dplyr::summarise(across(starts_with("p"), ~mean(.x, na.rm = T))) %>%
  dplyr::mutate(ID = as.character(ID),
                year = 2014) %>%
  dplyr::rename(cti = p_cti.fit, richness = p_richness.fit,
                Lm= p_mature.fit) %>%
  dplyr::select(ID, year, cti, richness, Lm) %>%
  glimpse()

# Join the data back to the zone data by ID
park_dat_meso <- as.data.frame(marine_parks_amp) %>%
  tibble::rownames_to_column() %>%
  dplyr::rename(ID = rowname) %>%
  left_join(errors) %>%
  left_join(means) %>%
  dplyr::select(zone, year, cti, cti_se, richness, richness_se,
                Lm, Lm_se) %>%
  dplyr::filter(!is.na(Lm)) %>%
  glimpse()

# plot year by species richness - plus a line for MPA gazetting time ---
gg_sr <- ggplot(data = park_dat_meso, aes(x = year, y = richness, fill = zone)) +
  geom_errorbar(data = park_dat_meso, aes(ymin = richness - richness_se,
                                             ymax = richness + richness_se),
                width = 0.8, position = position_dodge(width = 0.6)) +
  geom_point(size = 3, position = position_dodge(width = 0.6),
             stroke = 0.2, color = "black", alpha = 0.8, shape = 21) +
  theme_classic() +
  scale_x_continuous(limits = c(2013, 2024),
                     breaks = c(2013, 2015, 2017, 2019, 2021, 2023)) +
  geom_vline(xintercept = 2018, linetype = "dashed", color = "black", linewidth = 0.5, alpha = 0.5) +
  scale_fill_manual(values = c("Multiple Use Zone" = "#b9e6fb",
                               "Habitat Protection Zone" = "#fff8a3",
                               "National Park Zone" = "#7bbc63",
                               "Special Purpose Zone" = "#6daff4"),
                    name = "Australian Marine Parks") +
  labs(title = "a)", x = "Year", y = "Species richness")
gg_sr

# Greater than Lm carnivores
gg_lm <- ggplot(data = park_dat_meso,
                aes(x = year, y = Lm, fill = zone))+
  geom_errorbar(data = park_dat_meso,
                aes(ymin = Lm - Lm_se, ymax= Lm + Lm_se),
                width = 0.8, position = position_dodge(width = 0.6))+
  geom_point(size = 3, position = position_dodge(width = 0.6),
             stroke = 0.2, color = "black", alpha = 0.8, shape = 21)+
  theme_classic() +
  scale_x_continuous(limits = c(2013, 2024),
                     breaks = c(2013, 2015, 2017, 2019, 2021, 2023)) +
  geom_vline(xintercept = 2018, linetype = "dashed",color = "black",
             linewidth = 0.5,alpha = 0.5)+
  scale_fill_manual(values = c("Multiple Use Zone" = "#b9e6fb",
                               "Habitat Protection Zone" = "#fff8a3",
                               "National Park Zone" = "#7bbc63",
                               "Special Purpose Zone" = "#6daff4"),
                    name = "Australian Marine Parks") +
  labs(title = "b)", x = "Year", y = ">Lm large bodied carnivores")
gg_lm

# plot year by community thermal index - plus a line for MPA gazetting time ---

gg_cti <- ggplot() +
  geom_line(data = sst, aes(x = year, y = sst))+
  geom_ribbon(data = sst, aes(x = year, y = sst,
                              ymin = sst - sd, ymax = sst + sd),
              alpha = 0.2) +
  geom_errorbar(data = park_dat_meso, aes(x = year, y = cti, ymin = cti - cti_se,
                                             ymax = cti + cti_se, fill = zone), # This has a warning but it plots wrong if you remove fill
                width = 0.8, position = position_dodge(width = 0.6))+
  geom_point(data = park_dat_meso, aes(x = year, y = cti, fill = zone),size = 3,
             stroke = 0.2, color = "black", position = position_dodge(width = 0.6),
             alpha = 0.8, shape = 21)+
  theme_classic() +
  # scale_y_continuous(limits = c(0, 8)) +
  scale_x_continuous(limits = c(2013, 2024),
                     breaks = c(2013, 2015, 2017, 2019, 2021, 2023)) +
  geom_vline(xintercept = 2018, linetype = "dashed", color = "black",
             size = 0.5, alpha = 0.5) +
  scale_fill_manual(values = c("Multiple Use Zone" = "#b9e6fb",
                               "Habitat Protection Zone" = "#fff8a3",
                               "National Park Zone" = "#7bbc63",
                               "Special Purpose Zone" = "#6daff4"),
                    name = "Australian Marine Parks") +
  labs(title = "c)", x = "Year", y = "Community Temperature Index")
gg_cti


plot_grid <- gg_sr / gg_lm / gg_cti + plot_layout(guides = 'collect') +
  plot_annotation(title = 'Mesophotic (30 - 70 m)')
plot_grid

# Save out plot
ggsave(paste0("plots/geographe/fish/", name, "_mesophotic_control-plots.png"), plot_grid,
       height = 7, width = 8, dpi = 300, units = "in")
