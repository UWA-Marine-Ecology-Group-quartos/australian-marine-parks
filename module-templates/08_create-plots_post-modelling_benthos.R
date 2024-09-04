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

# Load functions
file.sources = list.files(pattern = "*.R", path = "functions/", full.names = T)
sapply(file.sources, source, .GlobalEnv)

dat <- readRDS(paste0("output/model-output/geographe/habitat/", name, "_predicted-habitat.rds"))

pred_class <- as.data.frame(dat, xy = T) %>%
  glimpse()

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
marine_parks <- st_read("data/south-west network/spatial/shapefiles/western-australia_marine-parks-all.shp") %>%
  dplyr::filter(name %in% c("Ngari Capes", "Geographe", "South-west Corner")) %>%
  glimpse()
plot(marine_parks["zone"])

marine_parks_amp <- marine_parks %>%
  dplyr::filter(epbc %in% "Commonwealth")
marine_parks_state <- marine_parks %>%
  dplyr::filter(epbc %in% "State")

# Normalise the inverse of standard error
pred_plot <- pred_class %>%
  dplyr::mutate(p_sand.alpha     = 1 - (p_sand.se.fit - min(p_sand.se.fit, na.rm = T))/(max(p_sand.se.fit, na.rm = T) - min(p_sand.se.fit, na.rm = T)),
                # p_rock.alpha     = 1 - (p_rock.se.fit - min(p_rock.se.fit))/(max(p_rock.se.fit) - min(p_rock.se.fit)),
                p_macro.alpha    = 1 - (p_macro.se.fit - min(p_macro.se.fit, na.rm = T))/(max(p_macro.se.fit, na.rm = T) - min(p_macro.se.fit, na.rm = T)),
                p_seagrass.alpha = 1 - (p_seagrass.se.fit - min(p_seagrass.se.fit, na.rm = T))/(max(p_seagrass.se.fit, na.rm = T) - min(p_seagrass.se.fit, na.rm = T)),
                p_inverts.alpha  = 1 - (p_inverts.se.fit - min(p_inverts.se.fit, na.rm = T))/(max(p_inverts.se.fit, na.rm = T) - min(p_inverts.se.fit, na.rm = T))) %>%
  glimpse()
summary(pred_plot)

prediction_limits = c(115.0539, 115.5539, -33.64861, -33.35361)

dominantbenthos_plot(prediction_limits) +
  theme(legend.position = "bottom",
        legend.direction = "horizontal",
        legend.box = "horizontal",
        legend.box.just = "left",
        legend.text = element_text(size = 5),
        legend.title = element_text(size = 7),
        legend.key.size = unit(0.5, "cm"),
        legend.margin = margin(t = -0.1, unit = "cm"))

ggsave(filename = paste0("plots/geographe/habitat/", name, "_predicted-dominant-habitat.png"),
       height = 6, width = 8, dpi = 600, units = "in", bg = "white")

pred_rast <- subset(dat, str_detect(names(dat), "(?<!se).fit") & # String don't contain "fit" preceded by "se"
                      str_detect(names(dat), "^(?!.*reef).*$")) # Strings don't contain "reef"
names(pred_rast)
names(pred_rast) <- c("Sand", "Macroalgae", "Seagrasses", "Sessile invertebrates")
plot(pred_rast)

individualbenthic_plot(prediction_limits)

ggsave(filename = paste0("plots/geographe/habitat/", name, "_predicted-individual-habitat.png"),
       height = 5.5, width = 8, dpi = 900, units = "in", bg = "white")

# p1 <- ggplot() +
#   geom_tile(data = pred_plot, aes(x = x, y = y, fill = p_inverts.alpha, alpha = p_inverts.fit)) +
#   scale_alpha_continuous(range = c(0, 1), guide = "none", name = "Sessile invertebrates") +
#   scale_fill_gradient(low = "white", high = "deeppink3", name = "Sessile invertebrates", na.value = "transparent") +
#   new_scale_fill() +
#   new_scale("alpha") +
#   geom_tile(data = pred_plot, aes(x = x, y = y, fill = p_sand.alpha, alpha = p_sand.fit)) +
#   scale_alpha_continuous(range = c(0, 1), guide = "none", name = "Sand") +
#   scale_fill_gradient(low = "white", high = "wheat", name = "Sand", na.value = "transparent") +
#   new_scale_fill() +
#   new_scale("alpha") +
#   # geom_tile(data = pred_plot, aes(x = x, y = y, fill = p_rock.alpha, alpha = p_rock.fit)) +
#   # scale_alpha_continuous(range = c(0, 1), guide = "none", name = "Rock") +
#   # scale_fill_gradient(low = "white", high = "grey40", name = "Rock", na.value = "transparent") +
#   # new_scale_fill() +
#   # new_scale("alpha") +
#   geom_tile(data = pred_plot, aes(x = x, y = y, fill = p_macro.alpha, alpha = p_macro.fit)) +
#   scale_alpha_continuous(range = c(0, 1), guide = "none", name = "Macroalgae") +
#   scale_fill_gradient(low = "white", high = "darkorange4", name = "Macroalgae", na.value = "transparent") +
#   new_scale_fill() +
#   new_scale("alpha") +
#   geom_tile(data = pred_plot, aes(x = x, y = y, fill = p_seagrass.alpha, alpha = p_seagrass.fit)) +
#   scale_alpha_continuous(range = c(0, 1), guide = "none", name = "Seagrass") +
#   scale_fill_gradient(low = "white", high = "forestgreen", name = "Seagrass", na.value = "transparent") +
#   new_scale_fill() +
#   new_scale("alpha") +
#   geom_sf(data = ausc, fill = "seashell2", colour = "black", size = 0.2) +
#   labs(x = "", y = "") +
#   geom_sf(data = marine_parks_amp, aes(colour = zone), fill = NA, show.legend = F,
#           linewidth = 0.75) +
#   amp_cols +
#   # geom_sf(data = marine.parks, fill = NA, aes(colour = ZONE_TYPE), show.legend = F) +
#   # scale_colour_manual(values = c("National Park Zone" = "#7bbc63")) +
#   theme_minimal() +
#   theme(legend.position = "bottom",
#         legend.direction = "horizontal",
#         legend.box = "horizontal",
#         legend.box.just = "left",
#         legend.text = element_text(size = 5),
#         legend.title = element_text(size = 7),
#         legend.key.size = unit(0.5, "cm"),
#         legend.margin = margin(t = -0.1, unit = "cm")
#         # text = element_text(size = 6),
#         # legend.box.margin = margin(l = -35)
#         ) +
#   coord_sf(xlim = c(min(pred_class$x), max(pred_class$x)),
#            ylim = c(min(pred_class$y), max(pred_class$y)), crs = 4326)

# Individual habitat

# p2 <- ggplot() +
#   # geom_raster(data = dplyr::filter(ind_class, !habitat %in% "Reef"),
#   #           aes(x, y, fill = Probability)) +
#   geom_spatraster(data = pred_rast) +
#   scale_fill_gradientn(colours = c("#fde725", "#21918c", "#440154"),
#                        na.value = "transparent", name = "Probability") +
#   new_scale_fill() +
#   geom_sf(data = ausc, fill = "seashell2", colour = "black", size = 0.2) +
#   geom_sf(data = marine_parks_amp, aes(colour = zone), fill = NA, show.legend = F,
#           linewidth = 0.75) +
#   amp_cols +
#   coord_sf(xlim = c(min(pred_class$x), max(pred_class$x)),
#            ylim = c(min(pred_class$y), max(pred_class$y)), crs = 4326) +
#   labs(x = NULL, y = NULL, fill = "Probability",                                    # Labels
#        colour = NULL) +
#   theme_minimal() +
#   facet_wrap(~lyr)

# Make temporal plots for habitat types, by ecosystem depth contour
# Shallow (0 - 30 m)
# Mesophotic (30 - 70 m)
preds <- readRDS(paste0("data/geographe/spatial/rasters/",
                        name, "_bathymetry-derivatives.rds"))

shallow <- preds[[1]] %>%
  clamp(upper = 0, lower = -30, values = F)
meso <- preds[[1]] %>%
  clamp(upper = -30, lower = -70, values = F)

dat.shallow <- dat %>%
  terra::mask(shallow)
plot(dat.shallow)
dat.meso <- dat %>%
  terra::mask(meso)
plot(dat.meso)

# This is a bit confusing - same file and will overwrite previous file, but have changed the names to make them match what Tim wants
marine_parks <- st_read("data/south-west network/spatial/shapefiles/western-australia_marine-parks-all.shp") %>%
  CheckEM::clean_names() %>%
  # dplyr::filter(name %in% c("Geographe", "Ngari Capes")) %>%
  dplyr::mutate(zone_new = case_when(
    str_detect(zone, "Other State Marine Park Zone")  ~ "State other zones",
    str_detect(zone, "Habitat Protection Zone") & str_detect(epbc, "State")  ~ "State HPZ",
    str_detect(zone, "Habitat Protection Zone") & str_detect(epbc, "Commonwealth")  ~ "AMP HPZ",
    str_detect(zone, "Sanctuary Zone")  ~ "State SZ (IUCN II)",
    str_detect(zone, "National Park Zone")  ~ "AMP NPZ (IUCN II)",
    str_detect(zone, "Special Purpose Zone") & str_detect(epbc, "State")  ~ "State other zones",
    str_detect(zone, "Special Purpose Zone") & str_detect(epbc, "Commonwealth")  ~ "AMP other zones",
    str_detect(zone, "Multiple Use Zone") & str_detect(epbc, "State") ~ "State other zones",
    str_detect(zone, "Multiple Use Zone") & str_detect(epbc, "Commonwealth") ~ "AMP other zones",
    str_detect(zone, "Recreational Use Zone") & str_detect(epbc, "State") ~ "State other zones",
    str_detect(zone, "Recreational Use Zone") & str_detect(epbc, "Commonwealth") ~ "AMP other zones",
    str_detect(zone, "General Use Zone")  ~ "State other zones",
    str_detect(zone, "Reef Observation Area")  ~ "State other zones"
  )) %>%
  glimpse()
  # dplyr::mutate(zone = case_when(
  #   str_detect(pattern = "Sanctuary", string = zone_type) ~ "NCMP SZ (IUCN II)",
  #   str_detect(pattern = "IUCN II", string = zone_type) ~ "GMP NPZ (IUCN II)",
  #   str_detect(pattern = "National Park", string = zone_type) ~ "GMP NPZ (IUCN II)",
  #   str_detect(pattern = "Recreational|Recreation", string = zone_type) ~ "NCMP other zones",
  #   str_detect(pattern = "Habitat Protection", string = zone_type) ~ "GMP HPZ",
  #   str_detect(pattern = "Special Purpose", string = zone_type) ~ "GMP other zones",
  #   str_detect(pattern = "Multiple Use", string = zone_type) ~ "GMP other zones",
  #   str_detect(pattern = "General", string = zone_type) ~ "NCMP other zones"))

# marine_parks <- st_read("data/south-west network/spatial/shapefiles/Collaborative_Australian_Protected_Areas_Database_(CAPAD)_2022_-_Marine.shp") %>%
#   CheckEM::clean_names() %>%
#   dplyr::filter(name %in% c("Geographe", "Ngari Capes")) %>%
#   dplyr::mutate(zone = case_when(
#     str_detect(pattern = "Sanctuary", string = zone_type) ~ "NCMP SZ (IUCN II)",
#     str_detect(pattern = "IUCN II", string = zone_type) ~ "GMP NPZ (IUCN II)",
#     str_detect(pattern = "National Park", string = zone_type) ~ "GMP NPZ (IUCN II)",
#     str_detect(pattern = "Recreational|Recreation", string = zone_type) ~ "NCMP other zones",
#     str_detect(pattern = "Habitat Protection", string = zone_type) ~ "GMP HPZ",
#     str_detect(pattern = "Special Purpose", string = zone_type) ~ "GMP other zones",
#     str_detect(pattern = "Multiple Use", string = zone_type) ~ "GMP other zones",
#     str_detect(pattern = "General", string = zone_type) ~ "NCMP other zones"))
# unique(marine_parks$zone)
# plot(marine_parks["zone"])

# Function for standard error
# se <- function(x) sd(x, na.rm = T)/sqrt(length(x[!is.na(x)]))

# Spatial standard error for each marine park zone in the data
# Be careful if you have multiple of the same zone type in dataset!
errors.shallow <- terra::extract(dat.shallow, marine_parks) %>%
  dplyr::group_by(ID) %>%
  dplyr::summarise(across(starts_with("p"), se)) %>%
  dplyr::mutate(ID = as.character(ID),
                year = 2014) %>%
  dplyr::rename(seagrass_se = pseagrass.fit, macroalgae_se = pmacroalg.fit,
                rock_se = prock.fit, sand_se = psand.fit, inverts_se = pinverts.fit) %>%
  dplyr::select(ID, year, seagrass_se, macroalgae_se, rock_se, sand_se, inverts_se) %>%
  glimpse()

# Mean metrics for each marine park zone in the data
# Be careful if you have multiple of the same zone type in dataset!
means.shallow <- terra::extract(dat.shallow, marine_parks) %>%
  dplyr::group_by(ID) %>%
  dplyr::summarise(across(starts_with("p"), ~mean(.x, na.rm = T))) %>%
  dplyr::mutate(ID = as.character(ID),
                year = 2014) %>%
  dplyr::rename(seagrass = pseagrass.fit, macroalgae = pmacroalg.fit,
                rock = prock.fit, sand = psand.fit, inverts = pinverts.fit) %>%
  dplyr::select(ID, year, seagrass, macroalgae, rock, sand, inverts) %>%
  glimpse()

# Join the data back to the zone data by ID
park_dat.shallow <- as.data.frame(marine_parks) %>%
  tibble::rownames_to_column() %>%
  dplyr::rename(ID = rowname) %>%
  left_join(errors.shallow) %>%
  left_join(means.shallow) %>%
  dplyr::select(zone, year, seagrass, seagrass_se, macroalgae, macroalgae_se,
                rock, rock_se, sand, sand_se, inverts, inverts_se) %>%
  dplyr::filter(!is.na(seagrass)) %>%
  dplyr::group_by(zone, year) %>%
  summarise(across(everything(), .f = list(mean = mean), na.rm = TRUE)) %>%
  ungroup() %>%
  glimpse()

# plot year by seagrass - plus a line for MPA gazetting time ---
gg_seagrass <- ggplot(data = temporal_dat, aes(x = year, y = seagrass_mean, fill = zone, shape = zone)) +
  geom_errorbar(data = temporal_dat, aes(ymin = seagrass_mean - seagrass_se_mean,
                                         ymax = seagrass_mean + seagrass_se_mean),
                width = 0.8, position = position_dodge(width = 0.6)) +
  geom_point(size = 3, position = position_dodge(width = 0.6),
             stroke = 0.2, color = "black", alpha = 0.8) +
  theme_classic() +
  scale_x_continuous(limits = c(2013, 2024),
                     breaks = c(2013, 2015, 2017, 2019, 2021, 2023)) +
  geom_vline(xintercept = 2018, linetype = "dashed", color = "black", linewidth = 0.5, alpha = 0.5) +
  scale_fill_manual(values = c("GMP other zones" = "#b9e6fb",
                               "GMP HPZ" = "#fff8a3",
                               "GMP NPZ (IUCN II)" = "#7bbc63",
                               "NCMP SZ (IUCN II)" = "#bfd054",
                               "NCMP other zones" = "#bddde1"),
                    name = "Marine Parks") +
  scale_shape_manual(values = c("GMP other zones" = 21,
                                "GMP HPZ" = 21,
                                "GMP NPZ (IUCN II)" = 21,
                                "NCMP SZ (IUCN II)" = 25,
                                "NCMP other zones" = 25),
                     name = "Marine Parks") +
  labs(title = "a)", x = "Year", y = "Seagrass")
gg_seagrass

# plot year by macroalgae - plus a line for MPA gazetting time ---
gg_macroalgae <- ggplot(data = temporal_dat, aes(x = year, y = macroalgae_mean, fill = zone, shape = zone)) +
  geom_errorbar(data = temporal_dat, aes(ymin = macroalgae_mean - macroalgae_se_mean,
                                         ymax = macroalgae_mean + macroalgae_se_mean),
                width = 0.8, position = position_dodge(width = 0.6)) +
  geom_point(size = 3, position = position_dodge(width = 0.6),
             stroke = 0.2, color = "black", alpha = 0.8) +
  theme_classic() +
  scale_x_continuous(limits = c(2013, 2024),
                     breaks = c(2013, 2015, 2017, 2019, 2021, 2023)) +
  geom_vline(xintercept = 2018, linetype = "dashed", color = "black", linewidth = 0.5, alpha = 0.5) +
  scale_fill_manual(values = c("GMP other zones" = "#b9e6fb",
                               "GMP HPZ" = "#fff8a3",
                               "GMP NPZ (IUCN II)" = "#7bbc63",
                               "NCMP SZ (IUCN II)" = "#bfd054",
                               "NCMP other zones" = "#bddde1"),
                    name = "Marine Parks") +
  scale_shape_manual(values = c("GMP other zones" = 21,
                                "GMP HPZ" = 21,
                                "GMP NPZ (IUCN II)" = 21,
                                "NCMP SZ (IUCN II)" = 25,
                                "NCMP other zones" = 25),
                     name = "Marine Parks") +
  labs(title = "b)", x = "Year", y = "Macroalgae")
gg_macroalgae

# plot year by rock - plus a line for MPA gazetting time ---
gg_rock <- ggplot(data = temporal_dat, aes(x = year, y = rock_mean, fill = zone, shape = zone)) +
  geom_errorbar(data = temporal_dat, aes(ymin = rock_mean - rock_se_mean,
                                         ymax = rock_mean + rock_se_mean),
                width = 0.8, position = position_dodge(width = 0.6)) +
  geom_point(size = 3, position = position_dodge(width = 0.6),
             stroke = 0.2, color = "black", alpha = 0.8) +
  theme_classic() +
  scale_x_continuous(limits = c(2013, 2024),
                     breaks = c(2013, 2015, 2017, 2019, 2021, 2023)) +
  geom_vline(xintercept = 2018, linetype = "dashed", color = "black", linewidth = 0.5, alpha = 0.5) +
  scale_fill_manual(values = c("GMP other zones" = "#b9e6fb",
                               "GMP HPZ" = "#fff8a3",
                               "GMP NPZ (IUCN II)" = "#7bbc63",
                               "NCMP SZ (IUCN II)" = "#bfd054",
                               "NCMP other zones" = "#bddde1"),
                    name = "Marine Parks") +
  scale_shape_manual(values = c("GMP other zones" = 21,
                                "GMP HPZ" = 21,
                                "GMP NPZ (IUCN II)" = 21,
                                "NCMP SZ (IUCN II)" = 25,
                                "NCMP other zones" = 25),
                     name = "Marine Parks") +
  labs(title = "c)", x = "Year", y = "Rock")
gg_rock

# plot year by sand - plus a line for MPA gazetting time ---
gg_sand <- ggplot(data = temporal_dat, aes(x = year, y = sand_mean, fill = zone, shape = zone)) +
  geom_errorbar(data = temporal_dat, aes(ymin = sand_mean - sand_se_mean,
                                         ymax = sand_mean + sand_se_mean),
                width = 0.8, position = position_dodge(width = 0.6)) +
  geom_point(size = 3, position = position_dodge(width = 0.6),
             stroke = 0.2, color = "black", alpha = 0.8) +
  theme_classic() +
  scale_x_continuous(limits = c(2013, 2024),
                     breaks = c(2013, 2015, 2017, 2019, 2021, 2023)) +
  geom_vline(xintercept = 2018, linetype = "dashed", color = "black", linewidth = 0.5, alpha = 0.5) +
  scale_fill_manual(values = c("GMP other zones" = "#b9e6fb",
                               "GMP HPZ" = "#fff8a3",
                               "GMP NPZ (IUCN II)" = "#7bbc63",
                               "NCMP SZ (IUCN II)" = "#bfd054",
                               "NCMP other zones" = "#bddde1"),
                    name = "Marine Parks") +
  scale_shape_manual(values = c("GMP other zones" = 21,
                                "GMP HPZ" = 21,
                                "GMP NPZ (IUCN II)" = 21,
                                "NCMP SZ (IUCN II)" = 25,
                                "NCMP other zones" = 25),
                     name = "Marine Parks") +
  labs(title = "d)", x = "Year", y = "Sand")
gg_sand

# plot year by inverts - plus a line for MPA gazetting time ---
gg_inverts <- ggplot(data = temporal_dat, aes(x = year, y = inverts_mean, fill = zone, shape = zone)) +
  geom_errorbar(data = temporal_dat, aes(ymin = inverts_mean - inverts_se_mean,
                                         ymax = inverts_mean + inverts_se_mean),
                width = 0.8, position = position_dodge(width = 0.6)) +
  geom_point(size = 3, position = position_dodge(width = 0.6),
             stroke = 0.2, color = "black", alpha = 0.8) +
  theme_classic() +
  scale_x_continuous(limits = c(2013, 2024),
                     breaks = c(2013, 2015, 2017, 2019, 2021, 2023)) +
  geom_vline(xintercept = 2018, linetype = "dashed", color = "black", linewidth = 0.5, alpha = 0.5) +
  scale_fill_manual(values = c("GMP other zones" = "#b9e6fb",
                               "GMP HPZ" = "#fff8a3",
                               "GMP NPZ (IUCN II)" = "#7bbc63",
                               "NCMP SZ (IUCN II)" = "#bfd054",
                               "NCMP other zones" = "#bddde1"),
                    name = "Marine Parks") +
  scale_shape_manual(values = c("GMP other zones" = 21,
                                "GMP HPZ" = 21,
                                "GMP NPZ (IUCN II)" = 21,
                                "NCMP SZ (IUCN II)" = 25,
                                "NCMP other zones" = 25),
                     name = "Marine Parks") +
  labs(title = "e)", x = "Year", y = "Sessile invertebrates")
gg_inverts

plot_grid <- gg_seagrass / gg_macroalgae / gg_rock / gg_sand / gg_inverts + plot_layout(guides = 'collect') +
  plot_annotation(title = "Shallow (0 - 30 m)")
plot_grid

# Save out plot
ggsave(paste0("plots/habitat/", name, "_shallow-control-plots.png"), plot_grid,
       height = 9, width = 8, dpi = 300, units = "in")


# Spatial standard error for each marine park zone in the data
dat <- rast(pred_class, crs = "epsg:4326")
plot(dat)

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

# Be careful if you have multiple of the same zone type in dataset!
errors.meso <- terra::extract(dat.meso, marine_parks) %>%
  dplyr::group_by(ID) %>%
  dplyr::summarise(across(starts_with("p"), se)) %>%
  dplyr::mutate(ID = as.character(ID),
                year = 2014) %>%
  dplyr::rename(seagrass_se = pseagrass.fit, macroalgae_se = pmacroalg.fit,
                rock_se = prock.fit, sand_se = psand.fit, inverts_se = pinverts.fit) %>%
  dplyr::select(ID, year, seagrass_se, macroalgae_se, rock_se, sand_se, inverts_se) %>%
  glimpse()

# Mean metrics for each marine park zone in the data
# Be careful if you have multiple of the same zone type in dataset!
means.meso <- terra::extract(dat.meso, marine_parks) %>%
  dplyr::group_by(ID) %>%
  dplyr::summarise(across(starts_with("p"), ~mean(.x, na.rm = T))) %>%
  dplyr::mutate(ID = as.character(ID),
                year = 2014) %>%
  dplyr::rename(seagrass = pseagrass.fit, macroalgae = pmacroalg.fit,
                rock = prock.fit, sand = psand.fit, inverts = pinverts.fit) %>%
  dplyr::select(ID, year, seagrass, macroalgae, rock, sand, inverts) %>%
  glimpse()

# Join the data back to the zone data by ID
park_dat.meso <- as.data.frame(marine_parks) %>%
  tibble::rownames_to_column() %>%
  dplyr::rename(ID = rowname) %>%
  left_join(errors.meso) %>%
  left_join(means.meso) %>%
  dplyr::select(zone, year, seagrass, seagrass_se, macroalgae, macroalgae_se,
                rock, rock_se, sand, sand_se, inverts, inverts_se) %>%
  dplyr::filter(!is.na(seagrass)) %>%
  dplyr::group_by(zone, year) %>%
  summarise(across(everything(), .f = list(mean = mean), na.rm = TRUE)) %>%
  ungroup() %>%
  glimpse()


# plot year by seagrass - plus a line for MPA gazetting time ---
gg_seagrass <- ggplot(data = temporal_dat, aes(x = year, y = seagrass_mean, fill = zone, shape = zone)) +
  geom_errorbar(data = temporal_dat, aes(ymin = seagrass_mean - seagrass_se_mean,
                                         ymax = seagrass_mean + seagrass_se_mean),
                width = 0.8, position = position_dodge(width = 0.6)) +
  geom_point(size = 3, position = position_dodge(width = 0.6),
             stroke = 0.2, color = "black", alpha = 0.8) +
  theme_classic() +
  scale_x_continuous(limits = c(2013, 2024),
                     breaks = c(2013, 2015, 2017, 2019, 2021, 2023)) +
  geom_vline(xintercept = 2018, linetype = "dashed", color = "black", linewidth = 0.5, alpha = 0.5) +
  scale_fill_manual(values = c("GMP other zones" = "#b9e6fb",
                               "NCMP SZ (IUCN II)" = "#bfd054",
                               "NCMP other zones" = "#bddde1"),
                    name = "Marine Parks") +
  scale_shape_manual(values = c("GMP other zones" = 21,
                                "NCMP SZ (IUCN II)" = 25,
                                "NCMP other zones" = 25),
                     name = "Marine Parks") +
  labs(title = "a)", x = "Year", y = "Seagrass")
gg_seagrass

# plot year by macroalgae - plus a line for MPA gazetting time ---
gg_macroalgae <- ggplot(data = temporal_dat, aes(x = year, y = macroalgae_mean, fill = zone, shape = zone)) +
  geom_errorbar(data = temporal_dat, aes(ymin = macroalgae_mean - macroalgae_se_mean,
                                         ymax = macroalgae_mean + macroalgae_se_mean),
                width = 0.8, position = position_dodge(width = 0.6)) +
  geom_point(size = 3, position = position_dodge(width = 0.6),
             stroke = 0.2, color = "black", alpha = 0.8) +
  theme_classic() +
  scale_x_continuous(limits = c(2013, 2024),
                     breaks = c(2013, 2015, 2017, 2019, 2021, 2023)) +
  geom_vline(xintercept = 2018, linetype = "dashed", color = "black", linewidth = 0.5, alpha = 0.5) +
  scale_fill_manual(values = c("GMP other zones" = "#b9e6fb",
                               "NCMP SZ (IUCN II)" = "#bfd054",
                               "NCMP other zones" = "#bddde1"),
                    name = "Marine Parks") +
  scale_shape_manual(values = c("GMP other zones" = 21,
                                "NCMP SZ (IUCN II)" = 25,
                                "NCMP other zones" = 25),
                     name = "Marine Parks") +
  labs(title = "b)", x = "Year", y = "Macroalgae")
gg_macroalgae

# plot year by rock - plus a line for MPA gazetting time ---
gg_rock <- ggplot(data = temporal_dat, aes(x = year, y = rock_mean, fill = zone, shape = zone)) +
  geom_errorbar(data = temporal_dat, aes(ymin = rock_mean - rock_se_mean,
                                         ymax = rock_mean + rock_se_mean),
                width = 0.8, position = position_dodge(width = 0.6)) +
  geom_point(size = 3, position = position_dodge(width = 0.6),
             stroke = 0.2, color = "black", alpha = 0.8) +
  theme_classic() +
  scale_x_continuous(limits = c(2013, 2024),
                     breaks = c(2013, 2015, 2017, 2019, 2021, 2023)) +
  geom_vline(xintercept = 2018, linetype = "dashed", color = "black", linewidth = 0.5, alpha = 0.5) +
  scale_fill_manual(values = c("GMP other zones" = "#b9e6fb",
                               "NCMP SZ (IUCN II)" = "#bfd054",
                               "NCMP other zones" = "#bddde1"),
                    name = "Marine Parks") +
  scale_shape_manual(values = c("GMP other zones" = 21,
                                "NCMP SZ (IUCN II)" = 25,
                                "NCMP other zones" = 25),
                     name = "Marine Parks") +
  labs(title = "c)", x = "Year", y = "Rock")
gg_rock

# plot year by sand - plus a line for MPA gazetting time ---
gg_sand <- ggplot(data = temporal_dat, aes(x = year, y = sand_mean, fill = zone, shape = zone)) +
  geom_errorbar(data = temporal_dat, aes(ymin = sand_mean - sand_se_mean,
                                         ymax = sand_mean + sand_se_mean),
                width = 0.8, position = position_dodge(width = 0.6)) +
  geom_point(size = 3, position = position_dodge(width = 0.6),
             stroke = 0.2, color = "black", alpha = 0.8) +
  theme_classic() +
  scale_x_continuous(limits = c(2013, 2024),
                     breaks = c(2013, 2015, 2017, 2019, 2021, 2023)) +
  geom_vline(xintercept = 2018, linetype = "dashed", color = "black", linewidth = 0.5, alpha = 0.5) +
  scale_fill_manual(values = c("GMP other zones" = "#b9e6fb",
                               "NCMP SZ (IUCN II)" = "#bfd054",
                               "NCMP other zones" = "#bddde1"),
                    name = "Marine Parks") +
  scale_shape_manual(values = c("GMP other zones" = 21,
                                "NCMP SZ (IUCN II)" = 25,
                                "NCMP other zones" = 25),
                     name = "Marine Parks") +
  labs(title = "d)", x = "Year", y = "Sand")
gg_sand

# plot year by inverts - plus a line for MPA gazetting time ---
gg_inverts <- ggplot(data = temporal_dat, aes(x = year, y = inverts_mean, fill = zone, shape = zone)) +
  geom_errorbar(data = temporal_dat, aes(ymin = inverts_mean - inverts_se_mean,
                                         ymax = inverts_mean + inverts_se_mean),
                width = 0.8, position = position_dodge(width = 0.6)) +
  geom_point(size = 3, position = position_dodge(width = 0.6),
             stroke = 0.2, color = "black", alpha = 0.8) +
  theme_classic() +
  scale_x_continuous(limits = c(2013, 2024),
                     breaks = c(2013, 2015, 2017, 2019, 2021, 2023)) +
  geom_vline(xintercept = 2018, linetype = "dashed", color = "black", linewidth = 0.5, alpha = 0.5) +
  scale_fill_manual(values = c("GMP other zones" = "#b9e6fb",
                               "NCMP SZ (IUCN II)" = "#bfd054",
                               "NCMP other zones" = "#bddde1"),
                    name = "Marine Parks") +
  scale_shape_manual(values = c("GMP other zones" = 21,
                                "NCMP SZ (IUCN II)" = 25,
                                "NCMP other zones" = 25),
                     name = "Marine Parks") +
  labs(title = "e)", x = "Year", y = "Sessile invertebrates")
gg_inverts

plot_grid <- gg_seagrass / gg_macroalgae / gg_rock / gg_sand / gg_inverts + plot_layout(guides = 'collect') +
  plot_annotation(title = "Mesophotic (30 - 70 m)")
plot_grid

# Save out plot
ggsave(paste0("plots/habitat/", name, "_mesophotic-control-plots.png"), plot_grid,
       height = 9, width = 8, dpi = 300, units = "in")
