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
name <- "DampierAMP"
park <- "dampier"

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

# Read in the data
dat <- readRDS(paste0("output/model-output/", park, "/habitat/", name, "_predicted-habitat.rds"))

# Convert the data to a dataframe for some plotting
pred_class <- as.data.frame(dat, xy = T) %>%
  glimpse()

# Individual habitat class predictions
ind_class <- pred_class %>%
  pivot_longer(cols = starts_with("p"), names_to = "habitat",
               values_to = "Probability") %>%
  dplyr::mutate(habitat = case_when( # This should handle missing habitat classes
    habitat %in% "p_inverts.fit" ~ "Sessile invertebrates",
    habitat %in% "p_macro.fit" ~ "Macroalgae",
    habitat %in% "p_rock.fit" ~ "Rock",
    habitat %in% "p_sand.fit" ~ "Sand",
    habitat %in% "p_seagrass.fit" ~ "Seagrass",
    habitat %in% "p_reef.fit" ~ "Reef",
    habitat %in% "p_black.fit" ~ "Black & Octocorals"
  )) %>%
  dplyr::filter(!is.na(habitat)) %>%
  dplyr::filter(!habitat %in% "Reef") %>%
  glimpse()
unique(ind_class$habitat)

# Set cropping extent - larger than most zoomed out plot
e <- ext(116.7, 117.7,-20.919, -20)

# Load necessary spatial files
# Australian outline and state and commonwealth marine parks
aus    <- st_read("data/south-west network/spatial/shapefiles/aus-shapefile-w-investigator-stokes.shp")
ausc <- st_crop(aus, e)

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

# Normalise the inverse of standard error
pred_plot <- normalise_se(data = pred_class)

# Set the limits for the plot
prediction_limits = c(116.779, 117.544, -20.738, -20.282)

# Create the plot
dominantbenthos_plot(prediction_limits) +
  theme( # Add theme items to sort out the legend
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.box = "horizontal",
    legend.box.just = "left",
    legend.text = element_text(size = 5),
    legend.title = element_text(size = 7),
    legend.key.size = unit(0.5, "cm"),
    legend.margin = margin(t = -0.1, unit = "cm"
    ))
# Save the plot
ggsave(filename = paste0("plots/", park, "/habitat/", name, "_predicted-dominant-habitat.png"),
       height = 6, width = 8, dpi = 600, units = "in", bg = "white")

# Subset the spatraster data to remove reef and standard error
pred_rast <- subset(dat, str_detect(names(dat), "(?<!se).fit") & # String don't contain "fit" preceded by "se"
                      str_detect(names(dat), "^(?!.*reef).*$")) # Strings don't contain "reef"
names(pred_rast)
names(pred_rast) <- c("Sand", "Sessile invertebrates", "Black & Octocorals") # Set the names - make sure this matches the order
pred_rast <- pred_rast[[c(1, 2)]]
plot(pred_rast)

# Create the plot - same x and y limits
individualbenthic_plot(prediction_limits)

# Save the plot
ggsave(filename = paste0("plots/", park, "/habitat/", name, "_predicted-individual-habitat.png"),
       height = 3, width = 8, dpi = 900, units = "in", bg = "white")

# Subset the spatraster data to remove reef and standard error
pred_rast <- subset(dat, str_detect(names(dat), "(?<!se).fit") & # String don't contain "fit" preceded by "se"
                      str_detect(names(dat), "^(?!.*reef).*$")) # Strings don't contain "reef"
names(pred_rast)
names(pred_rast) <- c("Sand", "Sessile invertebrates", "Black & Octocorals") # Set the names - make sure this matches the order
pred_rast <- pred_rast[[c(3)]]
plot(pred_rast)

# Create the plot - same x and y limits
individualbenthic_plot(prediction_limits)

# Save the plot
ggsave(filename = paste0("plots/", park, "/habitat/", name, "_predicted-black-octocorals.png"),
       height = 4, width = 8, dpi = 900, units = "in", bg = "white")

# Create the data (makes a dataframe for each ecosystem depth contour)
controldata_benthos(year = 2023, amp_abbrv = "DMP", state_abbrv = NA)

# Create and save the plot (shallow)
controlplot_benthos(data = park_dat.shallow, amp_abbrv = "DMP", state_abbrv = NA,
                    title = "Shallow (0 - 30 m)")
ggsave(paste0("plots/", park, "/habitat/", name, "_shallow-control-plots.png"),
       height = 6, width = 8, dpi = 300, units = "in")

# Create and save the plot (mesophotic)
controlplot_benthos(data = park_dat.meso, amp_abbrv = "DMP", state_abbrv = NA,
                    title = "Mesophotic (30 - 70 m)")
ggsave(paste0("plots/", park, "/habitat/", name, "_mesophotic-control-plots.png"),
       height = 6, width = 8, dpi = 300, units = "in")
