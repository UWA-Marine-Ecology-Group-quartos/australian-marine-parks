###
# Project: NESP 5.6 Project - North Report
# Data:    CAPAD, cropping polygon for North Australia
# Task:    Creating benthic habitat maps
# Author:  Annika Leunig
# Date:    March 2026
# Outputs: 1. Tidied North network extent aus MPs shapefile
#
###

# Table of contents
#     1. Load data and set up
#     2. Filter CAPAD to just South Australia parks
#     3. Combine shapefiles and save

# ==============================================================================
# 1. LOAD DATA and SETUP
# ==============================================================================
# Clear the environment
rm(list = ls())

# Load libraries
library(CheckEM)
library(sf)
library(tidyverse)

crop <- st_bbox(c(xmin = 109, xmax = 133, ymin = -30, ymax = -10), crs = 4326) %>%
  st_as_sfc()

# ==============================================================================
# 2. Filter CAPAD to just South Australia parks
# ==============================================================================
# Load and filter CAPAD to just SA MPs and format colours
capad <- st_read("data/north-west network/spatial/shapefiles/Collaborative_Australian_Protected_Areas_Database_(CAPAD)_2022_-_Marine.shp") %>%
  CheckEM::clean_names() %>%
  st_make_valid() %>%
  st_crop(crop) %>%
  dplyr::filter(!type %in% "Nature Reserve") %>%
  dplyr::mutate(zone = case_when(
    str_detect(pattern = "Sanctuary", string = zone_type) ~ "Sanctuary Zone",
    str_detect(pattern = "Reef Observation", string = zone_type) ~ "Sanctuary Zone",
    str_detect(pattern = "IUCN II", string = zone_type) ~ "National Park Zone",
    str_detect(pattern = "National Park", string = zone_type) ~ "National Park Zone",
    str_detect(pattern = "Recreational|Recreation", string = zone_type) ~ "Recreational Use Zone",
    str_detect(pattern = "Habitat Protection", string = zone_type) ~ "Habitat Protection Zone",
    str_detect(pattern = "Special Purpose", string = zone_type) ~ "Special Purpose Zone",
    str_detect(pattern = "Multiple Use", string = zone_type) ~ "Multiple Use Zone",
    str_detect(pattern = "General", string = zone_type) ~ "General Use Zone",
    str_detect(pattern = "Fish Habitat Protection Zone", string = type) ~ "General Use Zone",
    str_detect(pattern = "Marine Management Area", string = type) &
      str_detect(pattern = "Ia", string = iucn) ~ "Sanctuary Zone",
    .default = "Other State Marine Park Zone")) %>%
  dplyr::mutate(zone = if_else(zone %in% "Other State Marine Park Zone" & str_detect(zone_type, "IA"), "Sanctuary Zone", zone)) %>%
  dplyr::mutate(colour = case_when(zone %in% "Sanctuary Zone" & epbc %in% "State"~ "#bfd054",
                                   zone %in% "Sanctuary Zone" & epbc %in% "Commonwealth"~ "#f7c0d8",
                                   zone %in% "National Park Zone" ~ "#7bbc63",
                                   zone %in% "Recreational Use Zone" & epbc %in% "State" ~ "#f4e952",
                                   zone %in% "Recreational Use Zone" & epbc %in% "Commonwealth" ~ "#ffb36b",
                                   zone %in% "Habitat Protection Zone"& epbc %in% "State" ~ "#fffbcc",
                                   zone %in% "Habitat Protection Zone"& epbc %in% "Commonwealth" ~ "#fff8a3",
                                   zone %in% "Special Purpose Zone"& epbc %in% "State" ~ "#c5bcc9",
                                   zone %in% "Special Purpose Zone"& epbc %in% "Commonwealth" ~ "#6daff4",
                                   zone %in% "Multiple Use Zone" ~ "#b9e6fb",
                                   zone %in% "General Use Zone" ~ "#bddde1",
                                   zone %in% "Other State Marine Park Zone" ~ "gray80")) %>%
  dplyr::select(name, zone_type, zone, epbc, colour, geometry) %>%
  glimpse()


# Save north shapefile
st_write(capad, "data/north-west network/spatial/shapefiles/north-west-network-australia_marine-parks-all.shp", append = F)

plot(capad) # check
# ==============================================================================
# End of script
# ==============================================================================
