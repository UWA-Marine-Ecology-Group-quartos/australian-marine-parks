###
# Project: North Network Report
# Data:    CAPAD
# Task:    Creating North MPs shapefile
# Author:  Annika Leunig
# Date:    March 2026
# Outputs: 1. Tidied North network extent aus MPs shapefile
#
###

# Table of contents
#     1. Load data and set up
#     2. Filter CAPAD to just North Australia parks
#     3. Save new shapefile

###
# NOTE: This script is not needed unless cleaning up large CAPAD datasets (i.e swc)
###

# ==============================================================================
# 1. LOAD DATA and SETUP
# ==============================================================================
# Clear the environment
rm(list = ls())

# Load libraries
library(CheckEM)
library(sf)
library(tidyverse)

north_network_names <- c("Arafura", "Arnhem", "Gulf of Carpentaria", "Joseph Bonaparte Gulf",
                         "Limmen", "Oceanic Shoals", "Wessel", "West Cape York", "North Kimberley",
                         "Garig Gunak Barlu", "Limmen Bight", "Eight Mile Creek", "Morning Inlet - Bynoe River",
                         "Staaten-Gilbert", "Nassau River", "Pine River Bay",
                         "Dhimurru", "Thuwathu/Bujimulla", "Anindilyakwa", "Djelk - Stage 2",
                         "Crocodile Islands Maringa")

# ==============================================================================
# 2. Filter CAPAD to just North Australia parks
# ==============================================================================
# Load and filter CAPAD to just north network MPs (incl. IPAs) and format colours
capad <- st_read("data/north network/spatial/shapefiles/Collaborative_Australian_Protected_Areas_Database_(CAPAD)_2022_-_Marine.shp") %>%
  CheckEM::clean_names() %>%
  st_make_valid() %>%
  dplyr::filter(name %in% north_network_names) %>%
  dplyr::filter(!type %in% "Nature Reserve") %>%
  dplyr::mutate(zone = case_when(
    str_detect(pattern = "Indigenous Protected Area", string = type) ~ "Indigenous Protected Area",
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
  dplyr::mutate(colour = case_when(zone %in% "Indigenous Protected Area" ~ "#FFD8A8",
                                   zone %in% "Sanctuary Zone" & epbc %in% "State"~ "#bfd054",
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
st_write(capad, "data/north network/spatial/shapefiles/north-network-australia_marine-parks-all.shp", append = F)

plot(capad) # check
# ==============================================================================
# End of script
# ==============================================================================
