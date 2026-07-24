###
# Project: North-west Network Report
# Data:    CAPAD
# Task:    Creating North-west MPs shapefile
# Author:  Annika Leunig
# Date:    March 2026
# Outputs: 1. Tidied north-west network extent aus MPs shapefile
#
###

# Table of contents
#     1. Load data and set up
#     2. Filter CAPAD to just north-west Australia parks
#     3. Save new shapefile

###
# NOTE: Unlike the North network's CAPAD 2022 extract, this network's CAPAD 2024
# extract has ZONE_TYPE empty (NA) for every north-west record - confirmed
# against the raw attribute table. The zone_type case_when rules below are kept
# for consistency with the other network scripts, but for this network they will
# never fire, so a RES_NUMBER/IUCN-based fallback has been added directly after
# them to derive the same zone categories another way.
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

nw_network_names <- c(
  # Commonwealth AMPs (North-west Network)
  "Argo-Rowley Terrace", "Ashmore Reef", "Carnarvon Canyon", "Cartier Island",
  "Dampier", "Eighty Mile Beach", "Gascoyne", "Kimberley", "Mermaid Reef",
  "Montebello", "Ningaloo", "Roebuck", "Shark Bay",
  # WA state marine parks (Gascoyne-Pilbara-Kimberley)
  "Hamelin Pool", "Muiron Islands", "Barrow Island", "Thevenard Island",
  "Montebello Islands", "Yawuru Nagulagun / Roebuck Bay", "Yawuru",
  "Nyangumarta Warrarn", "Bardi Jawi Gaarra", "North Kimberley", "Mayala",
  "Lalang-gaddam", "Rowley Shoals", "Scott Reef"
)

# ==============================================================================
# 2. Filter CAPAD to just north-west Australia parks
# ==============================================================================
# Load and filter CAPAD to just north-west network MPs (incl. IPAs) and format colours
capad <- st_read("data/north-west network/spatial/shapefiles/Collaborative_Australian_Protected_Areas_Database_(CAPAD)_2024_-_Marine.shp") %>%
  CheckEM::clean_names() %>%
  st_make_valid() %>%
  dplyr::filter(name %in% nw_network_names) %>%
  dplyr::filter(!type %in% "Nature Reserve") %>%
  dplyr::mutate(amp_zone_code = str_extract(str_to_lower(res_number), "[a-z]{3}(?=\\d+$)")) %>%
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
    # zone_type fallback (CAPAD 2024 doesn't populate zone_type for this network) -
    # AMPs decoded from res_number (e.g. "nwkimhpz03" -> Habitat Protection Zone),
    # state parks approximated from IUCN category
    epbc %in% "Commonwealth" & amp_zone_code %in% "san" ~ "Sanctuary Zone",
    epbc %in% "Commonwealth" & amp_zone_code %in% "npz" ~ "National Park Zone",
    epbc %in% "Commonwealth" & amp_zone_code %in% "hpz" ~ "Habitat Protection Zone",
    epbc %in% "Commonwealth" & amp_zone_code %in% "muz" ~ "Multiple Use Zone",
    epbc %in% "Commonwealth" & amp_zone_code %in% "ruz" ~ "Recreational Use Zone",
    epbc %in% "Commonwealth" & amp_zone_code %in% c("spt", "spz") ~ "Special Purpose Zone",
    is.na(zone_type) & iucn %in% "Ia" ~ "Sanctuary Zone",
    is.na(zone_type) & iucn %in% "II" ~ "National Park Zone",
    is.na(zone_type) & iucn %in% "IV" ~ "Habitat Protection Zone",
    is.na(zone_type) & iucn %in% "VI" & epbc %in% "State" ~ "General Use Zone",
    .default = "Other State Marine Park Zone")) %>%
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


# Save north-west shapefile
st_write(capad, "data/north-west network/spatial/shapefiles/nw-network-australia_marine-parks-all.shp", append = F)

plot(capad) # check
# ==============================================================================
# End of script
# ==============================================================================
