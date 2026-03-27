###
# Data:    CAPAD, South australia cropping polygon
# Task:    Tidy CAPAD to a south australia marine park shp
# Author:  Annika Leunig (modification of Claude's code)
# Date:    Feb 2026
###

library(CheckEM)
library(sf)
library(tidyverse)


# Commonwealth and State marine parks at the scale of the location
crop <- st_read("data/south-west network/spatial/shapefiles/South_aus_crop-marine-parks.shp") %>%
  st_make_valid()

capad <- st_read("data/south-west network/spatial/shapefiles/Collaborative_Australian_Protected_Areas_Database_(CAPAD)_2022_-_Marine.shp") %>%
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


# Save SA shapefile
st_write(capad, "data/south-west network/spatial/shapefiles/south-australia_marine-parks-all.shp", append = F)

# combine Western Australia and South Australia tidied shapefiles
wa <- st_read("data/south-west network/spatial/shapefiles/western-australia_marine-parks-all.shp") %>%
  st_make_valid()

combined <- dplyr::bind_rows(list(capad,wa)) %>%
  st_make_valid()

# test <- combined %>%
#   dplyr::filter(zone_type %in% c("Unassigned (IUCN VI)",
#                                  "Unassigned (IUCN IV,VI)",
#                                  "Restricted Access Zone - RAZ-2 (IUCN IA)",
#                                  "Restricted Access Zone - RAZ-1 (IUCN IA)",
#                                  "Unassigned (IUCN IA)",
#                                  'Conservation Area (IUCN IA)',
#                                  "Unassigned (IUCN IV)",
#                                  "MP (Unclassified) (IUCN VI)",
#                                  "MMA (Unclassified) (IUCN VI)"))


# Save combined shapefile
st_write(combined, "data/south-west network/spatial/shapefiles/south-and-western-australia_marine-parks-all.shp", append = F)

