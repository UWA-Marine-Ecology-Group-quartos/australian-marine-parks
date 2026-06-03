library(sf)
library(tidyverse)
library(CheckEM)

# Commonwealth and State marine parks at the scale of the location
crop <- st_read("data/south-west network/spatial/shapefiles/temp_crop-marine-parks.shp") %>%
  st_make_valid()

capad <- st_read("data/south-west network/spatial/shapefiles/Collaborative_Australian_Protected_Areas_Database_(CAPAD)_2022_-_Marine.shp") %>%
  CheckEM::clean_names() %>%
  st_make_valid() %>%
  st_crop(crop) %>%
  dplyr::filter(!type %in% "Nature Reserve",
                !name %in% c("North Kimberley", "Eighty Mile Beach")) %>%
  dplyr::mutate(zone = case_when(
    str_detect(pattern = "Sanctuary", string = zone_type) ~ "Sanctuary Zone",
    str_detect(pattern = "National Park", string = zone_type) ~ "National Park Zone",
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

rottnest <- st_read("data/south-west network/spatial/shapefiles/Rottnest_Sanctuaries.shp") %>%
  st_make_valid() %>%
  st_transform(4326) %>%
  dplyr::mutate(name = "Rottnest",
                zone_type = "Sanctuary Zone",
                zone = "Sanctuary Zone",
                epbc = "State",
                colour = "#bfd054") %>%
  dplyr::select(name, zone_type, zone, epbc, colour, geometry) %>%
  glimpse()

abrolhos <- st_read("data/south-west network/spatial/shapefiles/Abrolhos_ROAs.shp") %>%
  st_make_valid() %>%
  dplyr::mutate(name = "Abrolhos",
                zone_type = "Sanctuary Zone",
                zone = "Reef Observation Area",
                epbc = "State",
                colour = "#ddccff") %>%
  dplyr::select(name, zone_type, zone, epbc, colour, geometry) %>%
  glimpse()

bardi <- st_read("data/south-west network/spatial/shapefiles/mcr-bamp-zoning-hwm_kim_20220819_DRAFT.shp") %>%
  st_make_valid() %>%
  st_transform(4326) %>%
  clean_names() %>%
  dplyr::filter(!descriptor %in% "island",
                !is.na(class_type)) %>%
  dplyr::mutate(zone = case_when(
    str_detect(pattern = "Sanctuary", string = class_type) ~ "Sanctuary Zone",
    str_detect(pattern = "Recreational|Recreation", string = class_type) ~ "Recreational Use Zone",
    str_detect(pattern = "Habitat Protection", string = class_type) ~ "Habitat Protection Zone",
    str_detect(pattern = "Special purpose", string = class_type) ~ "Special Purpose Zone",
    str_detect(pattern = "General", string = class_type) ~ "General Use Zone",
    .default = "Other State Marine Park Zone")) %>%
  dplyr::mutate(epbc = "State") %>%
  dplyr::mutate(colour = case_when(zone %in% "Sanctuary Zone" & epbc %in% "State"~ "#bfd054",
                                   zone %in% "Recreational Use Zone" & epbc %in% "State" ~ "#f4e952",
                                   zone %in% "Habitat Protection Zone"& epbc %in% "State" ~ "#fffbcc",
                                   zone %in% "Special Purpose Zone"& epbc %in% "State" ~ "#c5bcc9",
                                   zone %in% "General Use Zone" ~ "#bddde1",
                                   zone %in% "Other State Marine Park Zone" ~ "gray80")) %>%
  dplyr::rename(zone_type = class_type) %>%
  dplyr::select(name, zone_type, zone, epbc, colour, geometry) %>%
  glimpse()

eighty <- st_read("data/south-west network/spatial/shapefiles/mcr-embmp-zoning_kim_20170818.shp") %>%
  st_make_valid() %>%
  st_transform(4326) %>%
  clean_names() %>%
  dplyr::mutate(zone = case_when(
    str_detect(pattern = "Sanctuary", string = class_type) ~ "Sanctuary Zone",
    str_detect(pattern = "Recreational|Recreation", string = class_type) ~ "Recreational Use Zone",
    str_detect(pattern = "Habitat Protection", string = class_type) ~ "Habitat Protection Zone",
    str_detect(pattern = "Special purpose", string = class_type) ~ "Special Purpose Zone",
    str_detect(pattern = "General", string = class_type) ~ "General Use Zone",
    .default = "Other State Marine Park Zone")) %>%
  dplyr::mutate(epbc = "State") %>%
  dplyr::mutate(colour = case_when(zone %in% "Sanctuary Zone" & epbc %in% "State"~ "#bfd054",
                                   zone %in% "Recreational Use Zone" & epbc %in% "State" ~ "#f4e952",
                                   zone %in% "Habitat Protection Zone"& epbc %in% "State" ~ "#fffbcc",
                                   zone %in% "Special Purpose Zone"& epbc %in% "State" ~ "#c5bcc9",
                                   zone %in% "General Use Zone" ~ "#bddde1",
                                   zone %in% "Special Purpose Zone"& epbc %in% "State" ~ "#c5bcc9",
                                   zone %in% "Other State Marine Park Zone" ~ "gray80")) %>%
  dplyr::rename(zone_type = class_type) %>%
  dplyr::select(name, zone_type, zone, epbc, colour, geometry) %>%
  glimpse()

northkim <- st_read("data/south-west network/spatial/shapefiles/mcr-nkmp-zoning-hwm-m-plan_kim_20160913.shp") %>%
  st_make_valid() %>%
  st_transform(4326) %>%
  clean_names() %>%
  dplyr::mutate(zone = case_when(
    str_detect(pattern = "sanctuary", string = class_type) ~ "Sanctuary Zone",
    str_detect(pattern = "recreational|recreation", string = class_type) ~ "Recreational Use Zone",
    str_detect(pattern = "habitat Protection", string = class_type) ~ "Habitat Protection Zone",
    str_detect(pattern = "special purpose", string = class_type) ~ "Special Purpose Zone",
    str_detect(pattern = "general", string = class_type) ~ "General Use Zone",
    .default = "Other State Marine Park Zone")) %>%
  dplyr::mutate(epbc = "State") %>%
  dplyr::mutate(colour = case_when(zone %in% "Sanctuary Zone" & epbc %in% "State"~ "#bfd054",
                                   zone %in% "Recreational Use Zone" & epbc %in% "State" ~ "#f4e952",
                                   zone %in% "Habitat Protection Zone"& epbc %in% "State" ~ "#fffbcc",
                                   zone %in% "Special Purpose Zone"& epbc %in% "State" ~ "#c5bcc9",
                                   zone %in% "General Use Zone" ~ "#bddde1",
                                   zone %in% "Special Purpose Zone"& epbc %in% "State" ~ "#c5bcc9",
                                   zone %in% "Other State Marine Park Zone" ~ "gray80")) %>%
  dplyr::rename(zone_type = class_type) %>%
  dplyr::select(name, zone_type, zone, epbc, colour, geometry) %>%
  glimpse()

marine_parks <- dplyr::bind_rows(list(capad, rottnest, abrolhos, bardi, eighty, northkim)) %>%
  st_make_valid()

st_write(marine_parks, "data/south-west network/spatial/shapefiles/western-australia_marine-parks-all.shp", append = F)
