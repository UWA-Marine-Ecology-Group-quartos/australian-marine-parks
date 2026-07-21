###
# Project: NESP 5.6 Project - South west Corner Report
# Data:    Marine Regions (AMP network boundaries), CAPAD 2022 Marine
#          (Australian Marine Parks zoning) and AusBathyTopo 250 m bathymetry
# Task:    Calculate the % extent of Australian Marine Park National Park
#          Zone that falls within each natural-values common-language depth
#          contour (shallow, mesophotic, rariophotic, upper-slope, mid-slope,
#          lower-slope, abyssal), for the South-west Network and all other
#          AMP networks
# Author:  Annika Leunig
# Date:    Jul 2026
# Outputs: 1. output/south-west network/npz_depth_representation.csv
#             (table: rows = network, columns = % area per depth class)
#          2. output/south-west network/npz_depth_representation_sentences.csv
#             (the report sentence filled out for each network)
###

# Table of contents
#     1.  Set up and load data
#     2.  Filter National Park Zone (Commonwealth AMPs only)
#     3.  Filter marine (not land) network boundaries
#     4.  Assign each National Park Zone polygon to its network
#     5.  Depth class breaks and labels
#     6.  Function: calculate depth class breakdown for one network
#     7.  Run for South-west Network and all other networks
#     8.  Save output table
#     9.  Fill out report sentence for each network


# ==============================================================================
# 1. SET UP AND LOAD DATA
# ==============================================================================

# Clear your environment
rm(list= ls())

# Set the study name (for folder structure)
name <- "south-west"

# Load libraries
library(tidyverse)
library(sf)
library(terra)

# ── Load spatial files ────────────────────────────────────────────────────────
sf_use_s2(T)

# AMP network / marine region boundaries
marine_regions <- st_read("data/south-west network/spatial/shapefiles/marine_regions.shp") %>%
  st_make_valid()

# CAPAD 2022 Marine - all Commonwealth and State marine protected area zones
capad_marine <- st_read("data/south-west network/spatial/shapefiles/Collaborative_Australian_Protected_Areas_Database_(CAPAD)_2022_-_Marine.shp") %>%
  st_make_valid()

# Bathymetry (national coverage - do NOT crop to a fixed extent here, each
# network is cropped individually in section 6 so this works for every network,
# not just the South-west)
bathy_path <- "data/south-west network/spatial/rasters/AusBathyTopo__Australia__2024_250m_MSL_cog.tif"


# ==============================================================================
# 2. FILTER NATIONAL PARK ZONE (COMMONWEALTH AMPS ONLY)
# ==============================================================================
# CAPAD records the AMP no-take zone under two ZONE_TYPE labels depending on
# network ("National Park Zone (IUCN II)" and "Marine National Park Zone
# (IUCN II)", used for the South-east AMPs) - both are the same zone category,
# so both are matched here. State marine park zones are excluded (EPBC filter).
npz <- capad_marine %>%
  dplyr::filter(EPBC == "Commonwealth",
                str_detect(ZONE_TYPE, "National Park Zone")) %>%
  st_transform(st_crs(marine_regions))

# Sanity check - confirm what got matched before relying on it
message("ZONE_TYPE values matched as National Park Zone:")
print(unique(npz$ZONE_TYPE))
message(nrow(npz), " National Park Zone polygons found across ", n_distinct(npz$NAME), " marine parks")


# ==============================================================================
# 3. FILTER MARINE (NOT LAND) NETWORK BOUNDARIES
# ==============================================================================
networks <- marine_regions %>%
  dplyr::filter(ENVIRON == "Marine") %>%
  dplyr::select(REGION) %>%
  dplyr::rename(network = REGION)

message("Networks found: ", paste(sort(unique(networks$network)), collapse = ", "))


# ==============================================================================
# 4. ASSIGN EACH NATIONAL PARK ZONE POLYGON TO ITS NETWORK
# ==============================================================================
# Intersect (not just a centroid join) so zones that straddle a network
# boundary are split and counted proportionally in each network
npz_by_network <- st_intersection(npz, networks) %>%
  st_make_valid() %>%
  dplyr::filter(!st_is_empty(.))

unassigned_area_km2 <- as.numeric(sum(st_area(npz), na.rm = T) - sum(st_area(npz_by_network), na.rm = T)) / 1e6
if (unassigned_area_km2 > 1) {
  warning(round(unassigned_area_km2, 1), " km2 of National Park Zone did not fall inside any network boundary and will be excluded (check network extent covers all AMPs, e.g. external territories)")
}


# ==============================================================================
# 5. DEPTH CLASS BREAKS AND LABELS
# ==============================================================================
# Natural values common language depth contours, matching the breaks used in
# the network bathymetry maps (0, -30, -70, -200, -700, -2000, -4000, -6000)
depth_breaks <- c(0, -30, -70, -200, -700, -2000, -4000, -6000)
depth_labels <- c("shallow (<30 m)", "mesophotic (30-70 m)", "rariophotic (70-200 m)",
                  "upper-slope (200-700 m)", "mid-slope (700-2000 m)",
                  "lower-slope (2000-4000 m)", "abyssal (4000-6000 m)")


# ==============================================================================
# 6. FUNCTION: CALCULATE DEPTH CLASS BREAKDOWN FOR ONE NETWORK
# ==============================================================================
calculate_npz_depth_breakdown <- function(network_name, npz_by_network, bathy_path,
                                          depth_breaks, depth_labels) {

  npz_net <- npz_by_network %>%
    dplyr::filter(network %in% network_name)

  if (nrow(npz_net) == 0) {
    message("No National Park Zone in ", network_name, " - skipping")
    return(NULL)
  }

  # Crop the (national) bathymetry raster to this network's National Park
  # Zone extent only - keeps this efficient regardless of network size.
  # Reproject the VECTOR to match the raster's CRS (not the other way
  # around) so the bathymetry values are never resampled/degraded.
  bathy_crs <- crs(rast(bathy_path))
  npz_vect  <- vect(npz_net) %>% project(bathy_crs)
  e         <- as.vector(ext(npz_vect)) # xmin xmax ymin ymax
  buff      <- 0.05 * max(e["xmax"] - e["xmin"], e["ymax"] - e["ymin"])
  net_ext   <- ext(npz_vect) + buff

  bathy <- rast(bathy_path) %>%
    crop(net_ext) %>%
    clamp(upper = 0, values = F) %>% # drop land / above MSL cells
    mask(npz_vect)

  # Reclassify into natural-values depth classes
  # depth_breaks descend from 0 to -6000, so each class interval is
  # (from, to] i.e. (depth_breaks[i+1], depth_breaks[i]] -> become i
  rcl <- cbind(from   = depth_breaks[-1],
               to     = depth_breaks[-length(depth_breaks)],
               become = seq_along(depth_labels))
  bathy_class <- classify(bathy, rcl, include.lowest = T, right = T)
  levels(bathy_class) <- data.frame(id = 1:length(depth_labels), depth_class = depth_labels)

  # True cell area (km2), accounting for latitude, so this is accurate
  # regardless of whether the raster is geographic or projected
  area_r <- cellSize(bathy, unit = "km")

  zonal_area <- zonal(area_r, bathy_class, sum, na.rm = T) %>%
    dplyr::rename(area_km2 = area)

  total_km2 <- sum(zonal_area$area_km2, na.rm = T)

  out <- tibble(depth_class = depth_labels) %>%
    left_join(zonal_area, by = "depth_class") %>%
    dplyr::mutate(area_km2 = replace_na(area_km2, 0),
                  pct = round(100 * area_km2 / total_km2, 1)) %>%
    dplyr::select(depth_class, pct) %>%
    pivot_wider(names_from = depth_class, values_from = pct) %>%
    dplyr::mutate(network = network_name, total_npz_area_km2 = round(total_km2, 1),
                  .before = 1)

  return(out)
}


# ==============================================================================
# 7. RUN FOR SOUTH-WEST NETWORK AND ALL OTHER NETWORKS
# ==============================================================================
all_networks <- sort(unique(npz_by_network$network))

npz_depth_table <- map_dfr(all_networks, calculate_npz_depth_breakdown,
                           npz_by_network = npz_by_network,
                           bathy_path = bathy_path,
                           depth_breaks = depth_breaks,
                           depth_labels = depth_labels)

# Put South-west first, then alphabetical
npz_depth_table <- npz_depth_table %>%
  dplyr::arrange(network != "South-west", network)

print(npz_depth_table)


# ==============================================================================
# 8. SAVE OUTPUT TABLE
# ==============================================================================
dir.create(paste0("output/", name, " network"), recursive = T, showWarnings = F)

write_csv(npz_depth_table, paste0("output/", name, " network/npz_depth_representation.csv"))


# ==============================================================================
# 9. FILL OUT REPORT SENTENCE FOR EACH NETWORK
# ==============================================================================
fill_npz_sentence <- function(row) {
  glue::glue(
    "Within the {row$network} Network, the distribution of marine parks and ",
    "zones is broadly representative across ecosystem depth contours, with ",
    "National Park Zone extent across the natural values common language ",
    "depth contours of {row[['shallow (<30 m)']]}% in shallow (< 30 m), ",
    "{row[['mesophotic (30-70 m)']]}% in mesophotic (30-70 m), ",
    "{row[['rariophotic (70-200 m)']]}% in rariophotic (70-200 m), ",
    "{row[['upper-slope (200-700 m)']]}% in upper-slope (200-700 m), ",
    "{row[['mid-slope (700-2000 m)']]}% in mid-slope (700-2000 m), ",
    "{row[['lower-slope (2000-4000 m)']]}% in lower-slope (2000-4000) and ",
    "{row[['abyssal (4000-6000 m)']]}% in abyssal (4000-6000 m) depths."
  )
}

npz_sentences <- npz_depth_table %>%
  split(seq(nrow(.))) %>%
  map_chr(fill_npz_sentence) %>%
  tibble(network = npz_depth_table$network, sentence = .)

print(npz_sentences$sentence)

write_csv(npz_sentences, paste0("output/", name, " network/npz_depth_representation_sentences.csv"))

# ==============================================================================
# End of script
# ==============================================================================
