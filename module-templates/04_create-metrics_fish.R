###
# Project: NESP 4.20 - Marine Park Dashboard reporting
# Data:    Fish data synthesis
# Task:    Combine and format fish data for full subsets modelling
# Author:  Claude Spencer
# Date:    June 2024
###

# Clear the environment
rm(list= ls())

# Load necessary libraries
library(CheckEM)
library(tidyverse)
library(sf)
library(here)
library(leaflet)
library(googlesheets4)
library(terra)
library(tidyterra)

# Set the study name
name <- "GeographeAMP"

metadata_bathy_derivatives <- readRDS(paste0("data/geographe/tidy/", name, "_metadata-bathymetry-derivatives.rds")) %>%
  clean_names() %>%
  glimpse()

# This is formatted habitat from 03_create-metrics_habitat
benthos <- readRDS(paste0("data/geographe/tidy/", name, "_benthos-count.RDS")) %>%
  CheckEM::clean_names() %>%
  dplyr::select(campaignid, sample, reef, total_pts) %>%
  dplyr::mutate(reef = reef/total_pts) %>%
  dplyr::filter(!str_detect(sample, "MF")) %>% # Removes 2007 habitat data
  dplyr::mutate(campaignid = "2014-12_Geographe.Bay_stereoBRUVs") %>%
  glimpse()

# Maturity data from WA sheet - should this just get included in the life history?
maturity_mean <- CheckEM::maturity %>%
  dplyr::filter(!marine_region %in% c("NW", "N")) %>% # Change here for each marine park
  dplyr::group_by(family, genus, species, sex) %>%
  dplyr::slice(which.min(l50_mm)) %>%
  ungroup() %>%
  dplyr::group_by(family, genus, species) %>%
  dplyr::summarise(l50 = mean(l50_mm)) %>%
  ungroup() %>%
  glimpse()

large_bodied_carnivores <- CheckEM::australia_life_history %>%
  dplyr::filter(fb_trophic_level > 2.8) %>%
  dplyr::filter(length_max_cm > 40) %>%
  dplyr::filter(class %in% "Actinopterygii") %>%
  dplyr::filter(!order %in% c("Anguilliformes", "Ophidiiformes", "Notacanthiformes","Tetraodontiformes","Syngnathiformes",
                              "Synbranchiformes", "Stomiiformes", "Siluriformes", "Saccopharyngiformes", "Osmeriformes",
                              "Osteoglossiformes", "Lophiiformes", "Lampriformes", "Beloniformes", "Zeiformes")) %>%
  left_join(maturity_mean) %>%
  dplyr::mutate(fb_length_at_maturity_mm = fb_length_at_maturity_cm * 10) %>%
  dplyr::mutate(l50 = if_else(is.na(l50), fb_length_at_maturity_mm, l50)) %>%
  dplyr::filter(!is.na(l50)) %>%
  dplyr::select(family, genus, species, l50) %>%
  glimpse()

tempdat <- read.csv("data/geographe/raw/temp/2007-2014-Geographe-stereo-BRUVs.complete.maxn.csv") %>%
  dplyr::select(campaignid, sample, family, genus, species, maxn) %>%
  dplyr::rename(count = maxn) %>%
  dplyr::mutate(scientific_name = paste(genus, species, sep = " ")) %>%
  dplyr::filter(!campaignid %in% "2007-03_Capes.MF_stereoBRUVs") %>% # Remove 2007 data
  glimpse()

ta.sr <- tempdat %>%
  # readRDS(paste0("data/tidy/", name, "_count.rds")) %>%
  dplyr::group_by(campaignid, sample, scientific_name) %>%
  dplyr::summarise(count = sum(count)) %>% # Should do nothing for proper syntheses?
  pivot_wider(names_from = "scientific_name", values_from = count, values_fill = 0) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(total_abundance = rowSums(.[, 3:(ncol(.))], na.rm = TRUE),
                species_richness = rowSums(.[, 3:(ncol(.))] > 0)) %>%
  dplyr::select(campaignid, sample, total_abundance, species_richness) %>%
  pivot_longer(cols = c("total_abundance", "species_richness"), names_to = "response", values_to = "number") %>%
  glimpse()

# The RLS thermal niche is going to already be in the data
master <- CheckEM::australia_life_history %>%
  clean_names() %>%
  dplyr::filter(grepl('Australia', global_region),
                grepl('SW', marine_region)) %>% # Change country here
  dplyr::select(family, genus, species, rls_thermal_niche) %>%
  dplyr::distinct() %>%
  dplyr::glimpse()

cti <- tempdat %>%
  dplyr::ungroup() %>%
  dplyr::filter(count > 0) %>%
  left_join(master) %>%
  uncount(count) %>%
  dplyr::mutate(count = 1) %>%
  dplyr::filter(!is.na(rls_thermal_niche)) %>%
  dplyr::mutate(log_count = log10(count + 1),
                weightedsti = log_count*rls_thermal_niche) %>%
  dplyr::group_by(campaignid, sample) %>%
  dplyr::summarise(log_count = sum(log_count, na.rm = T),
                   w_sti = sum(weightedsti, na.rm = T),
                   CTI = w_sti/log_count,
                   number = mean(rls_thermal_niche, na.rm = T)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(response = "cti") %>%
  glimpse()

tidy_maxn <- bind_rows(ta.sr, cti) %>%
  dplyr::select(-c(log_count, w_sti, CTI)) %>%
  dplyr::left_join(benthos) %>%
  dplyr::left_join(metadata_bathy_derivatives) %>%
  dplyr::filter(!is.na(reef), # Errors to check - against the habitat
                !is.na(longitude_dd), # This should be fixed in the synthesis data downloaded from API
                !is.na(geoscience_roughness)) %>% # This should be fixed in the synthesis data downloaded from API
  glimpse()

saveRDS(tidy_maxn, file = paste0("data/geographe/tidy/", name, "_tidy-count.rds"))

templength <- read.csv("data/geographe/raw/temp/2007-2014-Geographe-stereo-BRUVs.expanded.length.csv") %>%
  dplyr::filter(successful.length %in% "Yes") %>%
  dplyr::select(campaignid, sample, family, genus, species, length)

lengths <- templength %>%
  left_join(large_bodied_carnivores) %>%
  dplyr::mutate(number = 1,
                scientific_name = paste(genus, species, sep = " ")) %>%
  glimpse()

all_species <- lengths %>%
  distinct(scientific_name) %>%
  glimpse()

test_species <- lengths %>%
  dplyr::filter(!is.na(l50)) %>%
  distinct(scientific_name) %>%
  glimpse()

metadata_length <- lengths %>%
  distinct(campaignid, sample) %>%
  glimpse()

big_carn <- lengths %>%
  dplyr::filter(length > l50) %>%
  dplyr::group_by(campaignid, sample) %>%
  dplyr::summarise(number = sum(number)) %>%
  ungroup() %>%
  right_join(metadata_length) %>%
  dplyr::mutate(number = ifelse(is.na(number), 0, number)) %>%
  dplyr::mutate(response = "greater than Lm carnivores") %>%
  left_join(benthos) %>%
  dplyr::glimpse()
# Check number of samples that are > 0
nrow(filter(big_carn, number > 0))/nrow(big_carn)

small_carn <- lengths %>%
  dplyr::filter(length < l50) %>%
  dplyr::group_by(campaignid, sample) %>%
  dplyr::summarise(number = sum(number)) %>%
  ungroup() %>%
  right_join(metadata_length) %>%
  dplyr::mutate(number = ifelse(is.na(number), 0, number)) %>%
  dplyr::mutate(response = "smaller than Lm carnivores") %>%
  left_join(benthos) %>%
  dplyr::glimpse()
# Check number of samples that are > 0
nrow(filter(small_carn, number > 0))/nrow(small_carn)

big_snap <- lengths %>%
  dplyr::filter(species %in% "auratus",
                length > l50) %>%
  dplyr::group_by(campaignid, sample) %>%
  dplyr::summarise(number = sum(number)) %>%
  ungroup() %>%
  right_join(metadata_length) %>%
  dplyr::mutate(number = ifelse(is.na(number), 0, number)) %>%
  dplyr::mutate(response = "greater than Lm Pink snapper") %>%
  left_join(benthos) %>%
  dplyr::glimpse()
# Check number of samples that are > 0
nrow(filter(big_snap, number > 0))/nrow(big_snap) # This won't run

small_snap <- lengths %>%
  dplyr::filter(species %in% "auratus",
                length < l50) %>%
  dplyr::group_by(campaignid, sample) %>%
  dplyr::summarise(number = sum(number)) %>%
  ungroup() %>%
  right_join(metadata_length) %>%
  dplyr::mutate(number = ifelse(is.na(number), 0, number)) %>%
  dplyr::mutate(response = "smaller than Lm Pink snapper") %>%
  left_join(benthos) %>%
  dplyr::glimpse()
# Check number of samples that are > 0
nrow(filter(small_snap, number > 0))/nrow(small_snap)

tidy_length <- bind_rows(big_carn, small_carn, small_snap) %>% # Removed snapper - not enough non-zero data
  dplyr::left_join(metadata_bathy_derivatives) %>%
  dplyr::filter(!is.na(reef), # Errors to check - against the habitat
                !is.na(longitude_dd), # This should be fixed in the synthesis data downloaded from API
                !is.na(geoscience_roughness)) %>% # This should be fixed in the synthesis data downloaded from API
  glimpse()

# Visualise spatial patterns
preds <- readRDS(paste0("data/geographe/spatial/rasters/", name, "_bathymetry-derivatives.rds"))
plot(preds)
names(preds)

ggplot() +
  geom_spatraster(data = preds, aes(fill = geoscience_depth)) +
  geom_point(data = tidy_length,
             aes(x = longitude_dd, y = latitude_dd, size = number, colour = I(if_else(number == 0, "white", "darkblue"))),
             show.legend = F) +
  facet_wrap(~response) +
  theme_classic() +
  coord_sf()

saveRDS(tidy_length, file = paste0("data/geographe/tidy/", name, "_tidy-length.rds"))
