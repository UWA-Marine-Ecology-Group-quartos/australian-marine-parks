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

# Set the study name
name <- "GeographeAMP"

metadata_bathy_derivatives <- readRDS(paste0("data/geographe/tidy/", name, "_metadata-bathymetry-derivatives.rds")) %>%
  clean_names() %>%
  glimpse()

# This is formatted habitat from 03_create-metrics_habitat
benthos <- readRDS(paste0("data/geographe/tidy/", name, "_benthos-count.RDS")) %>%
  CheckEM::clean_names() %>%
  dplyr::select(campaignid, sample, reef, total_pts) %>%
  dplyr::filter(!str_detect(sample, "MF")) %>%
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
  dplyr::filter(!campaignid %in% "2007-03_Capes.MF_stereoBRUVs") %>%
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
  glimpse()

# This data will have NAs for habitat in it - need to fix these at the source and re-export
saveRDS(tidy_maxn, file = paste0("data/geographe/tidy/", name, "_tidy-count.rds"))

templength <- read.csv("data/geographe/raw/temp/2007-2014-Geographe-stereo-BRUVs.expanded.length.csv") %>%
  dplyr::filter(successful.length %in% "Yes") %>%
  dplyr::select(campaignid, sample, family, genus, species, length)


lengths <- templength %>%
  left_join(benthos) %>%
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

metadata.length <- lengths %>%
  distinct(campaignid, sample, status) %>%
  glimpse()

big_carn <- lengths %>%
  dplyr::filter(length > l50, # This gets rid of the non-large bodied carnivore ones e.g. NAs
                !species %in% c("truttaceus")) %>%
  dplyr::group_by(campaignid, sample) %>%
  dplyr::summarise(number = sum(number)) %>%
  ungroup() %>%
  right_join(metadata.length) %>%
  dplyr::mutate(number = ifelse(is.na(number), 0, number)) %>%
  dplyr::mutate(response = "greater than Lm carinvores") %>%
  left_join(habitat) %>%
  dplyr::glimpse()

small_carn <- lengths %>%
  dplyr::filter(length < l50, # This gets rid of the non-large bodied carnivore ones e.g. NAs
                !species %in% c("truttaceus")) %>%
  dplyr::group_by(campaignid, sample) %>%
  dplyr::summarise(number = sum(number)) %>%
  ungroup() %>%
  right_join(metadata.length) %>%
  dplyr::mutate(number = ifelse(is.na(number), 0, number)) %>%
  dplyr::mutate(response = "smaller than Lm carnivores") %>%
  left_join(habitat) %>%
  dplyr::glimpse()

mature_nanny <- lengths %>%
  dplyr::filter(length > l50 &
                  species %in% c("gerrardi")) %>%
  dplyr::group_by(campaignid, sample) %>%
  dplyr::summarise(number = sum(number)) %>%
  ungroup() %>%
  right_join(metadata.length) %>%
  dplyr::mutate(number = if_else(is.na(number), 0, number)) %>%
  dplyr::mutate(response = "Nannygai greater than Lm") %>%
  left_join(habitat) %>%
  dplyr::glimpse()

immature_nanny <- lengths %>%
  dplyr::filter(length < l50 &
                  species %in% c("gerrardi")) %>%
  dplyr::group_by(campaignid, sample) %>%
  dplyr::summarise(number = sum(number)) %>%
  ungroup() %>%
  right_join(metadata.length) %>%
  dplyr::mutate(number = if_else(is.na(number), 0, number)) %>%
  dplyr::mutate(response = "Nannygai smaller than Lm") %>%
  left_join(habitat) %>%
  dplyr::glimpse()

tidy.length <- bind_rows(big_carn, small_carn, mature_nanny, immature_nanny) %>%
  dplyr::left_join(metadata.bathy.derivatives) %>%
  glimpse()

# Abundant species
top10 <- lengths %>%
  dplyr::filter(!is.na(l50)) %>%
  dplyr::mutate(scientific = paste(genus, species, sep = " ")) %>%
  dplyr::group_by(scientific) %>%
  dplyr::summarise(number = sum(number)) %>%
  arrange(desc(number)) %>%
  slice_head(n = 10) %>%
  glimpse()

plot_dat <- lengths %>%
  mutate(scientific = paste(genus, species, sep = " ")) %>%
  group_by(sample, longitude_dd, latitude_dd, scientific) %>%
  summarise(number = sum(number)) %>%
  ungroup() %>%
  glimpse()

# Spatial distribution - there are 15 with nannygai
ggplot() +
  geom_point(data = dplyr::filter(plot_dat, scientific %in% "Centroberyx gerrardi"),
             aes(x = longitude_dd, y = latitude_dd, size = number)) +
  theme_classic()

saveRDS(tidy.length, file = paste0("data/tidy/", name, "_tidy-length.rds"))
