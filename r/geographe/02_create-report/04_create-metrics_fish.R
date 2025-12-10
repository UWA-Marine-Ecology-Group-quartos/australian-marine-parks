###
# Project: NESP 4.20 - Marine Park Dashboard reporting
# Data:    Fish data synthesis
# Task:    Combine and format fish data for full subsets modelling
# Author:  Claude Spencer
# Date:    June 2024
###

# Clear the environment
rm(list = ls())

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
park <- "geographe"

metadata_bathy_derivatives <- readRDS(paste0("data/", park, "/tidy/", name, "_metadata-bathymetry-derivatives.rds")) %>%
  clean_names() %>%
  glimpse()

metadata <- readRDS(paste0("data/", park, "/raw/metadata.RDS"))

# This is formatted habitat from 03_create-metrics_habitat
benthos <- readRDS(paste0("data/", park, "/tidy/", name, "_benthos-count_combined.RDS")) %>%
  CheckEM::clean_names() %>%
  dplyr::select(campaignid, sample, year, status, reef, total_pts) %>%
  dplyr::mutate(reef = reef/total_pts) %>% # Model reef as proportion for fish prediction
  glimpse()

# # Maturity data from WA sheet
# maturity_mean <- CheckEM::maturity %>%
#   dplyr::filter(!marine_region %in% c("NW", "N")) %>% # Change here for each marine park (exclude regions)
#   dplyr::group_by(family, genus, species, sex) %>%
#   dplyr::slice(which.min(l50_mm)) %>%
#   ungroup() %>%
#   dplyr::group_by(family, genus, species) %>%
#   dplyr::summarise(l50 = mean(l50_mm)) %>% ##HE this averages across sexes, but sometimes big difference (e.g. double:half male:female)
#   ungroup() %>%
#   glimpse()
#
# large_bodied_carnivores <- CheckEM::australia_life_history %>% ##HE remove pelagics
#   dplyr::filter(length_max_cm > 20) %>%
#   dplyr::filter(class %in% "Actinopterygii") %>%
#   dplyr::filter(!order %in% c("Anguilliformes", "Ophidiiformes", "Notacanthiformes","Tetraodontiformes","Syngnathiformes",
#                               "Synbranchiformes", "Stomiiformes", "Siluriformes", "Saccopharyngiformes", "Osmeriformes",
#                               "Osteoglossiformes", "Lophiiformes", "Lampriformes", "Beloniformes", "Zeiformes", "Carangiformes")) %>%
#   left_join(maturity_mean) %>%
#   dplyr::mutate(fb_length_at_maturity_mm = fb_length_at_maturity_cm * 10) %>%
#   dplyr::mutate(l50 = if_else(is.na(l50), fb_length_at_maturity_mm, l50)) %>%
#   dplyr::filter(!is.na(l50)) %>%
#   dplyr::select(family, genus, species, l50) %>%
#   glimpse()

count <- readRDS(paste0("data/", park, "/raw/_count-with-zeros.RDS")) %>%
  dplyr::select(campaignid, sample, family, genus, species, count) %>%
  dplyr::mutate(scientific_name = paste(family, genus, species, sep = " ")) %>%
  glimpse()

ta.sr <- count %>%
  dplyr::select(-c(family, genus, species)) %>%
  dplyr::group_by(campaignid, sample, scientific_name) %>%
  pivot_wider(names_from = "scientific_name", values_from = count) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(total_abundance = rowSums(.[, 3:(ncol(.))], na.rm = T),
                species_richness = rowSums(.[, 3:(ncol(.))] > 0)) %>%
  dplyr::select(campaignid, sample, total_abundance, species_richness) %>%
  pivot_longer(cols = c("total_abundance", "species_richness"), names_to = "response", values_to = "count") %>%
  glimpse() # Should be nsamps * 2 = 594

cti <- CheckEM::create_cti(data = count) %>%
  dplyr::rename(count = cti) %>%
  dplyr::mutate(response = "cti") %>%
  glimpse()

tidy_maxn <- bind_rows(ta.sr, cti) %>% ## HE need to check missing aspects
  dplyr::select(-c(log_count, w_sti)) %>%
  dplyr::left_join(benthos) %>%
  dplyr::left_join(metadata) %>% # To join samples without valid bathymetry derivatives
  dplyr::left_join(metadata_bathy_derivatives) %>%
  dplyr::filter(!is.na(reef), # GBR3-4 has no habitat
                !is.na(geoscience_aspect)) %>% # Not valid values for modelling so will remove them now
  glimpse()

saveRDS(tidy_maxn, file = paste0("data/", park, "/tidy/", name, "_tidy-count.rds"))

# length <- readRDS(paste0("data/", park, "/raw/_length-with-zeros.RDS")) %>%
#   dplyr::select(campaignid, sample, family, genus, species, length_mm, count) %>%
#   left_join(large_bodied_carnivores) %>%
#   dplyr::mutate(scientific_name = paste(genus, species, sep = " ")) %>%
#   glimpse()
# length(unique(length$sample))
#
# all_species <- length %>%
#   distinct(scientific_name) %>%
#   glimpse()
#
# test_species <- length %>%
#   dplyr::filter(!is.na(l50)) %>%
#   distinct(scientific_name) %>%
#   glimpse()
#
# metadata_length <- length %>%
#   distinct(campaignid, sample) %>%
#   glimpse()
#
# big_carn <- length %>%
#   dplyr::filter(length_mm > l50) %>%
#   dplyr::group_by(campaignid, sample) %>%
#   dplyr::summarise(count = sum(count)) %>%
#   ungroup() %>%
#   right_join(metadata_length) %>%
#   dplyr::mutate(count = ifelse(is.na(count), 0, count)) %>%
#   dplyr::mutate(response = "greater than Lm carnivores") %>%
#   left_join(benthos) %>%
#   dplyr::glimpse()
# # Check number of samples that are > 0
# nrow(filter(big_carn, count > 0))/nrow(big_carn)
#
# small_carn <- length %>%
#   dplyr::filter(length_mm < l50) %>%
#   dplyr::group_by(campaignid, sample) %>%
#   dplyr::summarise(count = sum(count)) %>%
#   ungroup() %>%
#   right_join(metadata_length) %>%
#   dplyr::mutate(count = ifelse(is.na(count), 0, count)) %>%
#   dplyr::mutate(response = "smaller than Lm carnivores") %>%
#   left_join(benthos) %>%
#   dplyr::glimpse()
# # Check number of samples that are > 0
# nrow(filter(small_carn, count > 0))/nrow(small_carn)
#
# big_snap <- length %>%
#   dplyr::filter(species %in% "auratus",
#                 length_mm > l50) %>%
#   dplyr::group_by(campaignid, sample) %>%
#   dplyr::summarise(count = sum(count)) %>%
#   ungroup() %>%
#   right_join(metadata_length) %>%
#   dplyr::mutate(count = ifelse(is.na(count), 0, count)) %>%
#   dplyr::mutate(response = "greater than Lm Pink snapper") %>%
#   left_join(benthos) %>%
#   dplyr::glimpse()
# # Check number of samples that are > 0
# nrow(filter(big_snap, count > 0))/nrow(big_snap) # This won't run in model
#
# small_snap <- length %>%
#   dplyr::filter(species %in% "auratus",
#                 length_mm < l50) %>%
#   dplyr::group_by(campaignid, sample) %>%
#   dplyr::summarise(count = sum(count)) %>%
#   ungroup() %>%
#   right_join(metadata_length) %>%
#   dplyr::mutate(count = ifelse(is.na(count), 0, count)) %>%
#   dplyr::mutate(response = "smaller than Lm Pink snapper") %>%
#   left_join(benthos) %>%
#   dplyr::glimpse()
# # Check number of samples that are > 0
# nrow(filter(small_snap, count > 0))/nrow(small_snap)
#
# tidy_length <- bind_rows(big_carn, small_carn, big_snap, small_snap) %>% # Removed snapper - not enough non-zero data ##HE added snap
#   dplyr::left_join(metadata) %>%
#   dplyr::left_join(metadata_bathy_derivatives) %>%
#   dplyr::filter(!is.na(reef), # GBR3-4 missing habitat ##HE This will remove all 2024 until habitat is included
#                 !is.na(geoscience_aspect)) %>% # Not valid values for modelling so will remove them now
#   glimpse()
#
# # Visualise spatial patterns
# preds <- readRDS(paste0("data/", park, "/spatial/rasters/", name, "_bathymetry-derivatives.rds"))
# plot(preds) ##HE change aspect palette to cyclic
# names(preds)
#
# ggplot() +
#   geom_spatraster(data = preds, aes(fill = geoscience_depth)) +
#   geom_point(data = tidy_length,
#              aes(x = longitude_dd, y = latitude_dd, size = count, colour = I(if_else(count == 0, "white", "darkblue"))),
#              show.legend = F) +
#   facet_wrap(~response) +
#   theme_classic() +
#   coord_sf()
#
# saveRDS(tidy_length, file = paste0("data/", park, "/tidy/", name, "_tidy-length.rds"))

# Create df for calculating B20
length_b20 <- readRDS(paste0("data/", park, "/raw/_length-with-zeros.RDS")) %>%
  dplyr::select(campaignid, sample, family, genus, species, length_mm, count) %>%
  mutate(length_cm = length_mm / 10) %>%
  left_join(CheckEM::australia_life_history) %>%
  dplyr::mutate(scientific_name = paste(genus, species, sep = " ")) %>%
  glimpse() ##HE 8 metre KGW?

# Calculate mass from lengths
mass_b20 <- length_b20 %>%
  # Convert to TL if needed
  dplyr::mutate(adj_length = case_when(
    fb_length_weight_measure %in% "FL" ~ length_cm, # Leave as FL
    # Convert into Total Length to match Length-Weight calculations
    fb_length_weight_measure %in% "TL" & fb_ll_equation_type %in% "FL → TL" ~ (length_cm * fb_b_ll) + fb_a_ll, # Forwards converion
    fb_length_weight_measure %in% "TL" & fb_ll_equation_type %in% "TL → FL" ~ (length_cm - fb_a_ll) / fb_b_ll  # Inverse conversion
  )) %>% # Check for NAs: messages below
  dplyr::mutate(mass_g = (adj_length ^ fb_b) * fb_a * count) %>%
  dplyr::left_join(metadata) %>%
  dplyr::left_join(metadata_bathy_derivatives) %>%
  glimpse()

##HE The below is to work out which species are missing fishbase data
# message(paste(length(which(!is.na(mass_b20$length_cm))), "measured lengths in data"))
# message(paste(length(which(!is.na(mass_b20$adj_length))), "adjusted lengths in data"))
# message(paste(length(which(!is.na(mass_b20$length_cm))) - length(which(!is.na(mass_b20$adj_length))),
#               "measured lengths not converted to adjusted (missing)"))
#
# message(paste(length(which(!is.na(mass_b20$length_cm) &
#                              is.na(mass_b20$fb_length_weight_measure))), "because fb_length_weight_measure is NA"))
# message(paste(length(which(!is.na(mass_b20$length_cm) &
#                              is.na(mass_b20$fb_ll_equation_type) &
#                              mass_b20$fb_length_weight_measure == "TL")),
#               "because fb_length_weight_measure = TL (good) but fb_ll_equation_type is missing"))
# message(paste(length(which(mass_b20$fb_length_weight_measure == "SL" & !is.na(mass_b20$length_cm))),
#               "because fb_length_weight_measure is SL (not FL or TL)"))
#
# message(paste("These 3x reasons added =", length(which(!is.na(mass_b20$length_cm) &
#                                                          is.na(mass_b20$fb_length_weight_measure))) +
#                 length(which(!is.na(mass_b20$length_cm) &
#                                is.na(mass_b20$fb_ll_equation_type) &
#                                mass_b20$fb_length_weight_measure == "TL")) +
#                 length(which(mass_b20$fb_length_weight_measure == "SL" & !is.na(mass_b20$length_cm))),
#               "accounting for all missing adjusted lengths"))
#
# missing_info <- mass_b20 %>%
#   dplyr::filter(class %in% "Actinopterygii") %>%
#   dplyr::filter(!order %in% c("Anguilliformes", "Ophidiiformes", "Notacanthiformes","Tetraodontiformes","Syngnathiformes",
#                               "Synbranchiformes", "Stomiiformes", "Siluriformes", "Saccopharyngiformes", "Osmeriformes",
#                               "Osteoglossiformes", "Lophiiformes", "Lampriformes", "Beloniformes", "Zeiformes", "Carangiformes")) %>%
#   dplyr::filter(!length_cm < 20) %>%
#   filter(is.na(adj_length)) %>%
#   distinct(scientific_name, australian_common_name, .keep_all = TRUE) %>%
#   select(family, genus, species, australian_common_name, fb_length_weight_measure,
#          fb_a, fb_b, fb_ll_equation_type)
# write.csv(missing_info, file = paste0("data/", park, "/tidy/", name, "_b20_missing_info.csv"))

metadata_b20 <- length_b20 %>%
  distinct(campaignid, sample) %>%
  glimpse()

# Calculate B20* for each sample
tidy_b20 <- mass_b20 %>% ##HE this needs tweaking, not working 100% because some lengths have NA mass (fix in mass_b20)
  dplyr::filter(class %in% "Actinopterygii") %>%
  dplyr::filter(!order %in% c("Anguilliformes", "Ophidiiformes", "Notacanthiformes","Tetraodontiformes","Syngnathiformes",
                              "Synbranchiformes", "Stomiiformes", "Siluriformes", "Saccopharyngiformes", "Osmeriformes",
                              "Osteoglossiformes", "Lophiiformes", "Lampriformes", "Beloniformes", "Zeiformes", "Carangiformes")) %>%
  dplyr::filter(!length_cm < 20) %>%
  dplyr::group_by(campaignid, sample) %>%
  dplyr::summarise(count = sum(mass_g)) %>%
  ungroup() %>%
  right_join(metadata_b20) %>%
  dplyr::mutate(count = ifelse(is.na(count), 0, count)) %>%
  dplyr::mutate(response = "b20") %>%
  left_join(metadata) %>%
  left_join(metadata_bathy_derivatives) %>%
  left_join(benthos) %>%
  dplyr::filter(!is.na(reef), ##HE need to remove outliers
                !is.na(geoscience_aspect)) %>%
  dplyr::glimpse()
# Check number of samples that are > 0
nrow(filter(tidy_b20, count > 0))/nrow(tidy_b20)

saveRDS(tidy_b20, file = paste0("data/", park, "/tidy/", name, "_tidy-b20.rds"))
