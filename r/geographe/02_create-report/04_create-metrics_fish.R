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

spp_list <- count %>%
  dplyr::distinct(scientific_name, .keep_all = T) %>%
  dplyr::select(family, genus, species, scientific_name)

write.csv(spp_list, file = paste0("data/", park, "/tidy/", name, "_species_list.csv"))

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

spp_list <- count %>%
  dplyr::distinct(scientific_name, .keep_all = T) %>%
  dplyr::select(family, genus, species, scientific_name)

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
b20_length <- readRDS(paste0("data/", park, "/raw/_length-with-zeros.RDS")) %>%
  dplyr::select(campaignid, sample, family, genus, species, length_mm, count) %>%
  mutate(length_cm = length_mm / 10) %>%
  left_join(CheckEM::australia_life_history) %>%
  dplyr::mutate(scientific_name = paste(genus, species, sep = " ")) %>%
  glimpse() ##HE 8 metre KGW?

# 1) Calculate mass from lengths
biomass <- b20_length %>%
  mutate(
    adj_length = case_when(
      count == 0 ~ NA_real_,  # length irrelevant for absences
      fb_length_weight_measure %in% "FL" ~ length_cm,
      fb_length_weight_measure %in% "TL" & fb_ll_equation_type %in% "FL → TL" ~ (length_cm * fb_b_ll) + fb_a_ll,
      fb_length_weight_measure %in% "TL" & fb_ll_equation_type %in% "TL → FL" ~ (length_cm - fb_a_ll) / fb_b_ll,
      TRUE ~ NA_real_
    ),
    mass_g = case_when(
      count == 0 ~ 0,  # absences are zero biomass
      !is.na(adj_length) & !is.na(fb_a) & !is.na(fb_b) ~ (adj_length ^ fb_b) * fb_a * count,
      TRUE ~ NA_real_  # present but cannot compute biomass -> NA
    )
  ) %>%
  left_join(metadata, by = c("campaignid","sample")) %>%
  left_join(metadata_bathy_derivatives,
            by = c("campaignid","sample","longitude_dd","latitude_dd","status","year"))

# 2) Define inclusion + b20-specific biomass
b20_mass <- biomass %>%
  mutate(
    include_b20 = class == "Actinopterygii" &
      (is.na(rls_water_column) | rls_water_column != "pelagic non-site attached" ) &
      (count == 0 | (length_cm >= 20 & length_cm <= 800)),

    b20_mass_g = case_when(
      count == 0 ~ 0,                        # absence stays zero
      !include_b20 ~ 0,                      # excluded taxa/sizes contribute 0 to B20
      include_b20 & !is.na(mass_g) ~ mass_g, # included + computable biomass
      include_b20 & is.na(mass_g) ~ NA_real_ # included but missing -> NA (flag)
    )
  ) %>%
  filter(b20_mass_g <= 30000 | is.na(b20_mass_g)) # 190cm Centroberyx lineatus

b20_mass_check <- b20_mass %>%
  select(sample, year, scientific_name, b20_mass_g, length_cm)

sp_watercol <- b20_mass %>%
  distinct(scientific_name, rls_water_column)

# 3) Per sample × species B20 biomass
b20_by_sample <- b20_mass %>%
  group_by(year, sample, scientific_name) %>%
  summarise(
    present_n = sum(count, na.rm = TRUE),

    # If species never present on that BRUV => 0
    b20_sample = if (sum(count, na.rm = TRUE) == 0) 0
    # If present, but every INCLUDED present record has NA biomass => NA
    else if (all(is.na(b20_mass_g[count > 0 & include_b20]))) NA_real_
    # Otherwise sum included biomass (excluded rows are already 0)
    else sum(b20_mass_g, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  left_join(sp_watercol, by = "scientific_name")


# 4) Ensure every BRUV × species exists (zeros for absences)
all_samples <- metadata %>%
  distinct(year, sample)

b20_by_sample_complete <- b20_by_sample %>%
  # filter(!sample %in% "GB-BV-125") %>% # heaps of huge pinkies
  right_join(all_samples, by = c("year","sample")) %>%
  tidyr::complete(year, sample, scientific_name,
                  fill = list(b20_sample = 0, present_n = 0))

# 5) Species summaries per year
b20_species <- b20_by_sample_complete %>%
  group_by(year, scientific_name) %>%
  summarise(
    b20 = mean(b20_sample, na.rm = TRUE),
    sd  = sd(b20_sample, na.rm = TRUE),
    n   = sum(!is.na(b20_sample)),
    se  = sd / sqrt(n),
    .groups = "drop"
  ) %>%
  left_join(sp_watercol, by = "scientific_name")

saveRDS(b20_species, file = paste0("data/", park, "/tidy/", name, "_b20-species.rds"))

##HE The below is to work out which species are missing fishbase data
# message(paste(length(which(!is.na(biomass$length_cm))), "measured lengths in data"))
# message(paste(length(which(!is.na(biomass$adj_length))), "adjusted lengths in data"))
# message(paste(length(which(!is.na(biomass$length_cm))) - length(which(!is.na(biomass$adj_length))),
#               "measured lengths not converted to adjusted (missing)"))
#
# message(paste(length(which(!is.na(biomass$length_cm) &
#                              is.na(biomass$fb_length_weight_measure))), "because fb_length_weight_measure is NA"))
# message(paste(length(which(!is.na(biomass$length_cm) &
#                              is.na(biomass$fb_ll_equation_type) &
#                              biomass$fb_length_weight_measure == "TL")),
#               "because fb_length_weight_measure = TL (good) but fb_ll_equation_type is missing"))
# message(paste(length(which(biomass$fb_length_weight_measure == "SL" & !is.na(biomass$length_cm))),
#               "because fb_length_weight_measure is SL (not FL or TL)"))
#
# message(paste("These 3x reasons added =", length(which(!is.na(biomass$length_cm) &
#                                                          is.na(biomass$fb_length_weight_measure))) +
#                 length(which(!is.na(biomass$length_cm) &
#                                is.na(biomass$fb_ll_equation_type) &
#                                biomass$fb_length_weight_measure == "TL")) +
#                 length(which(biomass$fb_length_weight_measure == "SL" & !is.na(biomass$length_cm))),
#               "accounting for all missing adjusted lengths"))
#
# missing_info <- biomass %>%
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

b20_metadata <- biomass %>%
  distinct(year, sample) %>%
  glimpse()

# Calculate B20* for each sample
b20_tidy <- biomass %>% ##HE this needs tweaking, not working 100% because some lengths have NA mass (fix in biomass)
  mutate(
    include_b20 = class == "Actinopterygii" &
      # exclude pelagic non-site attached
      (is.na(rls_water_column) | rls_water_column != "pelagic non-site attached") &
      # B20 size rule, but keep absences
      (count == 0 | (length_cm >= 20 & length_cm <= 800)),

    b20_mass_g = case_when(
      count == 0 ~ 0,                        # absences
      !include_b20 ~ 0,                      # excluded rows contribute zero to B20
      include_b20 & !is.na(mass_g) ~ mass_g, # included + computable
      include_b20 & is.na(mass_g) ~ NA_real_ # included but missing -> NA (flag)
    )
  ) %>%
  group_by(year, sample) %>%
  summarise(
    # if you want a diagnostic:
    n_present = sum(count > 0, na.rm = TRUE),
    n_present_included = sum(count > 0 & include_b20, na.rm = TRUE),
    n_missing_mass_included = sum(count > 0 & include_b20 & is.na(b20_mass_g), na.rm = TRUE),

    # sample-level B20 biomass
    count = if (sum(count, na.rm = TRUE) == 0) 0
    else if (all(is.na(b20_mass_g[count > 0 & include_b20]))) NA_real_
    else sum(b20_mass_g, na.rm = TRUE),

    .groups = "drop"
  ) %>%
  right_join(b20_metadata, by = c("year","sample")) %>%   # keep all BRUVs
  mutate(
    count = ifelse(is.na(count), 0, count),
    response = "b20"
  ) %>%
  left_join(metadata, by = c("year","sample")) %>%
  left_join(metadata_bathy_derivatives,
            by = c("campaignid","sample","longitude_dd","latitude_dd","status","year")) %>%
  left_join(benthos, by = c("campaignid","sample","status","year")) %>%
  filter(!is.na(reef),
         !is.na(geoscience_aspect)) %>%
  glimpse()

# Check number of samples that are > 0
nrow(filter(b20_tidy, count > 0))/nrow(b20_tidy)

saveRDS(b20_tidy, file = paste0("data/", park, "/tidy/", name, "_tidy-b20.rds"))
