###
# Project: NESP 4.20 - Marine Park Dashboard reporting
# Data:    Marine Park monitoring data syntheses
# Task:    Call GlobalArchive API to download data syntheses
# Author:  Claude Spencer
# Date:    June 2024
###
rm(list = ls())

# Load libraries needed -----
library(httr)
library(tidyverse)
library(RJSONIO)
library(devtools)
# devtools::install_github("GlobalArchiveManual/CheckEM") # If there has been any updates to the package then CheckEM will install
library(CheckEM)

name <- "GeographeAMP"

load("secrets/token.rda")

file.sources <- list.files(pattern = "*.R", path = "temp functions/", full.names = T)
sapply(file.sources, source, .GlobalEnv)

# API call for metadata ----
metadata <- ga_api_metadata(token, synthesis_id = 14)

saveRDS(metadata, paste0("data/geographe/raw/", name, "_metadata.RDS"))

# API call for count data ----
count <- ga_api_count(token, synthesis_id = 14) %>%
  dplyr::select(sample_url, family, genus, species, count) %>%
  glimpse()

# API call for length data ----
length <- ga_api_length(token, synthesis_id = 14) %>%
  dplyr::select(sample_url, family, genus, species, length_mm, number) %>%
  glimpse()

# API call for benthos/habitat data ----
habitat <- ga_api_habitat(token, synthesis_id = 14) %>%
  dplyr::select(sample_url, count, starts_with("level"), family, genus, species) %>%
  glimpse()

# Add in zeros where species are not present in the count data ----
# NOTE: this creates a very large file, it also takes quite a while to run

count_metadata <- metadata %>%
  dplyr::filter(successful_count %in% TRUE)

length(unique(count_metadata$sample)) # 297 successful samples
length(unique(count$sample_url)) # 297 samples in the count

count_wide <- count %>%
  dplyr::full_join(count_metadata) %>%
  dplyr::filter(successful_count %in% TRUE) %>% # now has correct number of samples
  dplyr::select(campaignid, sample, family, genus, species, count) %>%
  tidyr::complete(nesting(campaignid, sample), nesting(family, genus, species)) %>%
  tidyr::replace_na(list(count = 0)) %>%
  dplyr::group_by(campaignid, sample, family, genus, species) %>%
  dplyr::summarise(count = sum(count)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(scientific = paste(family, genus, species, sep = " "))%>%
  dplyr::select(campaignid, sample, scientific, count)%>%
  spread(scientific, count, fill = 0)

length(unique(count_wide$sample))

count_families <- count %>%
  dplyr::mutate(scientific = paste(family, genus, species, sep = " ")) %>%
  dplyr::ungroup() %>%
  dplyr::select(c(family, genus, species, scientific)) %>%
  dplyr::distinct()

complete_count <- count_wide %>%
  pivot_longer(names_to = "scientific", values_to = "count", cols = 3:ncol(.)) %>%
  dplyr::inner_join(count_families, by = c("scientific")) %>%
  dplyr::full_join(count_metadata) %>%
  dplyr::filter(successful_count %in% TRUE) %>%
  glimpse()

# Save complete count (zeros added where a species is not present)
saveRDS(complete_count, paste0("data/geographe/raw/", name, "_complete_count.RDS"))

# Add in zeros where species are not present in the count data ----
# NOTE: this creates a very large file, it also takes quite a while to run

length_metadata <- metadata %>%
  dplyr::filter(successful_length %in% TRUE)

length(unique(length_metadata$sample)) # 239 samples
length(unique(length$sample_url)) # 154 samples in the length - check this as seems a bit wrong

complete_length <- length %>%
  dplyr::mutate(number = as.numeric(number)) %>%
  dplyr::filter(!is.na(number)) %>%
  tidyr::uncount(number) %>%
  dplyr::mutate(number = 1) %>%
  dplyr::full_join(length_metadata) %>%
  dplyr::filter(successful_length %in% TRUE) %>%
  dplyr::select(campaignid, sample, family, genus, species, length_mm, number) %>%
  tidyr::complete(nesting(campaignid, sample), nesting(family, genus, species)) %>%
  replace_na(list(number = 0)) %>%
  ungroup() %>%
  dplyr::mutate(length_mm = as.numeric(length_mm)) %>%
  full_join(length_metadata) %>%
  dplyr::filter(!is.na(number)) %>%
  dplyr::filter(successful_length %in% TRUE) %>%
  glimpse()

length(unique(complete_length$sample))

# Save complete count (zeros added where a species is not present)
saveRDS(complete_length, paste0("data/geographe/raw/", name, "_complete_length.RDS"))

# Tidy and join habitat with metadata
tidy_habitat <- habitat %>%
  left_join(metadata) %>% # Successful habitat columns not filled for this synthesis/campaign
  glimpse()
unique(tidy_habitat$level_2)

saveRDS(tidy_habitat, paste0("data/geographe/raw/", name, "_benthos.RDS"))
