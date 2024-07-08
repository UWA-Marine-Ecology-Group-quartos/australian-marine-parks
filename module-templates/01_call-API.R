###
# Project: NESP 4.20 - Marine Park Dashboard reporting
# Data:    Marine Park monitoring data syntheses
# Task:    Call GlobalArchive API to download data syntheses
# Author:  Claude Spencer
# Date:    June 2024
###

# Load libraries needed -----
library(httr)
library(tidyverse)
library(RJSONIO)
library(devtools)
# devtools::install_github("GlobalArchiveManual/CheckEM") # If there has been any updates to the package then CheckEM will install
library(CheckEM)

username <- "public"
password <- "sharedaccess"

test <- ga_api_habitat(username, password, synthesis_id = 20)
glimpse(test)


# First, API call to the GlobalArchive species list to join to the count and length data ----
species_list <- CheckEM::ga_api_species_list(username, password) # this needs to be called "species_list" to work later in the following functions

# API call for metadata ----
metadata_2019 <- CheckEM::ga_api_metadata(username, password, synthesis_id = 18)
metadata_2024 <- CheckEM::ga_api_metadata(username, password, synthesis_id = 19)

# Combine metadata ----
metadata <- bind_rows(metadata_2019, metadata_2024) %>% glimpse()

# API call for count data ----
count_2019 <- CheckEM::ga_api_count(username, password, synthesis_id = 18)
count_2024 <- CheckEM::ga_api_count(username, password, synthesis_id = 19)

# Combine count ----
count <- bind_rows(count_2019, count_2024) %>%
  dplyr::select(sample, family, genus, species, count) %>%
  glimpse()

# API call for length data ----
length_2019 <- CheckEM::ga_api_length(username, password, synthesis_id = 18)
length_2024 <- CheckEM::ga_api_length(username, password, synthesis_id = 19)

# Combine length ----
length <- bind_rows(length_2019, length_2024)  %>%
  dplyr::select(sample, family, genus, species, length, number) %>%
  glimpse()

# Save data ----
# You can either save the data as a csv file or an RDS file
# (RDS files are much faster to save/read in and preserve any column formatting)

# Save as CSVs
write.csv(metadata, "data/raw/australian-synthesis_metadata.csv", row.names = F)
write.csv(count, "data/raw/australian-synthesis_count.csv", row.names = F)
write.csv(length, "data/raw/australian-synthesis_length.csv", row.names = F)

# Save as RDS
saveRDS(metadata, "data/raw/australian-synthesis_metadata.RDS")
saveRDS(count, "data/raw/australian-synthesis_count.RDS")
saveRDS(length, "data/raw/australian-synthesis_length.RDS")

# Remove files no longer needed from memory ----
rm(metadata_2019, metadata_2024, count_2019, count_2024, length_2019, length_2024)

# Add in zeros where species are not present in the count data ----
# NOTE: this creates a very large file, it also takes quite a while to run

count_metadata <- metadata %>%
  dplyr::filter(successful_count %in% TRUE)

length(unique(count_metadata$sample)) # 27,706 successful samples
length(unique(count$sample)) # 27,414 samples in the count, will need to add in the zeros where no fish were observed

count_wide <- count %>%
  dplyr::full_join(count_metadata) %>%
  dplyr::filter(successful_count %in% TRUE) %>% # now has correct number of samples
  dplyr::select(campaign, sample, family, genus, species, count) %>%
  tidyr::complete(nesting(campaign, sample), nesting(family, genus, species)) %>%
  tidyr::replace_na(list(count = 0)) %>%
  dplyr::group_by(campaign, sample, family, genus, species) %>%
  dplyr::summarise(count = sum(count)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(scientific = paste(family, genus, species, sep = " "))%>%
  dplyr::select(campaign, sample, scientific, count)%>%
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
saveRDS(complete_count, "data/raw/australian-synthesis_complete_count.RDS")

# Add in zeros where species are not present in the count data ----
# NOTE: this creates a very large file, it also takes quite a while to run

length_metadata <- metadata %>%
  dplyr::filter(successful_length %in% TRUE)

length(unique(length_metadata$sample)) # 19,698 successful samples
length(unique(length$sample)) # 17,449 samples in the length, will need to add in the zeros where no fish were observed
length(unique(length$scientific))

complete_length <- length %>%
  dplyr::mutate(number = as.numeric(number)) %>%
  dplyr::filter(!is.na(number)) %>%
  tidyr::uncount(number) %>%
  dplyr::mutate(number = 1) %>%
  dplyr::full_join(length_metadata) %>%
  dplyr::filter(successful_length %in% TRUE) %>%
  dplyr::select(campaign, sample, family, genus, species, length, number) %>%
  tidyr::complete(nesting(campaign, sample), nesting(family, genus, species)) %>%
  replace_na(list(number = 0)) %>%
  ungroup() %>%
  dplyr::mutate(length_mm = as.numeric(length)) %>%
  full_join(length_metadata) %>%
  dplyr::filter(!is.na(number)) %>%
  dplyr::filter(successful_length %in% TRUE) %>%
  glimpse()

length(unique(complete_length$sample))

# Save complete count (zeros added where a species is not present)
saveRDS(complete_length, "data/raw/australian-synthesis_complete_length.RDS")

