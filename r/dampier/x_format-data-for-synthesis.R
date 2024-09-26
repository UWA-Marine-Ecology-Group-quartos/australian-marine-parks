# Script to format raw EM and TM exports into GA synthesis format

library(tidyverse)
library(CheckEM)

# BRUV synthesis

## Metadata
metadata <- read_metadata("data/dampier/raw/temp/BRUVs", method = "BRUVs") %>%
  clean_names() %>%
  dplyr::select(campaignid, opcode, longitude_dd, latitude_dd, date_time, depth_m, status, site, location, successful_count, successful_length,
                successful_habitat_forward, successful_habitat_backward,
                observer_count, observer_length, observer_habitat_forward, observer_habitat_backward) %>%
  glimpse()

## Count
count <- read_points("data/dampier/raw/temp/BRUVs", method = "BRUVs") %>%
  clean_names() %>%
  right_join(metadata) %>% # Join back samples with no fish
  dplyr::filter(successful_count %in% "Yes") %>%
  dplyr::select(campaignid, opcode, family, genus, species, number) %>%
  group_by(campaignid, opcode, family, genus, species) %>%
  summarise(count = sum(number)) %>%
  ungroup() %>%
  dplyr::group_by(campaignid, opcode, family, genus, species) %>%
  dplyr::slice(which.max(count)) %>%
  dplyr::ungroup() %>%
  tidyr::complete(nesting(campaignid, opcode), nesting(family, genus, species)) %>%
  replace_na(list(count = 0)) %>%
  dplyr::filter(!is.na(family)) %>% # If you have samples with no fish, then complete will add NAs in
  dplyr::mutate(scientific = paste(family, genus, species)) %>%
  left_join(metadata) %>%
  dplyr::filter(successful_count %in% "Yes") %>%
  glimpse()

## Length
length <- read_em_length("data/dampier/raw/temp/BRUVs") %>%
  clean_names() %>%
  right_join(metadata) %>%
  dplyr::filter(successful_length %in% "Yes", !is.na(family)) %>%
  dplyr::select(campaignid, opcode, family, genus, species, length_mm, number) %>%
  uncount(number) %>%
  dplyr::mutate(number = 1) %>%
  tidyr::complete(nesting(campaignid, opcode), nesting(family, genus, species)) %>%
  replace_na(list(number = 0)) %>%
  dplyr::filter(!is.na(family)) %>% # If you have samples with no fish, then complete will add NAs in
  dplyr::mutate(scientific = paste(family, genus, species)) %>%
  left_join(metadata) %>%
  dplyr::filter(successful_length %in% "Yes") %>%
  glimpse()

## Benthos
benthos <- read_TM("data/dampier/raw/temp/BOSS", "period") %>%
  dplyr::filter(relief_annotated %in% "no") %>%
  dplyr::select(campaignid, sample, level_2, level_3, scientific) %>%
  glimpse()


## Benthos relief


