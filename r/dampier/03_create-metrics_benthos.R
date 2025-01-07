###
# Project: NESP 4.20 - Marine Park Dashboard reporting
# Data:    Habitat data synthesis
# Task:    Combine and format benthos data for full subsets modelling
# Author:  Claude Spencer
# Date:    June 2024
###

rm(list = ls())

library(tidyverse)

# Set the study name
name <- "DampierAMP"
park <- "dampier"

# Using dummy data
# benthosold <- read.csv("data/geographe/raw/temp/2007-2014-Geographe-stereo-BRUVs_broad.habitat.csv") %>%
#   dplyr::select(campaignid, sample, starts_with("broad")) %>%
#   dplyr::rename(macroalgae = broad.macroalgae, seagrasses = broad.seagrasses,
#                 sand = broad.unconsolidated, rock = broad.consolidated, total_pts = broad.total.points.annotated) %>%
#   dplyr::mutate(sessile_invertebrates = broad.sponges + broad.stony.corals,
#                 reef = sessile_invertebrates + macroalgae) %>%
#   dplyr::select(-c(starts_with("broad"))) %>%
#   glimpse()

benthosboss <- readRDS(paste0("data/", park, "/raw/", name, "_BOSS_benthos.RDS")) %>%
  dplyr::rename(sample = period)
benthosbruv <- readRDS(paste0("data/", park, "/raw/", name, "_BRUVs_benthos.RDS")) %>%
  dplyr::rename(sample = opcode)

benthos <- bind_rows(benthosboss, benthosbruv) %>%
  dplyr::select(campaignid, sample, level_2, level_3, count) %>%
  dplyr::mutate(habitat = case_when(level_2 %in% "Macroalgae" ~ "macroalgae",
                                    level_3 %in% "Unconsolidated (soft)" ~ "sand",
                                    level_3 %in% "Consolidated (hard)" ~ "rock",
                                    level_2 %in% "Cnidaria" ~ "sessile_invertebrates",
                                    level_2 %in% "Sessile invertebrates" ~ "sessile_invertebrates",
                                    level_2 %in% "Sponges" ~ "sessile_invertebrates")) %>%
  dplyr::group_by(campaignid, sample, habitat) %>%
  summarise(count = sum(count)) %>%
  ungroup() %>%
  pivot_wider(names_from = habitat, values_from = count, values_fill = 0) %>%
  dplyr::mutate(total_pts = rowSums(.[3:ncol(.)], na.rm = T),
                reef = macroalgae + rock + sessile_invertebrates) %>%
  glimpse()

length(unique(benthos$sample))

saveRDS(benthos, paste0("data/", park, "/tidy/", name, "_benthos-count.RDS"))
