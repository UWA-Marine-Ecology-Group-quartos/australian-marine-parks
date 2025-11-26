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
name <- "GeographeAMP"
park <- "geographe"

# Using dummy data
# benthosold <- read.csv("data/geographe/raw/temp/2007-2014-Geographe-stereo-BRUVs_broad.habitat.csv") %>%
#   dplyr::select(campaignid, sample, starts_with("broad")) %>%
#   dplyr::rename(macroalgae = broad.macroalgae, seagrasses = broad.seagrasses,
#                 sand = broad.unconsolidated, rock = broad.consolidated, total_pts = broad.total.points.annotated) %>%
#   dplyr::mutate(sessile_invertebrates = broad.sponges + broad.stony.corals,
#                 reef = sessile_invertebrates + macroalgae) %>%
#   dplyr::select(-c(starts_with("broad"))) %>%
#   glimpse()

benthos <- readRDS(paste0("data/", park, "/raw/", name, "_benthos_combined.RDS")) %>%
  dplyr::select(campaignid, sample, macroalgae, seagrasses,
                sand = unconsolidated, rock = consolidated,
                sessile_invertebrates, total_pts = total_points_annotated) %>%
  dplyr::mutate(reef = macroalgae + rock + sessile_invertebrates) %>%
  glimpse()

length(unique(benthos$sample))

saveRDS(benthos, paste0("data/", park, "/tidy/", name, "_benthos-count_combined.RDS"))
