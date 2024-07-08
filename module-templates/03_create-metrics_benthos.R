###
# Project: NESP 4.20 - Marine Park Dashboard reporting
# Data:    Habitat data synthesis
# Task:    Combine and format benthos data for full subsets modelling
# Author:  Claude Spencer
# Date:    June 2024
###

# Set the study name
name <- "GeographeAMP"

# Using GA synthesis format - levels - https://dev.globalarchive.org/api/data/AnnotationSubject/7433/

# benthos <- readRDS("data/geographe/raw/", name, "_benthos-count.rds")


# Using dummy data
benthos <- read.csv("data/geographe/raw/temp/2007-2014-Geographe-stereo-BRUVs_broad.habitat.csv") %>%
  dplyr::select(campaignid, sample, starts_with("broad")) %>%
  dplyr::rename(macroalgae = broad.macroalgae, seagrasses = broad.seagrasses,
                sand = broad.unconsolidated, rock = broad.consolidated, total_pts = broad.total.points.annotated) %>%
  dplyr::mutate(sessile_invertebrates = broad.sponges + broad.stony.corals,
                reef = sessile_invertebrates + macroalgae) %>%
  dplyr::select(-c(starts_with("broad"))) %>%
  glimpse()

saveRDS(benthos, paste0("data/geographe/tidy/", name, "_benthos-count.RDS"))
