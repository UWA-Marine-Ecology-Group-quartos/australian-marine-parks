# Checking habitat data exported from TransectMeasure

# https://globalarchivemanual.github.io/CheckEM/articles/r-workflows/check-habitat.html

# install.packages('remotes')
library('remotes')
options(timeout=9999999)
# remotes::install_github("GlobalArchiveManual/CheckEM")
library(CheckEM)
library(tidyverse)
library(ggplot2)
library(ggbeeswarm)
library(leaflet)
library(leaflet.minicharts)
library(RColorBrewer)
library(here)
library(tidyverse)

park <- "geographe"
name <- "GeographeAMP"

#Couldn't find the .csv so created this from fish api call
metadata1 <- readRDS(paste0("data/", park, "/raw/metadata.RDS")) %>%
  filter(campaignid %in% "2024-04_Geographe_stereo-BRUVs") %>%
  glimpse()

write.csv(metadata1, file = paste0("data/", park, "/raw/metadata.csv"))

metadata <- read_metadata(here::here(paste0("data/", park, "/raw/"))) %>%
  dplyr::select(campaignid, sample, longitude_dd, latitude_dd, date_time, location, site, depth_m, successful_count, successful_length, successful_habitat_forward, successful_habitat_backward) %>%
  glimpse()

saveRDS(metadata, file = here::here(paste0("data/", park, "/tidy/", name, "_metadata.rds")))

points <- read_TM(here::here(paste0("data/", park, "/raw/2024_habitat/")),
                  sample = "opcode")

habitat <- points %>%
  dplyr::filter(relief_annotated %in% "No") %>%
  dplyr::select(campaignid, sample, starts_with("level"), scientific) %>%
  glimpse()

relief <- points %>%
  dplyr::filter(relief_annotated %in% "Yes") %>%
  dplyr::select(campaignid, sample, starts_with("level"), scientific) %>%
  glimpse()

num.points <- 20

wrong.points.habitat <- habitat %>%
  group_by(campaignid, sample) %>%
  summarise(points.annotated = n()) %>%
  left_join(metadata) %>%
  dplyr::mutate(expected = case_when(successful_habitat_forward %in% "Yes" & successful_habitat_backward %in% "Yes" ~ num.points * 2, successful_habitat_forward %in% "Yes" & successful_habitat_backward %in% "No" ~ num.points * 1, successful_habitat_forward %in% "No" & successful_habitat_backward %in% "Yes" ~ num.points * 1, successful_habitat_forward %in% "No" & successful_habitat_backward %in% "No" ~ num.points * 0)) %>%
  dplyr::filter(!points.annotated == expected) %>%
  glimpse()

wrong.points.relief <- relief %>%
  group_by(campaignid, sample) %>%
  summarise(points.annotated = n()) %>%
  left_join(metadata) %>%
  dplyr::mutate(expected = case_when(successful_habitat_forward %in% "Yes" & successful_habitat_backward %in% "Yes" ~ num.points * 2, successful_habitat_forward %in% "Yes" & successful_habitat_backward %in% "No" ~ num.points * 1, successful_habitat_forward %in% "No" & successful_habitat_backward %in% "Yes" ~ num.points * 1, successful_habitat_forward %in% "No" & successful_habitat_backward %in% "No" ~ num.points * 0)) %>%
  dplyr::filter(!points.annotated == expected) %>%
  glimpse()

habitat.missing.metadata <- anti_join(habitat, metadata, by = c("campaignid", "sample")) %>%
  glimpse()

metadata.missing.habitat <- anti_join(metadata, habitat, by = c("campaignid", "sample")) %>%
  glimpse()
