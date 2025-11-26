# Format habitat data

# https://globalarchivemanual.github.io/CheckEM/articles/r-workflows/format-visualise-habitat.html

rm(list = ls())

library('remotes')
options(timeout=9999999)
# remotes::install_github("GlobalArchiveManual/CheckEM")
library(CheckEM)
library(tidyverse)
library(ggbeeswarm)
library(RColorBrewer)
library(leaflet)
library(leaflet.minicharts)
library(here)

park <- "geographe"
name <- "GeographeAMP_2024"

habitat <- readRDS(here::here(paste0("data/", park, "/tidy/", name, "_habitat.RDS"))) %>%
  dplyr::mutate(habitat = case_when(level_2 %in% "Macroalgae" ~ level_2, level_2 %in% "Seagrasses" ~ level_2, level_2 %in% "Substrate" & level_3 %in% "Consolidated (hard)" ~ level_3, level_2 %in% "Substrate" & level_3 %in% "Unconsolidated (soft)" ~ level_3,  level_2 %in% "Sponges" ~ "Sessile invertebrates", level_2 %in% "Sessile invertebrates" ~ level_2, level_2 %in% "Bryozoa" ~ "Sessile invertebrates", level_2 %in% "Cnidaria" ~ "Sessile invertebrates")) %>%
  dplyr::select(campaignid, sample, habitat, count) %>%
  group_by(campaignid, sample, habitat) %>%
  dplyr::summarise(count = sum(count)) %>%
  dplyr::mutate(total_points_annotated = sum(count)) %>%
  ungroup() %>%
  pivot_wider(names_from = "habitat", values_from = "count", values_fill = 0) %>%
  dplyr::mutate(reef = Macroalgae + Seagrasses + `Sessile invertebrates` + `Consolidated (hard)`) %>%
  pivot_longer(cols = c("Macroalgae", "Seagrasses", "Sessile invertebrates", "Consolidated (hard)", "Unconsolidated (soft)", "reef"), names_to = "habitat", values_to = "count") %>%
  glimpse()

tidy.habitat <- habitat %>%
  clean_names() %>%
  glimpse()

plot.habitat <- tidy.habitat %>%
  pivot_wider(names_from = "habitat", values_from = "count", names_prefix = "broad.") %>%
  dplyr::rename(macroalgae = broad.Macroalgae, seagrasses = broad.Seagrasses,
                sessile_invertebrates = 'broad.Sessile invertebrates', consolidated = 'broad.Consolidated (hard)',
                unconsolidated = 'broad.Unconsolidated (soft)', reef = broad.reef) %>%
  glimpse()

saveRDS(plot.habitat, paste0("data/", park, "/raw/", name, "_benthos.RDS"))
