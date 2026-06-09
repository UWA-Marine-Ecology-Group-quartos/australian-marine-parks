###
# Project: NESP 4.20 - Marine Park Dashboard reporting
# Data:    Marine Park monitoring data syntheses
# Task:    Call GlobalArchive API to download data syntheses
# Author:  Claude Spencer
# Date:    June 2024
###
rm(list = ls())

script_dir <- dirname(
  rstudioapi::getActiveDocumentContext()$path
)

config <- yaml::read_yaml(
  file.path(script_dir, "00_config.yml")
)

name <- config$name
park <- config$park

# Load libraries needed -----

# TODO Run these once or as required:
# remotes::install_github("GlobalArchiveManual/CheckEM")
# CheckEM::ga_api_set_token()

library(tidyverse)
library(CheckEM)
options(timeout=600) # increase if more time needed for large data downloads

# Load the saved token
token <- readRDS("secrets/api_token.RDS")

# Load the metadata, count and length ----
CheckEM::ga_api_all_data(synthesis_id = "47", # TODO change synthesis ID for different project
                         token = token,
                         dir = paste0("data/", park, "/raw/"), # Check the directory
                         include_zeros = TRUE)

metadata <- readRDS(paste0("data/", park, "/raw/metadata.RDS")) %>%
  mutate(year = as.factor(year(date_time)),
         status = as.factor(status)) %>%
  glimpse()

saveRDS(metadata, paste0("data/", park, "/raw/metadata.RDS"))

##HE Use below code when 2024 habitat added to synthesis
# # Tidy and join habitat with metadata
# tidy_habitat <- benthos_summarised %>%
#   left_join(metadata) %>% # Successful habitat columns not filled for 2014 synthesis/campaign
#   glimpse()

# saveRDS(tidy_habitat, paste0("data/", park, "/raw/", name, "_benthos.RDS"))

##HE below was done for manually importing 2024 habitat data (not from GA)

benthos_new <- readRDS(paste0("data/", park, "/raw/", name, "_2024_benthos.RDS")) %>%
  mutate(sample = paste0(sample, "_NA"))
benthos_summarised <- readRDS(paste0("data/", park, "/raw/benthos_summarised.RDS"))

tidy_habitat <- bind_rows(benthos_summarised,benthos_new) %>%
  left_join(metadata, by = c("campaignid", "sample")) %>% ##HE 2 missing from metadata
  select(-c(reef,na,ends_with("_percent"))) %>%
  glimpse()

saveRDS(tidy_habitat, paste0("data/", park, "/raw/", name, "_benthos_combined.RDS"))
