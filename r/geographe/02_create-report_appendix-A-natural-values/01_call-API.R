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
library('remotes')
options(timeout=9999999)
remotes::install_github("GlobalArchiveManual/CheckEM")
library(CheckEM)
library(arrow)

name <- "GeographeAMP"
park <- "geographe"

# CheckEM::ga_api_set_token() # Run this once and then turn it off


# Load the saved token
token <- readRDS("secrets/api_token.RDS")


# Load the metadata, count and length ----
# This way does not include the zeros where a species isn't present - it returns a much smaller dataframe
CheckEM::ga_api_all_data(synthesis_id = "47", # Synthesis ID changes between projects
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
