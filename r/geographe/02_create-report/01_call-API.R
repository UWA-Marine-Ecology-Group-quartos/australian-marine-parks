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


# Tidy and join habitat with metadata
tidy_habitat <- benthos_summarised %>%
  left_join(metadata) %>% # Successful habitat columns not filled for 2014 synthesis/campaign
  glimpse()

##HE below was done for manually importing 2024 habitat data (not from GA)
# metadata <- readRDS(paste0("data/", park, "/raw/metadata.RDS"))
#
# benthos_summarised <- readRDS(paste0("data/", park, "/raw/benthos_summarised.RDS"))
# benthos_new <- readRDS(paste0("data/", park, "/raw/", name, "_2024_benthos.RDS"))
#
# tidy_habitat <- bind_rows(benthos_summarised,benthos_new) %>%
#   left_join(metadata) %>% ##HE 2 missing metadata
#   select(-c(reef,na,ends_with("_percent"))) %>%
#   glimpse()
#
# saveRDS(tidy_habitat, paste0("data/", park, "/raw/", name, "_benthos_combined.RDS"))

saveRDS(tidy_habitat, paste0("data/", park, "/raw/", name, "_benthos.RDS"))
