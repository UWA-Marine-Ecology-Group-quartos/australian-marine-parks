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
library(devtools)
# devtools::install_github("GlobalArchiveManual/CheckEM") # If there has been any updates to the package then CheckEM will install
library(CheckEM)
library(arrow)

name <- "GeographeAMP"

# CheckEM::ga_api_set_token() # Run this once and then turn it off


# Load the saved token
token <- readRDS("secrets/api_token.RDS")


# Load the metadata, count and length ----
# This way does not include the zeros where a species isn't present - it returns a much smaller dataframe
CheckEM::ga_api_all_data(synthesis_id = "14", # Synthesis ID changes between projects
                         token = token,
                         dir = "data/raw/", # Check the directory
                         include_zeros = TRUE)


# Tidy and join habitat with metadata
tidy_habitat <- benthos_summarised %>%
  left_join(metadata) %>% # Successful habitat columns not filled for this synthesis/campaign
  glimpse()

saveRDS(tidy_habitat, paste0("data/geographe/raw/", name, "_benthos.RDS"))
