library(readr)
library(dplyr)
library(googlesheets4)
library(stringr)

# Read in dropdown information ----
dropdown_data <- read_sheet("https://docs.google.com/spreadsheets/d/1Iplohv6mM-CnpE6uYBi4uQnuhCyZMNpCRMSJFFnJxjM/edit?usp=sharing",
                   sheet = "dropdowns")

# Read in network information ----
networks_and_parks <- read_csv("inst/shiny/amp-dashboard/data/networks-and-parks.csv")

# Read in summary data (temp) ----
summary_data <- read_sheet("https://docs.google.com/spreadsheets/d/1Iplohv6mM-CnpE6uYBi4uQnuhCyZMNpCRMSJFFnJxjM/edit?usp=sharing",
                            sheet = "summary_data")

# read in condition plot information ----
# Define the folder path containing the .rds files for the condition plots
folder_path <- "inst/shiny/amp-dashboard/plots/condition/demersal_fish"

# Get the list of .rds files in the folder
rds_files <- list.files(folder_path, pattern = "\\.rds$", full.names = TRUE)

# Function to extract "marine-park", "metric", and "years" from filename
extract_file_info <- function(filename) {
  parts <- strsplit(tools::file_path_sans_ext(basename(filename)), "_")[[1]]
  list(network = parts[1], marine_park = parts[2], metric = parts[3], years = parts[4])
}

# Create a dataframe containing file information
file_info <- do.call(rbind, lapply(rds_files, function(f) {
  info <- extract_file_info(f)
  data.frame(file = f, network = info$network, marine_park = info$marine_park, metric = info$metric, years = info$years,
             stringsAsFactors = FALSE)
})) %>%
  dplyr::mutate(file = stringr::str_replace_all(file, "inst/shiny/amp-dashboard/",""))

# read in temporal plot information ----
# For the temporal plots
# Define the folder path containing the .rds files for the condition plots
folder_path <- "inst/shiny/amp-dashboard/plots/temporal"

# Get the list of .rds files in the folder
rds_files <- list.files(folder_path, pattern = "\\.rds$", full.names = TRUE)

# Function to extract "marine-park", "metric", and "years" from filename
extract_file_info <- function(filename) {
  parts <- strsplit(tools::file_path_sans_ext(basename(filename)), "_")[[1]]
  list(network = parts[1], marine_park = parts[2], metric = parts[3], depth_classes = parts[4])
}

# Create a dataframe containing file information
temporal_file_info <- do.call(rbind, lapply(rds_files, function(f) {
  info <- extract_file_info(f)
  data.frame(file = f, network = info$network, marine_park = info$marine_park, metric = info$metric, depth_classes = info$depth_classes,
             stringsAsFactors = FALSE)
})) %>%
  dplyr::mutate(file = stringr::str_replace_all(file, "inst/shiny/amp-dashboard/",""))

# Read in example metadata for Geographe ----
metadata <- readRDS("data/geographe/tidy/GeographeAMP_metadata-bathymetry-derivatives.rds") %>%
  dplyr::mutate(network = "South-west") %>%
  dplyr::mutate(marine_park = "Geographe Marine Park")










# Read in rasters and tags ----
raster_data <- read_sheet("https://docs.google.com/spreadsheets/d/1BJLDy9pCjXSdFIJ-xczRC9Wj3kBkYLYnHnPczWoX9Eo/edit?usp=sharing",
                            sheet = "raster_tags") %>%
  dplyr::filter(!is.na(tile_service_url)) %>%
  dplyr::mutate(metric = if_else((indicator_group %in% "Large-bodied carnivores"& indicator_class %in% "Greater than maturity"), "Abundance of large-bodied generalist carnivores greater than Lm", NA)) %>%
  dplyr::mutate(metric = if_else(indicator_metric %in% "Community temperature index", "Community Temperature Index", metric)) %>%
  dplyr::mutate(tile_service_url = if_else(estimate %in% "Error", str_replace_all(tile_service_url, "viridis", "plasma"), tile_service_url)) %>%
  dplyr::mutate(metric = if_else(ecosystem_component %in% "Functional reef", "Functional reef", metric))


# Combine all information together -----

all_data <- structure(
  list(
    networks_and_parks = networks_and_parks,
    file_info = file_info,
    temporal_file_info = temporal_file_info,
    metadata = metadata,
    dropdown_data = dropdown_data,
    raster_data = raster_data,
    summary_data = summary_data
  ),
  class = "data"
)

# Save ----
save(all_data, file = here::here("data/all_data.Rdata"))
save(all_data, file = here::here("inst/shiny/amp-dashboard/data/all_data.Rdata")) #I'm not actually sure which ones of these works
