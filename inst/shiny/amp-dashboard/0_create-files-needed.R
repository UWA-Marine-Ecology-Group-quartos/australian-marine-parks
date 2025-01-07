library(readr)
library(dplyr)
library(googlesheets4)

dropdown_data <- read_sheet("https://docs.google.com/spreadsheets/d/1Iplohv6mM-CnpE6uYBi4uQnuhCyZMNpCRMSJFFnJxjM/edit?usp=sharing",
                   sheet = "dropdowns")


networks_and_parks <- read_csv("inst/shiny/amp-dashboard/data/networks-and-parks.csv")
#
# # read in plots (this is just an example, I will need to loop through these and turn them into a list?)
# geo_cti <- readRDS("inst/shiny/amp-dashboard/plots/geographe_gg_cti.RDS")
# geo_lm <- readRDS("inst/shiny/amp-dashboard/plots/geographe_gg_lm.RDS")
# geo_sr <- readRDS("inst/shiny/amp-dashboard/plots/geographe_gg_sr.RDS") # temporaily reading in SR

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


# For the temporal plots
# Define the folder path containing the .rds files for the condition plots
folder_path <- "inst/shiny/amp-dashboard/plots/temporal"

# Get the list of .rds files in the folder
rds_files <- list.files(folder_path, pattern = "\\.rds$", full.names = TRUE)

# Function to extract "marine-park", "metric", and "years" from filename
extract_file_info <- function(filename) {
  parts <- strsplit(tools::file_path_sans_ext(basename(filename)), "_")[[1]]
  list(network = parts[1], marine_park = parts[2], metric = parts[3])
}

# Create a dataframe containing file information
temporal_file_info <- do.call(rbind, lapply(rds_files, function(f) {
  info <- extract_file_info(f)
  data.frame(file = f, network = info$network, marine_park = info$marine_park, metric = info$metric,
             stringsAsFactors = FALSE)
})) %>%
  dplyr::mutate(file = stringr::str_replace_all(file, "inst/shiny/amp-dashboard/",""))


metadata <- readRDS("data/geographe/tidy/GeographeAMP_metadata-bathymetry-derivatives.rds")

all_data <- structure(
  list(
    networks_and_parks = networks_and_parks,
    # geo_cti = geo_cti,
    # geo_lm = geo_lm,
    # geo_sr = geo_sr,
    file_info = file_info,
    temporal_file_info = temporal_file_info,
    metadata = metadata,
    dropdown_data = dropdown_data
    # iucn.pal = iucn.pal
  ),
  class = "data"
)

save(all_data, file = here::here("data/all_data.Rdata"))
save(all_data, file = here::here("inst/shiny/amp-dashboard/data/all_data.Rdata")) #I'm not actually sure which ones of these works
