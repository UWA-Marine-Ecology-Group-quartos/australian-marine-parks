library(readr)
library(dplyr)
library(googlesheets4)
library(stringr)
library(httr)
library(jsonlite)

# Read in dropdown information ----
dropdown_data <- read_sheet("https://docs.google.com/spreadsheets/d/1Iplohv6mM-CnpE6uYBi4uQnuhCyZMNpCRMSJFFnJxjM/edit?usp=sharing",
                   sheet = "dropdowns")
2

# Get method data source----
method_data <- read_sheet("https://docs.google.com/spreadsheets/d/1Iplohv6mM-CnpE6uYBi4uQnuhCyZMNpCRMSJFFnJxjM/edit?usp=sharing",
                          sheet = "simplified_dummy_data") %>%
  dplyr::distinct(ecosystem_condition, method, network, marine_park_or_area)

# Read in network information ----
networks_and_parks <- read_csv("inst/shiny/amp-dashboard/data/networks-and-parks.csv")

# Read in summary data (temp) ----
summary_data <- read_sheet("https://docs.google.com/spreadsheets/d/1Iplohv6mM-CnpE6uYBi4uQnuhCyZMNpCRMSJFFnJxjM/edit?usp=sharing",
                            sheet = "summary_data")

# Read in data for text ----
text_data <- read_sheet("https://docs.google.com/spreadsheets/d/1Iplohv6mM-CnpE6uYBi4uQnuhCyZMNpCRMSJFFnJxjM/edit?usp=sharing",
                        sheet = "simplified_text_data")

# Read in metadata ----
meg_labsheets_bruvs <- read_sheet("https://docs.google.com/spreadsheets/d/1ZfW-XJKP0BmY2UXPNquTxnO5-iHnG9Kw3UuJbALCcrs/edit?usp=sharing",
                            sheet = "BRUVs CampaignTrack") %>%
  dplyr::filter(!is.na(network)) %>%
  dplyr::select(campaignid, network, marine_park)

temp_metadata <- data.frame()

for(campaign in unique(meg_labsheets_bruvs$campaignid)){

  print(campaign)

  campaign_metadata <- read_sheet("https://docs.google.com/spreadsheets/d/1ZfW-XJKP0BmY2UXPNquTxnO5-iHnG9Kw3UuJbALCcrs/edit?usp=sharing",
                              sheet = campaign) %>%
    mutate(across(everything(), as.character)) %>%
    dplyr::mutate(campaignid = campaign)

  temp_metadata <- bind_rows(temp_metadata, campaign_metadata)

}

metadata <- temp_metadata %>%
  dplyr::select(campaignid, opcode, latitude_dd, longitude_dd, depth_m, date_time) %>%
  dplyr::left_join(meg_labsheets_bruvs) %>%
  dplyr::mutate(latitude_dd = as.numeric(latitude_dd),
                longitude_dd = as.numeric(longitude_dd)) %>%
  dplyr::filter(!is.na(latitude_dd))

# read in condition plot information ----
# Define the folder path containing the .rds files for the condition plots
folder_path <- "inst/shiny/amp-dashboard/plots/condition"

# Get the list of .rds files in the folder
rds_files <- list.files(folder_path, pattern = "\\.rds$", full.names = TRUE, recursive = TRUE)

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

# # Read in example metadata for Geographe ----
# metadata <- readRDS("data/geographe/tidy/GeographeAMP_metadata-bathymetry-derivatives.rds") %>%
#   dplyr::mutate(network = "South-west") %>%
#   dplyr::mutate(marine_park = "Geographe Marine Park")

# Read in rasters and tags ----
raster_raw <- read_sheet("https://docs.google.com/spreadsheets/d/1BJLDy9pCjXSdFIJ-xczRC9Wj3kBkYLYnHnPczWoX9Eo/edit?usp=sharing",
                            sheet = "raster_tags") %>%
  dplyr::filter(!is.na(tile_service_url)) %>%
  dplyr::mutate(metric = dashboard_metric)

# Get raster min and max values ----

CheckEM::ga_api_set_token()
# Load the saved token
token <- readRDS("secrets/api_token.RDS")

# URL for the API endpoint
url <- paste0("https://dev.globalarchive.org/api/data/SynthesisRasterFile/")

# Include the token in the request headers
headers <- add_headers(Authorization = paste("Token", token))

# Send GET request with token-based authentication
response <- GET(url, headers)

# Check if the request was successful
if (status_code(response) == 200) {
  # Parse the JSON content
  data <- content(response, as = "text", encoding = "UTF-8")
  parsed_data <- fromJSON(data, flatten = TRUE)

  # View the parsed data
  print(parsed_data)
} else {
  # Print error message
  stop("Failed to fetch data: ", status_code(response))
}

raster_min_max <- parsed_data$results %>%
  dplyr::rename(tile_service_url = tiles_url_template) %>%
  glimpse

raster_data <- left_join(raster_raw, raster_min_max) %>%
  dplyr::mutate(tile_service_url = if_else(estimate %in% "Error", str_replace_all(tile_service_url, "viridis", "plasma"), tile_service_url)) %>%
  dplyr::mutate(tile_service_url = if_else(!estimate %in% "Error", str_replace_all(tile_service_url, "viridis", "jet"), tile_service_url)) %>%
  dplyr::rename(min = min_value, max = max_value) %>%
  dplyr::filter(!is.na(min) | !is.na(max)) %>%
  glimpse

# Combine all information together -----

all_data <- structure(
  list(
    networks_and_parks = networks_and_parks,
    file_info = file_info,
    temporal_file_info = temporal_file_info,
    metadata = metadata,
    dropdown_data = dropdown_data,
    raster_data = raster_data,
    summary_data = summary_data,
    text_data = text_data,
    method_data = method_data
  ),
  class = "data"
)

# Save ----
save(all_data, file = here::here("data/all_data.Rdata"))
save(all_data, file = here::here("inst/shiny/amp-dashboard/data/all_data.Rdata")) #I'm not actually sure which ones of these works
