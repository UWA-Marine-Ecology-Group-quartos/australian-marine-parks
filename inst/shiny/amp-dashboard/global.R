library(shiny)
library(bslib)
library(leaflet)
library(dplyr)

# Define the theme using bslib ----
theme <- bs_theme(
  bg = "#FFFFFF",  # Background color
  fg = "#000000",  # Foreground color
  primary = "#007BFF",  # Primary color
  secondary = "#6C757D",  # Secondary color
  base_font = font_google("Lato")  # Use Lato font from Google Fonts
)

# Generate 100 random points for the Leaflet map (dummy data) ----
set.seed(123)
dummy_points <- data.frame(
  lat = runif(100, min = -35.2, max = -33.5),  # Latitude range for SW corner of WA
  lng = runif(100, min = 114.5, max = 116.5)   # Longitude range for SW corner of WA
)

# Load data ----
load("data/all_data.Rdata")

# Remove extra park ----
networks_and_parks <- all_data$networks_and_parks %>%
  dplyr::filter(!park %in% "Coral Sea") # the Coral Sea does not have parks within it

# Create dummy list
south_west <- networks_and_parks %>%
  dplyr::filter(network %in% "South-west") %>%
  dplyr::pull(park)
