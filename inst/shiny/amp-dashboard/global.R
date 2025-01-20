library(shiny)
library(bslib)
library(bsicons)
library(leaflet)
library(dplyr)
library(tibble)
library(shinybusy)
library(shinyjs)
library(viridisLite)

# Load the data
# dropdown_data <- read.csv(here::here("data/dropdowns.csv"), stringsAsFactors = FALSE)

thematic::thematic_shiny()

# TODO - change the way that the habitat is plotted in the dropdowns - need one selectionfor each habitat type, because can't display them all on a map due to the prediction and error

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
  dplyr::filter(!park %in% "Coral Sea") %>% # the Coral Sea does not have parks within it
  dplyr::filter(network %in% c("South-west", "North-west")) %>% # temp filter for north and south WA
  dplyr::filter(park %in% c("Dampier", # temp filter for parks we have data for
                            "Ningaloo",
                            "Abrolhos",
                            "Geographe",
                            "South-west Corner")) %>%
  dplyr::glimpse()

# Create dummy list
south_west <- networks_and_parks %>%
  dplyr::filter(network %in% "South-west") %>%
  dplyr::pull(park)

# FISHNCLIPS
dat <- readRDS("data/fishnclips/dat.RDS") %>%
  dplyr::rename(latitude = latitude_dd, longitude = longitude_dd)

commonwealth.mp <- readRDS("data/fishnclips/commonwealth.mp.RDS")
state.mp <- readRDS("data/fishnclips/state.mp.RDS")
ngari.mp <- readRDS("data/fishnclips/ngari.mp.RDS")

state.pal <- colorFactor(c("#bfaf02", # conservation
                           "#7bbc63", # sanctuary = National Park
                           "#fdb930", # recreation
                           "#b9e6fb", # general use
                           '#ccc1d6' # special purpose
), state.mp$zone)

commonwealth.pal <- colorFactor(c("#f6c1d9", # Sanctuary
                                  "#7bbc63", # National Park
                                  "#fdb930", # Recreational Use
                                  "#fff7a3", # Habitat Protection
                                  '#b9e6fb', # Multiple Use
                                  '#ccc1d6'# Special Purpose
), commonwealth.mp$zone)

# Make icon for images and videos----
# html_legend <- "<div style='width: auto; height: 45px'> <div style='position: relative; display: inline-block; width: 36px; height: 45px' <img src='images/marker_red.png'> </div> <p style='position: relative; top: 15px; display: inline-block; ' > BRUV </p> </div>
# <div style='width: auto; height: 45px'> <div style='position: relative; display: inline-block; width: 36px; height: 45px' <img src='images/marker_red.png'> </div> <p style='position: relative; top: 15px; display: inline-block; ' > BRUV </p> </div>
# <div style='width: auto; height: 45px'> <div style='position: relative; display: inline-block; width: 36px; height: 45px' <img src='images/marker_red.png'> </div> <p style='position: relative; top: 15px; display: inline-block; ' > BRUV </p> </div>"

html_legend <- "<div style='padding: 10px; padding-bottom: 10px;'><h4 style='padding-top:0; padding-bottom:10px; margin: 0;'> Marker Legend </h4><br/>

<img src='https://github.com/UWAMEGFisheries/UWAMEGFisheries.github.io/blob/master/images/markers/marker_yellow.png?raw=true'
style='width:30px;height:30px;'> Fish highlights <br/>

<img src='https://github.com/UWAMEGFisheries/UWAMEGFisheries.github.io/blob/master/images/markers/marker_green.png?raw=true'
style='width:30px;height:30px;'> Habitat imagery (stereo-BRUV)<br/>

<img src='https://github.com/UWAMEGFisheries/UWAMEGFisheries.github.io/blob/master/images/markers/marker_pink.png?raw=true'
style='width:30px;height:30px;'> Habitat imagery (BOSS)<br/>

<img src='https://github.com/UWAMEGFisheries/UWAMEGFisheries.github.io/blob/master/images/markers/marker_purple.png?raw=true'
style='width:30px;height:30px;'> 3D models"
