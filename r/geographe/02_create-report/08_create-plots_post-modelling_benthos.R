###
# Project: NESP 4.20 - Marine Park Dashboard reporting
# Data:    Habitat data synthesis & habitat models derived from FSSgam
# Task:    Create post-modelling habitat figures for marine park reporting
# Author:  Claude Spencer
# Date:    June 2024
###

# Clear your environment
rm(list = ls())

# Set the study name
name <- "GeographeAMP"
park <- "geographe"

# Load libraries
library(tidyverse)
library(terra)
library(sf)
library(ggnewscale)
library(scales)
library(tidyterra)
library(patchwork)
library(scatterpie)


# Load functions
file.sources = list.files(pattern = "*.R", path = "functions/", full.names = T)
sapply(file.sources, source, .GlobalEnv)

# Set cropping extent - larger than most zoomed out plot
e <- ext(114.2, 115.8,-34.7, -33.1)

# Load necessary spatial files
# Australian outline and state and commonwealth marine parks
aus    <- st_read("data/south-west network/spatial/shapefiles/aus-shapefile-w-investigator-stokes.shp")
ausc <- st_crop(aus, e)

# Australian outline and state and commonwealth marine parks
marine_parks <- st_read("data/south-west network/spatial/shapefiles/western-australia_marine-parks-all.shp") %>%
  dplyr::filter(name %in% c("Ngari Capes", "Geographe", "South-west Corner")) %>%
  glimpse()
plot(marine_parks["zone"])

marine_parks_amp <- marine_parks %>%
  dplyr::filter(epbc %in% "Commonwealth")
marine_parks_state <- marine_parks %>%
  dplyr::filter(epbc %in% "State")

npz <- marine_parks[marine_parks$zone %in% "National Park Zone", ]
wasanc <- marine_parks[marine_parks$zone %in% "Sanctuary Zone", ]

cwatr <- st_read("data/south-west network/spatial/shapefiles/amb_coastal_waters_limit.shp") %>%
  st_make_valid() %>%
  st_crop(e)

# Read in the data (per year) ----
pred.years <- c(2014L, 2024L)

for(pred_year in pred.years) {

  print(pred_year)

  dat <- readRDS(paste0("output/model-output/", park, "/habitat/",
                        name, "_predicted-habitat_", pred_year, ".rds"))

  pred_class <- as.data.frame(dat, xy = T) %>%
    dplyr::mutate(year = pred_year) %>%
    glimpse()

  # Normalise the inverse of standard error
  pred_plot <- normalise_se(data = pred_class)

  # Set the limits for the plot
  prediction_limits = c(115.0539, 115.5539, -33.64861, -33.35361)

  # Create the plot
  dominantbenthos_plot(prediction_limits) + ##HE have to check exclusions in function
    theme( # Add theme items to sort out the legend
      legend.position = "bottom",
      legend.direction = "horizontal",
      legend.box = "horizontal",
      legend.box.just = "left",
      legend.text = element_text(size = 5),
      legend.title = element_text(size = 7),
      legend.key.size = unit(0.5, "cm"),
      legend.margin = margin(t = -0.1, unit = "cm"
      ))

  # Save the plot
  ggsave(filename = paste0("plots/", park, "/habitat/", name,
                           "_predicted-dominant-habitat_", pred_year, ".png"),
         height = 6, width = 8, dpi = 600, units = "in", bg = "white")

  # Subset the spatraster data to remove reef and standard error
  pred_rast <- subset(dat, str_detect(names(dat), "(?<!se)\\.fit$") & # fit not preceded by se
                        str_detect(names(dat), "^(?!.*reef).*$")) # Strings don't contain "reef"
  names(pred_rast)

  # Set the names - make sure this matches the order
  # (This assumes you have exactly Sand/Macroalgae/Seagrass remaining)
  names(pred_rast) <- c("Sand", "Macroalgae", "Seagrasses")
  plot(pred_rast)

  # Create the plot - same x and y limits
  individualbenthic_plot(prediction_limits)

  # Save the plot
  ggsave(filename = paste0("plots/", park, "/habitat/", name,
                           "_predicted-individual-habitat_", pred_year, ".png"),
         height = 5.5, width = 8, dpi = 900, units = "in", bg = "white")
}

# Create the data (makes a dataframe for each ecosystem depth contour)
control_all <- purrr::map(pred.years, \(yy) {
  dat_yy <- readRDS(paste0("output/model-output/", park, "/habitat/",
                           name, "_predicted-habitat_", yy, ".rds"))
  controldata_benthos(dat = dat_yy, year = yy, amp_abbrv = "GMP", state_abbrv = "NCMP")
})

park_dat.shallow <- purrr::map_dfr(control_all, "shallow")
park_dat.meso    <- purrr::map_dfr(control_all, "meso")
park_dat.rari    <- purrr::map_dfr(control_all, "rari")

# Shallow plot
controlplot_benthos(data = park_dat.shallow, amp_abbrv = "GMP", state_abbrv = "NCMP",
                    title = "Shallow (0 - 30 m)")
ggsave(paste0("plots/", park, "/habitat/", name, "_shallow-control-plots.png"),
       height = 9, width = 8, dpi = 300, units = "in")

# Mesophotic plot
controlplot_benthos(data = park_dat.meso, amp_abbrv = "GMP", state_abbrv = "NCMP",
                    title = "Mesophotic (30 - 70 m)")
ggsave(paste0("plots/", park, "/habitat/", name, "_mesophotic-control-plots.png"),
       height = 9, width = 8, dpi = 300, units = "in")

# (Optional) Rariphotic plot if you want it too:
# controlplot_benthos(data = park_dat.rari, amp_abbrv = "GMP", state_abbrv = "NCMP",
#                     title = "Rariphotic (70 - 200 m)")
# ggsave(paste0("plots/", park, "/habitat/", name, "_rariphotic-control-plots.png"),
#        height = 9, width = 8, dpi = 300, units = "in")


# Get depth for Scatterpie plot

# Set the extent of the study
e <- ext(114.8, 116, -33.8, -33)

# Load the bathymetry data (GA 250m resolution)
bathy <- rast("data/south-west network/spatial/rasters/AusBathyTopo__Australia__2024_250m_MSL_cog.tif") %>%
  crop(e) %>%
  clamp(upper = 0, lower = -250, values = F) %>%
  trim() %>%
  as.data.frame(xy = T, na.rm = T)

names(bathy)[3] <- "Depth"

# Scatterpies

metadata_bathy_derivatives <- readRDS(paste0("data/", park, "/tidy/", name, "_metadata-bathymetry-derivatives.rds")) %>%
  clean_names() %>%
  glimpse()

benthos <- readRDS(paste0("data/", park, "/tidy/", name, "_benthos-count_combined.RDS")) %>%
  dplyr::rename(
    Macroalgae = macroalgae,
    Seagrass = seagrasses,
    Sand = sand,
    Rock = rock,
    "Sessile invertebrates" = sessile_invertebrates
  ) %>%
  left_join(metadata_bathy_derivatives) %>%
  glimpse()

hab_fills <- scale_fill_manual(values = c("Sand" = "wheat",
                                          "Sessile invertebrates" = "plum",
                                          "Rock" = "grey40",
                                          "Macroalgae" = "darkgoldenrod4",
                                          "Seagrass" = "forestgreen"))

wampa_fills <- scale_fill_manual(values = c(
  # "Marine Management Area" = "#b7cfe1",
  # "Conservation Area" = "#b3a63d",
  "Sanctuary Zone" = "#bfd054",
  "General Use Zone" = "#bddde1",
  # "Recreation Area" = "#f4e952",
  "Special Purpose Zone" = "#c5bcc9"
  # "Marine Nature Reserve" = "#bfd054"
),
name = "State Marine Parks")

# depth colours
depth_fills <- scale_fill_manual(values = c("#a7cfe0","#9acbec","#98c4f7",  # Shallow to deep
                                            "#a3bbff", "#81a1fc"), guide = "none")

site_limits = c(115.0, 115.67,-33.3, -33.65)

ggplot() +
  geom_contour_filled(data = bathy, aes(x, y, z = Depth, fill = after_stat(level)), color = "black",
                      breaks = c(-30, -70, -200,-700, -2000, -4000), size = 0.1) +
  depth_fills +
  new_scale_fill() +
  # geom_sf(data = marine_parks_amp, aes(fill = zone), colour = NA, show.legend = F,
  #         linewidth = 0.75, alpha = 0.5) +
  # scale_fill_manual(name = "Australian Marine Parks",
  #                   values = with(marine_parks_amp, setNames(colour, zone))) +
  geom_sf(data = ausc, fill = "seashell2", colour = "black", size = 0.1) +
  geom_sf(data = wasanc ,fill = "#bfd054", alpha = 2/5, color = NA) +
  wampa_fills +
  labs(fill = "State Marine Parks") +
  new_scale_fill() +
  geom_sf(data = npz, fill = "#7bbc63",alpha = 2/5, color = NA) +
  geom_sf(data = cwatr, colour = "firebrick", alpha = 4/5, size = 0.3) +
  new_scale_fill() +
  geom_scatterpie(data = benthos, aes(x = longitude_dd, y = latitude_dd),
                  cols = c("Sand",
                           "Sessile invertebrates",
                           "Rock",
                           "Macroalgae",
                           "Seagrass"),
                  colour = NA, pie_scale = 0.45) +
  labs(x = "Longitude", y = "Latitude", fill = "Habitat") +
  hab_fills +
  coord_sf(xlim = c(site_limits[1], site_limits[2]), ylim = c(site_limits[3], site_limits[4]), crs = 4326) +
  theme_minimal() +
  theme(panel.background = element_rect(fill = "#b9d1d6", colour = NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

ggsave(paste0("plots/", park, "/habitat/", name, "_scatterpie.png"),
       height = 6, width = 10, dpi = 300, bg = "white")
