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
library(CheckEM)


# Load functions
file.sources = list.files(pattern = "*.R", path = "functions/", full.names = T)
sapply(file.sources, source, .GlobalEnv)

# Set cropping extent - larger than most zoomed out plot
e <- ext(114.2, 115.8,-34.7, -33.1)

# Load necessary spatial files
# Australian outline and state and commonwealth marine parks
aus    <- st_read("data/south-west network/spatial/shapefiles/aus-shapefile-w-investigator-stokes.shp")
ausc <- aus %>%
  st_crop(e) %>%
  st_transform(4326)

# Australian outline and state and commonwealth marine parks
marine_parks <- st_read("data/south-west network/spatial/shapefiles/western-australia_marine-parks-all.shp") %>%
  dplyr::filter(name %in% c("Ngari Capes", "Geographe", "South-west Corner")) %>%
  glimpse()
plot(marine_parks["zone"])

marine_parks_amp <- marine_parks %>%
  dplyr::filter(epbc %in% "Commonwealth") %>%
  st_transform(4326)
marine_parks_state <- marine_parks %>%
  dplyr::filter(epbc %in% "State") %>%
  st_transform(4326)

npz <- marine_parks[marine_parks$zone %in% "National Park Zone", ]
wasanc <- marine_parks[marine_parks$zone %in% "Sanctuary Zone", ]

cwatr <- st_read("data/south-west network/spatial/shapefiles/amb_coastal_waters_limit.shp") %>%
  st_make_valid() %>%
  st_crop(e) %>%
  st_transform(4326)

# Load the bathymetry data (GA 250m resolution)
bathy <- rast("data/south-west network/spatial/rasters/AusBathyTopo__Australia__2024_250m_MSL_cog.tif") %>%
  crop(e) %>%
  clamp(upper = 0, lower = -250, values = F) %>%
  trim() %>%
  as.data.frame(xy = T, na.rm = T) %>%
  glimpse()

names(bathy)[3] <- "Depth"

# Read in the data (per year) ----
years <- c(2014L, 2024L)

hab_cols <- c(
  "Sand" = "wheat",
  "Macroalgae" = "darkgoldenrod4",
  "Seagrass" = "forestgreen",
  "Rock" = "grey40",
  "Sessile invertebrates" = "plum"
)

for (yr in years) {

  message("Year: ", yr)

  dat <- readRDS(paste0("output/model-output/", park, "/habitat/",
                        name, "_predicted-habitat_", yr, ".rds"))

  pred_class <- as.data.frame(dat, xy = TRUE) %>%
    dplyr::mutate(year = yr) %>%
    glimpse()

  # Normalise the inverse of standard error
  pred_plot <- normalise_se(data = pred_class)

  # Set the limits for the plot
  prediction_limits <- c(115.0539, 115.5539, -33.64861, -33.35361)

  # ---- Dominant habitat categorical map (DISPLAY + SAVE) ----
  p_cat <- categoricalhabitat_plot(prediction_limits)

  print(p_cat)

  ggsave(
    filename = paste0("plots/", park, "/habitat/", name,
                      "_predicted-habitat-categorical_", yr, ".png"),
    plot = p_cat,
    height = 6, width = 8, dpi = 600, units = "in", bg = "white"
  )

  # ---- Dominant benthos ggplot (DISPLAY + SAVE) ----
  p_dom <- dominantbenthos_plot(prediction_limits) +
    theme(
      legend.position = "bottom",
      legend.direction = "horizontal",
      legend.box = "horizontal",
      legend.box.just = "left",
      legend.text = element_text(size = 5),
      legend.title = element_text(size = 7),
      legend.key.size = unit(0.5, "cm"),
      legend.margin = margin(t = -0.1, unit = "cm")
    )

  print(p_dom)

  ggsave(
    filename = paste0("plots/", park, "/habitat/", name,
                      "_predicted-dominant-habitat_", yr, ".png"),
    plot = p_dom,
    height = 6, width = 8, dpi = 600, units = "in", bg = "white"
  )

  # ---- Build pred_rast for individual plots ----
  pred_rast <- subset(
    dat,
    str_detect(names(dat), "(?<!se)\\.fit$") &     # fit not preceded by se
      str_detect(names(dat), "^(?!.*reef).*$")    # names don't contain "reef"
  )

  names(pred_rast) <- c("Sand", "Macroalgae", "Seagrasses", "Sessile Invertebrates", "Rock")

  # ---- Individual benthos ggplot (DISPLAY + SAVE) ----
  p_ind <- individualbenthic_plot(prediction_limits)

  print(p_ind)  # <-- this makes it show up when looping

  ggsave(
    filename = paste0("plots/", park, "/habitat/", name,
                      "_predicted-individual-habitat_", yr, ".png"),
    plot = p_ind,
    height = 5.5, width = 8, dpi = 900, units = "in", bg = "white"
  )
}

# Create the data (makes a dataframe for each ecosystem depth contour)
control_all <- purrr::map(years, \(yy) {
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


# ---- Scatterpie data prep ----

# Set the extent of the study
e <- ext(114.8, 116, -33.8, -33)

# Load the bathymetry data (GA 250m resolution)
bathy <- rast("data/south-west network/spatial/rasters/AusBathyTopo__Australia__2024_250m_MSL_cog.tif") %>%
  crop(e) %>%
  clamp(upper = 0, lower = -250, values = FALSE) %>%
  trim() %>%
  as.data.frame(xy = TRUE, na.rm = TRUE)

names(bathy)[3] <- "Depth"

metadata_bathy_derivatives <- readRDS(
  paste0("data/", park, "/tidy/", name, "_metadata-bathymetry-derivatives.rds")
) %>%
  clean_names()

benthos <- readRDS(
  paste0("data/", park, "/tidy/", name, "_benthos-count_combined.RDS")
) %>%
  dplyr::rename(
    Macroalgae = macroalgae,
    Seagrass = seagrasses,
    Sand = sand,
    Rock = rock,
    "Sessile invertebrates" = sessile_invertebrates
  ) %>%
  left_join(metadata_bathy_derivatives, by = c("campaignid", "sample", "year", "status")) %>%
  arrange(desc(Sand))

hab_fills <- scale_fill_manual(
  name = "Habitat",
  limits = c("Rock", "Sessile invertebrates", "Macroalgae", "Seagrass", "Sand"),
  values = c(
    "Rock" = "grey40",
    "Sessile invertebrates" = "plum",
    "Macroalgae" = "darkgoldenrod4",
    "Seagrass" = "forestgreen",
    "Sand" = "wheat"
  )
)

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

depth_fills <- scale_fill_manual(
  values = c("#a7cfe0", "#9acbec", "#98c4f7", "#a3bbff", "#81a1fc"),
  guide = "none"
)

site_limits <- c(115.0, 115.67, -33.3, -33.65)

years <- c(2014, 2024)

for (yr in years) {

  message("Year: ", yr)

  benthos_year <- benthos %>%
    dplyr::filter(as.character(year) == as.character(yr)) %>%
    dplyr::filter(
      is.finite(longitude_dd),
      is.finite(latitude_dd)
    ) %>%
    arrange(desc(Sand))

  p_scatterpie <- scatterpie_plot(site_limits = site_limits, pie_scale = 0.45)

  print(p_scatterpie)

  ggsave(
    filename = paste0(
      "plots/", park, "/habitat/", name, "_scatterpie_", yr, ".png"
    ),
    plot = p_scatterpie,
    height = 6,
    width = 10,
    dpi = 300,
    bg = "white"
  )
}
