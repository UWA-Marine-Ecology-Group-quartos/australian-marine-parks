###
# Project: NESP 4.20 - Marine Park Dashboard reporting
# Data:    Habitat data synthesis & habitat models derived from FSSgam
# Task:    Create post-modelling habitat figures for marine park reporting
# Author:  Claude Spencer
# Date:    June 2024
###

rm(list = ls())

name <- "GeographeAMP"
park <- "geographe"

library(tidyverse)
library(terra)
library(sf)
library(ggnewscale)
library(scales)
library(tidyterra)
library(patchwork)
library(stringr)

# Load functions
file.sources <- list.files(pattern = "*.R", path = "functions/", full.names = TRUE)
invisible(sapply(file.sources, source, .GlobalEnv))

# Ensure output dirs exist
dir.create(paste0("plots/", park, "/habitat/"), recursive = TRUE, showWarnings = FALSE)

# Read predicted habitat raster stack (ALL YEARS)
dat <- readRDS(paste0("output/model-output/", park, "/habitat/", name, "_predicted-habitat_ALLYEARS.rds"))

# Convert to dataframe for plotting / summaries
pred_class <- as.data.frame(dat, xy = TRUE)

# Long format by habitat + year
ind_class <- pred_class %>%
  pivot_longer(
    cols = starts_with("p_"),
    names_to = "var",
    values_to = "Probability"
  ) %>%
  separate(var, into = c("p", "habitat_code", "year"), sep = "_", remove = TRUE) %>%
  mutate(
    year = factor(year),
    habitat = recode(habitat_code,
                     inverts  = "Sessile invertebrates",
                     macro    = "Macroalgae",
                     rock     = "Rock",
                     sand     = "Sand",
                     seagrass = "Seagrass",
                     reef     = "Reef",
                     .default = NA_character_
    )
  ) %>%
  filter(!is.na(habitat)) %>%
  filter(!is.na(Probability)) %>%      # drop masked cells
  filter(habitat != "Reef")            # keep if you want to exclude reef

# Set cropping extent - larger than most zoomed out plot
e <- ext(114.2, 115.8, -34.7, -33.1)

# Load necessary spatial files (and keep CRS consistent with predictions: EPSG:4326)
aus <- st_read("data/south-west network/spatial/shapefiles/aus-shapefile-w-investigator-stokes.shp", quiet = TRUE) %>%
  st_transform(4326)
ausc <- st_crop(aus, st_as_sfc(st_bbox(c(xmin=114.2, xmax=115.8, ymin=-34.7, ymax=-33.1), crs = 4326)))

marine_parks <- st_read("data/south-west network/spatial/shapefiles/western-australia_marine-parks-all.shp", quiet = TRUE) %>%
  st_transform(4326) %>%
  dplyr::filter(name %in% c("Ngari Capes", "Geographe", "South-west Corner"))

# Split EPBC type
marine_parks_amp   <- marine_parks %>% dplyr::filter(epbc %in% "Commonwealth")
marine_parks_state <- marine_parks %>% dplyr::filter(epbc %in% "State")

# -------------------------------------------------------------------
# Plot 1: Dominant habitat map
# -------------------------------------------------------------------
# If your dominantbenthos_plot() expects certain global objects, keep these available:
#   - pred_class (df) OR dat (raster) depending on your function
#   - marine_parks / ausc etc
# pred_plot <- normalise_se(data = pred_class)  # only if function uses it (OK even if no SEs present)
# If normalise_se() errors because it expects se columns, comment it out.
pred_plot <- tryCatch(normalise_se(data = pred_class), error = function(e) NULL)

prediction_limits <- c(115.0539, 115.5539, -33.64861, -33.35361)

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

ggsave(
  filename = paste0("plots/", park, "/habitat/", name, "_predicted-dominant-habitat.png"),
  plot = p_dom,
  height = 6, width = 8, dpi = 600, units = "in", bg = "white"
)

# -------------------------------------------------------------------
# Plot 2: Individual habitat layers
# -------------------------------------------------------------------
# Your old code expected ".fit" and dropped reef.
# Your NEW layers are p_sand_2014, p_macro_2014, etc.
# Here we pick one year to plot (2014) and build a raster stack with clean names.
plot_year <- "2014"

pred_rast <- dat[[paste0(c("p_sand_", "p_macro_", "p_seagrass_"), plot_year)]]
names(pred_rast) <- c("Sand", "Macroalgae", "Seagrasses")

# If your individualbenthic_plot() uses a global `pred_rast`, this ensures it exists:
assign("pred_rast", pred_rast, envir = .GlobalEnv)

p_ind <- individualbenthic_plot(prediction_limits)

ggsave(
  filename = paste0("plots/", park, "/habitat/", name, "_predicted-individual-habitat_", plot_year, ".png"),
  plot = p_ind,
  height = 5.5, width = 8, dpi = 900, units = "in", bg = "white"
)

# -------------------------------------------------------------------
# Control plots (benthic) — note: your function call uses "year = 2014"
# -------------------------------------------------------------------
controldata_benthos(year = 2014, amp_abbrv = "GMP", state_abbrv = "NCMP")

p_shallow <- controlplot_benthos(
  data = park_dat.shallow, amp_abbrv = "GMP", state_abbrv = "NCMP",
  title = "Shallow (0 - 30 m)"
)
ggsave(
  paste0("plots/", park, "/habitat/", name, "_shallow-control-plots.png"),
  plot = p_shallow,
  height = 9, width = 8, dpi = 300, units = "in"
)

p_meso <- controlplot_benthos(
  data = park_dat.meso, amp_abbrv = "GMP", state_abbrv = "NCMP",
  title = "Mesophotic (30 - 70 m)"
)
ggsave(
  paste0("plots/", park, "/habitat/", name, "_mesophotic-control-plots.png"),
  plot = p_meso,
  height = 9, width = 8, dpi = 300, units = "in"
)
