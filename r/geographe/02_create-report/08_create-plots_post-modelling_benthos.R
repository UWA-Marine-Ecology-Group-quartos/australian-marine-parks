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
library(grid)
library(viridis)

# Load functions
file.sources <- list.files(pattern = "*.R", path = "functions/", full.names = TRUE)
sapply(file.sources, source, .GlobalEnv)

# Set cropping extent - larger than most zoomed out plot
e <- ext(114.2, 115.8, -34.7, -33.1)

# Load necessary spatial files
ausc <- st_read("data/south-west network/spatial/shapefiles/aus-shapefile-w-investigator-stokes.shp") %>%
  st_crop(e) %>%
  st_transform(4326)

marine_parks <- st_read("data/south-west network/spatial/shapefiles/western-australia_marine-parks-all.shp") %>%
  dplyr::filter(name %in% c("Ngari Capes", "Geographe", "South-west Corner"))

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
  clamp(upper = 0, lower = -250, values = FALSE) %>%
  trim() %>%
  as.data.frame(xy = TRUE, na.rm = TRUE)

names(bathy)[3] <- "Depth"

# Years to compare
years <- c(2014L, 2024L)

# Map pretty habitat names to raster layer prefixes in dat
habitat_lookup <- c(
  "Sand" = "sand",
  "Macroalgae" = "macro",
  "Seagrass" = "seagrass",
  "Sessile invertebrates" = "inverts",
  "Rock" = "rock"
)

# Optional habitat colours for other functions if needed
hab_cols <- c(
  "Sand" = "wheat",
  "Macroalgae" = "darkgoldenrod4",
  "Seagrass" = "forestgreen",
  "Rock" = "grey40",
  "Sessile invertebrates" = "plum"
)

# Plot extent
prediction_limits <- c(115.035, 115.57, -33.665, -33.34)

# Read all years once
dat_list <- setNames(vector("list", length(years)), years)

for (yr in years) {
  message("Reading year: ", yr)

  dat_list[[as.character(yr)]] <- readRDS(
    paste0(
      "output/model-output/", park, "/habitat/",
      name, "_predicted-habitat_", yr, ".rds"
    )
  )
}

# -------------------------------------------------------------------
# PART 1: Single-year plots (categorical + dominant benthos)
# -------------------------------------------------------------------
for (yr in years) {

  message("Building per-year plots for: ", yr)

  dat <- dat_list[[as.character(yr)]]

  pred_class <- as.data.frame(dat, xy = TRUE) %>%
    dplyr::mutate(year = yr)

  pred_plot <- normalise_se(data = pred_class)

  p_cat <- categoricalhabitat_plot_single(
    pred_plot = pred_plot,
    prediction_limits = prediction_limits
  )

  print(p_cat)

  ggsave(
    filename = paste0(
      "plots/", park, "/habitat/", name,
      "_predicted-habitat-categorical_", yr, ".png"
    ),
    plot = p_cat,
    height = 6,
    width = 8,
    dpi = 600,
    units = "in",
    bg = "white"
  )

  p_dom <- dominantbenthos_plot_single(
    pred_plot = pred_plot,
    prediction_limits = prediction_limits
  ) +
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
    filename = paste0(
      "plots/", park, "/habitat/", name,
      "_predicted-dominant-habitat_", yr, ".png"
    ),
    plot = p_dom,
    height = 6,
    width = 8,
    dpi = 600,
    units = "in",
    bg = "white"
  )
}

# -------------------------------------------------------------------
# PART 2: Multi-year categorical and dominant benthos + combined SE plot
# -------------------------------------------------------------------
p_dom_se <- dominantbenthos_plot_multi(
  dat_list = dat_list,
  prediction_limits = prediction_limits
)

print(p_dom_se)

ggsave(
  filename = paste0(
    "plots/", park, "/habitat/", name,
    "_predicted-dominant-benthos-and-combined-se_",
    paste(years, collapse = "-"), ".png"
  ),
  plot = p_dom_se,
  height = 7,
  width = 8,
  dpi = 900,
  units = "in",
  bg = "white"
)


p_cat_multi <- categoricalhabitat_plot_multi(
  dat_list = dat_list,
  prediction_limits = prediction_limits
)

print(p_cat_multi)

ggsave(
  filename = paste0(
    "plots/", park, "/habitat/", name,
    "_predicted-habitat-categorical_",
    paste(years, collapse = "-"), ".png"
  ),
  plot = p_cat_multi,
  height = 5,
  width = 10,
  dpi = 600,
  units = "in",
  bg = "white"
)

# -------------------------------------------------------------------
# PART 3: Multi-year individual habitat plots
# -------------------------------------------------------------------
for (habitat_name in names(habitat_lookup)) {

  message("Building individual habitat plot for: ", habitat_name)

  layer_stub <- habitat_lookup[[habitat_name]]

  p_hab <- individualbenthic_plot(
    habitat_name = habitat_name,
    layer_stub = layer_stub,
    dat_list = dat_list,
    prediction_limits = prediction_limits,
    pred_limits = NULL,   # use c(0, 1) for a fixed probability scale across taxa
    se_limits = NULL      # auto-scale within habitat across years
  )

  print(p_hab)

  out_name <- habitat_name %>%
    str_to_lower() %>%
    str_replace_all("\\s+", "-")

  ggsave(
    filename = paste0(
      "plots/", park, "/habitat/", name,
      "_predicted-individual-habitat_", out_name, "_",
      paste(years, collapse = "-"), ".png"
    ),
    plot = p_hab,
    height = 5,
    width = 8,
    dpi = 900,
    units = "in",
    bg = "white"
  )
}

# -------------------------------------------------------------------
# PART 4: Control plots by taxa, facetted by depth class
# -------------------------------------------------------------------

# Create the data (makes a dataframe for each ecosystem depth contour)
control_all <- purrr::map(years, \(yy) {
  dat_yy <- readRDS(
    paste0(
      "output/model-output/", park, "/habitat/",
      name, "_predicted-habitat_", yy, ".rds"
    )
  )
  controldata_benthos(dat = dat_yy, year = yy, amp_abbrv = "GMP", state_abbrv = "NCMP")
})

park_dat.shallow <- purrr::map_dfr(control_all, "shallow") %>%
  dplyr::mutate(depth_class = "Shallow (0 - 30 m)")

park_dat.meso <- purrr::map_dfr(control_all, "meso") %>%
  dplyr::mutate(depth_class = "Mesophotic (30 - 70 m)")

park_dat.rari <- purrr::map_dfr(control_all, "rari") %>%
  dplyr::mutate(depth_class = "Rariphotic (70 - 200 m)")

park_dat.control <- dplyr::bind_rows(
  park_dat.shallow,
  park_dat.meso,
  park_dat.rari
) %>%
  dplyr::mutate(
    depth_class = factor(
      depth_class,
      levels = c(
        "Shallow (0 - 30 m)",
        "Mesophotic (30 - 70 m)",
        "Rariphotic (70 - 200 m)"
      )
    )
  )

# Taxa to plot
taxa_lookup <- c(
  "seagrass"   = "Seagrass",
  "macroalgae" = "Macroalgae",
  "rock"       = "Rock",
  "sand"       = "Sand",
  "inverts"    = "Sessile invertebrates"
)

for (taxa_code in names(taxa_lookup)) {

  message("Building control plot for taxon: ", taxa_lookup[[taxa_code]])

  p_taxa <- controlplot_benthos(
    data = park_dat.control,
    taxa = taxa_code,
    amp_abbrv = "GMP",
    state_abbrv = "NCMP",
    taxa_label = taxa_lookup[[taxa_code]]
  )

  if (!is.null(p_taxa)) {

    print(p_taxa)

    out_name <- taxa_lookup[[taxa_code]] %>%
      stringr::str_to_lower() %>%
      stringr::str_replace_all("\\s+", "-")

    ggsave(
      filename = paste0(
        "plots/", park, "/habitat/", name, "_control-plot_", out_name, ".png"
      ),
      plot = p_taxa,
      height = 4,
      width = 6,
      dpi = 300,
      units = "in",
      bg = "white"
    )
  }
}

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
    dplyr::arrange(desc(Sand))

  p_scatterpie <- scatterpie_plot_single(
    benthos_year = benthos_year,
    site_limits = site_limits,
    pie_radius = 0.005
  )

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


p_scatterpie_multi <- scatterpie_plot_multi(
  benthos = benthos,
  years = years,
  site_limits = site_limits,
  pie_radius = 0.005
)

print(p_scatterpie_multi)

ggsave(
  filename = paste0(
    "plots/", park, "/habitat/", name, "_scatterpie_",
    paste(years, collapse = "-"), ".png"
  ),
  plot = p_scatterpie_multi,
  height = 6,
  width = 10,
  dpi = 300,
  bg = "white"
)
