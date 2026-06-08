###
# Project: NESP 4.20 - Marine Park Dashboard reporting
# Data:    BRAN2023 monthly SST, marine park shapefiles, aus outline
# Task:    Extract June 2011 SST from BRAN2023 (Leeuwin Current heatwave),
#          compute SST anomaly, and map over SWC extent with MPAs
# Author:  [Your name]
# Date:    May 2026
###

# ==============================================================================
# 1. SETUP
# ==============================================================================

rm(list = ls())

library(tidyverse)
library(terra)
library(sf)
library(tidyterra)
library(ggnewscale)
library(cowplot)
library(RColorBrewer)
library(ncdf4)
library(xml2)

options(timeout = 6000)

name <- "south-west"
park <- "network"

e     <- ext(106.0, 145.0, -45.0, -22.0)
e_vec <- c(106.0, 145.0, -45.0, -22.0)


# ==============================================================================
# 2. DOWNLOAD SINGLE MONTHLY FILE
# ==============================================================================

download_BRAN2023_monthly <- function(year, month, destination) {

  dir.create(destination, showWarnings = FALSE, recursive = TRUE)

  month_pad <- sprintf("%02d", month)
  filename  <- paste0("ocean_temp_", year, "_", month_pad, ".nc")
  destfile  <- file.path(destination, filename)
  url       <- paste0("https://thredds.nci.org.au/thredds/fileServer/gb6/BRAN/BRAN2023/daily/", filename)

  if (file.exists(destfile)) {
    message("Already exists, skipping: ", filename)
    return(destfile)
  }

  message("Downloading: ", filename, "\n  ", url)
  tryCatch({
    download.file(url, destfile = destfile, mode = "wb", quiet = FALSE)
    return(destfile)
  }, error = function(e) {
    message("Failed: ", filename, " — ", conditionMessage(e))
    return(NULL)
  })
}


# ==============================================================================
# 3. EXTRACT SURFACE SST FROM A MONTHLY FILE
# ==============================================================================
# Monthly files contain one time step per day stacked along Time dimension.
# We average all time steps (all days in month) to get monthly mean SST.

extract_sst_monthly_mean <- function(nc_file, extent_vec) {

  nc <- nc_open(nc_file)

  depth_dim <- nc$dim[["st_ocean"]]
  if (is.null(depth_dim)) depth_dim <- nc$dim[["depth"]]
  n_depth   <- length(depth_dim$vals)

  time_dim  <- nc$dim[["Time"]]
  if (is.null(time_dim)) time_dim <- nc$dim[["time"]]
  n_time    <- length(time_dim$vals)

  nc_close(nc)

  message("  Depth levels: ", n_depth, " | Time steps: ", n_time)

  r <- rast(nc_file, subds = "temp")

  sst_idx  <- seq(1L, n_depth * n_time, by = n_depth)
  sst      <- r[[sst_idx]]
  sst_crop <- crop(sst, ext(extent_vec[1], extent_vec[2],
                            extent_vec[3], extent_vec[4]))
  mean(sst_crop, na.rm = TRUE)
}


# ==============================================================================
# 4. LOAD SPATIAL DATA (used in all plots)
# ==============================================================================

aus <- st_read("data/south-west network/spatial/shapefiles/STE_2021_AUST_GDA2020.shp") %>%
  st_make_valid() %>%
  st_transform(4326)

marine_parks <- st_read("data/south-west network/spatial/shapefiles/south-and-western-australia_marine-parks-all.shp") %>%
  dplyr::filter(name %in% c(
    "Abrolhos", "Abrolhos Islands", "Bremer", "Eastern Recherche",
    "Ngari Capes", "Geographe", "South-west Corner", "Great Australian Bight",
    "Jurien", "Murat", "Jurien Bay", "Perth Canyon", "Southern Kangaroo Island",
    "Twilight", "Two Rocks", "Western Eyre", "Western Kangaroo Island",
    "Nuyts Archipelgo", "Thorny Passage", "Sir Joseph Banks Group",
    "Investigator", "West coast Bays", "Southern Spencer Gulf",
    "Upper Spencer Gulf", "Cottesloe Reef", "Rottnest", "Shoalwater Islands"
  )) %>%
  st_transform(4326)

marine_parks_amp   <- marine_parks %>% dplyr::filter(epbc == "Commonwealth")
marine_parks_state <- marine_parks %>%
  dplyr::filter(epbc == "State") %>%
  dplyr::mutate(zone = case_when(
    zone == "Reef Observation Area"   ~ "Sanctuary Zone",
    zone == "National Park Zone"      ~ "Sanctuary Zone",
    zone == "Habitat Protection Zone" ~ "Recreational Use Zone",
    TRUE ~ zone
  ))

terrnp <- st_read("data/south-west network/spatial/shapefiles/Collaborative_Australian_Protected_Areas_Database_(CAPAD)_2024_-_Terrestrial__.shp") %>%
  dplyr::filter(TYPE %in% c("Nature Reserve", "National Park")) %>%
  st_transform(4326)



# ==============================================================================
# 5. DOWNLOAD + EXTRACT JUNE 2011
# ==============================================================================

dest_dir <- "data/south-west network/spatial/rasters/BRAN/BRAN2023_SST_monthly/"

file_june2011 <- download_BRAN2023_monthly(2011, 6, dest_dir)

sst_june2011 <- extract_sst_monthly_mean(file_june2011, e_vec)
names(sst_june2011) <- "sst_mean_june2011"

writeRaster(sst_june2011,
            "data/south-west network/spatial/rasters/BRAN/BRAN2023_SST_june2011_mean.tif",
            overwrite = TRUE)

# ==============================================================================
# 6. PLOT — JUNE 2011 MEAN SST
# No title, no axis labels; coordinates shown; SST legend at bottom; MPAs on top
# ==============================================================================
sst_mean_df <- as.data.frame(sst_june2011, xy = TRUE, na.rm = TRUE)
colnames(sst_mean_df)[3] <- "sst"

p_sst_mean <- ggplot() +

  # Layer 1: SST mean raster
  geom_raster(data = sst_mean_df, aes(x = x, y = y, fill = sst), interpolate = FALSE) +
  scale_fill_gradientn(
    name     = "SST (°C)  June 2011",
    colours  = rev(brewer.pal(11, "RdYlBu")),
    na.value = NA,
    guide    = guide_colourbar(
      direction      = "horizontal",
      barwidth       = unit(10, "cm"),
      barheight      = unit(0.5, "cm"),
      title.position = "top",
      title.hjust    = 0.5
    )
  ) +

  # Layer 2: Land
  new_scale_fill() +
  geom_sf(data = aus, fill = "seashell2", colour = "grey80", linewidth = 0.15) +

  # Layer 3: Terrestrial parks
  new_scale_fill() +
  geom_sf(data = terrnp, aes(fill = TYPE), colour = NA, show.legend = FALSE) +
  scale_fill_manual(values = c("National Park" = "#c4cea6", "Nature Reserve" = "#e4d0bb")) +

  # Layer 4: State marine parks
  geom_sf(data = marine_parks_state,
          fill = alpha("grey70", 0.4), colour = "white", linewidth = 0.35) +

  # Layer 5: Commonwealth marine parks
  geom_sf(data = marine_parks_amp,
          fill = alpha("grey70", 0.4), colour = "white", linewidth = 0.35) +

  coord_sf(xlim = c(106, 145), ylim = c(-45, -22), crs = 4326, expand = FALSE) +
  scale_x_continuous(breaks = seq(110, 145, by = 5)) +
  scale_y_continuous(breaks = seq(-45, -22, by = 5)) +
  labs(x = NULL, y = NULL, title = NULL) +
  theme_minimal() +
  theme(
    legend.key.size      = unit(0.5, "cm"),
    legend.text          = element_text(size = 9),
    legend.title         = element_text(size = 10, face = "bold"),
    legend.position      = "bottom",
    legend.box           = "horizontal",
    legend.margin        = margin(t = 4),
    panel.grid           = element_blank(),
    panel.background     = element_rect(fill = "white", colour = NA),
    plot.background      = element_rect(fill = "white",   colour = NA),
    axis.text            = element_text(size = 9, colour = "grey40"),
    axis.ticks           = element_line(colour = "grey60"),
    plot.margin          = margin(t = 5, r = 10, b = 5, l = 5)
  )


ggsave(paste(paste0("plots/", park, "/spatial/SST/", name),
             "SST-june2011-mean.png", sep = "-"),
       plot = p_sst_mean, dpi = 300, width = 14, height = 9, bg = "white"
)



# ==============================================================================
# 7. CLIMATOLOGY — JUNE MEAN ACROSS REFERENCE YEARS (SKIP 2011)
# ==============================================================================
# Downloads one file per year, extracts surface SST, averages across years.
# Comment out and load .tif if already run once.

clim_years <- c(2010, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020)

clim_layers <- list()
for (yr in clim_years) {
  f <- download_BRAN2023_monthly(yr, 6, dest_dir)
  if (is.null(f)) { next }
  clim_layers[[as.character(yr)]] <- extract_sst_monthly_mean(f, e_vec)
}

sst_climatology <- mean(rast(clim_layers), na.rm = TRUE)
names(sst_climatology) <- "sst_climatology_june"

writeRaster(sst_climatology,
            "data/south-west network/spatial/rasters/BRAN/BRAN2023_SST_june_climatology.tif",
            overwrite = TRUE)

# --- Load precomputed files instead (if already run above): ---
# sst_june2011    <- rast("data/south-west network/spatial/rasters/BRAN/BRAN2023_SST_june2011_mean.tif")
# sst_climatology <- rast("data/south-west network/spatial/rasters/BRAN/BRAN2023_SST_june_climatology.tif")


# ==============================================================================
# 8. ANOMALY
# ==============================================================================

sst_clim_resamp <- resample(sst_climatology, sst_june2011, method = "bilinear")
sst_anomaly     <- sst_june2011 - sst_clim_resamp
names(sst_anomaly) <- "sst_anomaly_june2011"

writeRaster(sst_anomaly,
            "data/south-west network/spatial/rasters/BRAN/BRAN2023_SST_june2011_anomaly.tif",
            overwrite = TRUE)


# ==============================================================================
# 9. PLOT — JUNE 2011 SST ANOMALY
# ==============================================================================

# Load from disk if running plot section separately:
# sst_anomaly <- rast("data/south-west network/spatial/rasters/BRAN/BRAN2023_SST_june2011_anomaly.tif")

sst_df <- as.data.frame(sst_anomaly, xy = TRUE, na.rm = TRUE)
colnames(sst_df)[3] <- "anomaly"

anom_lim <- max(quantile(abs(sst_df$anomaly), 0.99, na.rm = TRUE), 0.5)

p_sst_anom <- ggplot() +

  # Layer 1: SST anomaly raster
  geom_tile(data = sst_df, aes(x = x, y = y, fill = anomaly)) +
  scale_fill_gradientn(
    name     = "SST anomaly (°C)\nJune 2011 vs climatology",
    colours  = rev(brewer.pal(11, "RdBu")),
    limits   = c(-anom_lim, anom_lim),
    oob      = scales::squish,
    na.value = NA,
    guide    = guide_colourbar(
      barwidth       = unit(0.5, "cm"),
      barheight      = unit(5,   "cm"),
      title.position = "top",
      title.hjust    = 0
    )
  ) +

  # Layer 2: Land
  new_scale_fill() +
  geom_sf(data = aus, fill = "seashell2", colour = "grey80", linewidth = 0.15) +

  # Layer 3: Terrestrial parks
  new_scale_fill() +
  geom_sf(data = terrnp, aes(fill = TYPE), colour = NA) +
  scale_fill_manual(
    values = c("National Park" = "#c4cea6", "Nature Reserve" = "#e4d0bb"),
    name   = "Terrestrial Parks",
    guide  = guide_legend(order = 3)
  ) +

  # Layer 4: State marine parks
  geom_sf(data = marine_parks_state,
          fill = alpha("grey70", 0.4), colour = "white", linewidth = 0.35) +

  # Layer 5: Commonwealth marine parks
  geom_sf(data = marine_parks_amp,
          fill = alpha("grey70", 0.4), colour = "white", linewidth = 0.35) +

  coord_sf(xlim = c(106, 145), ylim = c(-45, -22), crs = 4326, expand = FALSE) +
  scale_x_continuous(breaks = seq(110, 145, by = 5)) +
  scale_y_continuous(breaks = seq(-45, -22, by = 5)) +

  theme_minimal() +
  theme(
    legend.key.size  = unit(0.5,  "cm"),
    legend.text      = element_text(size = 8),
    legend.title     = element_text(size = 9, face = "bold"),
    legend.position  = "right",
    legend.box       = "vertical",
    panel.grid       = element_blank(),
    panel.background = element_rect(fill = "white", colour = NA),
    plot.background  = element_rect(fill = "white",   colour = NA),
    axis.text        = element_text(size = 9, colour = "grey40"),
    axis.ticks       = element_line(colour = "grey60"),
    plot.title       = element_text(face = "bold", size = 13, hjust = 0),
    plot.subtitle    = element_text(size = 9, colour = "grey40", hjust = 0),
    plot.margin      = margin(t = 8, r = 15, b = 5, l = 5)
  )

ggsave(paste(paste0("plots/", park, "/spatial/SST/", name),
             "SST-anomaly-june2011.png", sep = "-"),
       plot = p_sst_anom, dpi = 300, width = 14, height = 9, bg = "white"
)


# ==============================================================================
# legend option 2 - for SST
# ==============================================================================
p_sst_mean_right <- ggplot() +

  geom_raster(data = sst_mean_df, aes(x = x, y = y, fill = sst), interpolate = FALSE) +
  scale_fill_gradientn(
    name     = "SST (°C)  June 2011",
    colours  = rev(brewer.pal(11, "RdYlBu")),
    na.value = NA,
    guide    = guide_colourbar(
      barwidth       = unit(0.5, "cm"),
      barheight      = unit(5,   "cm"),
      title.position = "top",
      title.hjust    = 0
    )
  ) +

  new_scale_fill() +
  geom_sf(data = aus, fill = "seashell2", colour = "grey80", linewidth = 0.15) +

  new_scale_fill() +
  geom_sf(data = terrnp, aes(fill = TYPE), colour = NA) +           # show.legend removed
  scale_fill_manual(
    values = c("National Park" = "#c4cea6", "Nature Reserve" = "#e4d0bb"),
    name   = "Terrestrial Parks",
    guide  = guide_legend(order = 2)
  ) +

  geom_sf(data = marine_parks_state,
          fill = alpha("grey70", 0.4), colour = "white", linewidth = 0.35) +

  geom_sf(data = marine_parks_amp,
          fill = alpha("grey70", 0.4), colour = "white", linewidth = 0.35) +

  coord_sf(xlim = c(106, 145), ylim = c(-45, -22), crs = 4326, expand = FALSE) +
  scale_x_continuous(breaks = seq(110, 145, by = 5)) +
  scale_y_continuous(breaks = seq(-45, -22, by = 5)) +
  labs(x = NULL, y = NULL, title = NULL) +
  theme_minimal() +
  theme(
    legend.key.size  = unit(0.5, "cm"),
    legend.text      = element_text(size = 8),
    legend.title     = element_text(size = 9, face = "bold"),
    legend.position  = "right",
    legend.box       = "vertical",
    panel.grid       = element_blank(),
    panel.background = element_rect(fill = "white", colour = NA),
    plot.background  = element_rect(fill = "white", colour = NA),
    axis.text        = element_text(size = 9, colour = "grey40"),
    axis.ticks       = element_line(colour = "grey60"),
    plot.margin      = margin(t = 5, r = 10, b = 5, l = 5)
  )

ggsave(paste(paste0("plots/", park, "/spatial/SST/", name),
             "SST-june2011-mean-rightlegend.png", sep = "-"),
       plot = p_sst_mean_right, dpi = 300, width = 14, height = 9, bg = "white"
)
# ==============================================================================
# END
# ==============================================================================
