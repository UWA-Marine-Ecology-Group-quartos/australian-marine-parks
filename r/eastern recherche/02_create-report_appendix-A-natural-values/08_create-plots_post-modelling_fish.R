###
# Project: NESP 4.20 - Marine Park Dashboard reporting
# Data:    Fish data synthesis & habitat models derived from FSSgam
# Task:    Create post-modelling fish figures for marine park reporting
# Author:  Claude Spencer
# Date:    June 2024
###

# Clear your environment
rm(list = ls())

# Set the study name
script_dir <- dirname(
  rstudioapi::getActiveDocumentContext()$path
)

config <- yaml::read_yaml(
  file.path(script_dir, "00_config.yml")
)

name <- config$name
park <- config$park
years <- config$years

# Load libraries
library(tidyverse)
library(terra)
library(sf)
library(ggplot2)
library(ggnewscale)
library(scales)
library(viridis)
library(patchwork)
library(tidyterra)
library(tidytext)
library(ggtext)
library(CheckEM)

# Load functions
file.sources <- list.files(pattern = "*.R", path = "functions/", full.names = TRUE)
sapply(file.sources, source, .GlobalEnv)

# TODO Set cropping extent - larger than most zoomed out plot
e <- ext(123.1, 124.0, -34.7, -33.9)

# Load necessary spatial files
sf_use_s2(FALSE)

# Australian outline and state and commonwealth marine parks
marine_parks <- st_read("data/south-west network/spatial/shapefiles/western-australia_marine-parks-all.shp") %>%
  dplyr::filter(name %in% c("Eastern Recherche")) # TODO select relevant parks

marine_parks_amp <- marine_parks %>%
  dplyr::filter(epbc %in% "Commonwealth") %>%
  st_transform(4326)

marine_parks_state <- marine_parks %>%
  dplyr::filter(epbc %in% "State") %>%
  st_transform(4326)

# Australian outline
aus <- st_read("data/south-west network/spatial/shapefiles/aus-shapefile-w-investigator-stokes.shp")
ausc <- aus %>%
  st_crop(e) %>%
  st_transform(4326)

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

# Spatial predictions limits
prediction_limits <- c(123.1, 124.0, -34.7, -33.9)

# Pretty fish metric names mapped to raster layer stubs
fish_metric_lookup <- c(
  "Whole assemblage" = "richness",
  "CTI" = "cti",
  "Large Reef Fish Index*" = "b20",
  "Total abundance" = "abundance"
)

# Read all years once
dat_list <- setNames(vector("list", length(years)), years)

for (yr in years) {
  message("Reading year: ", yr)

  dat <- readRDS(
    paste0(
      "output/model-output/", park, "/fish/",
      name, "_predicted-fish_", yr, ".rds"
    )
  )

  if (!inherits(dat, "SpatRaster")) dat <- terra::rast(dat)
  terra::crs(dat) <- "EPSG:4326"

  dat_list[[as.character(yr)]] <- dat
}

# -------------------------------------------------------------------
# Fish metric plots
# -------------------------------------------------------------------
for (metric_name in names(fish_metric_lookup)) {

  message("Building fish metric plot for: ", metric_name)

  layer_stub <- fish_metric_lookup[[metric_name]]

  # Only build plot if every year has both prediction and SE layers
  has_all_layers <- all(unlist(lapply(dat_list, function(x) {
    c(
      paste0("p_", layer_stub, ".fit") %in% names(x),
      paste0("p_", layer_stub, ".se.fit") %in% names(x)
    )
  })))

  if (!has_all_layers) {
    message("Skipping ", metric_name, ": missing .fit or .se.fit layer in one or more years")
    next
  }

  p_metric <- fishmetric_plot(
    metric_name = metric_name,
    layer_stub = layer_stub,
    dat_list = dat_list,
    prediction_limits = prediction_limits,
    pred_limits = NULL,   # set numeric vector if you want fixed limits
    se_limits = NULL      # auto-scale within metric across years
  )

  print(p_metric)

  out_name <- metric_name %>%
    str_to_lower() %>%
    str_replace_all("[^a-z0-9]+", "-") %>%
    str_replace_all("(^-|-$)", "")

  ggsave(
    filename = paste0(
      "plots/", park, "/fish/", name,
      "_predicted-individual-fish-metric_", out_name, "_",
      paste(years, collapse = "-"), ".png"
    ),
    plot = p_metric,
    height = 5,
    width = 8,
    dpi = 900,
    units = "in",
    bg = "white"
  )

  saveRDS(p_metric,
          paste0( "plots/", park, "/fish/", name,
                  "_predicted-individual-fish-metric_", out_name, "_",
                  paste(years, collapse = "-"), ".rds")
  )
}

# -------------------------------------------------------------------
# Control plots by metric, facetted by depth class
# -------------------------------------------------------------------
controldata_fish <- function(dat, year, amp_abbrv) {

  marine_parks <- st_read("data/south-west network/spatial/shapefiles/western-australia_marine-parks-all.shp") %>%
    CheckEM::clean_names() %>%
    dplyr::filter(epbc %in% "Commonwealth") %>%
    dplyr::mutate(zone_new = case_when(
      str_detect(zone, "Habitat Protection Zone") ~ paste(amp_abbrv, "other zones"),
      str_detect(zone, "National Park Zone")       ~ paste(amp_abbrv, "NPZ (IUCN II)"),
      str_detect(zone, "Special Purpose Zone")     ~ paste(amp_abbrv, "other zones"),
      str_detect(zone, "Multiple Use Zone")        ~ paste(amp_abbrv, "other zones"),
      str_detect(zone, "Recreational Use Zone")    ~ paste(amp_abbrv, "other zones"),
      TRUE ~ NA_character_
    )) %>%
    dplyr::mutate(status = ifelse(str_detect(zone_new, "NPZ"), "No-Take", "Fished"))

  preds <- readRDS(paste0("data/", park, "/spatial/rasters/",
                          name, "_bathymetry-derivatives.rds")) %>%
    crop(dat)

  tempdat_v <- terra::vect(as.data.frame(dat, xy = TRUE), geom = c("x", "y"), crs = "epsg:4326")
  tempdat <- cbind(
    as.data.frame(dat, xy = TRUE),
    terra::extract(preds[[1]], tempdat_v, ID = FALSE)
  )

  depth_qs <- c(-2000, -200, -70, -30, 0)
  class_values <- 4:1
  reclass_matrix <- cbind(depth_qs[-length(depth_qs)], depth_qs[-1], class_values)

  edc <- classify(preds$geoscience_depth, rcl = reclass_matrix) %>%
    as.polygons() %>%
    st_as_sf()

  areas <- st_intersection(edc, marine_parks) %>%
    dplyr::mutate(area = st_area(.)) %>%
    dplyr::filter(area > units::set_units(625000, "m^2")) %>%
    dplyr::mutate(
      depth_contour = case_when(
        geoscience_depth == 1 ~ "shallow",
        geoscience_depth == 2 ~ "mesophotic",
        geoscience_depth == 3 ~ "rariphotic",
        geoscience_depth == 4 ~ "deep"
      ),
      filter = "no"
    ) %>%
    dplyr::select(zone, depth_contour, filter) %>%
    as.data.frame() %>%
    dplyr::select(-geometry)

  areas_shallow <- dplyr::filter(areas, depth_contour %in% "shallow") %>%
    dplyr::distinct(zone, filter, .keep_all = TRUE)

  areas_meso <- dplyr::filter(areas, depth_contour %in% "mesophotic") %>%
    dplyr::distinct(zone, filter, .keep_all = TRUE)

  areas_rari <- dplyr::filter(areas, depth_contour %in% "rariphotic") %>%
    dplyr::distinct(zone, filter, .keep_all = TRUE)

  replacement_se <- c(
    "cti_se"       = "p_cti.se.fit",
    "richness_se"  = "p_richness.se.fit",
    "abundance_se" = "p_abundance.se.fit",
    "b20_se"       = "p_b20.se.fit"
  )

  replacement_mean <- c(
    "cti"       = "p_cti.fit",
    "richness"  = "p_richness.fit",
    "abundance" = "p_abundance.fit",
    "b20"       = "p_b20.fit"
  )

  out <- list(shallow = NULL, meso = NULL, rari = NULL)

  # SHALLOW (0-30 m)
  if (any(tempdat$geoscience_depth < 0 & tempdat$geoscience_depth > -30, na.rm = TRUE)) {

    shallow <- preds[[1]] %>% clamp(upper = 0, lower = -30, values = FALSE)
    dat.shallow <- dat %>% terra::mask(shallow)

    errors.shallow <- terra::extract(dat.shallow, marine_parks) %>%
      dplyr::group_by(ID) %>%
      dplyr::summarise(
        dplyr::across(dplyr::ends_with(".se.fit"), \(x) mean(x, na.rm = TRUE)),
        .groups = "drop"
      ) %>%
      dplyr::mutate(ID = as.character(ID), year = year) %>%
      dplyr::rename(dplyr::any_of(replacement_se)) %>%
      dplyr::select(ID, year, dplyr::any_of(c("cti_se", "richness_se", "abundance_se", "b20_se")))

    means.shallow <- terra::extract(dat.shallow, marine_parks) %>%
      dplyr::group_by(ID) %>%
      dplyr::summarise(
        dplyr::across(dplyr::matches("^p_.*(?<!\\.se)\\.fit$", perl = TRUE), \(x) mean(x, na.rm = TRUE)),
        .groups = "drop"
      ) %>%
      dplyr::mutate(ID = as.character(ID), year = year) %>%
      dplyr::rename(dplyr::any_of(replacement_mean)) %>%
      dplyr::select(ID, year, dplyr::any_of(c("cti", "richness", "abundance", "b20")))

    out$shallow <- as.data.frame(marine_parks) %>%
      tibble::rownames_to_column() %>%
      dplyr::rename(ID = rowname) %>%
      dplyr::left_join(errors.shallow, by = "ID") %>%
      dplyr::left_join(means.shallow,  by = c("ID", "year")) %>%
      dplyr::left_join(areas_shallow,  by = "zone") %>%
      dplyr::filter(filter == "no") %>%
      dplyr::select(zone_new, status, year,
                    cti, cti_se,
                    richness, richness_se,
                    abundance, abundance_se,
                    b20, b20_se) %>%
      dplyr::filter(!is.na(b20)) %>%
      dplyr::group_by(zone_new, status, year) %>%
      dplyr::summarise(
        dplyr::across(dplyr::everything(), \(x) mean(x, na.rm = TRUE)),
        .groups = "drop"
      )
  }

  # MESOPHOTIC (30-70 m)
  if (any(tempdat$geoscience_depth < -30 & tempdat$geoscience_depth > -70, na.rm = TRUE)) {

    meso <- preds[[1]] %>% clamp(upper = -30, lower = -70, values = FALSE)
    dat.meso <- dat %>% terra::mask(meso)

    errors.meso <- terra::extract(dat.meso, marine_parks) %>%
      dplyr::group_by(ID) %>%
      dplyr::summarise(
        dplyr::across(dplyr::ends_with(".se.fit"), \(x) mean(x, na.rm = TRUE)),
        .groups = "drop"
      ) %>%
      dplyr::mutate(ID = as.character(ID), year = year) %>%
      dplyr::rename(dplyr::any_of(replacement_se)) %>%
      dplyr::select(ID, year, dplyr::any_of(c("cti_se", "richness_se", "abundance_se", "b20_se")))

    means.meso <- terra::extract(dat.meso, marine_parks) %>%
      dplyr::group_by(ID) %>%
      dplyr::summarise(
        dplyr::across(dplyr::matches("^p_.*(?<!\\.se)\\.fit$", perl = TRUE), \(x) mean(x, na.rm = TRUE)),
        .groups = "drop"
      ) %>%
      dplyr::mutate(ID = as.character(ID), year = year) %>%
      dplyr::rename(dplyr::any_of(replacement_mean)) %>%
      dplyr::select(ID, year, dplyr::any_of(c("cti", "richness", "abundance", "b20")))

    out$meso <- as.data.frame(marine_parks) %>%
      tibble::rownames_to_column() %>%
      dplyr::rename(ID = rowname) %>%
      dplyr::left_join(errors.meso, by = "ID") %>%
      dplyr::left_join(means.meso,  by = c("ID", "year")) %>%
      dplyr::left_join(areas_meso,  by = "zone") %>%
      dplyr::filter(filter == "no") %>%
      dplyr::select(zone_new, status, year,
                    cti, cti_se,
                    richness, richness_se,
                    abundance, abundance_se,
                    b20, b20_se) %>%
      dplyr::filter(!is.na(b20)) %>%
      dplyr::group_by(zone_new, status, year) %>%
      dplyr::summarise(
        dplyr::across(dplyr::everything(), \(x) mean(x, na.rm = TRUE)),
        .groups = "drop"
      )
  }

  # RARIPHOTIC (70-200 m)
  if (any(tempdat$geoscience_depth < -70 & tempdat$geoscience_depth > -200, na.rm = TRUE)) {

    rari <- preds[[1]] %>% clamp(upper = -70, lower = -200, values = FALSE)
    dat.rari <- dat %>% terra::mask(rari)

    errors.rari <- terra::extract(dat.rari, marine_parks) %>%
      dplyr::group_by(ID) %>%
      dplyr::summarise(
        dplyr::across(dplyr::ends_with(".se.fit"), \(x) mean(x, na.rm = TRUE)),
        .groups = "drop"
      ) %>%
      dplyr::mutate(ID = as.character(ID), year = year) %>%
      dplyr::rename(dplyr::any_of(replacement_se)) %>%
      dplyr::select(ID, year, dplyr::any_of(c("cti_se", "richness_se", "abundance_se", "b20_se")))

    means.rari <- terra::extract(dat.rari, marine_parks) %>%
      dplyr::group_by(ID) %>%
      dplyr::summarise(
        dplyr::across(dplyr::matches("^p_.*(?<!\\.se)\\.fit$", perl = TRUE), \(x) mean(x, na.rm = TRUE)),
        .groups = "drop"
      ) %>%
      dplyr::mutate(ID = as.character(ID), year = year) %>%
      dplyr::rename(dplyr::any_of(replacement_mean)) %>%
      dplyr::select(ID, year, dplyr::any_of(c("cti", "richness", "abundance", "b20")))

    out$rari <- as.data.frame(marine_parks) %>%
      tibble::rownames_to_column() %>%
      dplyr::rename(ID = rowname) %>%
      dplyr::left_join(errors.rari, by = "ID") %>%
      dplyr::left_join(means.rari,  by = c("ID", "year")) %>%
      dplyr::left_join(areas_rari,  by = "zone") %>%
      dplyr::filter(filter == "no") %>%
      dplyr::select(zone_new, status, year,
                    cti, cti_se,
                    richness, richness_se,
                    abundance, abundance_se,
                    b20, b20_se) %>%
      dplyr::filter(!is.na(b20)) %>%
      dplyr::group_by(zone_new, status, year) %>%
      dplyr::summarise(
        dplyr::across(dplyr::everything(), \(x) mean(x, na.rm = TRUE)),
        .groups = "drop"
      )
  }

  out
}

control_all <- purrr::map(years, \(yy) {
  dat_yy <- readRDS(
    paste0(
      "output/model-output/", park, "/fish/",
      name, "_predicted-fish_", yy, ".rds"
    )
  )
  if (!inherits(dat_yy, "SpatRaster")) dat_yy <- terra::rast(dat_yy)
  terra::crs(dat_yy) <- "EPSG:4326"
  controldata_fish(dat = dat_yy, year = yy, amp_abbrv = "ERMP") # TODO update amp_abbrv
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

metric_lookup <- c(
  "richness"  = "Species richness (per BRUV)",
  "cti"       = "Community Thermal Index (\u00B0C)",
  "b20"       = "Large reef fish index* (biomass g per BRUV)",
  "abundance" = "Total abundance (per BRUV)"
)

controlplot_fish <- function(data, metric, amp_abbrv,
                             metric_label = NULL,
                             depth_levels = c("Shallow (0 - 30 m)",
                                              "Mesophotic (30 - 70 m)",
                                              "Rariphotic (70 - 200 m)")) {

  mean_col <- metric
  se_col   <- paste0(metric, "_se")

  if (is.null(metric_label)) {
    metric_label <- dplyr::case_when(
      metric == "richness"  ~ "Species richness (per BRUV)",
      metric == "cti"       ~ "Community Thermal Index (\u00B0C)",
      metric == "b20"       ~ "Large reef fish index* (biomass g per BRUV)",
      metric == "abundance" ~ "Total abundance (per BRUV)",
      TRUE ~ stringr::str_to_title(metric)
    )
  }

  req_cols <- c("year", "zone_new", "depth_class", mean_col, se_col)
  if (!all(req_cols %in% names(data))) {
    stop("Data is missing one or more required columns: ",
         paste(setdiff(req_cols, names(data)), collapse = ", "))
  }

  plot_dat <- data %>%
    dplyr::filter(!is.na(.data[[mean_col]])) %>%
    dplyr::mutate(
      year = as.numeric(year),
      depth_class = factor(depth_class, levels = depth_levels),
      zone_new = factor(
        zone_new,
        levels = c(
          paste(amp_abbrv, "other zones")
        )
      )
    )

  if (nrow(plot_dat) == 0) {
    message("No data available to plot for ", metric_label)
    return(NULL)
  }

  fill_vals <- setNames(
    c( "#b9e6fb"),
    c(
      paste(amp_abbrv, "other zones")
    )
  )

  shape_vals <- setNames(
    c(21, 21),
    c(
      paste(amp_abbrv, "NPZ (IUCN II)"),
      paste(amp_abbrv, "other zones")
    )
  )

  shape_vals <- setNames(
    c(21),
    c(
      paste(amp_abbrv, "other zones")
    )
  )

  if (metric == "cti") {

    sst <- readRDS(
      paste0("data/", park, "/spatial/oceanography/",
             name, "_SST_time-series.rds")
    ) %>%
      dplyr::mutate(year = as.numeric(year)) %>%
      dplyr::group_by(year) %>%
      dplyr::summarise(
        sst = mean(sst, na.rm = TRUE),
        sd  = mean(sd,  na.rm = TRUE),
        .groups = "drop"
      )

    p <- ggplot() +
      geom_line(
        data = sst,
        aes(x = year, y = sst)
      ) +
      geom_ribbon(
        data = sst,
        aes(x = year, ymin = sst - sd, ymax = sst + sd),
        alpha = 0.2
      ) +
      geom_errorbar(
        data = plot_dat,
        aes(
          x = year,
          y = .data[[mean_col]],
          ymin = .data[[mean_col]] - .data[[se_col]],
          ymax = .data[[mean_col]] + .data[[se_col]],
          fill = zone_new,
          shape = zone_new
        ),
        width = 0.8,
        position = position_dodge(width = 0.6)
      ) +
      geom_point(
        data = plot_dat,
        aes(
          x = year,
          y = .data[[mean_col]],
          fill = zone_new,
          shape = zone_new
        ),
        size = 3,
        position = position_dodge(width = 0.6),
        stroke = 0.2,
        color = "black",
        alpha = 0.8
      ) +
      geom_vline(
        xintercept = 2023.5,
        linetype = "dashed",
        color = "black",
        linewidth = 0.5,
        alpha = 0.5
      ) +
      facet_wrap(~depth_class, ncol = 1, scales = "free_y") +
      theme_classic() +
      scale_x_continuous(breaks = c(2022, 2025)) +
      coord_cartesian(xlim = c(2021, 2026)) +
      scale_fill_manual(values = fill_vals, name = "Marine Parks", drop = FALSE) +
      scale_shape_manual(values = shape_vals, name = "Marine Parks", drop = FALSE) +
      labs(
        x = "Year",
        y = metric_label,
        title = NULL
      ) +
      theme(
        legend.position = "right",
        strip.background = element_blank(),
        strip.text = element_text(face = "bold")
      )

  } else {

    p <- ggplot(
      data = plot_dat,
      aes(x = year, y = .data[[mean_col]], fill = zone_new, shape = zone_new)
    ) +
      geom_errorbar(
        aes(
          ymin = pmax(.data[[mean_col]] - .data[[se_col]], 0),
          ymax = .data[[mean_col]] + .data[[se_col]]
        ),
        width = 0.8,
        position = position_dodge(width = 0.6)
      ) +
      geom_point(
        size = 3,
        position = position_dodge(width = 0.6),
        stroke = 0.2,
        color = "black",
        alpha = 0.8
      ) +
      geom_vline(
        xintercept = 2023.5,
        linetype = "dashed",
        color = "black",
        linewidth = 0.5,
        alpha = 0.5
      ) +
      facet_wrap(~depth_class, ncol = 1, scales = "free_y") +
      theme_classic() +
      scale_x_continuous(breaks = c(2022, 2025)) +
      coord_cartesian(xlim = c(2021, 2026)) +
      scale_fill_manual(values = fill_vals, name = "Marine Parks", drop = FALSE) +
      scale_shape_manual(values = shape_vals, name = "Marine Parks", drop = FALSE) +
      labs(
        x = "Year",
        y = metric_label,
        title = NULL
      ) +
      theme(
        legend.position = "right",
        strip.background = element_blank(),
        strip.text = element_text(face = "bold")
      )
  }

  return(p)
}
for (metric_code in names(metric_lookup)) {

  message("Building control plot for metric: ", metric_lookup[[metric_code]])

  p_metric <- controlplot_fish(
    data = park_dat.control,
    metric = metric_code,
    amp_abbrv = "ERMP", # TODO park abbreviations
    metric_label = metric_lookup[[metric_code]]
  )

  if (!is.null(p_metric)) {

    print(p_metric)

    out_name <- metric_lookup[[metric_code]] %>%
      stringr::str_to_lower() %>%
      stringr::str_replace_all("\u00b0", "") %>%
      stringr::str_replace_all("\\*", "") %>%
      stringr::str_replace_all("[()]", "") %>%
      stringr::str_replace_all("[[:space:]]+", "-")

    ggsave(
      filename = paste0(
        "plots/", park, "/fish/", name, "_control-plot_", out_name, ".png"
      ),
      plot = p_metric,
      height = 4,
      width = 6,
      dpi = 300,
      units = "in",
      bg = "white"
    )

    saveRDS(p_metric,
            paste0("plots/", park, "/fish/", name, "_control-plot_", out_name, ".rds")
    )
  }
}


# Stacked plots

theme_collapse<-theme(
  panel.grid.major=element_line(colour = "white"),
  panel.grid.minor=element_line(colour = "white", size = 0.25),
  plot.margin= grid::unit(c(0, 0, 0, 0), "in"))

theme.larger.text<-theme(
  strip.text.x = element_text(size = 5,angle = 0),
  strip.text.y = element_text(size = 5),
  axis.title.x=element_text(vjust=-0.0, size=10),
  axis.title.y=element_text(vjust=0.0,size=10),
  axis.text.x=element_text(size=8),
  axis.text.y=element_text(size=8),
  legend.title = element_text(family="TN",size=8),
  legend.text = element_text(family="TN",size=8))

# read in STI
sti <- CheckEM::australia_life_history %>%
  clean_names() %>%
  dplyr::select(family, genus, species, rls_thermal_niche) %>%
  mutate(scientific = paste(genus, species, sep = " ")) %>%
  dplyr::distinct() %>%
  glimpse()

# Create DF filter for Commonwealth waters only
marine_parks_amp <- st_read("data/south-west network/spatial/shapefiles/western-australia_marine-parks-all.shp") %>%
  dplyr::filter(name %in% c("Eastern Recherche")) %>% # TODO select relevant parks
  dplyr::filter(epbc == "Commonwealth") %>%
  st_transform(4326)

metadata_amp <- readRDS(paste0("data/", park, "/raw/metadata.RDS")) %>%
  distinct(campaignid, sample, .keep_all = TRUE) %>%
  st_as_sf(coords = c("longitude_dd", "latitude_dd"), crs = 4326, remove = FALSE) %>%
  st_join(
    marine_parks_amp %>% dplyr::select(name, epbc),
    join = st_within,
    left = FALSE
  ) %>%
  st_drop_geometry()

# -----------------------------
# Species Accumulation Curves
# -----------------------------

sac_df <- readRDS(paste0("data/", park, "/tidy/", name, "_species-accumulation.rds"))
sac_df <- sac_df %>%
  mutate(year = stringr::str_extract(Year, "^\\d{4}"))
base_theme <- theme_bw(base_size = 13)
yr_levels <- as.character(config$years)

sac_sample <- ggplot(
  sac_df %>%
    filter(curve == "Sample-based detection/non-detection") %>%
    mutate(year = factor(year, levels = yr_levels)),
  aes(x = x, y = richness, colour = status, fill = status, linetype = year)
) +
  geom_ribbon(aes(ymin = richness - sd, ymax = richness + sd), alpha = 0.18, colour = NA) +
  geom_line(linewidth = 1.2) +
  scale_linetype_manual(name = "Year", values = setNames(c("22", "solid"), yr_levels)) +
  scale_colour_manual(name = "Status", values = c("No-Take" = "#7bbc63", "Fished" = "#b9e6fb")) +
  scale_fill_manual(name = "Status", values = c("No-Take" = "#7bbc63", "Fished" = "#b9e6fb")) +
  labs(x = "Number of BRUV deployments", y = "Species richness") +
  base_theme +
  theme(legend.position = "right", legend.box = "vertical")


ggsave(
  paste0("plots/", park, "/fish/", name, "_SAC-sample.png"),
  plot = sac_sample,
  height = 4,
  width = 7,
  dpi = 600,
  units = "in",
  bg = "white"
)

saveRDS(sac_sample,
        paste0("plots/", park, "/fish/", name, "_SAC-sample.rds")
)

sac_individual <- ggplot(
  sac_df %>%
    filter(curve == "Individual-based rarefaction") %>%
    mutate(year = factor(year, levels = yr_levels)),
  aes(
    x = x,
    y = richness,
    colour = status,
    fill = status,
    linetype = year
  )
) +
  geom_ribbon(
    aes(ymin = richness - sd, ymax = richness + sd),
    alpha = 0.18,
    colour = NA
  ) +
  geom_line(linewidth = 1.2) +
  scale_linetype_manual(
    name = "Year",
    values = setNames(c("22", "solid"), yr_levels)
  ) +
  scale_colour_manual(
    name = "Status",
    values = c("No-Take" = "#7bbc63", "Fished" = "#b9e6fb")
  ) +
  scale_fill_manual(
    name = "Status",
    values = c("No-Take" = "#7bbc63", "Fished" = "#b9e6fb")
  ) +
  labs(x = "Cumulative MaxN individuals", y = "Species richness") +
  base_theme

sac_plot <- sac_sample / sac_individual +
  plot_layout(guides = "collect") +
  plot_annotation(tag_levels = "a", tag_suffix = ")") &
  theme(
    legend.position = "right",
    plot.tag = element_text(face = "bold", size = 14)
  )


sac_plot

ggsave(
  paste0("plots/", park, "/fish/", name, "_SAC-faceted.png"),
  plot = sac_plot,
  height = 8,
  width = 7,
  dpi = 600,
  units = "in",
  bg = "white"
)

# Read in maxn (Commonwealth only)
maxn <- readRDS(paste0("data/", park, "/raw/_count-with-zeros.RDS")) %>%
  semi_join(metadata_amp, by = c("campaignid", "sample")) %>%
  mutate(year = year(date_time)) %>%
  left_join(sti, by = c("family", "genus", "species")) %>%
  select(
    year, sample, scientific_name, family, genus, species, count,
    rls_thermal_niche
  ) %>%
  glimpse()

length(unique(maxn$sample)) * length(unique(maxn$scientific_name))

# workout mean maxn for each species ---
maxn.10 <- maxn %>%
  mutate(scientific = paste(genus, species, sep = " ")) %>%
  group_by(year, scientific) %>%
  summarise(
    maxn = mean(count, na.rm = TRUE),
    se   = sd(count, na.rm = TRUE) / sqrt(dplyr::n()),
    .groups = "drop") %>%
  group_by(year) %>%
  slice_max(order_by = maxn, n = 10, with_ties = FALSE) %>%
  ungroup() %>%
  left_join(sti) %>%
  glimpse()

spy1 <- maxn.10 %>% filter(year == years[1]) %>% pull(scientific)
spy2 <- maxn.10 %>% filter(year == years[2]) %>% pull(scientific)

unique_species <- union(
  setdiff(spy1, spy2),
  setdiff(spy2, spy1))

bar_maxn <- ggplot(
  maxn.10 %>%
    mutate(scientific_label = if_else(scientific %in% unique_species,
                                      paste0("**", scientific, "**"),
                                      scientific)),
  aes(x = reorder_within(scientific_label, maxn, year), y = maxn)
) +
  geom_col(colour = "black", linewidth = 0.25) +
  geom_errorbar(aes(ymin = pmax(maxn - se, 0), ymax = maxn + se), width = 0.2) +
  coord_flip() +
  facet_wrap(~year, scales = "free_y") +
  scale_x_reordered() +
  labs(
    x = "Species",
    y = expression(Average~abundance~(MaxN~per~BRUV))) +
  theme_bw() +
  theme_collapse +
  theme(axis.text.y = element_markdown(),
        panel.grid.major.x = element_line(color = "grey90"))

bar_maxn

ggsave(paste0("plots/", park, "/fish/", name, "_top_maxn_bar_plot.png"),
       plot = bar_maxn, height = 4, width = 9, dpi = 600, units = "in", bg = "white")

saveRDS(bar_maxn,
        paste0("plots/", park, "/fish/", name, "_top_maxn_bar_plot.rds")
)


# Thermal Index stacked plot
cti.10 <- maxn %>%
  mutate(scientific = paste(genus, species, sep = " ")) %>%
  group_by(year, scientific) %>%
  summarise(
    maxn = mean(count, na.rm = TRUE),
    se   = sd(count, na.rm = TRUE) / sqrt(dplyr::n()),
    .groups = "drop") %>%
  left_join(sti) %>%
  filter(!is.na(rls_thermal_niche)) %>%
  group_by(year) %>%
  slice_max(order_by = maxn, n = 10, with_ties = FALSE) %>%
  ungroup() %>%
  glimpse()

sp.cti.y1 <- cti.10 %>% filter(year == years[1]) %>% pull(scientific)
sp.cti.y2 <- cti.10 %>% filter(year == years[2]) %>% pull(scientific)

unique_species_cti <- union(
  setdiff(sp.cti.y1, sp.cti.y2),
  setdiff(sp.cti.y2, sp.cti.y1))

log1p10_trans <- trans_new(
  name = "log10p1",
  transform = function(x) log10(x + 1),
  inverse   = function(x) 10^x - 1
)

# choose the centering statistic
mid_niche <- median(cti.10$rls_thermal_niche, na.rm = TRUE)

# global limits across both facets/years
niche_limits <- range(cti.10$rls_thermal_niche, na.rm = TRUE)

bar_cti <- ggplot(
  cti.10 %>%
    mutate(
      scientific_label = if_else(scientific %in% unique_species_cti,
                                 paste0("**", scientific, "**"),
                                 scientific),
      niche_lab = scales::number(rls_thermal_niche, accuracy = 0.01)
    ),
  aes(
    x = reorder_within(scientific_label, rls_thermal_niche, year),
    y = maxn,
    fill = rls_thermal_niche
  )
) +
  geom_col(colour = "black", linewidth = 0.25) +
  geom_errorbar(
    aes(
      ymin = pmax(maxn - se, 0),
      ymax = maxn + se
    ),
    width = 0.2
  ) +
  geom_text(aes(y = 23, label = niche_lab), hjust = 0, size = 3) +
  coord_flip(clip = "off") +
  facet_wrap(~year, scales = "free_y") +
  scale_x_reordered() +
  scale_y_continuous(
    trans = log1p10_trans,
    expand = expansion(mult = c(0, 0.15)),
    breaks = c(0, 5, 10, 20),
    labels = scales::label_number()
  ) +
  # centre GREY at the mean thermal niche
  scale_fill_gradientn(
    colours = c("#2b83ba", "grey", "#d7191c"),
    values  = scales::rescale(c(niche_limits[1],
                                mid_niche,
                                niche_limits[2])),
    limits = niche_limits,
    na.value = "grey80"
  ) +
  guides(fill = "none") +
  labs(
    x = "Species",
    y = expression(Log[10]~(Average~abundance~+~1))
  ) +
  theme_bw() +
  theme_collapse +
  theme(axis.text.y = element_markdown(),
        panel.grid.major.x = element_line(color = "grey90"))

bar_cti

ggsave(paste0("plots/", park, "/fish/", name, "_top_maxn_cti_bar_plot.png"),
       plot = bar_cti, height = 4, width = 9, dpi = 600, units = "in", bg = "white")

saveRDS(bar_cti,
        paste0("plots/", park, "/fish/", name, "_top_maxn_cti_bar_plot.rds")
)

# B20 ---------------------------------------------------------------------

# read in b20 species summaries (already mean + sd per year x species)
b20 <- readRDS(paste0("data/", park, "/tidy/", name, "_b20-species_amp.rds"))

# top 10 b20 per year using combined values only
b20.10 <- b20 %>%
  filter(status == "Combined") %>%
  group_by(year) %>%
  slice_max(order_by = b20, n = 10, with_ties = FALSE) %>%
  ungroup()

# species unique to either year's top 10 (for bold labels)
spy1_b20 <- b20.10 %>%
  filter(year == years[1]) %>%
  pull(scientific_name)

spy2_b20 <- b20.10 %>%
  filter(year == years[2]) %>%
  pull(scientific_name)

unique_species_b20 <- union(
  setdiff(spy1_b20, spy2_b20),
  setdiff(spy2_b20, spy1_b20)
)

# common plot function
plot_b20_bars <- function(plot_data, fill_values, fill_breaks) {
  ggplot(
    plot_data %>%
      mutate(
        scientific_label = if_else(
          scientific_name %in% unique_species_b20,
          paste0("**", scientific_name, "**"),
          scientific_name
        )
      ),
    aes(
      x = reorder_within(scientific_label, b20, year),
      y = b20,
      fill = status
    )
  ) +
    geom_col(
      position = position_dodge(width = 0.8),
      width = 0.7,
      colour = "black",
      linewidth = 0.25
    ) +
    geom_errorbar(
      aes(ymin = pmax(b20 - se, 0), ymax = b20 + se),
      position = position_dodge(width = 0.8),
      width = 0.2
    ) +
    coord_flip() +
    scale_y_continuous(
      trans = scales::pseudo_log_trans(base = 10),
      breaks = c(0, 1, 10, 100, 1000),
      labels = scales::label_number()
    ) +
    facet_wrap(~year, scales = "free_y") +
    scale_x_reordered() +
    scale_fill_manual(
      values = fill_values,
      breaks = fill_breaks
    ) +
    labs(
      x = "Species",
      y = expression(Average~biomass~(B20~per~BRUV)),
      fill = "Status"
    ) +
    theme_bw() +
    theme_collapse +
    theme(
      axis.text.y = element_markdown(),
      panel.grid.major.x = element_line(color = "grey90")
    )
}

# -------------------------------------------------------------------------
# Plot 1: both years split into Fished / No-Take
# -------------------------------------------------------------------------

b20_plot_split <- b20 %>%
  filter(status != "Combined") %>%
  semi_join(b20.10, by = c("year", "scientific_name")) %>%
  mutate(
    status = if_else(status %in% "Fished", "Open", status),
    status = factor(status, levels = c("Open", "No-Take"))
  )

bar_b20 <- plot_b20_bars(
  plot_data   = b20_plot_split,
  fill_values = c("Open" = "white", "No-Take" = "grey40"),
  fill_breaks = c("No-Take", "Open")
)

bar_b20

ggsave(
  paste0("plots/", park, "/fish/", name, "_top_b20_bar_plot.png"),
  plot   = bar_b20,
  height = 4,
  width  = 9,
  dpi    = 600,
  units  = "in",
  bg     = "white"
)

# -------------------------------------------------------------------------
# Plot 2: 2014 Combined, 2024 split into Fished / No-Take
# -------------------------------------------------------------------------

b20_plot_mixed <- b20 %>%
  semi_join(b20.10, by = c("year", "scientific_name")) %>%
  filter(
    (year == years[1] & status == "Combined") |
      (year == years[2] & status %in% c("Fished", "No-Take"))
  ) %>%
  mutate(
    status = if_else(status %in% c("Combined", "Fished"), "Open", status),
    status = factor(status, levels = c("Open", "No-Take"))
  )

bar_b20_v2 <- plot_b20_bars(
  plot_data   = b20_plot_mixed,
  fill_values = c(
    "Open"   = "white",
    "No-Take"  = "grey40"
  ),
  fill_breaks = c("No-Take", "Open")
)

bar_b20_v2

ggsave(
  paste0("plots/", park, "/fish/", name, "_top_b20_bar_plot_mixed.png"),
  plot   = bar_b20_v2,
  height = 4,
  width  = 9,
  dpi    = 600,
  units  = "in",
  bg     = "white"
)

saveRDS(bar_b20_v2,
        paste0("plots/", park, "/fish/", name, "_top_b20_bar_plot_mixed.rds")
)

