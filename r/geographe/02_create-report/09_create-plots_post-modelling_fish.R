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
name <- "GeographeAMP"
park <- "geographe"

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
library(png)
library(lwgeom)
library(tidytext)
library(ggtext)

# Load functions
file.sources = list.files(pattern = "*.R", path = "functions/", full.names = T)
sapply(file.sources, source, .GlobalEnv)

# Set cropping extent - larger than most zoomed out plot
e <- ext(114.2, 115.8, -34.7, -33.1)

# Load necessary spatial files
sf_use_s2(FALSE)  # Switch off spatial geometry for cropping

# Australian outline and state and commonwealth marine parks
marine_parks <- st_read("data/south-west network/spatial/shapefiles/western-australia_marine-parks-all.shp") %>%
  dplyr::filter(name %in% c("Ngari Capes", "Geographe", "South-west Corner")) %>%
  glimpse()
plot(marine_parks["zone"])

marine_parks_amp <- marine_parks %>% dplyr::filter(epbc %in% "Commonwealth")
marine_parks_state <- marine_parks %>% dplyr::filter(epbc %in% "State")

# Australian outline
aus <- st_read("data/south-west network/spatial/shapefiles/aus-shapefile-w-investigator-stokes.shp")
ausc <- st_crop(aus, e)

# Spatial predictions limits
prediction_limits <- c(115.0539, 115.5539, -33.64861, -33.35361)

# ------------------------------------------------------------
# PLOTS: loop years (mirrors habitat Script 08)
# ------------------------------------------------------------
pred.years <- c(2014L, 2024L)

for (pred_year in pred.years) {

  print(pred_year)

  # Read year-specific predictions
  dat <- readRDS(paste0("output/model-output/", park, "/fish/",
                        name, "_predicted-fish_", pred_year, ".rds"))

  # Ensure SpatRaster + CRS (fixes the unused crs arg error)
  if (!inherits(dat, "SpatRaster")) dat <- terra::rast(dat)
  terra::crs(dat) <- "EPSG:4326"

  plot(dat)

  fishmetric_plot(prediction_limits, dat = dat, year = pred_year)

  ggsave(paste0("plots/", park, "/fish/", name,
                "_individual-predictions_", pred_year, ".png"),
         width = 9, height = 5, dpi = 300, units = "in", bg = "white")
}

# ------------------------------------------------------------
# CONTROL DATA: mirrors habitat Script 08 (combine years on plots)
# ------------------------------------------------------------

pred.years <- c(2014L, 2024L)

# Create the data (returns a list per year: shallow/meso/rari)
control_all <- purrr::map(pred.years, \(yy) {

  dat_yy <- readRDS(paste0("output/model-output/", park, "/fish/",
                           name, "_predicted-fish_", yy, ".rds"))
  if (!inherits(dat_yy, "SpatRaster")) dat_yy <- terra::rast(dat_yy)
  terra::crs(dat_yy) <- "EPSG:4326"

  controldata_fish(dat = dat_yy, year = yy, amp_abbrv = "GMP", state_abbrv = "NCMP")
})

# Bind years together per depth band (so year is combined on plots)
park_dat.shallow <- purrr::map_dfr(control_all, "shallow")
park_dat.meso    <- purrr::map_dfr(control_all, "meso")
park_dat.rari    <- purrr::map_dfr(control_all, "rari")

# Shallow plot (both years together)
(p_shallow <- controlplot_fish(data = park_dat.shallow, amp_abbrv = "GMP", state_abbrv = "NCMP",
                              title = "Shallow (0 - 30 m)"))
ggsave(paste0("plots/", park, "/fish/", name, "_shallow-control-plots.png"),
       plot = p_shallow, height = 9, width = 8, dpi = 300, units = "in", bg = "white")

# Mesophotic plot (both years together)
(p_meso <- controlplot_fish(data = park_dat.meso, amp_abbrv = "GMP", state_abbrv = "NCMP",
                           title = "Mesophotic (30 - 70 m)"))
ggsave(paste0("plots/", park, "/fish/", name, "_mesophotic-control-plots.png"),
       plot = p_meso, height = 9, width = 8, dpi = 300, units = "in", bg = "white")

# Rariphotic:
# p_rari <- controlplot_fish(data = park_dat.rari, amp_abbrv = "GMP", state_abbrv = "NCMP",
#                            title = "Rariphotic (70 - 200 m)")
# ggsave(paste0("plots/", park, "/fish/", name, "_rariphotic-control-plots.png"),
#        plot = p_rari, height = 9, width = 8, dpi = 300, units = "in", bg = "white")


# Stacked plots

# Clear your environment
rm(list = ls())

# Set the study name
name <- "GeographeAMP"
park <- "geographe"

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
library(png)
library(lwgeom)
library(tidytext)
library(ggtext)

# Load functions
file.sources = list.files(pattern = "*.R", path = "functions/", full.names = T)
sapply(file.sources, source, .GlobalEnv)

# Set cropping extent - larger than most zoomed out plot
e <- ext(114.2, 115.8, -34.7, -33.1)

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

# read in maxn
maxn <- readRDS(paste0("data/", park, "/raw/_count-with-zeros.RDS")) %>%
  mutate(year = year(date_time)) %>%
  # dplyr::filter(!count > 200, # Remove some outliers
  #               # !sample %in% "779", ##HE what was 779?
  #               geoscience_roughness < 4) %>% # Remove outliers in roughness
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
  # dplyr::filter(!scientific%in%c('Carangoides sp1', 'Unknown spp'))%>%
  group_by(year) %>%
  slice_max(order_by = maxn, n = 10, with_ties = FALSE) %>%
  ungroup() %>%
  glimpse()

sp14 <- maxn.10 %>% filter(year == 2014) %>% pull(scientific)
sp24 <- maxn.10 %>% filter(year == 2024) %>% pull(scientific)

unique_species <- union(
  setdiff(sp14, sp24),
  setdiff(sp24, sp14))

bar_maxn <- ggplot(
  maxn.10 %>%
    mutate(scientific_label = if_else(scientific %in% unique_species,
                                      paste0("**", scientific, "**"),
                                      scientific)),
  aes(x = reorder_within(scientific_label, maxn, year), y = maxn)
) +
  geom_col() +
  geom_errorbar(aes(ymin = pmax(maxn - se, 0), ymax = maxn + se), width = 0.2) +
  coord_flip() +
  facet_wrap(~year, scales = "free_y") +
  scale_x_reordered() +
  labs(
    x = "Species",
    y = expression(Average~abundance~(MaxN~per~BRUV))) +
  theme_bw() +
  theme_collapse +
  theme(axis.text.y = element_markdown())

bar_maxn

ggsave(paste0("plots/", park, "/fish/", name, "_top_maxn_bar_plot.png"),
       plot = bar_maxn, height = 4, width = 9, dpi = 300, units = "in", bg = "white")

# read in b20 species summaries (already mean + sd per year x species)
b20 <- readRDS(paste0("data/", park, "/tidy/", name, "_b20-species.rds")) %>%
  glimpse()

# top 10 b20 per year (2014 & 2024)
b20.10 <- b20 %>%
  group_by(year) %>%
  slice_max(order_by = b20, n = 10, with_ties = FALSE) %>%
  ungroup() %>%
  glimpse()

# species unique to either year's top 10 (for bold labels)
sp14_b20 <- b20.10 %>% filter(year == 2014) %>% pull(scientific_name)
sp24_b20 <- b20.10 %>% filter(year == 2024) %>% pull(scientific_name)

unique_species_b20 <- union(
  setdiff(sp14_b20, sp24_b20),
  setdiff(sp24_b20, sp14_b20)
)

# plot
bar_b20 <- ggplot(
  b20.10 %>%
    mutate(scientific_label = if_else(scientific_name %in% unique_species_b20,
                                      paste0("**", scientific_name, "**"),
                                      scientific_name)),
  aes(x = reorder_within(scientific_label, b20, year), y = b20)
) +
  geom_col() +
  geom_errorbar(
    aes(ymin = pmax(b20 - se, 0), ymax = b20 + se),
    width = 0.2
  ) +
  coord_flip() +
  scale_y_log10() +   # <- log transform biomass axis
  facet_wrap(~year, scales = "free_y") +
  scale_x_reordered() +
  labs(
    x = "Species",
    y = expression(Average~biomass~(B20~per~BRUV))
  ) +
  theme_bw() +
  theme_collapse +
  theme(axis.text.y = element_markdown())

bar_b20

ggsave(paste0("plots/", park, "/fish/", name, "_top_b20_bar_plot.png"),
  plot = bar_b20, height = 4, width = 9, dpi = 300, units = "in", bg = "white")
