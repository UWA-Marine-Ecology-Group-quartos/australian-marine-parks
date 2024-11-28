rm(list = ls())

library(terra)
library(tidyterra)
library(tidyverse)
library(sf)
library(patchwork)

# Load functions
file.sources = list.files(pattern = "*.R", path = "functions/", full.names = T)
sapply(file.sources, source, .GlobalEnv)

# Set the study name
name <- "DampierAMP"
park <- "dampier"

# Set the extent of the study
e <- ext(116.7, 117.7,-20.919, -20)

# Read in shapefile data for maps
aus <- st_read("data/south-west network/spatial/shapefiles/aus-shapefile-w-investigator-stokes.shp")
ausc <- st_crop(aus, e)

# Spatial plots
## SST
sst <- rast(paste0("data/", park, "/spatial/oceanography/", name, "_SST_raster.rds")) %>%
  subset(names(.) %in% c("Jan", "Mar", "May", "Jul", "Sep", "Nov"))
names(sst)
sst <- sst[[c("Jan", "Mar", "May", "Jul", "Sep", "Nov")]]
names(sst)

prediction_limits = c(116.779, 117.544, -20.738, -20.282)

plot_sst(prediction_limits) +
  theme(axis.text = element_text(size = 6))

# ggplot() +
#   geom_spatraster(data = sst) +
#   scale_fill_viridis_c(na.value = NA) +
#   geom_sf(data = aus) +
#   facet_wrap(~lyr) +
#   theme_minimal() +
#   theme(axis.text = element_text(size = 6)) +
#   labs(fill = "SST (°C)") +
#   coord_sf(xlim = c(115.0526, 115.5551),
#            ylim = c(-33.65736, -33.35236),
#            crs = 4326)
ggsave(paste0("plots/", park, "/spatial/", name, "_SST.png"),
       height = 3.8, width = 8, dpi = 600, bg = "white", units = "in")

## SLA
sla <- rast(paste0("data/", park, "/spatial/oceanography/", name, "_SLA_raster.rds")) %>%
  subset(names(.) %in% c("Jan", "Mar", "May", "Jul", "Sep", "Nov"))
names(sla)

plot_sla(prediction_limits) +
  theme(axis.text = element_text(size = 6))

ggsave(paste0("plots/", park, "/spatial/", name, "_SLA.png"),
       height = 3.8, width = 8, dpi = 600, bg = "white", units = "in")

## DHW
dhw <- rast(paste0("data/", park, "/spatial/oceanography/", name, "_DHW_raster.rds"))
names(dhw)

plot_dhw(prediction_limits) +
  theme(axis.text = element_text(size = 6))

ggsave(paste0("plots/", park, "/spatial/", name, "_DHW.png"),
       height = 2.7, width = 8, dpi = 600, bg = "white", units = "in")

pressure_data()

maxyear = c(2013, 2022)
pressure_plot(maxyear)

ggsave(filename = paste0('plots/', park, '/spatial/', name, '_oceanography_time-series.png'),
       dpi = 300, units = "in", bg = "white",
       width = 6, height = 6.75)






# ggplot() +
#   geom_spatraster(data = sla) +
#   scale_fill_viridis_c(na.value = NA) +
#   geom_sf(data = aus) +
#   facet_wrap(~lyr) +
#   theme_minimal() +
#   theme(axis.text = element_text(size = 6)) +
#   labs(fill = "SLA (m)") +
#   coord_sf(xlim = c(115.0526, 115.5551),
#            ylim = c(-33.65736, -33.35236),
#            crs = 4326)

# ggplot() +
#   geom_spatraster(data = dhw) +
#   scale_fill_viridis_c(na.value = NA) +
#   geom_sf(data = aus) +
#   facet_wrap(~lyr) +
#   theme_minimal() +
#   # theme(axis.text = element_text(size = 6)) +
#   labs(fill = "DHW (°C/weeks)") +
#   coord_sf(xlim = c(115.0526, 115.5551),
#            ylim = c(-33.65736, -33.35236),
#            crs = 4326)

# Time series plots
## Acidification
# acid_ts <- readRDS(paste0("data/", park, "/spatial/oceanography/",
#                           name, "_Acidification_time-series.rds")) %>%
#   dplyr::mutate(year = as.numeric(year)) %>%
#   dplyr::filter(!year %in% c("1870", "2013")) %>% # These 2 years have inaccurate averages as they are only on 6 months
#   dplyr::group_by(year) %>%
#   summarise(acidification = mean(acidification, na.rm = T), sd = mean(sd, na.rm = T)) %>%
#   ungroup() %>%
#   glimpse()

# acd_mean_plot <- ggplot(data = acid_ts, aes(x = year, y = acidification)) +
#   geom_line() +
#   geom_ribbon(aes(ymin = acidification-sd, ymax = acidification+sd),
#               fill = "black",alpha = 0.15) +
#   theme_classic() +
#   labs(x = "Year", y = "pH")
# acd_mean_plot #plot with the other time series
#
# ## SLA by season (winter/summer)
# # sla_ts <- readRDS(paste0("data/", park, "/spatial/oceanography/",
# #                          name, "_SLA_time-series.rds")) %>%
# #   dplyr::mutate(year = as.numeric(year)) %>%
# #   dplyr::filter(season %in% c("Summer", "Winter")) %>%
# #   dplyr::group_by(year, season) %>%
# #   summarise(sla = mean(sla, na.rm = T), sd = mean(sd, na.rm = T)) %>%
# #   ungroup() %>%
# #   glimpse()
#
# sla_mean_plot <- ggplot() +
#   geom_line(data = sla_ts, aes(x = year, y = sla, color = season)) +
#   geom_ribbon(data = sla_ts,aes(x = year, y = sla,
#                                   ymin = sla - sd,
#                                   ymax = sla + sd, fill = season),
#               alpha = 0.2, show.legend = F) +
#   theme_classic() +
#   labs(x = "Year", y = "SLA (m)", color = "Season") +
#   scale_color_manual(labels = c("Summer","Winter"), values = c("#e1ad68","#256b61"))+
#   scale_fill_manual(labels = c("Summer","Winter"), values = c("#e1ad68","#256b61"))
# sla_mean_plot
#
# ## SST by season (winter/summer)
# # sst_ts <- readRDS(paste0("data/", park, "/spatial/oceanography/",
# #                          name, "_SST_time-series.rds")) %>%
# #   dplyr::mutate(year = as.numeric(year)) %>%
# #   dplyr::filter(season %in% c("Summer", "Winter")) %>%
# #   dplyr::group_by(year, season) %>%
# #   summarise(sst = mean(sst, na.rm = T), sd = mean(sd, na.rm = T)) %>%
# #   ungroup() %>%
# #   glimpse()
#
# sst_mean_plot <- ggplot() +
#   geom_line(data = sst_ts, aes(x = year, y = sst, color = season)) +
#   geom_ribbon(data = sst_ts,aes(x = year, y = sst,
#                                 ymin = sst - sd,
#                                 ymax = sst + sd, fill = season),
#               alpha = 0.2, show.legend = F) +
#   theme_classic() +
#   labs(x = "Year", y = "SST (°C)", color = "Season")+
#   scale_color_manual(labels = c("Summer","Winter"), values = c("#e1ad68","#256b61"))+
#   scale_fill_manual(labels = c("Summer","Winter"), values = c("#e1ad68","#256b61"))
# sst_mean_plot
#
# ## DHW with lines for maximal value
# # dhw_ts <- readRDS(paste0("data/geographe/spatial/oceanography/",
# #                          name, "_DHW_time-series.rds")) %>%
# #   dplyr::mutate(year = as.numeric(year)) %>%
# #   dplyr::group_by(year) %>%
# #   summarise(dhw = mean(dhw, na.rm = T), sd = mean(sd, na.rm = T)) %>%
# #   ungroup() %>%
# #   glimpse()
#
# dhw_mean_plot <- ggplot() +
#   geom_vline(xintercept = 2011, color = "red", linetype = 5, alpha = 0.5) +
#   geom_vline(xintercept = 2012, color = "red", linetype = 5, alpha = 0.5) +
#   geom_line(data = dhw_ts, aes(x = year, y = dhw)) +
#   geom_ribbon(data = dhw_ts,aes(x = year, y = dhw,
#                                 ymin = dhw - sd,
#                                 ymax = dhw + sd),
#               alpha = 0.2, show.legend = F) +
#   theme_classic() +
#   labs(x = "Year", y = "DHW (°C/weeks)")
# dhw_mean_plot
#
# acd_mean_plot / sla_mean_plot / sst_mean_plot / dhw_mean_plot
# ggsave(filename = paste0('plots/geographe/spatial/', name, '_oceanography_time-series.png'),
#        dpi = 300, units = "in", bg = "white",
#     width = 6, height = 6.75)
