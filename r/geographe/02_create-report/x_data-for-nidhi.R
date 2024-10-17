rm(list = ls())

# Set the study name
name <- "nidhi"
park <- "geographe"

# Load libraries
library(sf)
library(terra)
library(stars)
library(starsExtra)
library(tidyverse)
library(tidyterra)
library(patchwork)
library(RNetCDF)

# Set the extent of the study
e <- ext(113.5, 115.5, -34.55, -33.5)

# Sea surface temperature
nc_sst <- open.nc(paste0("data/", park, "/spatial/oceanography/SST_WA.nc"), write = TRUE)
print.nc(nc_sst) # shows you all the file details
time_nc <- var.get.nc(nc_sst, 'time')  # NC_CHAR time:units = "days since 1981-01-01 00:00:00" ;
time_nc_sst <- utcal.nc("seconds since 1981-01-01 00:00:00", time_nc, type = "c")
dates_sst <- as.Date(time_nc_sst)
close.nc(nc_sst) # GDAL errors otherwise

rast_sst <- rast(paste0("data/", park, "/spatial/oceanography/SST_WA.nc"),
                 subds = "sea_surface_temperature") %>%
  crop(e) %>%
  trim()
plot(rast_sst)
names(rast_sst) <- dates_sst
time(rast_sst) <- dates_sst

sst_tsdf <- terra::global(rast_sst, fun = "mean", na.rm = T) %>%
  tibble::rownames_to_column() %>%
  cbind(terra::global(rast_sst, fun = "sd", na.rm = T)) %>%
  tidyr::separate(rowname, into = c("year", "month", "day"), sep = "-") %>%
  dplyr::group_by(year, month) %>%
  summarise(sst = mean(mean, na.rm = T) - 273.15, # Convert kelvin to celsius
            sd = mean(sd, na.rm = T)) %>%
  ungroup() %>%
  glimpse()

saveRDS(sst_tsdf, paste0("data/", park, "/spatial/oceanography/", name, "_SST_time-series.rds"))

testall <- sst_tsdf %>%
  dplyr::group_by(year) %>%
  summarise(sst = mean(sst),
            sd = mean(sd))

ggplot() +
  geom_line(data = testall, aes(group = 1, x = year, y = sst))

testwint <- sst_tsdf %>%
  dplyr::filter(month %in% c("06", "07", "08")) %>%
  dplyr::group_by(year) %>%
  summarise(sst = mean(sst),
            sd = mean(sd))

ggplot() +
  geom_line(data = testwint, aes(group = 1, x = year, y = sst))
