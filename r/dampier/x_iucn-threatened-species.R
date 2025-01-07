rm(list = ls())

library(CheckEM)
library(tidyverse)

name <- "DampierAMP"
park <- "dampier"

dat <- readRDS("data/dampier/raw/dampierAMP_BRUVs_complete_count.RDS") %>%
  dplyr::filter(count > 0) %>%
  distinct(family, genus, species) %>%
  left_join(CheckEM::australia_life_history) %>%
  dplyr::select(scientific_name, australian_common_name, epbc_threat_status, iucn_ranking) %>%
  dplyr::mutate(epbc_threat_status = if_else(str_detect(scientific_name, "apraefrontalis|foliosquama"), "Critically Endangered", epbc_threat_status)) %>%
  dplyr::filter(if_any(c(epbc_threat_status, iucn_ranking), ~ !is.na(.)),
                !(iucn_ranking %in% c("Least Concern", "Data Deficient")) | !is.na(epbc_threat_status)) %>%
  arrange(scientific_name) %>%
  glimpse()

write.csv(dat, file = paste0("data/", park, "/tidy/", name, "_threatened-species.csv"),
          row.names = F)

num.spp <- readRDS("data/dampier/raw/dampierAMP_BRUVs_complete_count.RDS") %>%
  dplyr::filter(count > 0) %>%
  distinct(scientific) %>%
  glimpse()

num.all <- readRDS("data/dampier/raw/dampierAMP_BRUVs_complete_count.RDS") %>%
  dplyr::filter(count > 0) %>%
  summarise(count = sum(count)) %>%
  glimpse()

# Try and make a spatial plot of the endangered species
# speclist <- unique(dat$scientific_name)

dat_threat <- readRDS("data/dampier/raw/dampierAMP_BRUVs_complete_count.RDS") %>%
  # left_join(CheckEM::australia_life_history) %>%
  # dplyr::mutate(scientific_name = paste(genus, species, sep = " ")) %>%
  dplyr::filter(species %in% c("apraefrontalis", "foliosquama", "mokarran")) %>%
  dplyr::mutate(scientific_name = paste(genus, species, sep = " ")) %>%
  dplyr::filter(count > 0) %>%
  st_as_sf(coords = c("longitude_dd", "latitude_dd"), crs = 4326) %>%
  glimpse()

e <- ext(116.7, 117.7,-20.919, -20)

marine_parks <- st_read("data/south-west network/spatial/shapefiles/western-australia_marine-parks-all.shp") %>%
  dplyr::filter(name %in% c("Dampier")) %>%
  arrange(zone) %>%
  glimpse()
plot(marine_parks["zone"])

marine_parks_amp <- marine_parks %>%
  dplyr::filter(epbc %in% "Commonwealth")
marine_parks_state <- marine_parks %>%
  dplyr::filter(epbc %in% "State")

# Australian outline and state and commonwealth marine parks
aus    <- st_read("data/south-west network/spatial/shapefiles/aus-shapefile-w-investigator-stokes.shp")
ausc <- st_crop(aus, e)

# Terrestrial parks
terrnp <- st_read("data/south-west network/spatial/shapefiles/Legislated_Lands_and_Waters_DBCA_011.shp") %>%  # Terrestrial reserves
  dplyr::filter(leg_catego %in% c("Nature Reserve", "National Park"))
plot(terrnp["leg_catego"])

terr_fills <- scale_fill_manual(values = c("National Park" = "#c4cea6",          # Set the colours for terrestrial parks
                                           "Nature Reserve" = "#e4d0bb"),
                                name = "Terrestrial Parks")

site_limits = c(116.779, 117.544, -20.738, -20.282) # For Dampier match it to the first plot

bathy <- rast("data/south-west network/spatial/rasters/Australian_Bathymetry_and_Topography_2023_250m_MSL_cog.tif") %>%
  crop(e) %>%
  clamp(upper = 0, values = F)
names(bathy) <- "Depth"
plot(bathy)

ggplot() +
  geom_spatraster(data = bathy, show.legend = F, maxcell = Inf) +
  scale_fill_distiller(palette = "Blues", na.value = NA) +
  new_scale_fill() +
  geom_sf(data = ausc, fill = "seashell2", colour = "grey80", size = 0.1) +
  new_scale_fill() +
  geom_sf(data = terrnp, aes(fill = leg_catego), colour = NA, alpha = 0.8) +
  terr_fills +
  new_scale_fill() +
  geom_sf(data = marine_parks_amp, aes(fill = zone), colour = NA, alpha = 0.8) +
  scale_fill_manual(name = "Australian Marine Parks", guide = "legend",
                    values = with(marine_parks_amp, setNames(colour, zone))) +
  new_scale_fill() +
  geom_sf(data = dat_threat, aes(colour = scientific_name), size = 3, shape = 10) +
  scale_colour_manual(values = c("#3D348B", "#F7B801", "#F35B04"), name = "EPBC Threatened fauna",
                      labels = c(expression(italic("Aipysurus apraefrontalis")),
                                 expression(italic("Aipysurus foliosquama")),
                                 expression(italic("Sphryna mokarran")))) +
  coord_sf(xlim = c(site_limits[1], site_limits[2]), ylim = c(site_limits[3], site_limits[4]), crs = 4326) +
  theme_minimal()

ggsave(paste(paste0('plots/', park, '/spatial/', name) , 'threatened-species.png',
             sep = "-"), dpi = 600, width = 8, height = 3.5, bg = "white")
