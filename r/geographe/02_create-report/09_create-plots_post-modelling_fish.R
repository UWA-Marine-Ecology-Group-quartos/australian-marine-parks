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
p_shallow <- controlplot_fish(data = park_dat.shallow, amp_abbrv = "GMP", state_abbrv = "NCMP",
                              title = "Shallow (0 - 30 m)")
ggsave(paste0("plots/", park, "/fish/", name, "_shallow-control-plots.png"),
       plot = p_shallow, height = 9, width = 8, dpi = 300, units = "in", bg = "white")

# Mesophotic plot (both years together)
p_meso <- controlplot_fish(data = park_dat.meso, amp_abbrv = "GMP", state_abbrv = "NCMP",
                           title = "Mesophotic (30 - 70 m)")
ggsave(paste0("plots/", park, "/fish/", name, "_mesophotic-control-plots.png"),
       plot = p_meso, height = 9, width = 8, dpi = 300, units = "in", bg = "white")

# Rariphotic:
# p_rari <- controlplot_fish(data = park_dat.rari, amp_abbrv = "GMP", state_abbrv = "NCMP",
#                            title = "Rariphotic (70 - 200 m)")
# ggsave(paste0("plots/", park, "/fish/", name, "_rariphotic-control-plots.png"),
#        plot = p_rari, height = 9, width = 8, dpi = 300, units = "in", bg = "white")


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

# read in maxn
maxn <- readRDS(paste0("data/", park, "/raw/_count-with-zeros.RDS")) %>%
  mutate(year = as.factor(year(date_time))) %>%
  # dplyr::filter(!count > 200, # Remove some outliers
  #               # !sample %in% "779", ##HE what was 779?
  #               geoscience_roughness < 4) %>% # Remove outliers in roughness
  glimpse()

# workout mean maxn for each species ---
maxn.10.2014 <- maxn%>%
  filter(campaignid %in% "2014-12_Geographe.Bay_stereoBRUVs") %>%
  mutate(scientific=paste(genus,species,sep=" "))%>%
  group_by(scientific)%>%
  dplyr::summarise(maxn=mean(count))%>%
  ungroup()%>%
  top_n(10)%>%
  # dplyr::filter(!scientific%in%c('Carangoides sp1', 'Unknown spp'))%>%
  glimpse()

#have a look
bar <- ggplot(maxn.10, aes(x=reorder(scientific,maxn), y=maxn)) +
  geom_bar(stat="identity",position=position_dodge())+
  coord_flip()+
  xlab("Species")+
  ylab(expression(Overall~abundance~(Sigma~MaxN)))+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  theme_collapse
bar

#load fish pictures
#1 Pseudocaranx spp
p.s <- as.raster(readPNG("data/images/Pseudocaranx dentex-3cm.png"))

#2. Coris auricularis
c.a <- as.raster(readPNG("data/images/Coris auricularis-3cmL.png"))

#3. Parequula melbournensis - none

#4. Pempheris klunzingeri
p.k <- as.raster(readPNG("data/images/Pempheris klunzingeri-3cmL.png"))

#5. Trachurus novaezelandiae
t.n <- as.raster(readPNG("data/images/Trachurus_novaezelandiae_nb_TAYLOR.png"))

#6. Neatypus obliquus
n.o <- as.raster(readPNG("data/images/Neatypus obliquus-3cmL.png"))

#7. Ophthalmolepis lineolatus
o.l <- as.raster(readPNG("data/images/Opthalmolepis lineolatus-3cm.png"))

#8. Sillago spp.
s.s <- as.raster(readPNG("data/images/Sillago_bassensis_nb_TAYLOR.png"))

#9. Chromis klunzingeri - use chromis westaustralis
c.k <- as.raster(readPNG("data/images/Chromis westaustralis-3cmL.png"))

#10. Trygonorrhina dumerilii - none


#plot final bar plot
bar.top.10<-ggplot(maxn.10%>%mutate(scientific=str_replace_all(.$scientific,
                                                               c("Pseudocaranx spp"="Pseudocaranx spp*"))), aes(x=reorder(scientific,maxn), y=maxn)) +
  geom_bar(stat="identity",colour="black",fill="lightgrey",position=position_dodge())+
  ylim (0, 4000)+
  coord_flip()+
  xlab("Species")+
  ylab(expression(Overall~abundance~(Sigma~MaxN)))+
  theme_bw()+
  theme(axis.text.y = element_text(face="italic"))+
  theme_collapse+
  theme.larger.text+
  annotation_raster(p.s, xmin=9.7,xmax=10.3,ymin=3396 + 50, ymax=3396 + 800)+            #1
  annotation_raster(c.a, xmin=8.7,xmax=9.3,ymin=2310 + 50, ymax=2310 + 800)+               #2
  # annotation_raster(c.spp, xmin=7.75, xmax=8.25, ymin=2500, ymax=2900)+         #3
  annotation_raster(p.k, xmin=6.8,xmax=7.2,ymin=1736 + 50, ymax=1736 + 500)+               #4
  annotation_raster(t.n, xmin=5.75,xmax=6.25,ymin=1243 + 50, ymax=1243 + 700)+             #5
  annotation_raster(n.o, xmin=4.7,xmax=5.3,ymin=971 + 50, ymax=971 + 700)+               #6
  annotation_raster(o.l, xmin=3.7,xmax=4.3,ymin=690 + 50, ymax=690 + 900)+               #7
  annotation_raster(s.s, xmin=2.75,xmax=3.25,ymin=566 + 50, ymax=566 + 900)+               #8
  annotation_raster(c.k, xmin=1.8,xmax=2.2,ymin=477 + 50, ymax=477 + 400) #+                #9
# annotation_raster(c.aus, xmin=0.75,xmax=1.25,ymin=650, ymax=1100)             #10
# ggtitle("10 most abundant species") +
# theme(plot.title = element_text(hjust = 0))
bar.top.10

#save out plot
ggsave("plots/fish/abundant.fish.bar.png",bar.top.10, dpi = 600, width = 6.0, height = 6.0)

#targeted species top 10 abundance
# Read in life history
maturity_mean <- CheckEM::maturity %>%
  dplyr::filter(!marine_region %in% c("NW", "N")) %>% # Change here for each marine park
  dplyr::group_by(family, genus, species, sex) %>%
  dplyr::slice(which.min(l50_mm)) %>%
  ungroup() %>%
  dplyr::group_by(family, genus, species) %>%
  dplyr::summarise(l50 = mean(l50_mm)) %>%
  ungroup() %>%
  glimpse()

large_bodied_carnivores <- CheckEM::australia_life_history %>%
  dplyr::filter(fb_trophic_level > 2.8) %>%
  dplyr::filter(length_max_cm > 40) %>%
  dplyr::filter(class %in% "Actinopterygii") %>%
  dplyr::filter(!order %in% c("Anguilliformes", "Ophidiiformes", "Notacanthiformes","Tetraodontiformes","Syngnathiformes",
                              "Synbranchiformes", "Stomiiformes", "Siluriformes", "Saccopharyngiformes", "Osmeriformes",
                              "Osteoglossiformes", "Lophiiformes", "Lampriformes", "Beloniformes", "Zeiformes")) %>%
  left_join(maturity_mean) %>%
  dplyr::mutate(fb_length_at_maturity_mm = fb_length_at_maturity_cm * 10) %>%
  dplyr::mutate(l50 = if_else(is.na(l50), fb_length_at_maturity_mm, l50)) %>%
  dplyr::filter(!is.na(l50)) %>%
  dplyr::select(family, genus, species, l50) %>%
  glimpse()

fished.species <- maxn %>%
  dplyr::mutate(scientific = paste(genus, species, sep = " ")) %>%
  dplyr::left_join(large_bodied_carnivores) %>%
  dplyr::filter(!is.na(l50)) %>%
  glimpse()

maxn.fished.10 <- fished.species %>%
  group_by(scientific) %>%
  dplyr::summarise(maxn=sum(maxn)) %>%
  ungroup() %>%
  top_n(10) %>%
  glimpse()

#have a look
bar <- ggplot(maxn.fished.10, aes(x=reorder(scientific,maxn), y=maxn)) +
  geom_bar(stat="identity",position=position_dodge())+
  coord_flip()+
  xlab("Species")+
  ylab(expression(Overall~abundance~(Sigma~MaxN)))+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  theme_collapse
bar

# 1. Trachurus novaezelandiae
# Already loaded

# 2. Chrysophrys auratus
c.a <- as.raster(readPNG("data/images/Chrysophrys auratus 3cm.png"))

# 3. Sillaginodes punctatus
s.p <- as.raster(readPNG("data/images/Sillaginodes_punctatus_nb_TAYLOR.png"))

# 4. Sphryaena novahollandiae

# 5. Seriola hippos
s.h <- as.raster(readPNG("data/images/Seriola_hippos_nb_HQ_TAYLOR.png"))

# 6. Chorodon rubescens
c.r <- as.raster(readPNG("data/images/Choerodon rubescens 3cm.png"))

# 7. Glaucosoma hebraicum
g.h <- as.raster(readPNG("data/images/Glaucosoma hebraicum 3cm.png"))

# 8. Epinephelides armatus
e.a <- as.raster(readPNG("data/images/Epinephelides armatus-5cmL.png"))

# 9. Nemadactylus valenciennesi
n.v <- as.raster(readPNG("data/images/Nemadactylus valenciennesi-3cm.png"))

# 10. Sillago schomburgkii
# Already loaded

#plot final bar plot
bar.fished.10 <- ggplot(dplyr::filter(maxn.fished.10, !scientific %in% "Rhabdosargus sarba"), aes(x=reorder(scientific, maxn), y=maxn)) +
  geom_bar(stat = "identity", colour = "black", fill = "lightgrey", position = position_dodge())+
  ylim (0, 1500)+
  coord_flip()+
  xlab("Species")+
  ylab(expression(Overall~abundance~(Sigma~MaxN)))+
  theme_bw()+
  theme(axis.text.y = element_text(face="italic"))+
  theme_collapse +
  theme.larger.text +
  annotation_raster(t.n, xmin = 9.7, xmax = 10.3, ymin = 1243 + 20, ymax = 1243 + 300)+   #1
  annotation_raster(c.a, xmin = 8.5, xmax = 9.5, ymin = 216 + 20, ymax = 216 + 400)+    #2
  annotation_raster(s.p, xmin = 7.75, xmax = 8.25, ymin = 144 + 20, ymax = 144 + 300)+  #3
  # annotation_raster(s.n, xmin = 6.65, xmax = 7.35, ymin = 110 + 20, ymax = 110 + 200)+    #4
  annotation_raster(s.h, xmin = 5.5, xmax = 6.5, ymin = 84 + 20, ymax = 84 + 500)+      #5
  annotation_raster(c.r, xmin = 4.7, xmax = 5.3, ymin = 60 + 20, ymax = 60 + 350)+      #6
  annotation_raster(g.h, xmin = 3.5, xmax = 4.5, ymin = 59 + 20, ymax = 59 + 420)+        #7
  annotation_raster(e.a, xmin = 2.65, xmax = 3.35, ymin = 29 + 20, ymax = 29 + 280)+      #8
  annotation_raster(n.v, xmin = 1.5, xmax = 2.5, ymin = 24 + 20, ymax = 24 + 350)+      #9
  annotation_raster(s.s, xmin = 0.75, xmax = 1.25, ymin = 14 + 20, ymax = 14 + 300)       #10
# ggtitle("10 most abundant species") +
# theme(plot.title = element_text(hjust = 0))
bar.fished.10

#save out plot
ggsave("plots/fish/abundant.targets.bar.png", bar.fished.10, dpi = 600, width = 6.0, height = 6.0)
