###
# Project: NESP 4.20 - Marine Park Dashboard reporting
# Data:    Habitat data synthesis
# Task:    Model habitat data using the full subsets approach from @beckyfisher/FSSgam
# Author:  Claude Spencer
# Date:    June 2024
###

rm(list=ls())

library(CheckEM)
library(tidyverse)
library(mgcv)
library(devtools)
library(FSSgam)
library(patchwork)
library(foreach)
library(doParallel)
library(terra)
library(sf)

# Set the study name
name <- "GeographeAMP"
park <- "geographe"

metadata_bathy_derivatives <- readRDS(paste0("data/", park, "/tidy/", name, "_metadata-bathymetry-derivatives.rds")) %>%
  clean_names() %>%
  glimpse()

# Bring in and format the data----
habi <- readRDS(paste0("data/", park, "/tidy/", name, "_benthos-count_combined.RDS")) %>%
  left_join(metadata_bathy_derivatives) %>%
  dplyr::filter(!is.na(geoscience_roughness)) %>%
  dplyr::filter(!geoscience_roughness > 3) %>% # Filter outliers - check later when more data is added
  glimpse()

model_dat <- habi %>%
  pivot_longer(cols = c(macroalgae, sand, rock, sessile_invertebrates, reef, seagrasses),
               names_to = "response", values_to = "number") %>%
  glimpse()

# Set predictor variables---
pred.vars <- c("geoscience_depth", "geoscience_aspect", "geoscience_roughness", "geoscience_detrended")

# Check for correlation of predictor variables- remove anything highly correlated (>0.95)---
round(cor(model_dat[ , pred.vars]), 2) # Roughness and depth 0.43 correlated

# Review of individual predictors for even distribution---
CheckEM::plot_transformations(pred.vars = pred.vars, dat = model_dat)

# Check to make sure Response vector has not more than 80% zeros----
unique.vars = unique(as.character(model_dat$response))

unique.vars.use = character()
for(i in 1:length(unique.vars)){
  temp.dat = model_dat[which(model_dat$response == unique.vars[i]),]
  if(length(which(temp.dat$number == 0))/nrow(temp.dat)< 0.8){
    unique.vars.use = c(unique.vars.use, unique.vars[i])}
}

unique.vars.use                                                                 # All good

# Run the full subset model selection----
outdir    <- paste0("output/model-output/", park, "/habitat/")
use.dat   <- model_dat[model_dat$response %in% c(unique.vars.use), ]
out.all   <- list()
var.imp   <- list()
resp.vars <- unique.vars.use
factor.vars <- c("status", "year")

# Loop through the FSS function for each Abiotic taxa----
for(i in 1:length(resp.vars)){
  print(resp.vars[i])
  use.dat <- model_dat[model_dat$response == resp.vars[i],]
  use.dat   <- as.data.frame(use.dat)
  Model1  <- gam(cbind(number, (total_pts - number)) ~
                   s(geoscience_depth, bs = 'cr'),
                 family = binomial("logit"),  data = use.dat)

  model.set <- generate.model.set(use.dat = use.dat,
                                  test.fit = Model1,
                                  pred.vars.cont = pred.vars,
                                  pred.vars.fact = factor.vars,
                                  cyclic.vars = c("geoscience_aspect"),
                                  k = 3,
                                  cov.cutoff = 0.7,
                                  max.predictors = 5
  )
  out.list <- fit.model.set(model.set,
                            max.models = 600,
                            parallel = T,
                            r2.type = "dev")
  names(out.list)

  out.list$failed.models # examine the list of failed models
  mod.table <- out.list$mod.data.out  # look at the model selection table
  mod.table <- mod.table[order(mod.table$AICc), ]
  mod.table$cumsum.wi <- cumsum(mod.table$wi.AICc)
  out.i     <- mod.table[which(mod.table$delta.AICc <= 2), ]
  out.all   <- c(out.all, list(out.i))
  var.imp   <- c(var.imp, list(out.list$variable.importance$aic$variable.weights.raw))



  # plot the best models
  for(m in 1:nrow(out.i)){
    best.model.name <- as.character(out.i$modname[m])

    png(file = paste(outdir, m, resp.vars[i], "mod_fits.png", sep = ""))
    if(best.model.name != "null"){
      par(mfrow = c(3, 1), mar = c(9, 4, 3, 1))
      best.model = out.list$success.models[[best.model.name]]
      plot(best.model, all.terms = T, pages = 1, residuals = T, pch = 16)
      mtext(side = 2, text = resp.vars[i], outer = F)}
    dev.off()
  }
}

# Model fits and importance---
names(out.all) <- resp.vars
names(var.imp) <- resp.vars
all.mod.fits <- list_rbind(out.all, names_to = "response")
all.var.imp  <- do.call("rbind", var.imp)
write.csv(all.mod.fits[ , -2], file = paste0(outdir, name, "_abiotic_all.mod.fits.csv"))
write.csv(all.var.imp,         file = paste0(outdir, name, "_abiotic_all.var.imp.csv"))

# Sand
m_sand <- gam(cbind(sand, total_pts - sand) ~
                year + status +
                s(geoscience_aspect, by = year, k = 5, bs = "cc")  +
                s(geoscience_depth, by = year, k = 5, bs = "cr") +
                s(geoscience_roughness, by = year, k = 5, bs = "cr"),
              data = habi, method = "REML", family = binomial("logit"))
summary(m_sand)

# Rock - too rare to model

# Macroalgae
m_macro <- gam(cbind(macroalgae, total_pts - macroalgae) ~
                 year + status +
                 s(geoscience_aspect, by = year, k = 5, bs = "cc")  +
                 s(geoscience_depth, by = year, k = 5, bs = "cr") +
                 s(geoscience_detrended, by = year, k = 5, bs = "cr"),
               data = habi, method = "REML", family = binomial("logit"))
summary(m_macro)

# Seagrass
m_seagrass <- gam(cbind(seagrasses, total_pts - seagrasses) ~
                    year + status +
                    s(geoscience_aspect, by = year, k = 5, bs = "cc")  +
                    s(geoscience_depth, by = year, k = 5, bs = "cr") +
                    s(geoscience_detrended, by = year, k = 5, bs = "cr"),
                  data = habi, method = "REML", family = binomial("logit"))
summary(m_seagrass)

# Inverts
# m_inverts <- gam(cbind(sessile_invertebrates, total_pts - sessile_invertebrates) ~
#                    s(geoscience_aspect,     k = 5, bs = "cc")  +
#                    s(geoscience_depth, k = 5, bs = "cr") +
#                    s(geoscience_detrended, k = 5, bs = "cr"),
#                  data = habi, method = "REML", family = binomial("logit"))
# summary(m_inverts)

# Reef
m_reef <- gam(cbind(reef, total_pts - reef) ~
                year + status +
                s(geoscience_aspect, by = year, k = 5, bs = "cc")  +
                s(geoscience_detrended, by = year, k = 5, bs = "cr") +
                s(geoscience_roughness, by = year, k = 5, bs = "cr"),
              data = habi, method = "REML", family = binomial("logit"))
summary(m_reef)

# Read predictor rasters to predict onto
preds <- readRDS(paste0("data/", park, "/spatial/rasters/", name, "_bathymetry-derivatives.rds"))
preddf <- preds %>%
  as.data.frame(xy = T, na.rm = T)

# Extract status to predict onto
marine_parks <- st_read("data/south-west network/spatial/shapefiles/western-australia_marine-parks-all.shp") %>%
  dplyr::filter(name %in% c("Ngari Capes", "Geographe", "South-west Corner")) %>%
  dplyr::filter(zone_type %in% c("Sanctuary Zone (IUCN VI)",
                                 "National Park Zone (IUCN II)")) %>%
  dplyr::mutate(status = "No-Take") %>%
  vect() %>%
  glimpse()

predv <- vect(preddf, geom = c("x", "y"), crs = "epsg:4326")

preddf_s <- cbind(preddf, terra::extract(marine_parks, predv)) %>%
  dplyr::mutate(status = as.factor(ifelse(is.na(status), "Fished", "No-Take"))) %>%
  glimpse()

preddf_s2014 <- preddf_s %>% dplyr::mutate(year = 2014L)
preddf_s2024 <- preddf_s %>% dplyr::mutate(year = 2024L)

preddf_sy <- dplyr::bind_rows(preddf_s2014, preddf_s2024) %>%
  dplyr::mutate(year = factor(year, levels = levels(habi$year))) %>%  # <- critical
  glimpse()

# predict, rasterise and plot
predhab <- cbind(preddf_sy,
                "p_macro"    = predict(m_macro, preddf_sy, type = "response", se.fit = T),
                "p_sand"     = predict(m_sand, preddf_sy, type = "response", se.fit = T),
                "p_seagrass" = predict(m_seagrass, preddf_sy, type = "response", se.fit = T),
                # "p_inverts"  = predict(m_inverts, preddf_sy, type = "response", se.fit = T),
                "p_reef"     = predict(m_reef, preddf_sy, type = "response", se.fit = T)) %>%
  glimpse()

prasts_2014 <- rast(predhab %>%
                      dplyr::filter(as.character(year) %in% "2014") %>%
                      dplyr::select(x, y, starts_with("p_")),
                    crs = "epsg:4326")

prasts_2024 <- rast(predhab %>%
                      dplyr::filter(as.character(year) %in% "2024") %>%
                      dplyr::select(x, y, starts_with("p_")),
                    crs = "epsg:4326")
plot(prasts_2014)
summary(prasts_2014)
plot(prasts_2024)
summary(prasts_2024)

# Calculate MESS and mask predictions ----
resp.vars <- c("p_sand", "p_macro", "p_seagrass", "p_reef")
pred.years <- c(2014L, 2024L)

for(y in 1:length(pred.years)) {

  print(pred.years[y])

  # XY for the given year (same style as yours)
  xy <- habi %>%
    dplyr::filter(year %in% pred.years[y]) %>%
    dplyr::select(longitude_dd , latitude_dd) %>%
    dplyr::rename(x = longitude_dd, y = latitude_dd) %>%
    glimpse()

  for(i in 1:length(resp.vars)) {

    print(resp.vars[i])
    mod <- get(str_replace_all(resp.vars[i], "p_", "m_"))

    # prediction raster for the given year only
    temppred <- predhab %>%
      dplyr::filter(year %in% pred.years[y]) %>%
      dplyr::select(x, y, paste0(resp.vars[i], ".fit"),
                    paste0(resp.vars[i], ".se.fit")) %>%
      rast(crs = "epsg:4326")

    # pull out only the geoscience predictors used by the model
    geo.vars <- names(mod$model)[startsWith(names(mod$model), "geoscience")]

    dat <- terra::extract(subset(preds, geo.vars), xy) %>%
      dplyr::select(-ID)

    messrast <- predicts::mess(subset(preds, geo.vars), dat) %>%
      terra::clamp(lower = -0.01, values = F)

    messrast <- terra::crop(messrast, temppred)
    temppred_m <- terra::mask(temppred, messrast)

    if (i == 1) {
      preddf_m <- temppred_m
    } else {
      preddf_m <- rast(list(preddf_m, temppred_m))
    }
  }

  # Save per-year outputs (separate)
  saveRDS(preddf_m,
          paste0("output/model-output/", park, "/habitat/",
                 name, "_predicted-habitat_", pred.years[y], ".rds"))

  writeRaster(preddf_m,
              paste0("output/model-output/", park, "/habitat/",
                     names(preddf_m), "_predicted_", pred.years[y], ".tif"),
              overwrite = TRUE)
}
