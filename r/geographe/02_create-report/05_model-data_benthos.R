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

# Set the study name
name <- "GeographeAMP"
park <- "geographe"

metadata_bathy_derivatives <- readRDS(paste0("data/", park, "/tidy/", name, "_metadata-bathymetry-derivatives.rds")) %>%
  clean_names() %>%
  glimpse()

# Bring in and format the data----
habi2014 <- readRDS(paste0("data/", park, "/tidy/", name, "_benthos-count_combined.RDS")) %>%
  filter(substr(campaignid, 1, 4) %in% 2014) %>% # Seperate years
  left_join(metadata_bathy_derivatives) %>%
  dplyr::filter(!is.na(geoscience_roughness)) %>%
  dplyr::filter(!geoscience_roughness > 3) %>% # Filter outliers - check later when more data is added
  glimpse()

habi2024 <- readRDS(paste0("data/", park, "/tidy/", name, "_benthos-count_combined.RDS")) %>%
  filter(substr(campaignid, 1, 4) %in% 2024) %>% # Seperate years
  left_join(metadata_bathy_derivatives) %>%
  dplyr::filter(!is.na(geoscience_roughness)) %>%
  dplyr::filter(!geoscience_roughness > 3) %>% # Filter outliers - check later when more data is added
  glimpse()

model_dat <- habi %>%
  pivot_longer(cols = c(macroalgae, sand, rock, sessile_invertebrates, reef, seagrasses),
               names_to = "response", values_to = "number")

# Set predictor variables---
pred.vars <- c("geoscience_depth", "geoscience_aspect", "geoscience_roughness", "geoscience_detrended")

# Check for correlation of predictor variables- remove anything highly correlated (>0.95)---
round(cor(model_dat[ , pred.vars]), 2) # Roughness and depth 0.35 correlated

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
factor.vars <- c("status", "campaignid")

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
                                  cyclic.vars = c("aspect"),
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
                s(geoscience_aspect,     k = 5, bs = "cc")  +
                s(geoscience_depth, k = 5, bs = "cr") +
                s(geoscience_detrended, k = 5, bs = "cr"),
              data = habi, method = "REML", family = binomial("logit"))
summary(m_sand)

# Rock - too rare to model

# Macroalgae
m_macro <- gam(cbind(macroalgae, total_pts - macroalgae) ~
                 s(geoscience_aspect,     k = 5, bs = "cc")  +
                 s(geoscience_depth, k = 5, bs = "cr") +
                 s(geoscience_detrended, k = 5, bs = "cr"),
               data = habi, method = "REML", family = binomial("logit"))
summary(m_macro)

# Seagrass
m_seagrass <- gam(cbind(seagrasses, total_pts - seagrasses) ~
                    s(geoscience_aspect,     k = 5, bs = "cc")  +
                    s(geoscience_depth, k = 5, bs = "cr") +
                    s(geoscience_detrended, k = 5, bs = "cr"),
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
                s(geoscience_aspect,     k = 5, bs = "cc")  +
                s(geoscience_detrended, k = 5, bs = "cr") +
                s(geoscience_roughness, k = 5, bs = "cr"),
              data = habi, method = "REML", family = binomial("logit"))
summary(m_reef)

preds <- readRDS(paste0("data/", park, "/spatial/rasters/", name, "_bathymetry-derivatives.rds"))
preddf <- preds %>%
  as.data.frame(xy = T, na.rm = T)

# predict, rasterise and plot
predhab <- cbind(preddf,
                "p_macro"    = predict(m_macro, preddf, type = "response", se.fit = T),
                "p_sand"     = predict(m_sand, preddf, type = "response", se.fit = T),
                "p_seagrass" = predict(m_seagrass, preddf, type = "response", se.fit = T),
                # "p_inverts"  = predict(m_inverts, preddf, type = "response", se.fit = T),
                "p_reef"     = predict(m_reef, preddf, type = "response", se.fit = T)) %>%
  glimpse()

prasts <- rast(predhab %>% dplyr::select(x, y, starts_with("p_")),
               crs = "epsg:4326")
plot(prasts)
summary(prasts)

# Calculate MESS and mask predictions
xy <- habi %>%
  dplyr::select(longitude_dd , latitude_dd) %>%
  dplyr::rename(x = longitude_dd, y = latitude_dd) %>%
  glimpse()

resp.vars <- c("p_sand", "p_macro",
               "p_seagrass", "p_reef")

for(i in 1:length(resp.vars)) {
  print(resp.vars[i])
  mod <- get(str_replace_all(resp.vars[i], "p_", "m_"))

  temppred <- predhab %>%
    dplyr::select(x, y, paste0(resp.vars[i], ".fit"),
                  paste0(resp.vars[i], ".se.fit")) %>%
    rast(crs = "epsg:4326")

  dat <- terra::extract(subset(preds, names(mod$model)[2:length(names(mod$model))]), xy) %>%
    dplyr::select(-ID)
  messrast <- predicts::mess(subset(preds, names(mod$model)[2:length(names(mod$model))]), dat) %>%
    terra::clamp(lower = -0.01, values = F)
  messrast <- terra::crop(messrast, temppred)
  temppred_m <- terra::mask(temppred, messrast)


  if (i == 1) {
    preddf_m <- temppred_m
  }
  else {
    preddf_m <- rast(list(preddf_m, temppred_m))
  }
}

saveRDS(preddf_m, paste0("output/model-output/", park, "/habitat/", name, "_predicted-habitat.rds"))      # Ignored

writeRaster(preddf_m, paste0("output/model-output/", park, "/habitat/", names(preddf_m), "_predicted.tif"),
            overwrite = TRUE)
