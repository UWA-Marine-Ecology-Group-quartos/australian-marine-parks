###
# Project: NESP 4.20 - Marine Park Dashboard reporting
# Data:    Fish data synthesis
# Task:    Model fish data using the full subsets approach from @beckyfisher/FSSgam
# Author:  Claude Spencer
# Date:    June 2024
###

rm(list = ls())

# Set the study name
name <- "DampierAMP"
park <- "dampier"

library(mgcv)
library(tidyverse)
library(terra)
library(sf)
library(predicts)
library(FSSgam)
library(CheckEM)

tidy_maxn <- readRDS(paste0("data/", park, "/tidy/", name, "_tidy-count.rds")) %>%
  glimpse()

# # Re-set the predictors for modeling----
names(tidy_maxn)
pred.vars = c("reef", "geoscience_depth", "geoscience_aspect",
              "geoscience_roughness", "geoscience_detrended")

# Check to make sure Response vector has not more than 80% zeros----
unique.vars <- unique(as.character(tidy_maxn$response))

resp.vars <- character()
for(i in 1:length(unique.vars)){
  temp.dat <- tidy_maxn[which(tidy_maxn$response == unique.vars[i]), ]
  if(length(which(temp.dat$number == 0)) / nrow(temp.dat) < 0.8){
    resp.vars <- c(resp.vars, unique.vars[i])}
}
resp.vars # All good

# Run the full subset model selection----
savedir <- "output/model-output/dampier/fish/"
factor.vars <- c("status")
out.all     <- list()
var.imp     <- list()

# Loop through the FSS function for each Taxa----
for(i in 1:length(resp.vars)){
  print(resp.vars[i])
  use.dat <- as.data.frame(tidy_maxn[which(tidy_maxn$response == resp.vars[i]), ])
  use.dat$status <- as.factor(use.dat$status)
  use.dat$campaignid <- as.factor(use.dat$campaignid)
  Model1  <- gam(number ~ s(geoscience_depth, k = 3, bs = 'cr'),
                 family = gaussian(link = "identity"),  data = use.dat)

  model.set <- generate.model.set(use.dat = use.dat,
                                  test.fit = Model1,
                                  pred.vars.cont = pred.vars,
                                  pred.vars.fact = factor.vars,
                                  cyclic.vars = "geoscience_aspect",
                                  k = 3,
                                  factor.smooth.interactions = F,
                                  max.predictors = 5
  )
  out.list <- fit.model.set(model.set,
                            max.models = 600,
                            parallel = T)
  names(out.list)

  out.list$failed.models # examine the list of failed models
  mod.table <- out.list$mod.data.out  # look at the model selection table
  mod.table <- mod.table[order(mod.table$AICc), ]
  mod.table$cumsum.wi <- cumsum(mod.table$wi.AICc)
  out.i   <- mod.table[which(mod.table$delta.AICc <= 2), ]
  out.all <- c(out.all,list(out.i))
  # var.imp=c(var.imp,list(out.list$variable.importance$aic$variable.weights.raw)) #Either raw importance score
  var.imp <- c(var.imp,list(out.list$variable.importance$aic$variable.weights.raw)) #Or importance score weighted by r2

  # plot the best models
  for(m in 1:nrow(out.i)){
    best.model.name = as.character(out.i$modname[m])
    png(file = paste(savedir, paste(name, m, resp.vars[i], "mod_fits.png", sep = "_"), sep = "/"))
    if(best.model.name != "null"){
      par(mfrow = c(3, 1), mar = c(9, 4, 3, 1))
      best.model = out.list$success.models[[best.model.name]]
      plot(best.model,all.terms = T, pages = 1, residuals = T, pch = 16)
      mtext(side = 2, text = resp.vars[i], outer = F)}
    dev.off()
  }
}

# Save model fits, data, and importance scores---
names(out.all) <- resp.vars
names(var.imp) <- resp.vars
all.mod.fits   <- do.call("rbind",out.all)
all.var.imp    <- do.call("rbind",var.imp)
write.csv(all.mod.fits[ , -2], file = paste(savedir, paste(name, "all.mod.fits.csv", sep = "_"), sep = "/"))
write.csv(all.var.imp, file = paste(savedir, paste(name, "all.var.imp.csv", sep = "_"), sep = "/"))

tidy_length <- readRDS(paste0("data/", park, "/tidy/", name, "_tidy-length.rds")) %>%
  glimpse()

# Check to make sure Response vector has not more than 80% zeros----
unique.vars <- unique(tidy_length$response)

resp.vars <- character()
for(i in 1:length(unique.vars)){
  temp.dat <- tidy_length[which(tidy_length$response == unique.vars[i]), ]
  if(length(which(temp.dat$number == 0)) / nrow(temp.dat) < 0.8){
    resp.vars <- c(resp.vars, unique.vars[i])}
}
resp.vars

# Run the full subset model selection----
name_length <- paste(name,"length", sep = "_")
out.all = list()
var.imp = list()

# Loop through the FSS function for each Taxa----
for(i in 1:length(resp.vars)){
  print(resp.vars[i])
  use.dat = as.data.frame(tidy_length[which(tidy_length$response==resp.vars[i]),])
  use.dat$campaignid <- as.factor(use.dat$campaignid)
  use.dat$status <- as.factor(use.dat$status)
  Model1  <- gam(number ~ s(geoscience_depth, k = 3, bs = 'cr'),
                 family = tw(),  data = use.dat)

  model.set <- generate.model.set(use.dat = use.dat,
                                  test.fit = Model1,
                                  pred.vars.cont = pred.vars,
                                  pred.vars.fact = factor.vars,
                                  cyclic.vars = "aspect",
                                  k = 3,
                                  factor.smooth.interactions = F,
                                  max.predictors = 5
  )
  out.list=fit.model.set(model.set,
                         max.models=600,
                         parallel=T)
  names(out.list)

  out.list$failed.models # examine the list of failed models
  mod.table=out.list$mod.data.out  # look at the model selection table
  mod.table=mod.table[order(mod.table$AICc),]
  mod.table$cumsum.wi=cumsum(mod.table$wi.AICc)
  out.i=mod.table[which(mod.table$delta.AICc<=2),]
  out.all=c(out.all,list(out.i))
  # var.imp=c(var.imp,list(out.list$variable.importance$aic$variable.weights.raw)) #Either raw importance score
  var.imp=c(var.imp,list(out.list$variable.importance$aic$variable.weights.raw)) #Or importance score weighted by r2

  # plot the best models
  for(m in 1:nrow(out.i)){
    best.model.name=as.character(out.i$modname[m])
    png(file = paste(savedir, paste(name_length, m, resp.vars[i], "mod_fits.png", sep = "_"), sep = "/"))
    if(best.model.name!="null"){
      par(mfrow=c(3,1),mar=c(9,4,3,1))
      best.model=out.list$success.models[[best.model.name]]
      plot(best.model,all.terms=T,pages=1,residuals=T,pch=16)
      mtext(side=2,text=resp.vars[i],outer=F)}
    dev.off()
  }
}

# Model fits and importance---
names(out.all) = resp.vars
names(var.imp) = resp.vars
all.mod.fits = do.call("rbind", out.all)
all.var.imp = do.call("rbind", var.imp)
write.csv(all.mod.fits[ , -2], file = paste(savedir, paste(name_length, "all.mod.fits.csv", sep = "_"), sep = "/"))
write.csv(all.var.imp, file = paste(savedir, paste(name_length, "all.var.imp.csv", sep = "_"), sep = "/"))

# read in
fabund <- bind_rows(tidy_maxn, tidy_length) %>%
  glimpse()

# Load predictions in Spatraster format
preds <- readRDS(paste(paste0('data/',park, '/spatial/rasters/', name),    # This is ignored - too big!
                       'bathymetry-derivatives.rds', sep = "_"))
plot(preds)

# Predictors as a dataframe for modelling
preddf <- preds %>%
  as.data.frame(xy = T) %>%
  glimpse()

# Predicted reef - extents don't match
pred_reef <- readRDS(paste0("output/model-output/", park, "/habitat/",
                            name, "_predicted-habitat.rds")) %>%
  terra::subset("p_reef.fit")
plot(pred_reef)
names(pred_reef) <- "reef"

# Add predicted reef on for fish modelling
presp <- vect(preddf, geom = c("x", "y"))
preddf <- cbind(preddf, terra::extract(pred_reef, presp, ID = F))
names(preddf)

# Back to spatraster
preds <- rast(preddf, crs = "epsg:4326")
plot(preds)

# Add on status for prediction status
status <- rast("data/south-west network/spatial/rasters/status_raster.tif")
plot(status)

# Add status on for fish modelling
preddf <- cbind(preddf, terra::extract(status, presp, ID = F)) %>%
  dplyr::mutate(status = if_else(status == 1, "No-take", "Fished"))
names(preddf)

# use formula from top model from FSSGam model selection
unique(fabund$response)
# Use species richness, CTI, greater than Lm carnivores,
# smaller than Lm carnivores, smaller than Lm snapper

# Species richness
m_richness <- gam(number ~ s(geoscience_detrended, k = 3, bs = "cr") +
                    s(reef, k = 3, bs = "cr"),
                  data = fabund %>% dplyr::filter(response %in% "species_richness"),
                  family = gaussian(link = "identity"))
summary(m_richness)
plot(m_richness)

# CTI
m_cti <- gam(number ~ s(geoscience_depth, k = 3, bs = "cr"),
             data = fabund %>% dplyr::filter(response %in% "cti"),
             family = gaussian(link = "identity"))
summary(m_cti)
plot(m_cti)

# Greater than Lm large bodied carnivores
m_mature <- gam(number ~ s(reef, k = 3, bs = "cr"),
                data = fabund %>% dplyr::filter(response %in% "greater than Lm carnivores"),
                family = tw())
summary(m_mature)
plot(m_mature)

# Smaller than Lm large bodied carnivores
m_immature <- gam(number ~ s(geoscience_detrended, k = 3, bs = "cr") +
                    status,
                  data = fabund %>% dplyr::filter(response %in% "smaller than Lm carnivores"),
                  family = tw())
summary(m_immature)

# Predict

predicted_fish <- cbind(preddf,
                        "p_mature" = mgcv::predict.gam(m_mature, preddf, type = "response",
                                                       se.fit = T),
                        "p_immature" = mgcv::predict.gam(m_immature, preddf, type = "response",
                                                         se.fit = T),
                        "p_cti" = mgcv::predict.gam(m_cti, preddf, type = "response",
                                                    se.fit = T),
                        "p_richness" = mgcv::predict.gam(m_richness, preddf, type = "response",
                                                         se.fit = T))

prasts <- rast(predicted_fish %>% dplyr::select(x, y, starts_with("p_")),
               crs = "epsg:4326")
plot(prasts)

# Calculate MESS and mask predictions
xy <- fabund %>%
  dplyr::select(longitude_dd , latitude_dd) %>%
  dplyr::rename(x = longitude_dd, y = latitude_dd) %>%
  distinct(x, y) %>%
  glimpse()

# resp.vars <- names(preddf)[18:ncol(preddf)]
resp.vars <- c("p_mature", "p_cti",
               "p_richness", "p_immature")

for(i in 1:length(resp.vars)) {
  print(resp.vars[i])
  mod <- get(str_replace_all(resp.vars[i], "p_", "m_"))

  if (length(setdiff(names(mod$model)[2:length(names(mod$model))], "status")) > 1) {
  temppred <- predicted_fish %>%
    dplyr::select(x, y, paste0(resp.vars[i], ".fit"),
                  paste0(resp.vars[i], ".se.fit")) %>%
    rast(crs = "epsg:4326")

  dat <- terra::extract(subset(preds, setdiff(names(mod$model)[2:length(names(mod$model))], "status")), xy, ID = F)
  messrast <- predicts::mess(subset(preds, setdiff(names(mod$model)[2:length(names(mod$model))], "status")), dat) %>%
    terra::clamp(lower = -0.01, values = F) %>%
    terra::crop(temppred)
  temppred_m <- terra::mask(temppred, messrast)
  }

  if (length(setdiff(names(mod$model)[2:length(names(mod$model))], "status")) == 1) {
    temppred <- predicted_fish %>%
      dplyr::select(x, y, paste0(resp.vars[i], ".fit"),
                    paste0(resp.vars[i], ".se.fit")) %>%
      rast(crs = "epsg:4326")

    dat <- terra::extract(subset(preds, setdiff(names(mod$model)[2:length(names(mod$model))], "status")), xy, ID = F)
    messrast <- subset(preds, setdiff(names(mod$model)[2:length(names(mod$model))], "status")) %>%
      clamp(lower = min(dat), upper = max(dat), values = F) %>%
      terra::crop(temppred)
    temppred_m <- terra::mask(temppred, messrast)
  }


  if (i == 1) {
    preddf_m <- as.data.frame(temppred_m, xy = T)
  }
  else {
    preddf_m <- as.data.frame(temppred_m, xy = T) %>%
      full_join(preddf_m)
  }
}

glimpse(preddf_m)

sites <- st_as_sf(tidy_maxn, coords = c("longitude_dd", "latitude_dd"), crs = 4326) %>%
  st_transform(9473) %>%
  st_union()

buffer <- sites %>%
  st_buffer(dist = 10000) %>%
  st_transform(4326) %>%
  vect()

remove <- st_read("data/dampier/spatial/shapefiles/remove-shipping-channel.shp")

predfish <- rast(preddf_m, crs = "epsg:4326") %>%
  mask(buffer) %>%
  mask(remove, inverse = T) %>%
  trim()
plot(predfish)

preddf_m <- as.data.frame(predfish, xy = T)

saveRDS(preddf_m, paste0("output/model-output/", park, "/fish/", name,
                         "_predicted-fish.RDS"))

writeRaster(predfish, paste0("output/model-output/", park, "/fish/", names(predfish), "_predicted.tif"),
            overwrite = TRUE)
