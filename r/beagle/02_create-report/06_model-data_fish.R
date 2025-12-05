###
# Project: NESP 4.20 - Marine Park Dashboard reporting
# Data:    Fish data synthesis
# Task:    Model fish data using the full subsets approach from @beckyfisher/FSSgam
# Author:  Claude Spencer
# Date:    June 2024
###

rm(list = ls())

# Set the study name
name <- "BeagleAMP"
park <- "beagle"

library(mgcv)
library(tidyverse)
library(terra)
library(sf)
library(predicts)
library(FSSgam)
library(CheckEM)

tidy_maxn <- readRDS(paste0("data/", park, "/tidy/", name, "_tidy-count.rds")) %>%
  dplyr::filter(!count > 200, # Remove some outliers
                # !sample %in% "779", ##HE what was 779?
                geoscience_roughness < 4) %>% # Remove outliers in roughness
  glimpse()

# # Re-set the predictors for modeling----
names(tidy_maxn)
pred.vars = c("reef", "geoscience_depth",
              "geoscience_roughness", "geoscience_detrended", "status", "campaignid")

# model_dat <- habi %>%
#   pivot_longer(cols = c(macroalgae, sand, rock, sessile_invertebrates, reef, seagrasses),
#                names_to = "response", values_to = "number")
#
# # Set predictor variables---
# pred.vars <- c("geoscience_depth", "geoscience_aspect", "geoscience_roughness", "geoscience_detrended")
#
# # Check for correlation of predictor variables- remove anything highly correlated (>0.95)---
# round(cor(model_dat[ , pred.vars]), 2) # Roughness and depth 0.35 correlated
#
# # Review of individual predictors for even distribution---
# CheckEM::plot_transformations(pred.vars = pred.vars, dat = model_dat)

# Check to make sure Response vector has not more than 80% zeros----
unique.vars <- unique(as.character(tidy_maxn$response))

resp.vars <- character()
for(i in 1:length(unique.vars)){
  temp.dat <- tidy_maxn[which(tidy_maxn$response == unique.vars[i]), ]
  if(length(which(temp.dat$count == 0)) / nrow(temp.dat) < 0.8){
    resp.vars <- c(resp.vars, unique.vars[i])}
}
resp.vars # All good

# Run the full subset model selection----
savedir <- "output/model-output/geographe/fish/"
factor.vars <- c("status", "campaignid")
out.all     <- list()
var.imp     <- list()

# Loop through the FSS function for each Taxa----
for(i in 1:length(resp.vars)){
  print(resp.vars[i])
  use.dat <- as.data.frame(tidy_maxn[which(tidy_maxn$response == resp.vars[i]), ])
  use.dat$status <- as.factor(use.dat$status)
  use.dat$campaignid <- as.factor(use.dat$campaignid)
  Model1  <- gam(count ~ s(geoscience_depth, k = 3, bs = 'cr'),
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
                            parallel = T,
                            r2.type = "dev")
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

# tidy_length <- readRDS(paste0("data/geographe/tidy/", name, "_tidy-length.rds")) %>%
#   dplyr::filter(geoscience_roughness < 3) %>%
#   glimpse()
#
# # Check to make sure Response vector has not more than 80% zeros----
# unique.vars <- unique(tidy_length$response)
#
# resp.vars <- character()
# for(i in 1:length(unique.vars)){
#   temp.dat <- tidy_length[which(tidy_length$response == unique.vars[i]), ]
#   if(length(which(temp.dat$count == 0)) / nrow(temp.dat) < 0.8){
#     resp.vars <- c(resp.vars, unique.vars[i])}
# }
# resp.vars
#
# # Run the full subset model selection----
# name_length <- paste(name,"length", sep = "_")
# out.all = list()
# var.imp = list()
#
# # Loop through the FSS function for each Taxa----
# for(i in 1:length(resp.vars)){
#   print(resp.vars[i])
#   use.dat = as.data.frame(tidy_length[which(tidy_length$response==resp.vars[i]),])
#   use.dat$campaignid <- as.factor(use.dat$campaignid)
#   use.dat$status <- as.factor(use.dat$status)
#   Model1  <- gam(count ~ s(geoscience_depth, k = 3, bs = 'cr'),
#                  family = tw(),  data = use.dat)
#
#   model.set <- generate.model.set(use.dat = use.dat,
#                                   test.fit = Model1,
#                                   pred.vars.cont = pred.vars,
#                                   pred.vars.fact = factor.vars,
#                                   cyclic.vars = "geoscience_aspect",
#                                   k = 3,
#                                   factor.smooth.interactions = F,
#                                   max.predictors = 5
#   )
#   out.list=fit.model.set(model.set,
#                          max.models=600,
#                          parallel=T,
#                          r2.type = "dev")
#   names(out.list)
#
#   out.list$failed.models # examine the list of failed models
#   mod.table=out.list$mod.data.out  # look at the model selection table
#   mod.table=mod.table[order(mod.table$AICc),]
#   mod.table$cumsum.wi=cumsum(mod.table$wi.AICc)
#   out.i=mod.table[which(mod.table$delta.AICc<=2),]
#   out.all=c(out.all,list(out.i))
#   # var.imp=c(var.imp,list(out.list$variable.importance$aic$variable.weights.raw)) #Either raw importance score
#   var.imp=c(var.imp,list(out.list$variable.importance$aic$variable.weights.raw)) #Or importance score weighted by r2
#
#   # plot the best models
#   for(m in 1:nrow(out.i)){
#     best.model.name=as.character(out.i$modname[m])
#     png(file = paste(savedir, paste(name_length, m, resp.vars[i], "mod_fits.png", sep = "_"), sep = "/"))
#     if(best.model.name!="null"){
#       par(mfrow=c(3,1),mar=c(9,4,3,1))
#       best.model=out.list$success.models[[best.model.name]]
#       plot(best.model,all.terms=T,pages=1,residuals=T,pch=16)
#       mtext(side=2,text=resp.vars[i],outer=F)}
#     dev.off()
#   }
# }
#
# # Model fits and importance---
# names(out.all) = resp.vars
# names(var.imp) = resp.vars
# all.mod.fits = do.call("rbind", out.all)
# all.var.imp = do.call("rbind", var.imp)
# write.csv(all.mod.fits[ , -2], file = paste(savedir, paste(name_length, "all.mod.fits.csv", sep = "_"), sep = "/"))
# write.csv(all.var.imp, file = paste(savedir, paste(name_length, "all.var.imp.csv", sep = "_"), sep = "/"))

tidy_b20 <- readRDS(paste0("data/", park, "/tidy/", name, "_tidy-b20.rds")) %>%
  dplyr::filter(geoscience_roughness < 3) %>%
  glimpse()

# Check to make sure Response vector has not more than 90% zeros----
unique.vars <- unique(tidy_b20$response)

resp.vars <- character()
for(i in 1:length(unique.vars)){
  temp.dat <- tidy_b20[which(tidy_b20$response == unique.vars[i]), ]
  if(length(which(temp.dat$count == 0)) / nrow(temp.dat) < 0.9){ ##HE change back to 80% when cleaned up
    resp.vars <- c(resp.vars, unique.vars[i])}
}
resp.vars

# Run the full subset model selection----
name_b20 <- paste(name,"b20", sep = "_")
out.all = list()
var.imp = list()

# Loop through the FSS function for each Taxa----
for(i in 1:length(resp.vars)){
  print(resp.vars[i])
  use.dat = as.data.frame(tidy_b20[which(tidy_b20$response==resp.vars[i]),])
  use.dat$campaignid <- as.factor(use.dat$campaignid)
  use.dat$status <- as.factor(use.dat$status)
  Model1  <- gam(count ~ s(geoscience_depth, k = 3, bs = 'cr'),
                 gaussian(link = "identity"),  data = use.dat) ##HE changed to gaussion

  model.set <- generate.model.set(use.dat = use.dat,
                                  test.fit = Model1,
                                  pred.vars.cont = pred.vars,
                                  pred.vars.fact = factor.vars,
                                  cyclic.vars = "geoscience_aspect",
                                  k = 3,
                                  factor.smooth.interactions = F,
                                  max.predictors = 5
  )
  out.list=fit.model.set(model.set,
                         max.models=600,
                         parallel=T,
                         r2.type = "dev")
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
    png(file = paste(savedir, paste(name_b20, m, resp.vars[i], "mod_fits.png", sep = "_"), sep = "/"))
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
write.csv(all.mod.fits[ , -2], file = paste(savedir, paste(name_b20, "all.mod.fits.csv", sep = "_"), sep = "/"))
write.csv(all.var.imp, file = paste(savedir, paste(name_b20, "all.var.imp.csv", sep = "_"), sep = "/"))

# read in
fabund <- bind_rows(tidy_maxn, tidy_b20) %>%
  glimpse()

# Load predictions in Spatraster format
preds <- readRDS(paste(paste0('data/geographe/spatial/rasters/', name),    # This is ignored - too big!
                       'bathymetry-derivatives.rds', sep = "_"))
plot(preds)

# Predictors as a dataframe for modelling
preddf <- preds %>%
  as.data.frame(xy = T) %>%
  glimpse()

# Predicted reef
pred_reef <- readRDS(paste0("output/model-output/geographe/habitat/",
                            name, "_predicted-habitat.rds")) %>%
  terra::subset("p_reef.fit")
names(pred_reef) <- "reef"
plot(pred_reef)

# Add predicted reef on for fish modelling
presp <- vect(preddf, geom = c("x", "y"))
preddf <- cbind(preddf, terra::extract(pred_reef, presp))
names(preddf)

# Back to spatraster
preds <- rast(preddf, crs = "epsg:4326")
plot(preds)

# use formula from top model from FSSGam model selection
unique(fabund$response)
# Use species richness, CTI, B20

#Total abundance
m_abundance <- gam(count ~ s(geoscience_detrended, k = 3, bs = "cr") +
                    s(reef, k = 3, bs = "cr") +
                    status +
                    campaignid,
                  data = fabund %>% dplyr::filter(response %in% "total_abundance"),
                  family = gaussian(link = "identity"))
summary(m_abundance)
plot(m_abundance)

# Species richness
m_richness <- gam(count ~ s(geoscience_aspect, k = 3, bs = "cc") +
                    s(reef, k = 3, bs = "cr"),
                  data = fabund %>% dplyr::filter(response %in% "species_richness"),
                  family = gaussian(link = "identity"))
summary(m_richness)
plot(m_richness)

# CTI
m_cti <- gam(count ~ s(reef, k = 3, bs = "cr") +
               s(geoscience_detrended, k = 3, bs = "cr"),
             data = fabund %>% dplyr::filter(response %in% "cti"),
             family = gaussian(link = "identity"))
summary(m_cti)
plot(m_cti)

# B20
m_b20 <- gam(count ~ s(reef, k = 3, bs = "cr") +
               s(geoscience_detrended, k = 3, bs = "cr"), ##HE should add status and campaignid but doesn't work for predicted_fish
             data = fabund %>% dplyr::filter(response %in% "b20"),
             family = tw())
summary(m_b20)
plot(m_b20, all.terms = TRUE)

predicted_fish <- cbind(preddf,
                        "p_b20" = mgcv::predict.gam(m_b20, preddf, type = "response",
                                                       se.fit = T),
                        # "p_immature" = mgcv::predict.gam(m_immature, preddf, type = "response",
                        #                                  se.fit = T),
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
resp.vars <- c("p_b20", "p_cti",
               "p_richness")

##HE not sure if this worked, predfish looks the same as prasts?
for(i in 1:length(resp.vars)) {
  print(resp.vars[i])
  mod <- get(str_replace_all(resp.vars[i], "p_", "m_"))

  temppred <- predicted_fish %>%
    dplyr::select(x, y, paste0(resp.vars[i], ".fit"),
                  paste0(resp.vars[i], ".se.fit")) %>%
    rast(crs = "epsg:4326")

  ##HE there is an error below when responses have different number of predictors
  dat <- terra::extract(subset(preds, names(mod$model)[2:length(names(mod$model))]), xy) %>%
    dplyr::select(-ID)
  messrast <- predicts::mess(subset(preds, names(mod$model)[2:length(names(mod$model))]), dat) %>%
    terra::clamp(lower = -0.01, values = F)
  messrast <- terra::crop(messrast, temppred)
  temppred_m <- terra::mask(temppred, messrast)


  if (i == 1) {
    preddf_m <- as.data.frame(temppred_m, xy = T)
  }
  else {
    preddf_m <- as.data.frame(temppred_m, xy = T) %>%
      full_join(preddf_m)
  }
}

glimpse(preddf_m)

saveRDS(preddf_m, paste0("output/model-output/geographe/fish/", name,
                         "_predicted-fish.RDS"))

predfish <- rast(preddf_m, crs = "epsg:4326")
plot(predfish)

writeRaster(predfish, paste0("output/model-output/geographe/fish/", names(predfish), "_predicted.tif"),
            overwrite = TRUE)
