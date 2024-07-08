###
# Project: NESP 4.20 - Marine Park Dashboard reporting
# Data:    Fish data synthesis
# Task:    Model fish data using the full subsets approach from @beckyfisher/FSSgam
# Author:  Claude Spencer
# Date:    June 2024
###

rm(list = ls())

# Set the study name
name <- "GeographeAMP"

library(mgcv)
library(tidyverse)
library(terra)
library(sf)
library(predicts)
library(nlraa)
library(FSSgam)
library(CheckEM)

tidy_maxn <- readRDS(paste0("data/geographe/tidy/", name, "_tidy-count.rds")) %>%
  dplyr::filter(!sample %in% "779",
                !number > 200, # Remove some outliers
                geoscience_roughness < 4) %>% # Remove outliers in roughness
  glimpse()

unique(tidy_maxn$response)

test <- tidy_maxn %>%
  group_by(campaignid, longitude_dd, latitude_dd) %>%
  dplyr::summarise(n = n())

ggplot() +
  geom_point(data = tidy_maxn, aes(x = sample, y = number), alpha = 0.5)

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
savedir <- "output/model-output/geographe/fish/"
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
                                  cyclic.vars = "aspect",
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

tidy_length <- readRDS(paste0("data/geographe/tidy/", name, "_tidy-length.rds")) %>%
  dplyr::filter(geoscience_roughness < 3) %>%
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

fabund <- bind_rows(dat1,dat2) %>%
  dplyr::mutate(year = as.factor(year))
levels(fabund$year)

# Load predictions in Spatraster format
preds <- readRDS(paste(paste0('data/spatial/rasters/raw bathymetry/', name),    # This is ignored - too big!
                       'spatial_covariates.rds', sep = "_"))

# Predictors as a dataframe for modelling
preddf <- preds %>%
  as.data.frame(xy = T) %>%
  glimpse()

# Predicted reef
pred_reef <- readRDS(paste0("output/habitat/",
                            name, "_predicted-habitat.rds")) %>%
  dplyr::rename(reef = p_reef.fit) %>%
  dplyr::select(x, y, reef) %>%
  rast(type = "xyz", crs = "epsg:4326")
plot(pred_reef)

# Add predicted reef on for fish modelling
presp <- vect(preddf, geom = c("x", "y"))
preddf <- cbind(preddf, terra::extract(pred_reef, presp))
names(preddf)

# Extract status to predict onto
wasanc <- st_read("data/spatial/shapefiles/WA_MPA_2020.shp", crs = 4283) %>%
  dplyr::filter(str_detect(ZONE_TYPE, "Sanctuary")) %>%
  dplyr::mutate(GAZ_YEAR = ifelse(NAME %in% "Ngari Capes",
                                  as.numeric(str_extract(LATEST_GAZ, "^.{4}")),
                                  as.numeric(str_extract(GAZ_DATE, "^.{4}")))) %>%
  dplyr::mutate(YEARS_SINCE_GAZ = 2023 - GAZ_YEAR) %>%
  dplyr::select(GAZ_YEAR, YEARS_SINCE_GAZ, geometry) %>%
  dplyr::mutate(status = "No-take") %>%
  st_transform(4326) %>%
  glimpse()

# Rottnest
rottsanc <- st_read("data/spatial/shapefiles/Rottnest_Sanctuaries.shp") %>%
  dplyr::mutate(GAZ_YEAR = 2007) %>%
  dplyr::mutate(YEARS_SINCE_GAZ = 2023 - GAZ_YEAR) %>%
  dplyr::select(GAZ_YEAR, YEARS_SINCE_GAZ, geometry) %>%
  dplyr::mutate(status = "No-take") %>%
  st_transform(4326) %>%
  glimpse()

# Australian Marine Parks
aumpa <- st_read("data/spatial/shapefiles/AustraliaNetworkMarineParks.shp") %>%
  dplyr::filter(ZoneName %in% "National Park Zone") %>%
  dplyr::mutate(GAZ_YEAR = 2018) %>%
  dplyr::mutate(YEARS_SINCE_GAZ = 2023 - GAZ_YEAR) %>%
  dplyr::select(GAZ_YEAR, YEARS_SINCE_GAZ, geometry) %>%
  dplyr::mutate(status = "No-take") %>%
  st_transform(4326) %>%
  glimpse()

allsanc <- bind_rows(wasanc, rottsanc, aumpa)
plot(allsanc)

allsancv <- vect(allsanc)
plot(allsancv)

predv <- vect(preddf, geom = c("x", "y"), crs = "epsg:4326")

preddf <- cbind(preddf, terra::extract(allsancv, predv)) %>%
  dplyr::mutate(status = ifelse(is.na(status), "Fished", "No-take"))

preds <- rast(preddf, crs = "epsg:4326")
plot(preds)

# use formula from top model from FSSGam model selection
# Greater than size of maturity openness+recfish+reef+UCUR+VCUR
unique(fabund$scientific)

# Species richness
m_richness <- gam(number ~ s(log.recfish, k = 3, bs = "cr") +
                    s(PROD, k = 3, bs = "cr") +
                    s(reef, k = 3, bs = "cr") +
                    s(roughness, k = 3, bs = "cr") +
                    s(SST, k = 3, bs = "cr") +
                    s(year, bs = "re"),
                  data = fabund %>% dplyr::filter(scientific %in% "species.richness"),
                  family = gaussian(link = "identity"))
summary(m_richness)
plot(m_richness)

# CTI
m_cti <- gam(number ~ s(log.gravity, k = 3, bs = "cr") +
               s(PROD, k = 3, bs = "cr") +
               s(reef, k = 3, bs = "cr") +
               s(SST, k = 3, bs = "cr") +
               s(year, bs = "re"),
             data = fabund %>% dplyr::filter(scientific %in% "cti"),
             family = gaussian(link = "identity"))
summary(m_cti)


# Greater than Lm large bodied carnivores
m_mature <- gam(number ~ s(reef, k = 3, bs = "cr") +
                  s(roughness, k = 3, bs = "cr") +
                  s(SLA, k = 3, bs = "cr") +
                  s(SST, k = 3, bs = "cr") +
                  status +
                  s(year, bs = "re"),
                data = fabund %>% dplyr::filter(scientific %in% "greater than Lm carinvores"),
                family = tw())
summary(m_mature)

# Smaller than Lm large bodied carnivores
m_immature <- gam(number ~ s(reef, k = 3, bs = "cr") +
                    s(roughness, k = 3, bs = "cr") +
                    s(SLA, k = 3, bs = "cr") +
                    s(SST, k = 3, bs = "cr") +
                    s(year, bs = "re"),
                  data = fabund %>% dplyr::filter(scientific %in% "smaller than Lm carnivores"),
                  family = tw())
summary(m_immature)

# Smaller than Lm pinkies
m_pinkies <- gam(number ~ s(reef, k = 3, bs = "cr") +
                   s(roughness, k = 3, bs = "cr") +
                   s(SLA, k = 3, bs = "cr") +
                   s(SST, k = 3, bs = "cr") +
                   s(year, bs = "re"),
                 data = fabund %>% dplyr::filter(scientific %in% "smaller than Lm Pink snapper"),
                 family = tw())
summary(m_pinkies)

# predict, rasterise and plot

# preddf <- cbind(preddf,
#                 "p_mature" = nlraa::predict_gam(m_mature, preddf, type = "response",
#                                          interval = "confidence", level = 0.9, exclude = "s(year, bs = 're')", newdata.guaranteed = T),
#                 "p_immature" = nlraa::predict_gam(m_immature, preddf, type = "response",
#                                        interval = "confidence", level = 0.9, exclude = "s(year, bs = 're')", newdata.guaranteed = T),
#                 "p_cti" = nlraa::predict_gam(m_cti, preddf, type = "response",
#                                   interval = "confidence", level = 0.9, exclude = "s(year, bs = 're')", newdata.guaranteed = T),
#                 "p_richness" = nlraa::predict_gam(m_richness, preddf, type = "response",
#                                        interval = "confidence", level = 0.9, exclude = "s(year, bs = 're')", newdata.guaranteed = T),
#                 "p_pinkies" = nlraa::predict_gam(m_pinkies, preddf, type = "response",
#                                                   interval = "confidence", level = 0.9, exclude = "s(year, bs = 're')", newdata.guaranteed = T))


# Not entirely sure if this is the best way to do this - but it shouldn't affect result as random effect is excluded
preddf$year <- "2022"
preddf$year <- as.factor(preddf$year)
levels(preddf$year) <- levels(fabund$year)
levels(preddf$year)

predicted_fish <- cbind(preddf,
                        "p_mature" = mgcv::predict.gam(m_mature, preddf, type = "response",
                                                       se.fit = T),
                        "p_immature" = mgcv::predict.gam(m_immature, preddf, type = "response",
                                                         se.fit = T),
                        "p_cti" = mgcv::predict.gam(m_cti, preddf, type = "response",
                                                    se.fit = T),
                        "p_richness" = mgcv::predict.gam(m_richness, preddf, type = "response",
                                                         se.fit = T),
                        "p_pinkies" = mgcv::predict.gam(m_pinkies, preddf, type = "response",
                                                        se.fit = T))

prasts <- rast(predicted_fish %>% dplyr::select(x, y, starts_with("p_")),
               crs = crs(preds))
plot(prasts)

# Calculate MESS and mask predictions
xy <- fabund %>%
  dplyr::select(longitude , latitude) %>%
  glimpse()

# CTI
temppred <- subset(prasts, "p_cti.fit")

mod <- gam(number ~ s(log.gravity, k = 3, bs = "cr") +
             s(PROD, k = 3, bs = "cr") +
             s(reef, k = 3, bs = "cr") +
             s(SST, k = 3, bs = "cr"),
           data = fabund %>% dplyr::filter(scientific %in% "cti"),
           family = gaussian(link = "identity"))

dat <- terra::extract(subset(preds, c("log.gravity", "PROD", "reef", "SST")), xy) %>%
  dplyr::select(-ID)
messrast <- predicts::mess(subset(preds, c("log.gravity", "PROD", "reef", "SST")), dat) %>%
  terra::clamp(lower = -0.01, values = F) %>%
  terra::crop(temppred)
cti_mess <- terra::mask(temppred, messrast)
cti_se <- subset(prasts, "p_cti.se.fit") %>%
  terra::crop(cti_mess) %>%
  terra::mask(cti_mess)
cti_mess <- rast(list(c(cti_mess, cti_se)))

# Species richness
temppred <- subset(prasts, "p_richness.fit")

mod <- gam(number ~ s(log.recfish, k = 3, bs = "cr") +
             s(PROD, k = 3, bs = "cr") +
             s(reef, k = 3, bs = "cr") +
             s(roughness, k = 3, bs = "cr") +
             s(SST, k = 3, bs = "cr"),
           data = fabund %>% dplyr::filter(scientific %in% "species.richness"),
           family = gaussian(link = "identity"))

dat <- terra::extract(subset(preds, c("log.recfish", "PROD", "reef", "roughness", "SST")), xy) %>%
  dplyr::select(-ID)
messrast <- predicts::mess(subset(preds, c("log.recfish", "PROD", "reef", "roughness", "SST")), dat) %>%
  terra::clamp(lower = -0.01, values = F) %>%
  terra::crop(temppred)
richness_mess <- terra::mask(temppred, messrast)
richness_se <- subset(prasts, "p_richness.se.fit") %>%
  terra::crop(richness_mess) %>%
  terra::mask(richness_mess)
richness_mess <- rast(list(c(richness_mess, richness_se)))

# Greater than Lm
temppred <- subset(prasts, "p_mature.fit")

mod <- gam(number ~ s(reef, k = 3, bs = "cr") +
             s(roughness, k = 3, bs = "cr") +
             s(SLA, k = 3, bs = "cr") +
             s(SST, k = 3, bs = "cr"),
           data = fabund %>% dplyr::filter(scientific %in% "greater than Lm carinvores"),
           family = tw())

dat <- terra::extract(subset(preds, c("reef", "roughness", "SLA", "SST")), xy) %>%
  dplyr::select(-ID)
messrast <- predicts::mess(subset(preds, c("reef", "roughness", "SLA", "SST")), dat) %>%
  terra::clamp(lower = -0.01, values = F) %>%
  terra::crop(temppred)
mature_mess <- terra::mask(temppred, messrast)
mature_se <- subset(prasts, "p_mature.se.fit") %>%
  terra::crop(mature_mess) %>%
  terra::mask(mature_mess)
mature_mess <- rast(list(c(mature_mess, mature_se)))

# Smaller than Lm
temppred <- subset(prasts, "p_immature.fit")

mod <- gam(number ~ s(reef, k = 3, bs = "cr") +
             s(roughness, k = 3, bs = "cr") +
             s(SLA, k = 3, bs = "cr") +
             s(SST, k = 3, bs = "cr"),
           data = fabund %>% dplyr::filter(scientific %in% "smaller than Lm carnivores"),
           family = tw())

dat <- terra::extract(subset(preds, c("reef", "roughness", "SLA", "SST")), xy) %>%
  dplyr::select(-ID)
messrast <- predicts::mess(subset(preds, c("reef", "roughness", "SLA", "SST")), dat) %>%
  terra::clamp(lower = -0.01, values = F) %>%
  terra::crop(temppred)
immature_mess <- terra::mask(temppred, messrast)
immature_se <- subset(prasts, "p_immature.se.fit") %>%
  terra::crop(immature_mess) %>%
  terra::mask(immature_mess)
immature_mess <- rast(list(c(immature_mess, immature_se)))

# Smaller than Lm Pink snapper
temppred <- subset(prasts, "p_pinkies.fit")

mod <- gam(number ~ s(reef, k = 3, bs = "cr") +
             s(roughness, k = 3, bs = "cr") +
             s(SLA, k = 3, bs = "cr") +
             s(SST, k = 3, bs = "cr") +
             s(year, bs = "re"),
           data = fabund %>% dplyr::filter(scientific %in% "smaller than Lm Pink snapper"),
           family = tw())

dat <- terra::extract(subset(preds, c("reef", "roughness", "SLA", "SST")), xy) %>%
  dplyr::select(-ID)
messrast <- predicts::mess(subset(preds, c("reef", "roughness", "SLA", "SST")), dat) %>%
  terra::clamp(lower = -0.01, values = F) %>%
  terra::crop(temppred)
pinkies_mess <- terra::mask(temppred, messrast)
pinkies_se <- subset(prasts, "p_pinkies.se.fit") %>%
  terra::crop(pinkies_mess) %>%
  terra::mask(pinkies_mess)
pinkies_mess <- rast(list(c(pinkies_mess, pinkies_se)))

# Join all the predictions
pred_fish_rast <- rast(list(c(cti_mess, richness_mess, mature_mess, immature_mess, pinkies_mess)))

saveRDS(prasts, paste0("output/fish/", name, "_predicted-fish-unmasked.RDS"))
saveRDS(pred_fish_rast, paste0("output/fish/", name, "_predicted-fish.RDS"))
