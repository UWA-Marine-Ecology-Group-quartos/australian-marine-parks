rm(list = ls())

# Load required libraries
library(mgcv)
library(tidyverse)
library(FSSgam)

# Create a mock dataset for `tidy_maxn`
set.seed(42)
tidy_maxn <- tibble(
  number = sample(4:297, 100, replace = TRUE),
  reef = runif(100, 0, 0.1),
  geoscience_depth = runif(100, -40, -15),
  geoscience_aspect = runif(100, 0, 360),
  geoscience_roughness = runif(100, 0, 6),
  geoscience_detrended = runif(100, -6, 5.5),
  response = sample(c("species_richness", "total_abundance"), 100, replace = TRUE),
  status = sample(c("protected", "unprotected"), 100, replace = TRUE),
  campaignid = sample(letters[1:5], 100, replace = TRUE)
)

# Re-set the predictors for modeling
pred.vars <- c("reef", "geoscience_depth", "geoscience_aspect",
               "geoscience_roughness", "geoscience_detrended")

# Check the response variable distribution
resp.vars <- tidy_maxn %>%
  group_by(response) %>%
  filter(mean(number == 0) < 0.8) %>%
  pull(response) %>%
  unique()
print(resp.vars)

# Example model fitting for a single response variable
use.dat <- tidy_maxn %>% filter(response == resp.vars[1]) %>% as.data.frame()
Model1 <- gam(number ~ s(geoscience_depth, k = 3, bs = 'cr'),
              family = gaussian(link = "identity"), data = use.dat)

# Generate the model set
model.set <- generate.model.set(
  use.dat = use.dat,
  test.fit = Model1,
  pred.vars.cont = pred.vars,
  pred.vars.fact = "status",
  cyclic.vars = "geoscience_aspect",
  k = 3,
  factor.smooth.interactions = FALSE,
  max.predictors = 3
)

# Fit the model set
out.list <- fit.model.set(model.set, max.models = 10, parallel = FALSE)

# Examine results
mod.table <- out.list$mod.data.out %>%
  arrange(AICc) %>%
  mutate(cumsum.wi = cumsum(wi.AICc))
