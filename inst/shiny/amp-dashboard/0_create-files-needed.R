library(readr)

networks_and_parks <- read_csv("inst/shiny/amp-dashboard/data/networks-and-parks.csv")

# read in plots (this is just an example, I will need to loop through these and turn them into a list?)
geo_cti <- readRDS("inst/shiny/amp-dashboard/plots/geographe_gg_cti.RDS")
geo_lm <- readRDS("inst/shiny/amp-dashboard/plots/geographe_gg_lm.RDS")
geo_sr <- readRDS("inst/shiny/amp-dashboard/plots/geographe_gg_sr.RDS") # temporaily reading in SR

metadata <- readRDS("data/geographe/tidy/GeographeAMP_metadata-bathymetry-derivatives.rds")

all_data <- structure(
  list(
    networks_and_parks = networks_and_parks,
    geo_cti = geo_cti,
    geo_lm = geo_lm,
    geo_sr = geo_sr,
    metadata = metadata
    # iucn.pal = iucn.pal
  ),
  class = "data"
)

save(all_data, file = here::here("data/all_data.Rdata"))
save(all_data, file = here::here("inst/shiny/amp-dashboard/data/all_data.Rdata")) #I'm not actually sure which ones of these works
