library(readr)

networks_and_parks <- read_csv("inst/shiny/amp-dashboard/data/networks-and-parks.csv")

all_data <- structure(
  list(
    networks_and_parks = networks_and_parks
    # iucn.pal = iucn.pal
  ),
  class = "data"
)

save(all_data, file = here::here("data/all_data.Rdata"))
save(all_data, file = here::here("inst/shiny/amp-dashboard/data/all_data.Rdata")) #I'm not actually sure which ones of these works
