###
# Project: NESP 4.20 - Marine Park Dashboard reporting
# Data:    AusBathy, marine parks, bioregions, marine regions, FHPA
# Task:    WA context map with bathymetry, hillshade, bioregions, sanctuary zones
# Author:  Annika Leunig
# Date:    March 2026
###

rm(list = ls())

# Set study name
name <- "south-west"
park <- "network"

# Load libraries
library(sf)
library(terra)
library(tidyverse)
library(tidyterra)
library(ggnewscale)
library(cowplot)

sf_use_s2(TRUE)

# ==============================================================================
# 1. SET RASTER EXTENT
# ==============================================================================

e <- ext(106.0, 124.0, -39.0, -23.0)

# ==============================================================================
# 2. LOAD SPATIAL DATA
# ==============================================================================

aus <- st_read("data/south-west network/spatial/shapefiles/STE_2021_AUST_GDA2020.shp") %>%
  st_make_valid() %>%
  st_transform(4326)

terrnp <- st_read("data/south-west network/spatial/shapefiles/Legislated_Lands_and_Waters_DBCA_011.shp") %>%
  dplyr::filter(leg_catego %in% c("Nature Reserve", "National Park")) %>%
  st_transform(4326)

marine_parks <- st_read("data/south-west network/spatial/shapefiles/south-and-western-australia_marine-parks-all.shp") %>%
  dplyr::filter(name %in% c("Abrolhos", "Abrolhos Islands", "Bremer", "Eastern Recherche",
                            "Ngari Capes", "Geographe", "South-west Corner",
                            "Great Australian Bight", "Jurien", "Murat", "Jurien Bay",
                            "Perth Canyon", "Southern Kangaroo Island", "Twilight",
                            "Two Rocks", "Western Eyre", "Western Kangaroo Island",
                            "Nuyts Archipelgo", "Thorny Passage", "Sir Joseph Banks Group",
                            "Investigator", "West coast Bays", "Southern Spencer Gulf",
                            "Upper Spencer Gulf", "Cottesloe Reef", "Rottnest",
                            "Shoalwater Islands")) %>%
  st_transform(4326)

mp_cwlth_sanctuary <- marine_parks %>%
  dplyr::filter(epbc == "Commonwealth", zone == "National Park Zone")

mp_state_sanctuary <- marine_parks %>%
  dplyr::filter(epbc == "State") %>%
  dplyr::mutate(zone = case_when(
    zone == "Reef Observation Area" ~ "Sanctuary Zone",
    zone == "National Park Zone"    ~ "Sanctuary Zone",
    TRUE                            ~ zone
  )) %>%
  dplyr::filter(zone == "Sanctuary Zone")

fhpa <- st_read("data/south-west network/spatial/shapefiles/Fish_Habitat_Protection_Areas_DPIRD_049.shp") %>%
  st_make_valid() %>%
  st_transform(4326)

bioregions <- st_read("data/south-west network/spatial/shapefiles/marine_regions.shp") %>%
  st_make_valid() %>%
  st_transform(4326) %>%
  dplyr::filter(!REGION %in% c("Southern Inland", "Northern Inland"))

bioregion_centroids <- bioregions %>%
  st_point_on_surface() %>%
  mutate(
    lon = st_coordinates(.)[, 1],
    lat = st_coordinates(.)[, 2]
  ) %>%
  st_drop_geometry()

# ==============================================================================
# 3. LOAD AND PREPARE RASTERS
# ==============================================================================

bathy <- rast("data/south-west network/spatial/rasters/AusBathyTopo__Australia__2024_250m_MSL_cog.tif") %>%
  project("EPSG:4326", method = "bilinear") %>%
  crop(e)

bathy_shelf        <- clamp(bathy, lower = -200, upper = 0, values = FALSE)
names(bathy_shelf) <- "depth"
bathy_df           <- as.data.frame(bathy_shelf, xy = TRUE, na.rm = TRUE)
colnames(bathy_df)[3] <- "depth"

slope  <- terrain(bathy, v = "slope",  unit = "radians")
aspect <- terrain(bathy, v = "aspect", unit = "radians")
hs     <- shade(slope, aspect, angle = 35, direction = 315, normalize = TRUE)
names(hs) <- "hillshade"
hs_df     <- as.data.frame(hs, xy = TRUE, na.rm = TRUE)
colnames(hs_df)[3] <- "hillshade"

# ==============================================================================
# 4. COLOUR PALETTES
# ==============================================================================

# Original custom ramp — even spread red to purple
shelf_palette <- colorRampPalette(c(
  "#3a2a8c",
  "#3355b8",
  "#2e7abf",
  "#3d9dc4",
  "#5ab8c0",
  "#7ec4a0",
  "#a8d478",
  "#c8d968",
  "#ddd870",
  "#e8c87a",
  "#e0a882",
  "#d4877a"
))(200)

# Original custom ramp — shallow compressed
shelf_palette_shallow <- colorRampPalette(c(
  "#1a0a5c",
  "#221570",
  "#2a1a82",
  "#3a2a8c",
  "#342f9e",
  "#3040b0",
  "#2e50bc",
  "#2e5aba",
  "#2e65bb",
  "#2e70bc",
  "#2e7abf",
  "#3282c0",
  "#3590c2",
  "#3a9ec4",
  "#45aac4",
  "#5ab8c0",
  "#7ec4a0",
  "#a8d478",
  "#ddd870",
  "#d4877a"
))(200)

# Blue ramp — even spread
shelf_palette_blue <- colorRampPalette(c(
  "#020b2d",
  "#05215a",
  "#0a3a8a",
  "#1256b4",
  "#1a72d4",
  "#2090e8",
  "#30aaf0",
  "#50c0f4",
  "#80d4f8",
  "#b0e4fc",
  "#d8f2ff"
))(200)

# Blue ramp — shallow compressed
shelf_palette_blue_shallow <- colorRampPalette(c(
  "#020b2d",
  "#05215a",
  "#0a3a8a",
  "#0e4aa0",
  "#1256b4",
  "#1a72d4",
  "#1e84e4",
  "#2090e8",
  "#28a0f0",
  "#30aaf0",
  "#40b8f4",
  "#50c0f4",
  "#68ccf8",
  "#88d8fc",
  "#b0e8ff",
  "#d8f4ff"
))(200)

# Blue ramp 2
shelf_palette_blue2 <- colorRampPalette(c(
  "#0C1838",
  "#081D58",
  "#253494",
  "#225EA8",
  "#3491C3",
  "#33C7CC",
  "#6EECD3",
  "#DBF49C",
  "#F3F7B5",
  "#FFD79F"
))(200)



# ==============================================================================
# 5. MAP FUNCTION
# ==============================================================================

make_bathy_map <- function(plot_limits,
                           show_bioregions = FALSE,
                           show_legend     = TRUE,
                           label_size      = 3.5,
                           palette         = shelf_palette) {

  legend_pos <- if (show_legend) "right" else "none"

  p <- ggplot() +

    geom_tile(data = hs_df, aes(x = x, y = y, fill = hillshade),
              alpha = 0.3, show.legend = FALSE) +
    scale_fill_gradient(low      = "grey20",
                        high     = "#e8e8e8",
                        na.value = NA,
                        guide    = "none") +

    new_scale_fill() +
    geom_tile(data = bathy_df, aes(x = x, y = y, fill = depth)) +
    scale_fill_gradientn(
      colours  = palette,
      limits   = c(-200, 0),
      na.value = NA,
      name     = "Depth (m)",
      breaks   = c(0, -50, -100, -150, -200),
      labels   = c("0", "-50", "-100", "-150", "-200"),
      guide    = if (show_legend) guide_colorbar(
        barwidth       = 1.2,
        barheight      = 8,
        title.position = "top",
        ticks          = TRUE,
        order          = 1
      ) else "none"
    ) +

    geom_contour(data = bathy_df, aes(x = x, y = y, z = depth),
                 breaks    = c(-120, -60),
                 colour    = "white",
                 alpha     = 0.35,
                 linewidth = 0.25) +

    geom_sf(data = aus, fill = "seashell2", colour = "grey50", linewidth = 0.25) +

    new_scale_fill() +
    geom_sf(data = terrnp, aes(fill = leg_catego), colour = NA, alpha = 0.8) +
    scale_fill_manual(
      values = c("National Park"  = "#c4cea6",
                 "Nature Reserve" = "#e4d0bb"),
      name   = "Terrestrial Parks",
      guide  = if (show_legend) guide_legend(
        order          = 3,
        title.position = "top",
        override.aes   = list(alpha = 0.8)
      ) else "none"
    ) +

    new_scale_fill() +
    geom_sf(data = mp_cwlth_sanctuary,
            aes(fill = "National Park Zone"),
            colour    = "#7bac6e",
            linewidth = 0.15,
            alpha     = 0.75) +
    geom_sf(data = mp_state_sanctuary,
            aes(fill = "Sanctuary Zone"),
            colour    = "#bfd054",
            linewidth = 0.15,
            alpha     = 0.75) +
    geom_sf(data = fhpa,
            aes(fill = "Fish Habitat Protection Area"),
            colour    = "#c0392b",
            linewidth = 0.15,
            alpha     = 0.75) +
    scale_fill_manual(
      name   = "No-take Zones",
      values = c("National Park Zone"           = "#7bac6e",
                 "Sanctuary Zone"               = "#bfd054",
                 "Fish Habitat Protection Area" = "#c0392b"),
      guide  = if (show_legend) guide_legend(
        order          = 2,
        title.position = "top",
        override.aes   = list(alpha = 0.75)
      ) else "none"
    )

  if (show_bioregions) {
    p <- p +
      geom_sf(data = bioregions, fill = NA, colour = "grey30",
              linewidth = 0.6, linetype = "solid") +
      geom_text(data = bioregion_centroids,
                aes(x = lon, y = lat,
                    label = stringr::str_wrap(REGION, width = 12)),
                colour     = "#1a3a6e",
                size       = label_size,
                fontface   = "bold",
                lineheight = 0.85)
  }

  p <- p +
    coord_sf(xlim = plot_limits[1:2], ylim = plot_limits[3:4],
             crs = 4326, expand = FALSE) +
    labs(x = NULL, y = NULL) +
    theme_minimal() +
    theme(
      legend.key.size  = unit(0.5, "cm"),
      legend.text      = element_text(size = 9),
      legend.title     = element_text(size = 10, face = "bold"),
      legend.position  = legend_pos,
      legend.box       = "vertical",
      legend.spacing.y = unit(0.3, "cm"),
      panel.grid       = element_blank(),
      panel.background = element_rect(fill = "white", colour = NA),
      plot.background  = element_rect(fill = "white", colour = NA),
      axis.text        = element_text(size = 9,  colour = "grey40"),
      axis.ticks       = element_line(colour = "grey60"),
      panel.border     = element_rect(fill = NA, colour = "grey60", linewidth = 0.4),
      plot.margin      = margin(t = 5, r = 5, b = 5, l = 5)
    )

  return(p)
}

# ==============================================================================
# 6. CREATE OUTPUT FOLDER
# ==============================================================================

out_dir <- paste0("plots/", park, "/spatial/notake_bathy_overview_maps")

# ==============================================================================
# 7. WA OVERVIEW — bioregions + legend, both palettes
# ==============================================================================

ggsave(file.path(out_dir, paste0(name, "-wa-overview-colorramp-TEST.png")),
       plot = make_bathy_map(plot_limits     = c(108.0, 122.0, -38.0, -24.0),
                             show_bioregions = TRUE,
                             show_legend     = TRUE,
                             palette         = shelf_palette),
       dpi = 600, width = 10, height = 12, bg = "white")

ggsave(file.path(out_dir, paste0(name, "-wa-overview-blue-TEST.png")),
       plot = make_bathy_map(plot_limits     = c(108.0, 122.0, -38.0, -24.0),
                             show_bioregions = TRUE,
                             show_legend     = TRUE,
                             palette         = shelf_palette_blue),
       dpi = 600, width = 10, height = 12, bg = "white")

# ==============================================================================
# 8. SOUTH-WEST CORNER — no bioregions, no legend, both palettes
# ==============================================================================

ggsave(file.path(out_dir, paste0(name, "-swc-colorramp-TEST.png")),
       plot = make_bathy_map(plot_limits     = c(114.2, 116.0, -34.4, -33.5),
                             show_bioregions = FALSE,
                             show_legend     = FALSE,
                             palette         = shelf_palette),
       dpi = 600, width = 10, height = 6, bg = "white")

ggsave(file.path(out_dir, paste0(name, "-swc-blue-TEST.png")),
       plot = make_bathy_map(plot_limits     = c(114.2, 116.0, -34.4, -33.5),
                             show_bioregions = FALSE,
                             show_legend     = FALSE,
                             palette         = shelf_palette_blue2),
       dpi = 600, width = 10, height = 6, bg = "white")

# ==============================================================================
# 9. JURIEN BAY — no bioregions, no legend, both palettes
# ==============================================================================

ggsave(file.path(out_dir, paste0(name, "-jurien-colorramp-TEST.png")),
       plot = make_bathy_map(plot_limits     = c(114.8, 115.4, -30.8, -30.0),
                             show_bioregions = FALSE,
                             show_legend     = FALSE,
                             palette         = shelf_palette),
       dpi = 600, width = 10, height = 8, bg = "white")

ggsave(file.path(out_dir, paste0(name, "-jurien-blue-TEST2.png")),
       plot = make_bathy_map(plot_limits     = c(114.8, 115.4, -30.8, -30.0),
                             show_bioregions = FALSE,
                             show_legend     = FALSE,
                             palette         = shelf_palette_blue),
       dpi = 600, width = 10, height = 8, bg = "white")

# ==============================================================================
# 10. ROCKINGHAM — no bioregions, no legend, both palettes
# ==============================================================================

ggsave(file.path(out_dir, paste0(name, "-rockingham-colorramp-TEST.png")),
       plot = make_bathy_map(plot_limits     = c(115.4, 116.0, -32.4, -31.8),
                             show_bioregions = FALSE,
                             show_legend     = FALSE,
                             palette         = shelf_palette),
       dpi = 600, width = 10, height = 8, bg = "white")

ggsave(file.path(out_dir, paste0(name, "-rockingham-blue-TEST.png")),
       plot = make_bathy_map(plot_limits     = c(115.4, 116.0, -32.4, -31.8),
                             show_bioregions = FALSE,
                             show_legend     = FALSE,
                             palette         = shelf_palette_blue),
       dpi = 600, width = 10, height = 8, bg = "white")

# ==============================================================================
# 11. KALBARRI — no bioregions, no legend, both palettes
# ==============================================================================

ggsave(file.path(out_dir, paste0(name, "-kalbarri-colorramp-TEST.png")),
       plot = make_bathy_map(plot_limits     = c(113.8, 114.6, -28.2, -27.4),
                             show_bioregions = FALSE,
                             show_legend     = FALSE,
                             palette         = shelf_palette),
       dpi = 600, width = 10, height = 8, bg = "white")

ggsave(file.path(out_dir, paste0(name, "-kalbarri-blue-TEST.png")),
       plot = make_bathy_map(plot_limits     = c(113.8, 114.6, -28.2, -27.4),
                             show_bioregions = FALSE,
                             show_legend     = FALSE,
                             palette         = shelf_palette_blue),
       dpi = 600, width = 10, height = 8, bg = "white")

# ==============================================================================
# 12. LANCELIN — no bioregions, no legend, both palettes
# ==============================================================================

ggsave(file.path(out_dir, paste0(name, "-lancelin-colorramp-TEST.png")),
       plot = make_bathy_map(plot_limits     = c(115.1, 115.6, -31.2, -30.8),
                             show_bioregions = FALSE,
                             show_legend     = FALSE,
                             palette         = shelf_palette),
       dpi = 600, width = 10, height = 8, bg = "white")

ggsave(file.path(out_dir, paste0(name, "-lancelin-blue-TEST.png")),
       plot = make_bathy_map(plot_limits     = c(115.1, 115.6, -31.2, -30.8),
                             show_bioregions = FALSE,
                             show_legend     = FALSE,
                             palette         = shelf_palette_blue),
       dpi = 600, width = 10, height = 8, bg = "white")
