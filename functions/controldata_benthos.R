controldata_benthos <- function(year, amp_abbrv, state_abbrv) {

  marine_parks <- st_read("data/south-west network/spatial/shapefiles/western-australia_marine-parks-all.shp") %>%
    CheckEM::clean_names() %>%
    # dplyr::filter(name %in% c("Geographe", "Ngari Capes")) %>%
    dplyr::mutate(zone_new = case_when(
      str_detect(zone, "Other State Marine Park Zone")  ~ paste(state_abbrv, "other zones"),
      str_detect(zone, "Habitat Protection Zone") & str_detect(epbc, "State")  ~ paste(state_abbrv,"HPZ"),
      str_detect(zone, "Habitat Protection Zone") & str_detect(epbc, "Commonwealth")  ~ paste(amp_abbrv, "HPZ"),
      str_detect(zone, "Sanctuary Zone")  ~ paste(state_abbrv, "SZ (IUCN II)"),
      str_detect(zone, "National Park Zone")  ~ paste(amp_abbrv ,"NPZ (IUCN II)"),
      str_detect(zone, "Special Purpose Zone") & str_detect(epbc, "State")  ~ paste(state_abbrv, "other zones"),
      str_detect(zone, "Special Purpose Zone") & str_detect(epbc, "Commonwealth")  ~ paste(amp_abbrv, "other zones"),
      str_detect(zone, "Multiple Use Zone") & str_detect(epbc, "State") ~ paste(state_abbrv, "other zones"),
      str_detect(zone, "Multiple Use Zone") & str_detect(epbc, "Commonwealth") ~ paste(amp_abbrv, "other zones"),
      str_detect(zone, "Recreational Use Zone") & str_detect(epbc, "State") ~ paste(state_abbrv, "other zones"),
      str_detect(zone, "Recreational Use Zone") & str_detect(epbc, "Commonwealth") ~ paste(amp_abbrv, "other zones"),
      str_detect(zone, "General Use Zone")  ~ paste(state_abbrv, "other zones"),
      str_detect(zone, "Reef Observation Area")  ~ paste(state_abbrv, "other zones")
    ))

  preds <- readRDS(paste0("data/", park, "/spatial/rasters/",
                          name, "_bathymetry-derivatives.rds")) %>%
    crop(dat)
  tempdat_v <- vect(as.data.frame(dat, xy = T), geom = c("x", "y"), crs = "epsg:4326")
  tempdat <- cbind(as.data.frame(dat, xy = T), terra::extract(preds[[1]], tempdat_v, ID = F))

  depth_qs <- c(-2000, -200, -70, -30, 0)
  class_values <- 4:1
  reclass_matrix <- cbind(depth_qs[-length(depth_qs)], depth_qs[-1], class_values)
  edc <- classify(preds$geoscience_depth, rcl = reclass_matrix) %>%
    as.polygons() %>%
    st_as_sf()

  areas <- st_intersection(edc, marine_parks) %>%
    dplyr::mutate(area = st_area(.)) %>%
    dplyr::filter(area > units::set_units(625000, "m^2")) %>%
    dplyr::mutate(depth_contour = case_when(geoscience_depth == 1 ~ "shallow",
                                            geoscience_depth == 2 ~ "mesophotic",
                                            geoscience_depth == 3 ~ "rariphotic",
                                            geoscience_depth == 4 ~ "deep"),
                  filter = "no") %>%
    dplyr::select(zone, depth_contour, filter) %>%
    as.data.frame() %>%
    dplyr::select(-geometry)
  areas_shallow <- dplyr::filter(areas, depth_contour %in% "shallow")
  areas_meso <- dplyr::filter(areas, depth_contour %in% "mesophotic")
  areas_rari <- dplyr::filter(areas, depth_contour %in% "rariphotic")

  replacement_se <- c("seagrass_se"   = "p_seagrass.fit",
                        "macroalgae_se" = "p_macro.fit",
                        "rock_se"       = "p_rock.fit",
                        "sand_se"       = "p_sand.fit",
                        "inverts_se"    = "p_inverts.fit")
  replacement_mean   <- c("seagrass"   = "p_seagrass.fit",
                        "macroalgae" = "p_macro.fit",
                        "rock"       = "p_rock.fit",
                        "sand"       = "p_sand.fit",
                        "inverts"    = "p_inverts.fit")

  # SHALLOW (0-30 m)
  if(any(tempdat$geoscience_depth < 0 & tempdat$geoscience_depth > -30)) { # a

    shallow <- preds[[1]] %>%
      clamp(upper = 0, lower = -30, values = F)
    dat.shallow <- dat %>%
      terra::mask(shallow)

    errors.shallow <- terra::extract(dat.shallow, marine_parks) %>%
      dplyr::group_by(ID) %>%
      dplyr::summarise(across(starts_with("p"), se)) %>%
      dplyr::mutate(ID = as.character(ID),
                    year = year) %>%
      # Conditional renaming
      dplyr::rename(any_of(replacement_se)) %>%
      # dplyr::rename_with(~ if_else(.x == "p_seagrass.fit", "seagrass_se", .x), "p_seagrass.fit") %>%
      # dplyr::rename_with(~ if_else(.x == "p_macro.fit", "macroalgae_se", .x), "p_macro.fit") %>%
      # dplyr::rename_with(~ if_else(.x == "p_rock.fit", "rock_se", .x), "p_rock.fit") %>%
      # dplyr::rename_with(~ if_else(.x == "p_sand.fit", "sand_se", .x), "p_sand.fit") %>%
      # dplyr::rename_with(~ if_else(.x == "p_inverts.fit", "inverts_se", .x), "p_inverts.fit") %>%
      # Use any_of() to safely select columns if they exist
      dplyr::select(ID, year, any_of(c("seagrass_se", "macroalgae_se",
                                       "rock_se", "sand_se", "inverts_se")))

    means.shallow <- terra::extract(dat.shallow, marine_parks) %>%
      dplyr::group_by(ID) %>%
      dplyr::summarise(across(starts_with("p"), ~mean(.x, na.rm = T))) %>%
      dplyr::mutate(ID = as.character(ID),
                    year = year) %>%
      # Conditional renaming
      dplyr::rename(any_of(replacement_mean)) %>%
      # dplyr::rename_with(~ if_else(.x == "p_seagrass.fit", "seagrass", .x), "p_seagrass.fit") %>%
      # dplyr::rename_with(~ if_else(.x == "p_macro.fit", "macroalgae", .x), "p_macro.fit") %>%
      # dplyr::rename_with(~ if_else(.x == "p_rock.fit", "rock", .x), "p_rock.fit") %>%
      # dplyr::rename_with(~ if_else(.x == "p_sand.fit", "sand", .x), "p_sand.fit") %>%
      # dplyr::rename_with(~ if_else(.x == "p_inverts.fit", "inverts", .x), "p_inverts.fit") %>%
      # Use any_of() to safely select columns if they exist
      dplyr::select(ID, year, any_of(c("seagrass", "macroalgae",
                                       "rock", "sand", "inverts")))

    # Join the data back to the zone data by ID
    park_dat.shallow <- as.data.frame(marine_parks) %>%
      tibble::rownames_to_column() %>%
      dplyr::rename(ID = rowname) %>%
      left_join(errors.shallow) %>%
      left_join(means.shallow) %>%
      left_join(areas_shallow) %>%
      dplyr::filter(filter == "no") %>%
      dplyr::select(zone_new, year, any_of(c("seagrass", "seagrass_se", "macroalgae", "macroalgae_se",
                                             "rock", "rock_se", "sand", "sand_se", "inverts", "inverts_se"))) %>%
      # dplyr::filter(!is.na(seagrass)) %>%
      dplyr::group_by(zone_new, year) %>%
      summarise(across(everything(), .f = list(mean = mean), na.rm = TRUE)) %>%
      ungroup()
    assign("park_dat.shallow", park_dat.shallow, envir = .GlobalEnv)
  }

  # MESOPHOTIC (30-70 m)
  if(any(tempdat$geoscience_depth < -30 & tempdat$geoscience_depth > -70)) { # b
    meso <- preds[[1]] %>%
      clamp(upper = -30, lower = -70, values = F)
    dat.meso <- dat %>%
      terra::mask(meso)

    errors.meso <- terra::extract(dat.meso, marine_parks) %>%
      dplyr::group_by(ID) %>%
      dplyr::summarise(across(starts_with("p"), se)) %>%
      dplyr::mutate(ID = as.character(ID),
                    year = year) %>%
      # Conditional renaming
      dplyr::rename(any_of(replacement_se)) %>%
      # dplyr::rename_with(~ if_else(.x == "p_seagrass.fit", "seagrass_se", .x), "p_seagrass.fit") %>%
      # dplyr::rename_with(~ if_else(.x == "p_macro.fit", "macroalgae_se", .x), "p_macro.fit") %>%
      # dplyr::rename_with(~ if_else(.x == "p_rock.fit", "rock_se", .x), "p_rock.fit") %>%
      # dplyr::rename_with(~ if_else(.x == "p_sand.fit", "sand_se", .x), "p_sand.fit") %>%
      # dplyr::rename_with(~ if_else(.x == "p_inverts.fit", "inverts_se", .x), "p_inverts.fit") %>%
      # Use any_of() to safely select columns if they exist
      dplyr::select(ID, year, any_of(c("seagrass_se", "macroalgae_se",
                                       "rock_se", "sand_se", "inverts_se")))

    means.meso <- terra::extract(dat.meso, marine_parks) %>%
      dplyr::group_by(ID) %>%
      dplyr::summarise(across(starts_with("p"), ~mean(.x, na.rm = T))) %>%
      dplyr::mutate(ID = as.character(ID),
                    year = year) %>%
      # Conditional renaming
      dplyr::rename(any_of(replacement_mean)) %>%
      # dplyr::rename_with(~ if_else(.x == "p_seagrass.fit", "seagrass", .x), "p_seagrass.fit") %>%
      # dplyr::rename_with(~ if_else(.x == "p_macro.fit", "macroalgae", .x), "p_macro.fit") %>%
      # dplyr::rename_with(~ if_else(.x == "p_rock.fit", "rock", .x), "p_rock.fit") %>%
      # dplyr::rename_with(~ if_else(.x == "p_sand.fit", "sand", .x), "p_sand.fit") %>%
      # dplyr::rename_with(~ if_else(.x == "p_inverts.fit", "inverts", .x), "p_inverts.fit") %>%
      # Use any_of() to safely select columns if they exist
      dplyr::select(ID, year, any_of(c("seagrass", "macroalgae",
                                       "rock", "sand", "inverts")))

    # Join the data back to the zone data by ID
    park_dat.meso <- as.data.frame(marine_parks) %>%
      tibble::rownames_to_column() %>%
      dplyr::rename(ID = rowname) %>%
      left_join(errors.meso) %>%
      left_join(means.meso) %>%
      left_join(areas_meso) %>%
      dplyr::filter(filter == "no") %>%
      dplyr::select(zone_new, year, any_of(c("seagrass", "seagrass_se", "macroalgae", "macroalgae_se",
                                             "rock", "rock_se", "sand", "sand_se", "inverts", "inverts_se"))) %>%
      # dplyr::filter(!is.na(seagrass)) %>%
      dplyr::group_by(zone_new, year) %>%
      summarise(across(everything(), .f = list(mean = mean), na.rm = TRUE)) %>%
      ungroup()
    assign("park_dat.meso", park_dat.meso, envir = .GlobalEnv)
  }

  # RARIPHOTIC (70-200 m)
  if(any(tempdat$geoscience_depth < -70 & tempdat$geoscience_depth > -200)) { # c
    rari <- preds[[1]] %>%
      clamp(upper = -70, lower = -200, values = F)
    dat.rari <- dat %>%
      terra::mask(rari)

    errors.rari <- terra::extract(dat.rari, marine_parks) %>%
      dplyr::group_by(ID) %>%
      dplyr::summarise(across(starts_with("p"), se)) %>%
      dplyr::mutate(ID = as.character(ID),
                    year = year) %>%
      # Conditional renaming
      dplyr::rename(any_of(replacement_se)) %>%
      # dplyr::rename_with(~ if_else(.x == "p_seagrass.fit", "seagrass_se", .x), "p_seagrass.fit") %>%
      # dplyr::rename_with(~ if_else(.x == "p_macro.fit", "macroalgae_se", .x), "p_macro.fit") %>%
      # dplyr::rename_with(~ if_else(.x == "p_rock.fit", "rock_se", .x), "p_rock.fit") %>%
      # dplyr::rename_with(~ if_else(.x == "p_sand.fit", "sand_se", .x), "p_sand.fit") %>%
      # dplyr::rename_with(~ if_else(.x == "p_inverts.fit", "inverts_se", .x), "p_inverts.fit") %>%
      # Use any_of() to safely select columns if they exist
      dplyr::select(ID, year, any_of(c("seagrass_se", "macroalgae_se",
                                       "rock_se", "sand_se", "inverts_se")))

    means.rari <- terra::extract(dat.rari, marine_parks) %>%
      dplyr::group_by(ID) %>%
      dplyr::summarise(across(starts_with("p"), ~mean(.x, na.rm = T))) %>%
      dplyr::mutate(ID = as.character(ID),
                    year = year) %>%
      # Conditional renaming
      dplyr::rename(any_of(replacement_mean)) %>%
      # dplyr::rename_with(~ if_else(.x == "p_seagrass.fit", "seagrass", .x), "p_seagrass.fit") %>%
      # dplyr::rename_with(~ if_else(.x == "p_macro.fit", "macroalgae", .x), "p_macro.fit") %>%
      # dplyr::rename_with(~ if_else(.x == "p_rock.fit", "rock", .x), "p_rock.fit") %>%
      # dplyr::rename_with(~ if_else(.x == "p_sand.fit", "sand", .x), "p_sand.fit") %>%
      # dplyr::rename_with(~ if_else(.x == "p_inverts.fit", "inverts", .x), "p_inverts.fit") %>%
      # Use any_of() to safely select columns if they exist
      dplyr::select(ID, year, any_of(c("seagrass", "macroalgae",
                                       "rock", "sand", "inverts")))

    # Join the data back to the zone data by ID
    park_dat.rari <- as.data.frame(marine_parks) %>%
      tibble::rownames_to_column() %>%
      dplyr::rename(ID = rowname) %>%
      left_join(errors.shallow) %>%
      left_join(means.shallow) %>%
      left_join(areas_rari) %>%
      dplyr::filter(filter == "no") %>%
      dplyr::select(zone_new, year, any_of(c("seagrass", "seagrass_se", "macroalgae", "macroalgae_se",
                    "rock", "rock_se", "sand", "sand_se", "inverts", "inverts_se"))) %>%
      # dplyr::filter(!is.na(seagrass)) %>%
      dplyr::group_by(zone_new, year) %>%
      summarise(across(everything(), .f = list(mean = mean), na.rm = TRUE)) %>%
      ungroup()
    assign("park_dat.rari", park_dat.rari, envir = .GlobalEnv)
  }

}


