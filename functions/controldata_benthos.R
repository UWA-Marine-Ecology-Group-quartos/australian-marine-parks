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
    )) %>%
    glimpse()

  preds <- readRDS(paste0("data/geographe/spatial/rasters/",
                          name, "_bathymetry-derivatives.rds")) %>%
    crop(dat)
  tempdat_v <- vect(as.data.frame(dat, xy = T), geom = c("x", "y"), crs = "epsg:4326")
  tempdat <- cbind(as.data.frame(dat, xy = T), terra::extract(preds[[1]], tempdat_v, ID = F))

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
      dplyr::rename(seagrass_se = p_seagrass.fit,
                    macroalgae_se = p_macro.fit,
                    # rock_se = p_rock.fit,
                    sand_se = p_sand.fit,
                    inverts_se = p_inverts.fit) %>%
      dplyr::select(ID, year, seagrass_se, macroalgae_se,
                    # rock_se,
                    sand_se, inverts_se)

    means.shallow <- terra::extract(dat.shallow, marine_parks) %>%
      dplyr::group_by(ID) %>%
      dplyr::summarise(across(starts_with("p"), ~mean(.x, na.rm = T))) %>%
      dplyr::mutate(ID = as.character(ID),
                    year = year) %>%
      dplyr::rename(seagrass = p_seagrass.fit, macroalgae = p_macro.fit,
                    # rock = prock.fit,
                    sand = p_sand.fit, inverts = p_inverts.fit) %>%
      dplyr::select(ID, year, seagrass, macroalgae,
                    # rock,
                    sand, inverts)

    # Join the data back to the zone data by ID
    park_dat.shallow <- as.data.frame(marine_parks) %>%
      tibble::rownames_to_column() %>%
      dplyr::rename(ID = rowname) %>%
      left_join(errors.shallow) %>%
      left_join(means.shallow) %>%
      dplyr::select(zone_new, year, seagrass, seagrass_se, macroalgae, macroalgae_se,
                    # rock, rock_se,
                    sand, sand_se, inverts, inverts_se) %>%
      dplyr::filter(!is.na(seagrass)) %>%
      dplyr::group_by(zone_new, year) %>%
      summarise(across(everything(), .f = list(mean = mean), na.rm = TRUE)) %>%
      ungroup() %>%
      glimpse()
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
      dplyr::rename(seagrass_se = p_seagrass.fit,
                    macroalgae_se = p_macro.fit,
                    # rock_se = p_rock.fit,
                    sand_se = p_sand.fit,
                    inverts_se = p_inverts.fit) %>%
      dplyr::select(ID, year, seagrass_se, macroalgae_se,
                    # rock_se,
                    sand_se, inverts_se)

    means.meso <- terra::extract(dat.meso, marine_parks) %>%
      dplyr::group_by(ID) %>%
      dplyr::summarise(across(starts_with("p"), ~mean(.x, na.rm = T))) %>%
      dplyr::mutate(ID = as.character(ID),
                    year = year) %>%
      dplyr::rename(seagrass = p_seagrass.fit, macroalgae = p_macro.fit,
                    # rock = prock.fit,
                    sand = p_sand.fit, inverts = p_inverts.fit) %>%
      dplyr::select(ID, year, seagrass, macroalgae,
                    # rock,
                    sand, inverts)

    # Join the data back to the zone data by ID
    park_dat.meso <- as.data.frame(marine_parks) %>%
      tibble::rownames_to_column() %>%
      dplyr::rename(ID = rowname) %>%
      left_join(errors.meso) %>%
      left_join(means.meso) %>%
      dplyr::select(zone_new, year, seagrass, seagrass_se, macroalgae, macroalgae_se,
                    # rock, rock_se,
                    sand, sand_se, inverts, inverts_se) %>%
      dplyr::filter(!is.na(seagrass)) %>%
      dplyr::group_by(zone_new, year) %>%
      summarise(across(everything(), .f = list(mean = mean), na.rm = TRUE)) %>%
      ungroup() %>%
      glimpse()
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
      dplyr::rename(seagrass_se = p_seagrass.fit,
                    macroalgae_se = p_macro.fit,
                    # rock_se = p_rock.fit,
                    sand_se = p_sand.fit,
                    inverts_se = p_inverts.fit) %>%
      dplyr::select(ID, year, seagrass_se, macroalgae_se,
                    # rock_se,
                    sand_se, inverts_se)

    means.rari <- terra::extract(dat.rari, marine_parks) %>%
      dplyr::group_by(ID) %>%
      dplyr::summarise(across(starts_with("p"), ~mean(.x, na.rm = T))) %>%
      dplyr::mutate(ID = as.character(ID),
                    year = year) %>%
      dplyr::rename(seagrass = p_seagrass.fit, macroalgae = p_macro.fit,
                    # rock = prock.fit,
                    sand = p_sand.fit, inverts = p_inverts.fit) %>%
      dplyr::select(ID, year, seagrass, macroalgae,
                    # rock,
                    sand, inverts)

    # Join the data back to the zone data by ID
    park_dat.rari <- as.data.frame(marine_parks) %>%
      tibble::rownames_to_column() %>%
      dplyr::rename(ID = rowname) %>%
      left_join(errors.shallow) %>%
      left_join(means.shallow) %>%
      dplyr::select(zone_new, year, seagrass, seagrass_se, macroalgae, macroalgae_se,
                    # rock, rock_se,
                    sand, sand_se, inverts, inverts_se) %>%
      dplyr::filter(!is.na(seagrass)) %>%
      dplyr::group_by(zone_new, year) %>%
      summarise(across(everything(), .f = list(mean = mean), na.rm = TRUE)) %>%
      ungroup() %>%
      glimpse()
    assign("park_dat.rari", park_dat.rari, envir = .GlobalEnv)
  }

}


