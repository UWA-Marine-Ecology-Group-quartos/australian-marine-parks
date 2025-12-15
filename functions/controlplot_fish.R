controlplot_fish <- function(data, amp_abbrv, state_abbrv, title) {

  # ---- Backwards-compatibility: map new names -> old "*_mean" names ----
  # (So your original plotting code can stay almost unchanged)
  if (!("richness_mean" %in% names(data)) && ("richness" %in% names(data))) {
    data <- data %>% dplyr::mutate(richness_mean = richness)
  }
  if (!("richness_se_mean" %in% names(data)) && ("richness_se" %in% names(data))) {
    data <- data %>% dplyr::mutate(richness_se_mean = richness_se)
  }

  if (!("cti_mean" %in% names(data)) && ("cti" %in% names(data))) {
    data <- data %>% dplyr::mutate(cti_mean = cti)
  }
  if (!("cti_se_mean" %in% names(data)) && ("cti_se" %in% names(data))) {
    data <- data %>% dplyr::mutate(cti_se_mean = cti_se)
  }

  # Optional: if you later add Lm outputs, keep this mapping pattern.
  if (!("Lm_mean" %in% names(data)) && ("Lm" %in% names(data))) {
    data <- data %>% dplyr::mutate(Lm_mean = Lm)
  }
  if (!("Lm_se_mean" %in% names(data)) && ("Lm_se" %in% names(data))) {
    data <- data %>% dplyr::mutate(Lm_se_mean = Lm_se)
  }

  # ---- Add a plotting label that includes status (if present) ----
  if ("status" %in% names(data)) {
    data <- data %>% dplyr::mutate(zone_lab = paste(zone_new, "(", status, ")"))
  } else {
    data <- data %>% dplyr::mutate(zone_lab = zone_new)
  }

  plot_list <- list()

  # Build palette keys (must match the labels used in fill/shape)
  base_levels <- c(paste(amp_abbrv, "other zones"),
                   paste(amp_abbrv, "HPZ"),
                   paste(amp_abbrv, "NPZ (IUCN II)"),
                   paste(state_abbrv, "SZ (IUCN II)"),
                   paste(state_abbrv, "other zones"))

  # If status exists, expand keys to "ZONE (STATUS)"
  if ("status" %in% names(data)) {
    keys <- c(paste0(base_levels, " (Fished)"),
              paste0(base_levels, " (No-Take)"))
    vals_fill  <- rep(c("#b9e6fb", "#fff8a3", "#7bbc63", "#bfd054", "#bddde1"), times = 2)
    vals_shape <- rep(c(21, 21, 21, 25, 25), times = 2)
    names(vals_fill)  <- keys
    names(vals_shape) <- keys
  } else {
    keys <- base_levels
    vals_fill  <- c("#b9e6fb", "#fff8a3", "#7bbc63", "#bfd054", "#bddde1")
    vals_shape <- c(21, 21, 21, 25, 25)
    names(vals_fill)  <- keys
    names(vals_shape) <- keys
  }

  # ---- Species Richness ----
  if (all(c("year", "richness_mean", "richness_se_mean", "zone_lab") %in% names(data))) {

    gg_sr <- ggplot(data = data, aes(x = year, y = richness_mean, fill = zone_lab, shape = zone_lab)) +
      geom_errorbar(aes(ymin = richness_mean - richness_se_mean,
                        ymax = richness_mean + richness_se_mean),
                    width = 0.8, position = position_dodge(width = 0.6)) +
      geom_point(size = 3, position = position_dodge(width = 0.6),
                 stroke = 0.2, color = "black", alpha = 0.8, shape = 21) +
      theme_classic() +
      scale_x_continuous(limits = c(2013, 2024),
                         breaks = c(2013, 2015, 2017, 2019, 2021, 2023)) +
      geom_vline(xintercept = 2018, linetype = "dashed", color = "black", linewidth = 0.5, alpha = 0.5) +
      scale_fill_manual(values = vals_fill, name = "Marine Parks") +
      scale_shape_manual(values = vals_shape, name = "Marine Parks") +
      labs(x = "Year", y = "Species richness")

    plot_list[["species.richness"]] <- gg_sr
  }

  # ---- >Lm large bodied carnivores (only if present) ----
  if (all(c("year", "Lm_mean", "Lm_se_mean", "zone_lab") %in% names(data))) {

    gg_lm <- ggplot(data = data, aes(x = year, y = Lm_mean, fill = zone_lab, shape = zone_lab)) +
      geom_errorbar(aes(ymin = Lm_mean - Lm_se_mean,
                        ymax = Lm_mean + Lm_se_mean),
                    width = 0.8, position = position_dodge(width = 0.6)) +
      geom_point(size = 3, position = position_dodge(width = 0.6),
                 stroke = 0.2, color = "black", alpha = 0.8, shape = 21) +
      theme_classic() +
      scale_x_continuous(limits = c(2013, 2024),
                         breaks = c(2013, 2015, 2017, 2019, 2021, 2023)) +
      geom_vline(xintercept = 2018, linetype = "dashed", color = "black", linewidth = 0.5, alpha = 0.5) +
      scale_fill_manual(values = vals_fill, name = "Marine Parks") +
      scale_shape_manual(values = vals_shape, name = "Marine Parks") +
      labs(x = "Year", y = ">Lm large bodied carnivores")

    plot_list[["mature"]] <- gg_lm
  }

  # ---- CTI ----
  if (all(c("year", "cti_mean", "cti_se_mean", "zone_lab") %in% names(data))) {

    sst <- readRDS(paste0("data/", park, "/spatial/oceanography/",
                          name, "_SST_time-series.rds")) %>%
      dplyr::mutate(year = as.numeric(year)) %>%
      dplyr::group_by(year) %>%
      dplyr::summarise(sst = mean(sst, na.rm = TRUE),
                       sd  = mean(sd,  na.rm = TRUE),
                       .groups = "drop")

    gg_cti <- ggplot() +
      geom_line(data = sst, aes(x = year, y = sst)) +
      geom_ribbon(data = sst, aes(x = year, y = sst, ymin = sst - sd, ymax = sst + sd),
                  alpha = 0.2) +
      geom_errorbar(data = data,
                    aes(x = year, y = cti_mean,
                        ymin = cti_mean - cti_se_mean,
                        ymax = cti_mean + cti_se_mean,
                        fill = zone_lab, shape = zone_lab),
                    width = 0.8, position = position_dodge(width = 0.6)) +
      geom_point(data = data,
                 aes(x = year, y = cti_mean, fill = zone_lab, shape = zone_lab),
                 size = 3, stroke = 0.2, color = "black",
                 position = position_dodge(width = 0.6),
                 alpha = 0.8, shape = 21) +
      theme_classic() +
      scale_x_continuous(limits = c(2013, 2024),
                         breaks = c(2013, 2015, 2017, 2019, 2021, 2023)) +
      geom_vline(xintercept = 2018, linetype = "dashed", color = "black",
                 linewidth = 0.5, alpha = 0.5) +
      scale_fill_manual(values = vals_fill, name = "Marine Parks") +
      scale_shape_manual(values = vals_shape, name = "Marine Parks") +
      labs(x = "Year", y = "Community Temperature Index")

    plot_list[["cti"]] <- gg_cti
  }

  if (length(plot_list) > 0) {
    combined_plot <- wrap_plots(plot_list, ncol = 1, guides = "collect") +
      plot_annotation(tag_levels = "a", title = title)

    return(combined_plot)   # <-- IMPORTANT for ggsave(plot = ...)
  } else {
    message("No plots were created.")
    return(NULL)
  }
}
