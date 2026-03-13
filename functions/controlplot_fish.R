controlplot_fish <- function(data, amp_abbrv, state_abbrv, title) {

  data <- data %>% dplyr::mutate(year = as.numeric(year))

  # Same palette/shape mapping as benthos
  zone_levels <- c(paste(amp_abbrv, "other zones"),
                   paste(amp_abbrv, "HPZ"),
                   paste(amp_abbrv, "NPZ (IUCN II)"),
                   paste(state_abbrv, "SZ (IUCN II)"),
                   paste(state_abbrv, "other zones"))

  fill_vals <- setNames(
    c("#b9e6fb", "#fff8a3", "#7bbc63", "#bfd054", "#bddde1"),
    zone_levels
  )
  shape_vals <- setNames(
    c(21, 21, 21, 25, 25),
    zone_levels
  )

  plot_list <- list()

  # ---- Species richness ----
  if (all(c("year", "richness", "richness_se", "zone_new") %in% names(data))) {

    gg_sr <- ggplot(data, aes(x = year, y = richness, fill = zone_new, shape = zone_new)) +
      geom_errorbar(aes(ymin = richness - richness_se,
                        ymax = richness + richness_se),
                    width = 0.8, position = position_dodge(width = 0.6)) +
      # geom_line(aes(group = zone_new, colour = zone_new),
      #           position = position_dodge(width = 0.6),
      #           linewidth = 0.6, alpha = 0.9) +
      geom_point(size = 3, position = position_dodge(width = 0.6),
                 stroke = 0.2, color = "black", alpha = 0.8) +
      theme_classic() +
      scale_x_continuous(
        breaks = c(2014, 2024)) +
      coord_cartesian(xlim = c(2013, 2025), ylim = c(0, NA)) +
      geom_vline(xintercept = 2018, linetype = "dashed", color = "black",
                 linewidth = 0.5, alpha = 0.5) +
      scale_fill_manual(values = fill_vals, name = "Marine Parks") +
      scale_shape_manual(values = shape_vals, name = "Marine Parks") +
      scale_colour_manual(values = fill_vals, guide = "none") +
      labs(x = "Year", y = "Species richness")

    plot_list[["species.richness"]] <- gg_sr
  }

  # ---- CTI ----
  if (all(c("year", "cti", "cti_se", "zone_new") %in% names(data))) {

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
                    aes(x = year, y = cti,
                        ymin = cti - cti_se,
                        ymax = cti + cti_se,
                        fill = zone_new, shape = zone_new),
                    width = 0.8, position = position_dodge(width = 0.6)) +
      # geom_line(data = data,
      #           aes(x = year, y = cti, group = zone_new, colour = zone_new),
      #           position = position_dodge(width = 0.6),
      #           linewidth = 0.6, alpha = 0.9) +
      geom_point(data = data,
                 aes(x = year, y = cti, fill = zone_new, shape = zone_new),
                 size = 3, stroke = 0.2, color = "black",
                 position = position_dodge(width = 0.6),
                 alpha = 0.8) +
      theme_classic() +
      scale_x_continuous(
        breaks = c(2014, 2024)) +
      coord_cartesian(xlim = c(2013, 2025)) +
      geom_vline(xintercept = 2018, linetype = "dashed", color = "black",
                 linewidth = 0.5, alpha = 0.5) +
      scale_fill_manual(values = fill_vals, name = "Marine Parks") +
      scale_shape_manual(values = shape_vals, name = "Marine Parks") +
      scale_colour_manual(values = fill_vals, guide = "none") +
      labs(x = "Year", y = "Community Temperature Index")

    plot_list[["cti"]] <- gg_cti
  }

  # ---- Abundance ----
  if (all(c("year", "abundance", "abundance_se", "zone_new") %in% names(data))) {

    gg_ab <- ggplot(data, aes(x = year, y = abundance, fill = zone_new, shape = zone_new)) +
      geom_errorbar(aes(ymin = abundance - abundance_se,
                        ymax = abundance + abundance_se),
                    width = 0.8, position = position_dodge(width = 0.6)) +
      # geom_line(aes(group = zone_new, colour = zone_new),
      #           position = position_dodge(width = 0.6),
      #           linewidth = 0.6, alpha = 0.9) +
      geom_point(size = 3, position = position_dodge(width = 0.6),
                 stroke = 0.2, color = "black", alpha = 0.8) +
      theme_classic() +
      scale_x_continuous(
        breaks = c(2014, 2024)) +
      coord_cartesian(xlim = c(2013, 2025), ylim = c(0, NA)) +
      geom_vline(xintercept = 2018, linetype = "dashed", color = "black",
                 linewidth = 0.5, alpha = 0.5) +
      scale_fill_manual(values = fill_vals, name = "Marine Parks") +
      scale_shape_manual(values = shape_vals, name = "Marine Parks") +
      scale_colour_manual(values = fill_vals, guide = "none") +
      labs(x = "Year", y = "Total abundance")

    plot_list[["abundance"]] <- gg_ab
  }

  # ---- B20 ----
  if (all(c("year", "b20", "b20_se", "zone_new") %in% names(data))) {

    gg_b20 <- ggplot(data, aes(x = year, y = b20, fill = zone_new, shape = zone_new)) +
      geom_errorbar(aes(ymin = b20 - b20_se,
                        ymax = b20 + b20_se),
                    width = 0.8, position = position_dodge(width = 0.6)) +
      # geom_line(aes(group = zone_new, colour = zone_new),
      #           position = position_dodge(width = 0.6),
      #           linewidth = 0.6, alpha = 0.9) +
      geom_point(size = 3, position = position_dodge(width = 0.6),
                 stroke = 0.2, color = "black", alpha = 0.8) +
      theme_classic() +
      scale_x_continuous(
        breaks = c(2014, 2024)) +
      coord_cartesian(xlim = c(2013, 2025), ylim = c(0, NA)) +
      geom_vline(xintercept = 2018, linetype = "dashed", color = "black",
                 linewidth = 0.5, alpha = 0.5) +
      scale_fill_manual(values = fill_vals, name = "Marine Parks") +
      scale_shape_manual(values = shape_vals, name = "Marine Parks") +
      scale_colour_manual(values = fill_vals, guide = "none") +
      labs(x = "Year", y = "B20")

    plot_list[["b20"]] <- gg_b20
  }

  if (length(plot_list) > 0) {
    combined_plot <- wrap_plots(plot_list, ncol = 1, guides = "collect") +
      plot_annotation(tag_levels = "a", title = title)
    return(combined_plot)
  } else {
    message("No plots were created.")
    return(NULL)
  }
}
