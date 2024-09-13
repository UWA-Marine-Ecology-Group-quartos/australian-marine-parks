controlplot_fish <- function(data, amp_abbrv, state_abbrv, title) {

  plot_list <- list()
  if (all(c("year", "richness_mean", "richness_se_mean", "zone_new") %in% names(data))) {
    gg_sr <- ggplot(data = data, aes(x = year, y = richness_mean, fill = zone_new, shape = zone_new)) +
      geom_errorbar(data = data, aes(ymin = richness_mean - richness_se_mean,
                                                 ymax = richness_mean + richness_se_mean),
                    width = 0.8, position = position_dodge(width = 0.6)) +
      geom_point(size = 3, position = position_dodge(width = 0.6),
                 stroke = 0.2, color = "black", alpha = 0.8, shape = 21) +
      theme_classic() +
      scale_x_continuous(limits = c(2013, 2024),
                         breaks = c(2013, 2015, 2017, 2019, 2021, 2023)) +
      geom_vline(xintercept = 2018, linetype = "dashed", color = "black", linewidth = 0.5, alpha = 0.5) +
      scale_fill_manual(values = setNames(
        c("#b9e6fb", "#fff8a3", "#7bbc63", "#bfd054", "#bddde1"),
        c(paste(amp_abbrv, "other zones"),
          paste(amp_abbrv, "HPZ"),
          paste(amp_abbrv, "NPZ (IUCN II)"),
          paste(state_abbrv, "SZ (IUCN II)"),
          paste(state_abbrv, "other zones"))
      ), name = "Marine Parks") +
      scale_shape_manual(values = setNames(
        c(21, 21, 21, 25, 25),
        c(paste(amp_abbrv, "other zones"),
          paste(amp_abbrv, "HPZ"),
          paste(amp_abbrv, "NPZ (IUCN II)"),
          paste(state_abbrv, "SZ (IUCN II)"),
          paste(state_abbrv, "other zones"))
      ), name = "Marine Parks") +
      labs(x = "Year", y = "Species richness")
    plot_list[["species.richness"]] <- gg_sr  # Store the plot
  }

  if (all(c("year", "Lm_mean", "Lm_se_mean", "zone_new") %in% names(data))) {
    gg_lm <- ggplot(data = data, aes(x = year, y = Lm_mean, fill = zone_new, shape = zone_new)) +
      geom_errorbar(data = data, aes(ymin = Lm_mean - Lm_se_mean,
                                                 ymax = Lm_mean + Lm_se_mean),
                    width = 0.8, position = position_dodge(width = 0.6)) +
      geom_point(size = 3, position = position_dodge(width = 0.6),
                 stroke = 0.2, color = "black", alpha = 0.8, shape = 21) +
      theme_classic() +
      scale_x_continuous(limits = c(2013, 2024),
                         breaks = c(2013, 2015, 2017, 2019, 2021, 2023)) +
      geom_vline(xintercept = 2018, linetype = "dashed", color = "black", linewidth = 0.5, alpha = 0.5) +
      scale_fill_manual(values = setNames(
        c("#b9e6fb", "#fff8a3", "#7bbc63", "#bfd054", "#bddde1"),
        c(paste(amp_abbrv, "other zones"),
          paste(amp_abbrv, "HPZ"),
          paste(amp_abbrv, "NPZ (IUCN II)"),
          paste(state_abbrv, "SZ (IUCN II)"),
          paste(state_abbrv, "other zones"))
      ), name = "Marine Parks") +
      scale_shape_manual(values = setNames(
        c(21, 21, 21, 25, 25),
        c(paste(amp_abbrv, "other zones"),
          paste(amp_abbrv, "HPZ"),
          paste(amp_abbrv, "NPZ (IUCN II)"),
          paste(state_abbrv, "SZ (IUCN II)"),
          paste(state_abbrv, "other zones"))
      ), name = "Marine Parks") +
      labs(x = "Year", y = ">Lm large bodied carnivores")
    plot_list[["mature"]] <- gg_lm  # Store the plot
  }

  if (all(c("year", "cti_mean", "cti_se_mean", "zone_new") %in% names(data))) {
    sst <- readRDS(paste0("data/", park, "/spatial/oceanography/",
                          name, "_SST_time-series.rds")) %>%
      dplyr::mutate(year = as.numeric(year)) %>%
      group_by(year) %>%
      summarise(sst = mean(sst, na.rm = T), sd = mean(sd, na.rm = T))

    gg_cti <- ggplot() +
      geom_line(data = sst, aes(x = year, y = sst))+
      geom_ribbon(data = sst, aes(x = year, y = sst,
                                  ymin = sst - sd, ymax = sst + sd),
                  alpha = 0.2) +
      geom_errorbar(data = data, aes(x = year, y = cti_mean, ymin = cti_mean - cti_se_mean,
                                                 ymax = cti_mean + cti_se_mean, fill = zone_new, shape = zone_new), # This has a warning but it plots wrong if you remove fill
                    width = 0.8, position = position_dodge(width = 0.6))+
      geom_point(data = data, aes(x = year, y = cti_mean, fill = zone_new, shape = zone_new),size = 3,
                 stroke = 0.2, color = "black", position = position_dodge(width = 0.6),
                 alpha = 0.8, shape = 21)+
      theme_classic() +
      scale_x_continuous(limits = c(2013, 2024),
                         breaks = c(2013, 2015, 2017, 2019, 2021, 2023)) +
      geom_vline(xintercept = 2018, linetype = "dashed", color = "black",
                 size = 0.5, alpha = 0.5) +
      scale_fill_manual(values = setNames(
        c("#b9e6fb", "#fff8a3", "#7bbc63", "#bfd054", "#bddde1"),
        c(paste(amp_abbrv, "other zones"),
          paste(amp_abbrv, "HPZ"),
          paste(amp_abbrv, "NPZ (IUCN II)"),
          paste(state_abbrv, "SZ (IUCN II)"),
          paste(state_abbrv, "other zones"))
      ), name = "Marine Parks") +
      scale_shape_manual(values = setNames(
        c(21, 21, 21, 25, 25),
        c(paste(amp_abbrv, "other zones"),
          paste(amp_abbrv, "HPZ"),
          paste(amp_abbrv, "NPZ (IUCN II)"),
          paste(state_abbrv, "SZ (IUCN II)"),
          paste(state_abbrv, "other zones"))
      ), name = "Marine Parks") +
      labs(x = "Year", y = "Community Temperature Index")
    plot_list[["cti"]] <- gg_cti  # Store the plot
  }


  if (length(plot_list) > 0) {
    combined_plot <- wrap_plots(plot_list, ncol = 1, guides = "collect") + plot_annotation(tag_levels = "a",
                                                                                           title = title)  # Combine the plots
    print(combined_plot)
  } else {
    message("No plots were created.")
  }
}
