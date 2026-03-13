controlplot_benthos <- function(data, amp_abbrv, state_abbrv, title) {

  plot_list <- list()
  if (all(c("year", "seagrass_mean", "seagrass_se_mean", "zone_new") %in% names(data))) {
    gg_seagrass <- ggplot(data = data, aes(x = year, y = seagrass_mean, fill = zone_new, shape = zone_new)) +
      geom_errorbar(data = data, aes(ymin = seagrass_mean - seagrass_se_mean,
                                             ymax = seagrass_mean + seagrass_se_mean),
                    width = 0.8, position = position_dodge(width = 0.6)) +
      # geom_line(aes(group = zone_new, colour = zone_new),
      #           position = position_dodge(width = 0.6),
      #           linewidth = 0.6,
      #           alpha = 0.9) +
      geom_point(size = 3, position = position_dodge(width = 0.6),
                 stroke = 0.2, color = "black", alpha = 0.8) +
      theme_classic() +
      scale_x_continuous(
        breaks = c(2014, 2024)) +
      coord_cartesian(xlim = c(2013, 2025), ylim = c(0, NA)) +
      geom_vline(xintercept = 2018, linetype = "dashed", color = "black", linewidth = 0.5, alpha = 0.5) +
      scale_fill_manual(values = setNames(
        c("#b9e6fb", "#fff8a3", "#7bbc63", "#bfd054", "#bddde1"),
        c(paste(amp_abbrv, "other zones"),
          paste(amp_abbrv, "HPZ"),
          paste(amp_abbrv, "NPZ (IUCN II)"),
          paste(state_abbrv, "SZ (IUCN II)"),
          paste(state_abbrv, "other zones"))
      ), name = "Marine Parks") +
      scale_colour_manual(values = setNames(
        c("#b9e6fb", "#fff8a3", "#7bbc63", "#bfd054", "#bddde1"),
        c(paste(amp_abbrv, "other zones"),
          paste(amp_abbrv, "HPZ"),
          paste(amp_abbrv, "NPZ (IUCN II)"),
          paste(state_abbrv, "SZ (IUCN II)"),
          paste(state_abbrv, "other zones"))
      ), guide = "none") +
      scale_shape_manual(values = setNames(
        c(21, 21, 21, 25, 25),
        c(paste(amp_abbrv, "other zones"),
          paste(amp_abbrv, "HPZ"),
          paste(amp_abbrv, "NPZ (IUCN II)"),
          paste(state_abbrv, "SZ (IUCN II)"),
          paste(state_abbrv, "other zones"))
      ), name = "Marine Parks") +
      labs(x = "Year", y = "Seagrass")
    plot_list[["seagrass"]] <- gg_seagrass  # Store the plot
  }

  if (all(c("year", "macroalgae_mean", "macroalgae_se_mean", "zone_new") %in% names(data))) {
    gg_macroalgae <- ggplot(data = data, aes(x = year, y = macroalgae_mean, fill = zone_new, shape = zone_new)) +
      geom_errorbar(data = data, aes(ymin = macroalgae_mean - macroalgae_se_mean,
                                             ymax = macroalgae_mean + macroalgae_se_mean),
                    width = 0.8, position = position_dodge(width = 0.6)) +
      # geom_line(aes(group = zone_new, colour = zone_new),
      #           position = position_dodge(width = 0.6),
      #           linewidth = 0.6,
      #           alpha = 0.9) +
      geom_point(size = 3, position = position_dodge(width = 0.6),
                 stroke = 0.2, color = "black", alpha = 0.8) +
      theme_classic() +
      scale_x_continuous(
        breaks = c(2014, 2024)) +
      coord_cartesian(xlim = c(2013, 2025), ylim = c(0, NA)) +
      geom_vline(xintercept = 2018, linetype = "dashed", color = "black", linewidth = 0.5, alpha = 0.5) +
      scale_fill_manual(values = setNames(
        c("#b9e6fb", "#fff8a3", "#7bbc63", "#bfd054", "#bddde1"),
        c(paste(amp_abbrv, "other zones"),
          paste(amp_abbrv, "HPZ"),
          paste(amp_abbrv, "NPZ (IUCN II)"),
          paste(state_abbrv, "SZ (IUCN II)"),
          paste(state_abbrv, "other zones"))
      ), name = "Marine Parks") +
      scale_colour_manual(values = setNames(
        c("#b9e6fb", "#fff8a3", "#7bbc63", "#bfd054", "#bddde1"),
        c(paste(amp_abbrv, "other zones"),
          paste(amp_abbrv, "HPZ"),
          paste(amp_abbrv, "NPZ (IUCN II)"),
          paste(state_abbrv, "SZ (IUCN II)"),
          paste(state_abbrv, "other zones"))
      ), guide = "none") +
      scale_shape_manual(values = setNames(
        c(21, 21, 21, 25, 25),
        c(paste(amp_abbrv, "other zones"),
          paste(amp_abbrv, "HPZ"),
          paste(amp_abbrv, "NPZ (IUCN II)"),
          paste(state_abbrv, "SZ (IUCN II)"),
          paste(state_abbrv, "other zones"))
      ), name = "Marine Parks") +
      labs(x = "Year", y = "Macroalgae")
    plot_list[["macroalgae"]] <- gg_macroalgae  # Store the plot
  }

  if (all(c("year", "rock_mean", "rock_se_mean", "zone_new") %in% names(data))) {
    gg_rock <- ggplot(data = data, aes(x = year, y = rock_mean, fill = zone_new, shape = zone_new)) +
      geom_errorbar(data = data, aes(ymin = rock_mean - rock_se_mean,
                                             ymax = rock_mean + rock_se_mean),
                    width = 0.8, position = position_dodge(width = 0.6)) +
      # geom_line(aes(group = zone_new, colour = zone_new),
      #           position = position_dodge(width = 0.6),
      #           linewidth = 0.6,
      #           alpha = 0.9) +
      geom_point(size = 3, position = position_dodge(width = 0.6),
                 stroke = 0.2, color = "black", alpha = 0.8) +
      theme_classic() +
      scale_x_continuous(
        breaks = c(2014, 2024)) +
      coord_cartesian(xlim = c(2013, 2025), ylim = c(0, NA)) +
      geom_vline(xintercept = 2018, linetype = "dashed", color = "black", linewidth = 0.5, alpha = 0.5) +
      scale_fill_manual(values = setNames(
        c("#b9e6fb", "#fff8a3", "#7bbc63", "#bfd054", "#bddde1"),
        c(paste(amp_abbrv, "other zones"),
          paste(amp_abbrv, "HPZ"),
          paste(amp_abbrv, "NPZ (IUCN II)"),
          paste(state_abbrv, "SZ (IUCN II)"),
          paste(state_abbrv, "other zones"))
      ), name = "Marine Parks") +
      scale_colour_manual(values = setNames(
        c("#b9e6fb", "#fff8a3", "#7bbc63", "#bfd054", "#bddde1"),
        c(paste(amp_abbrv, "other zones"),
          paste(amp_abbrv, "HPZ"),
          paste(amp_abbrv, "NPZ (IUCN II)"),
          paste(state_abbrv, "SZ (IUCN II)"),
          paste(state_abbrv, "other zones"))
      ), guide = "none") +
      scale_shape_manual(values = setNames(
        c(21, 21, 21, 25, 25),
        c(paste(amp_abbrv, "other zones"),
          paste(amp_abbrv, "HPZ"),
          paste(amp_abbrv, "NPZ (IUCN II)"),
          paste(state_abbrv, "SZ (IUCN II)"),
          paste(state_abbrv, "other zones"))
      ), name = "Marine Parks") +
      labs(x = "Year", y = "Rock")
    plot_list[["rock"]] <- gg_rock  # Store the plot
  }

  if (all(c("year", "sand_mean", "sand_se_mean", "zone_new") %in% names(data))) {
    gg_sand <- ggplot(data = data, aes(x = year, y = sand_mean, fill = zone_new, shape = zone_new)) +
      geom_errorbar(data = data, aes(ymin = sand_mean - sand_se_mean,
                                             ymax = sand_mean + sand_se_mean),
                    width = 0.8, position = position_dodge(width = 0.6)) +
      # geom_line(aes(group = zone_new, colour = zone_new),
      #           position = position_dodge(width = 0.6),
      #           linewidth = 0.6,
      #           alpha = 0.9) +
      geom_point(size = 3, position = position_dodge(width = 0.6),
                 stroke = 0.2, color = "black", alpha = 0.8) +
      theme_classic() +
      scale_x_continuous(
        breaks = c(2014, 2024)) +
      coord_cartesian(xlim = c(2013, 2025), ylim = c(0, NA)) +
      geom_vline(xintercept = 2018, linetype = "dashed", color = "black", linewidth = 0.5, alpha = 0.5) +
      scale_fill_manual(values = setNames(
        c("#b9e6fb", "#fff8a3", "#7bbc63", "#bfd054", "#bddde1"),
        c(paste(amp_abbrv, "other zones"),
          paste(amp_abbrv, "HPZ"),
          paste(amp_abbrv, "NPZ (IUCN II)"),
          paste(state_abbrv, "SZ (IUCN II)"),
          paste(state_abbrv, "other zones"))
      ), name = "Marine Parks") +
      scale_colour_manual(values = setNames(
        c("#b9e6fb", "#fff8a3", "#7bbc63", "#bfd054", "#bddde1"),
        c(paste(amp_abbrv, "other zones"),
          paste(amp_abbrv, "HPZ"),
          paste(amp_abbrv, "NPZ (IUCN II)"),
          paste(state_abbrv, "SZ (IUCN II)"),
          paste(state_abbrv, "other zones"))
      ), guide = "none") +
      scale_shape_manual(values = setNames(
        c(21, 21, 21, 25, 25),
        c(paste(amp_abbrv, "other zones"),
          paste(amp_abbrv, "HPZ"),
          paste(amp_abbrv, "NPZ (IUCN II)"),
          paste(state_abbrv, "SZ (IUCN II)"),
          paste(state_abbrv, "other zones"))
      ), name = "Marine Parks") +
      labs(x = "Year", y = "Sand")
    plot_list[["sand"]] <- gg_sand  # Store the plot
  }

  if (all(c("year", "inverts_mean", "inverts_se_mean", "zone_new") %in% names(data))) {
    gg_inverts <- ggplot(data = data, aes(x = year, y = inverts_mean, fill = zone_new, shape = zone_new)) +
      geom_errorbar(data = data, aes(ymin = inverts_mean - inverts_se_mean,
                                             ymax = inverts_mean + inverts_se_mean),
                    width = 0.8, position = position_dodge(width = 0.6)) +
      # geom_line(aes(group = zone_new, colour = zone_new),
      #           position = position_dodge(width = 0.6),
      #           linewidth = 0.6,
      #           alpha = 0.9) +
      geom_point(size = 3, position = position_dodge(width = 0.6),
                 stroke = 0.2, color = "black", alpha = 0.8) +
      theme_classic() +
      scale_x_continuous(
        breaks = c(2014, 2024)) +
      coord_cartesian(xlim = c(2013, 2025), ylim = c(0, NA)) +
      geom_vline(xintercept = 2018, linetype = "dashed", color = "black", linewidth = 0.5, alpha = 0.5) +
      scale_fill_manual(values = setNames(
        c("#b9e6fb", "#fff8a3", "#7bbc63", "#bfd054", "#bddde1"),
        c(paste(amp_abbrv, "other zones"),
          paste(amp_abbrv, "HPZ"),
          paste(amp_abbrv, "NPZ (IUCN II)"),
          paste(state_abbrv, "SZ (IUCN II)"),
          paste(state_abbrv, "other zones"))
      ), name = "Marine Parks") +
      scale_colour_manual(values = setNames(
        c("#b9e6fb", "#fff8a3", "#7bbc63", "#bfd054", "#bddde1"),
        c(paste(amp_abbrv, "other zones"),
          paste(amp_abbrv, "HPZ"),
          paste(amp_abbrv, "NPZ (IUCN II)"),
          paste(state_abbrv, "SZ (IUCN II)"),
          paste(state_abbrv, "other zones"))
      ), guide = "none") +
      scale_shape_manual(values = setNames(
        c(21, 21, 21, 25, 25),
        c(paste(amp_abbrv, "other zones"),
          paste(amp_abbrv, "HPZ"),
          paste(amp_abbrv, "NPZ (IUCN II)"),
          paste(state_abbrv, "SZ (IUCN II)"),
          paste(state_abbrv, "other zones"))
      ), name = "Marine Parks") +
      labs(x = "Year", y = "Sessile invertebrates")
    plot_list[["inverts"]] <- gg_inverts  # Store the plot
  }

  if (length(plot_list) > 0) {
    combined_plot <- wrap_plots(plot_list, ncol = 1, guides = "collect") + plot_annotation(tag_levels = "a",
                                                                       title = title)  # Combine the plots
    print(combined_plot)
  } else {
    message("No plots were created.")
  }
}
