library(dplyr)
library(ggplot2)
library(googlesheets4)
library(patchwork)
library(cowplot)

# read in dummy temporal data ----
# TODO - replace this with real data

data <- read_sheet("https://docs.google.com/spreadsheets/d/1Iplohv6mM-CnpE6uYBi4uQnuhCyZMNpCRMSJFFnJxjM/edit?usp=sharing",
                   sheet = "temporal_data") %>%
  mutate(year = as.numeric(format(date, "%Y")))

# # do a plot with the first one just as a test to get the code right -----
#
# test_data <- data %>%
#   dplyr::filter(marine_park %in% "Geographe Marine Park") %>%
#   dplyr::filter(ecosystem_condition %in% "Demersal fish") %>%
#   dplyr::filter(metric %in% "Abundance of large-bodied generalist carnivores greater than Lm") %>%
#   glimpse
#
#
# # Create the plot
# p <- ggplot(test_data, aes(x = year, y = number, fill = zone, group = zone, shape = zone, col = zone)) +
#   geom_errorbar(aes(ymin = number - se, ymax = number + se), width = 0.02) +
#   geom_point(size = 3,
#              stroke = 0.2, color = "black", alpha = 0.8, shape = 21) +
#   geom_line() +
#   geom_vline(xintercept = 2018, linetype = "dashed", color = "black") +
#   # theme_classic() +
#
#
#
#   scale_color_manual(values = c("Multiple Use Zone" = "#b9e6fb",
#                                "Habitat Protection Zone" = "#fff8a3",
#                                "National Park Zone" = "#7bbc63",
#                                "Special Purpose Zone" = "#6BB1E5",
#                                "Sanctuary Zone" = "#bfd054",
#                                "Other Zones" = "#bddde1"), name = "Australian Marine Parks") +
#
#   scale_fill_manual(values = c("Multiple Use Zone" = "#b9e6fb",
#                                 "Habitat Protection Zone" = "#fff8a3",
#                                 "National Park Zone" = "#7bbc63",
#                                 "Special Purpose Zone" = "#6BB1E5",
#                                 "Sanctuary Zone" = "#bfd054",
#                                 "Other Zones" = "#bddde1"), name = "Australian Marine Parks") +
#
#   labs(
#     x = "Year",
#     y = "> Lm large-bodied carnivores",
#     color = "Australian Marine Parks"
#   ) +
#   labs(x = "Year", y = str_wrap(unique(test_data$metric), 40))
#
# p
#
# final_plot <- p +
#   plot_annotation(
#   title = unique(test_data$marine_park),
#   subtitle = unique(test_data$metric),
#   theme = theme(
#     plot.title = element_text(size = 18, face = "bold"),  # Adjust title size and style
#     plot.subtitle = element_text(size = 16, face = "italic")  # Adjust subtitle size and style
#   ))
#
# final_plot
#
# # Define file names for saving
# file_prefix <- paste(unique(test_data$network), unique(test_data$marine_park), unique(test_data$metric), sep = "_")
#
# # Save the plot as RDS
# # saveRDS(plot, file = paste0("plots/condition/temporal/", file_prefix, ".rds"))


## Now begin real data ----

# Get unique combinations of variables
combinations <- data %>%
  distinct(network, marine_park, ecosystem_condition, metric)

# Loop through each combination
for (i in seq_len(nrow(combinations))) {
  # Filter data for the current combination
  current_combo <- combinations[i, ]

  filtered_data <- data %>%
    filter(
      network == current_combo$network,
      marine_park == current_combo$marine_park,
      ecosystem_condition == current_combo$ecosystem_condition,
      metric == current_combo$metric
    )

  # Skip if no data is found
  if (nrow(filtered_data) == 0) next

  # Get unique depth classes
  depth_classes <- unique(filtered_data$depth_class)

  # Create a list to store plots for each depth class
  depth_plots <- list()

  # Loop through each depth class
  for (depth in depth_classes) {
    # Filter data for the current depth class
    depth_data <- filtered_data %>% filter(depth_class == depth)

    # Create the plot for the current depth class
    p <- ggplot(depth_data, aes(x = year, y = number, fill = zone, group = zone, shape = zone, col = zone)) +
      geom_errorbar(aes(ymin = number - se, ymax = number + se), width = 0.02) +
      geom_point(size = 3,
                 stroke = 0.2, color = "black", alpha = 0.8, shape = 21) +
      geom_line() +
      geom_vline(xintercept = 2018, linetype = "dashed", color = "black") +
      scale_color_manual(values = c(
        "Multiple Use Zone" = "#b9e6fb",
        "Habitat Protection Zone" = "#fff8a3",
        "National Park Zone" = "#7bbc63",
        "Special Purpose Zone" = "#6BB1E5",
        "Sanctuary Zone" = "#bfd054",
        "Other Zones" = "#bddde1"
      ), name = "Australian Marine Parks") +
      scale_fill_manual(values = c(
        "Multiple Use Zone" = "#b9e6fb",
        "Habitat Protection Zone" = "#fff8a3",
        "National Park Zone" = "#7bbc63",
        "Special Purpose Zone" = "#6BB1E5",
        "Sanctuary Zone" = "#bfd054",
        "Other Zones" = "#bddde1"
      ), name = "Australian Marine Parks") +
      labs(
        x = "Year",
        y = "> Lm large-bodied carnivores",
        color = "Australian Marine Parks",
        title = paste("Depth:", depth)
      ) +
      labs(x = "Year", y = str_wrap(unique(depth_data$metric), 30)) +
      theme_bw() +
      theme(#axis.title.y = element_blank(), # Remove y-axis labels for individual plots
            legend.position = "none",      # Suppress individual legends
            axis.title = element_text(size = 16), # Larger axis titles
            axis.text = element_text(size = 14), # Larger axis text
            legend.title = element_text(size = 16), # Larger legend title
            legend.text = element_text(size = 14), # Larger legend text
            plot.title = element_text(size = 18, face = "italic"), # Larger plot title
            strip.text = element_text(size = 16), # Larger facet strip text
            axis.line = element_line(colour = "black"),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.border = element_blank(),
            panel.background = element_blank())

    depth_plots[[depth]] <- p
  }

  # # Combine all depth plots into a single stacked plot
  # final_plot <- wrap_plots(depth_plots, ncol = 1) +
  #   plot_annotation(
  #     # title = unique(filtered_data$marine_park),
  #     # subtitle = unique(filtered_data$metric),
  #     theme = theme(
  #       plot.title = element_text(size = 18, face = "bold"),
  #       plot.subtitle = element_text(size = 16, face = "italic")
  #     )
  #   )
  #
  # # Define file name
  # file_prefix <- paste(
  #   unique(filtered_data$network),
  #   unique(filtered_data$marine_park),
  #   unique(filtered_data$metric),
  #   length(depth_classes),
  #   sep = "_"
  # )
  #

  # Combine all depth plots into a single stacked plot
  combined_plot <- wrap_plots(depth_plots, ncol = 1) #&
    #theme(plot.margin = unit(c(1, 1, 1, 4), "lines")) # Adjust margin for shared y-axis

  # Add shared legend and y-axis label
  final_plot <- combined_plot +
    plot_layout(guides = "collect") + # Collect legends into one
    plot_annotation(
      title = unique(filtered_data$marine_park),
      theme = theme(
        plot.title = element_text(size = 18, face = "bold"),
        plot.subtitle = element_text(size = 16, face = "italic")
      )
    ) &
    # labs(
    #   y = unique(filtered_data$metric) # Shared y-axis label
    # ) &
    theme(
      legend.position = "top", # Position legend at the bottom
      legend.title = element_text(size = 16),
      legend.text = element_text(size = 14)#,
      # axis.title.y = element_text(size = 16)
    )


  # Save the plot as RDS
  file_prefix <- paste(
    unique(filtered_data$network),
    unique(filtered_data$marine_park),
    unique(filtered_data$metric),
    length(depth_classes),
    sep = "_"
  )

  saveRDS(final_plot, file = paste0("plots/temporal/", file_prefix, ".rds"))
  saveRDS(final_plot, file = paste0("inst/shiny/amp-dashboard/plots/temporal/", file_prefix, ".rds"))
#
#   # Save the plot as RDS
#   saveRDS(final_plot, file = paste0("plots/temporal/", file_prefix, ".rds"))
#   saveRDS(final_plot, file = paste0("inst/shiny/amp-dashboard/plots/temporal/", file_prefix, ".rds"))
}

