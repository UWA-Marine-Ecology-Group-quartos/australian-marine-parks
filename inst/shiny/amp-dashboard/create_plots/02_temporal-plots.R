library(dplyr)
library(ggplot)
library(googlesheets4)

# read in dummy temporal data ----
# TODO - replace this with real data

data <- read_sheet("https://docs.google.com/spreadsheets/d/1Iplohv6mM-CnpE6uYBi4uQnuhCyZMNpCRMSJFFnJxjM/edit?usp=sharing",
                   sheet = "temporal_data") %>%
  mutate(year = as.numeric(format(date, "%Y")))

# do a plot with the first one just as a test to get the code right -----

test_data <- data %>%
  dplyr::filter(marine_park %in% "Geographe Marine Park") %>%
  dplyr::filter(ecosystem_condition %in% "Demersal fish") %>%
  dplyr::filter(metric %in% "Abundance of large-bodied generalist carnivores greater than Lm") %>%
  glimpse


# Create the plot
p <- ggplot(test_data, aes(x = year, y = number, fill = zone, group = zone, shape = zone, col = zone)) +
  geom_errorbar(aes(ymin = number - se, ymax = number + se), width = 0.02) +
  geom_point(size = 3,
             stroke = 0.2, color = "black", alpha = 0.8, shape = 21) +
  geom_line() +
  geom_vline(xintercept = 2018, linetype = "dashed", color = "black") +
  # theme_classic() +



  scale_color_manual(values = c("Multiple Use Zone" = "#b9e6fb",
                               "Habitat Protection Zone" = "#fff8a3",
                               "National Park Zone" = "#7bbc63",
                               "Special Purpose Zone" = "#6BB1E5",
                               "Sanctuary Zone" = "#bfd054",
                               "Other Zones" = "#bddde1"), name = "Australian Marine Parks") +

  scale_fill_manual(values = c("Multiple Use Zone" = "#b9e6fb",
                                "Habitat Protection Zone" = "#fff8a3",
                                "National Park Zone" = "#7bbc63",
                                "Special Purpose Zone" = "#6BB1E5",
                                "Sanctuary Zone" = "#bfd054",
                                "Other Zones" = "#bddde1"), name = "Australian Marine Parks") +

  labs(
    x = "Year",
    y = "> Lm large-bodied carnivores",
    color = "Australian Marine Parks"
  ) +
  labs(x = "Year", y = str_wrap(unique(test_data$metric), 40))

p

final_plot <- p +
  plot_annotation(
  title = unique(test_data$marine_park),
  subtitle = unique(test_data$metric),
  theme = theme(
    plot.title = element_text(size = 18, face = "bold"),  # Adjust title size and style
    plot.subtitle = element_text(size = 16, face = "italic")  # Adjust subtitle size and style
  ))

final_plot

# Define file names for saving
file_prefix <- paste(unique(test_data$network), unique(test_data$marine_park), unique(test_data$metric), sep = "_")

# Save the plot as RDS
saveRDS(plot, file = paste0("plots/condition/temporal/", file_prefix, ".rds"))


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

  # Create the plot
  p <- ggplot(filtered_data, aes(x = year, y = number, fill = zone, group = zone, shape = zone, col = zone)) +
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
      color = "Australian Marine Parks"
    ) +
    labs(x = "Year", y = str_wrap(unique(filtered_data$metric), 40))

  final_plot <- p +
    plot_annotation(
      title = unique(filtered_data$marine_park),
      subtitle = unique(filtered_data$metric),
      theme = theme(
        plot.title = element_text(size = 18, face = "bold"),
        plot.subtitle = element_text(size = 16, face = "italic")
      )
    )

  # Define file name
  file_prefix <- paste(
    unique(filtered_data$network),
    unique(filtered_data$marine_park),
    unique(filtered_data$metric),
    sep = "_"
  )

  # Save the plot as RDS
  saveRDS(final_plot, file = paste0("plots/temporal/", file_prefix, ".rds"))
  saveRDS(final_plot, file = paste0("inst/shiny/amp-dashboard/plots/temporal/", file_prefix, ".rds"))
}
