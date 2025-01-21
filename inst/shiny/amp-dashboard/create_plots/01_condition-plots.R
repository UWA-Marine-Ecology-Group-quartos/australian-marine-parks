# Required Libraries
library(ggplot2)
library(dplyr)
library(ggforce)
library(patchwork)
library(ggimage) # for adding icons
library(stringr)

# Define the function
create_plots <- function(data, output_dir = "plots/") {

  # # Ensure the output directory exists
  # if (!dir.exists(output_dir)) {
  #   dir.create(output_dir, recursive = TRUE)
  # }

  # Define alpha values based on Trend_Confidence
  alpha_values <- c("High" = 1, "Limited" = 0.7, "Very limited" = 0.4)

  # Define color schemes for Condition
  condition_colors <- c("Very good" = "#8ECCB0", "Good" = "#CADF84", "Poor" = "#F8A792", "Very poor" = "#EF454D")

  # Function to create the plot for each unique combination
  create_single_plot <- function(sub_data) {

    # Create a separate dataframe with unique years and an index for positioning
    years <- data.frame(Year = unique(sub_data$year)) %>%
      mutate(Index = row_number())  # Create an index column

    # Convert Condition to a numeric variable for positioning
    sub_data$Condition_numeric <- as.numeric(factor(sub_data$condition, levels = c("Very poor", "Poor", "Good", "Very good")))

    # Define arrows for Trend (Right arrow for 'Improved', Left arrow for 'Deteriorated')
    sub_data$Trend_Arrow <- ifelse(sub_data$trend == "Improved", "\u2192", ifelse(sub_data$trend == "Deteriorated", "\u2190", ""))

    # Add a column for the path to the confidence icons
    sub_data$Confidence_Icon <- ifelse(sub_data$confidence_condition == "High", "images/confidence_high.png",
                                       ifelse(sub_data$confidence_condition == "Limited", "images/confidence_limited.png",
                                              "images/confidence_very-limited.png"))

    # Define alpha values based on Trend_Confidence
    sub_data$Alpha <- alpha_values[sub_data$confidence_trend]

    # Function to create the shape corners using the year index
    create_shape <- function(index, condition_numeric) {
      data.frame(
        x = c(condition_numeric - 0.3, condition_numeric + 0.3, condition_numeric + 0.3, condition_numeric - 0.3),
        y = c(index - 0.2, index - 0.2, index + 0.2, index + 0.2),  # Use index for y
        Year = years$Year[index],
        Condition_numeric = condition_numeric
      )
    }

    # Apply the function to create shape data for each row in the original dataframe
    shape_data <- bind_rows(lapply(1:nrow(sub_data), function(i) {
      create_shape(match(sub_data$year[i], years$Year), sub_data$Condition_numeric[i])  # Match index based on year
    }))

    # Plotting with ggforce::geom_shape for rounded rectangles
    plot_condition <- ggplot() +

      # Add horizontal lines from "Very poor" to "Very good" for each year
      geom_segment(aes(x = 1, xend = 4,
                       y = years$Index, yend = years$Index),
                   color = "grey70", linetype = "solid", size = 1) +

      # Add short tick marks (±0.3 from each year) for each condition
      # geom_segment(data = expand.grid(Condition_numeric = unique(sub_data$Condition_numeric), Index = years$Index),
      #              aes(x = Condition_numeric, xend = Condition_numeric,
      #                  y = Index - 0.15, yend = Index + 0.15),
      #              color = "grey70", size = 1) +

      # Add short vertical tick marks for every condition at x = 1, 2, 3, and 4 (fixed positions)
      geom_segment(
        data = expand.grid(Condition_numeric = 1:4, Index = years$Index),  # Fixed conditions (1:4)
        aes(x = Condition_numeric, xend = Condition_numeric,
            y = Index - 0.15, yend = Index + 0.15),  # ±0.15 from each index
        color = "grey70", size = 1
      ) +

      # Rounded rectangles for Condition with variable alpha based on confidence
      geom_shape(data = shape_data,
                 aes(x = x,
                     y = y,
                     group = interaction(Year, Condition_numeric),
                     fill = factor(Condition_numeric, levels = 1:4, labels = c("Very poor", "Poor", "Good", "Very good"))
                 ),
                 radius = unit(0.8, 'cm')) +

      # Color settings for the conditions
      scale_fill_manual(values = condition_colors) +

      # Add the condition label below each tile in uppercase
      geom_text(data = sub_data, aes(x = Condition_numeric,
                                     y = years$Index[match(year, years$Year)] - 0.3,
                                     label = toupper(condition)),
                size = 8, color = "black", fontface = "bold") +

      # Add trend arrows for improving or deteriorating trends (horizontal)
      geom_text(data = sub_data, aes(x = Condition_numeric,
                                     y = years$Index[match(year, years$Year)],
                                     label = Trend_Arrow,
                                     alpha = Alpha),  # Alpha based on Trend_Confidence
                size = 16, color = "black", hjust = 0.5, vjust = 0.3, fontface = "bold") +

      # Adding horizontal line for 'Stable' trend
      geom_segment(data = subset(sub_data, trend == "Stable"),
                   aes(x = Condition_numeric - 0.1, xend = Condition_numeric + 0.1,
                       y = years$Index[match(year, years$Year)],
                       yend = years$Index[match(year, years$Year)],
                       alpha = Alpha),  # Alpha based on Trend_Confidence
                   color = "black", size = 1.2) +

      # Add icons for Condition_Confidence above each tile
      geom_image(data = sub_data, aes(x = Condition_numeric,
                                      y = years$Index[match(year, years$Year)] + 0.3,
                                      image = Confidence_Icon),
                 size = 0.15) +  # Adjust the size as needed

      # Theme settings
      theme_minimal() +
      theme(
        text = element_text(family = "Helvetica"),  # Choose a more infographic font
        axis.text.x = element_blank(),              # Remove x-axis text
        axis.text.y = element_blank(),              # Remove y-axis text
        axis.title.x = element_blank(),             # Remove x-axis title
        axis.title.y = element_blank(),             # Remove y-axis title
        axis.ticks.x = element_blank(),             # Remove ticks from the x-axis
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "none",
        plot.margin = margin(10, 10, 10, 10)        # Adjust margins
      )

    # Create a separate dataframe with unique years
    years <- data.frame(Year = unique(sub_data$year))

    # Create an index for each year for positioning
    years <- years %>%
      mutate(Index = row_number())  # Create an index column

    # Function to create the shape corners for the year rectangles
    create_shape_year <- function(year, index) {
      data.frame(
        x = c(1 - 0.3, 1 + 0.3, 1 + 0.3, 1 - 0.3),
        y = c(index - 0.3, index - 0.3, index + 0.3, index + 0.3),
        Year = year
      )
    }

    # Apply the function to the years dataframe to create shape data for each year
    shape_data_years <- bind_rows(lapply(1:nrow(years), function(i) {
      create_shape_year(years$Year[i], years$Index[i])
    }))

    # Create the plot for stacked rectangles with a vertical segment
    year_legend <- ggplot() +
      # Add vertical segment connecting the rectangles
      geom_segment(aes(x = 1, xend = 1, y = min(years$Index), yend = max(years$Index)),
                   color = "lightgrey", size = 1) +

      # Rounded rectangles for each year
      geom_shape(data = shape_data_years, aes(x = x, y = y, group = Year),
                 fill = ifelse(shape_data_years$Year == max(years$Year), "#133946", "white"),
                 color = "black", radius = unit(0.5, 'cm')) +

      # Add bold text in the middle of each rectangle for the year
      geom_text(data = years, aes(x = 1, y = Index, label = Year),
                size = 6, fontface = "bold",
                color = ifelse(years$Year == max(years$Year), "white", "#133946")) +

      theme_void()  # Remove axes and gridlines

    # Create a dynamic title using combination$network, combination$marine_park_or_area, and combination$metric
    dynamic_title <- paste0(#combination$network,
                            #" Network: ",
                            combination$marine_park_or_area#,
                            #" (",
                            #combination$depth_m, " m)"
                            )

    # Combine year legend and condition plot
    final_plot <- year_legend + plot_condition +
      plot_layout(widths = c(2, 10))+
      plot_annotation(
        # title = dynamic_title,
        # subtitle = combination$metric,
        theme = theme(
          plot.title = element_text(size = 18, face = "bold"),  # Adjust title size and style
          plot.subtitle = element_text(size = 16, face = "italic")  # Adjust subtitle size and style
        )
      )

    return(final_plot)
  }

  # Loop through each unique combination of network, marine_park_or_area, and metric
  unique_combinations <- data %>%
    distinct(network, marine_park_or_area, ecosystem_condition
             #, depth_m
             )

  for (i in 1:nrow(unique_combinations)) {
    combination <- unique_combinations[i, ]

    # Filter data for the current combination
    sub_data <- data %>%
      filter(network == combination$network,
             marine_park_or_area == combination$marine_park_or_area,
             ecosystem_condition == combination$ecosystem_condition#,
             #depth_m == combination$depth_m
             ) %>% glimpse()

    num_years <- length(unique(sub_data$year))

    # Create the plot for the current combination
    plot <- create_single_plot(sub_data)

    # Adjust plot height based on the number of years in the data
    plot_height <- 2 + nrow(sub_data) * 0.5

    # Define file names for saving
    file_prefix <- paste(combination$network, combination$marine_park_or_area, combination$ecosystem_condition, num_years
                         #, combination$depth_m
                         , sep = "_")

    tidy_ecosystem <- str_replace_all(str_to_lower(unique(sub_data$ecosystem_condition)), " ", "_")

    # Save the plot as RDS
    saveRDS(plot, file = paste0(output_dir, "/", tidy_ecosystem, "/", file_prefix, ".rds"))

    # Save the plot as PNG
    ggsave(filename = paste0(output_dir, "/", tidy_ecosystem, "/", file_prefix, ".png"),
           plot = plot, width = 10, height = plot_height, units = "in", dpi = 300)

    # Save the plot as RDS
    saveRDS(plot, file = paste0("plots/condition", tidy_ecosystem, "/", file_prefix, ".rds"))

    # Save the plot as PNG
    ggsave(filename = paste0("plots/condition", tidy_ecosystem, "/", file_prefix, ".png"),
           plot = plot, width = 10, height = plot_height, units = "in", dpi = 300)
  }
}

# Example usage:
# create_plots(my_data)

library(googlesheets4)

data <- read_sheet("https://docs.google.com/spreadsheets/d/1Iplohv6mM-CnpE6uYBi4uQnuhCyZMNpCRMSJFFnJxjM/edit?usp=sharing",
                   sheet = "simplified_dummy_data")

create_plots(data, output_dir = "inst/shiny/amp-dashboard/plots/condition/")
