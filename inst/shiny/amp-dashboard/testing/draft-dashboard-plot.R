# Required Libraries
library(ggplot2)
library(dplyr)
library(ggforce)
library(patchwork)
library(ggimage) # for adding icons

# Dummy Data with a missing year (2022)
data <- data.frame(
  Year = c(2020, 2021, 2023, 2024),  # 2022 is missing
  Condition = c("Good", "Poor", "Very poor", "Very good"),
  Trend = c("Improved", "Deteriorated", "Stable", "Improved"),
  Condition_Confidence = c("High", "Limited", "Very limited", "High"),
  Trend_Confidence = c("High", "Limited", "High", "Very limited")
)

# Create a separate dataframe with unique years and an index
years <- data.frame(Year = unique(data$Year)) %>%
  mutate(Index = row_number())  # Create an index column for positioning

# Convert Condition to a numeric variable for positioning
data$Condition_numeric <- as.numeric(factor(data$Condition, levels = c("Very poor", "Poor", "Good", "Very good")))

# Define arrows for Trend (Right arrow for 'Improved', Left arrow for 'Deteriorated')
data$Trend_Arrow <- ifelse(data$Trend == "Improved", "\u2192", ifelse(data$Trend == "Deteriorated", "\u2190", ""))

# Add a column for the path to the confidence icons
data$Confidence_Icon <- ifelse(data$Condition_Confidence == "High", "images/confidence_high.png",
                               ifelse(data$Condition_Confidence == "Limited", "images/confidence_limited.png",
                                      "images/confidence_very-limited.png"))

# Define alpha values based on Trend_Confidence
alpha_values <- c("High" = 1, "Limited" = 0.7, "Very limited" = 0.4)
data$Alpha <- alpha_values[data$Trend_Confidence]

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
shape_data <- bind_rows(lapply(1:nrow(data), function(i) {
  create_shape(match(data$Year[i], years$Year), data$Condition_numeric[i])  # Match index based on year
}))

# Define color schemes for Condition
condition_colors <- c("Very good" = "#8ECCB0", "Good" = "#CADF84", "Poor" = "#F8A792", "Very poor" = "#EF454D")

# Plotting with ggforce::geom_shape for rounded rectangles
plot_condition <- ggplot() +

  # Add horizontal lines from "Very poor" to "Very good" for each year
  geom_segment(aes(x = 1, xend = 4,
                   y = years$Index, yend = years$Index),
               color = "grey70", linetype = "solid", size = 1) +

  # Add short tick marks (±0.3 from each year) for each condition
  geom_segment(data = expand.grid(Condition_numeric = unique(data$Condition_numeric), Index = years$Index),
               aes(x = Condition_numeric, xend = Condition_numeric,
                   y = Index - 0.15, yend = Index + 0.15),
               color = "grey70", size = 1) +

  # Rounded rectangles for Condition with variable alpha based on confidence
  geom_shape(data = shape_data,
             aes(x = x,
                 y = y,
                 group = interaction(Year, Condition_numeric),
                 # fill = "white"
                 fill = factor(Condition_numeric, levels = 1:4, labels = c("Very poor", "Poor", "Good", "Very good"))
                 ),
             radius = unit(0.8, 'cm')) +

  # Color settings for the conditions
  scale_fill_manual(values = condition_colors) +

  # Add the condition label below each tile in uppercase
  geom_text(data = data, aes(x = Condition_numeric,
                             y = years$Index[match(Year, years$Year)] - 0.4,
                             label = toupper(Condition)),
            size = 5, color = "black", fontface = "bold") +

  # Add trend arrows for improving or deteriorating trends (horizontal)
  geom_text(data = data, aes(x = Condition_numeric,
                             y = years$Index[match(Year, years$Year)],
                             label = Trend_Arrow,
                             alpha = Alpha),  # Alpha based on Trend_Confidence
            size = 18, color = "black", hjust = 0.5, vjust = 0.3, fontface = "bold") +

  # Adding horizontal line for 'Stable' trend
  geom_segment(data = subset(data, Trend == "Stable"),
               aes(x = Condition_numeric - 0.1, xend = Condition_numeric + 0.1,
                   y = years$Index[match(Year, years$Year)],
                   yend = years$Index[match(Year, years$Year)],
                   alpha = Alpha),  # Alpha based on Trend_Confidence
               color = "black", size = 1.2) +

  # Add icons for Condition_Confidence above each tile
  geom_image(data = data, aes(x = Condition_numeric,
                              y = years$Index[match(Year, years$Year)] + 0.4,
                              image = Confidence_Icon),
             size = 0.15) +  # Adjust the size as needed

  # Axis and titles
  labs(#title = "Community Temperature Index",
       y = "Year",
       x = "Condition",
       fill = "Condition") +

  # Theme settings
  theme_minimal() +
  theme(
    text = element_text(family = "Helvetica"),  # Choose a more infographic font
    axis.text.x = element_blank(),              # Remove x-axis text
    axis.text.y = element_blank(),              # Remove y-axis text
    axis.title.x = element_blank(),             # Remove x-axis title
    axis.title.y = element_blank(),             # Remove y-axis title
    axis.ticks.x = element_blank(),             # Remove ticks from the x-axis
    # axis.text.y = element_text(size = 14),      # Increase size of y-axis text (Year)
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.title = element_text(hjust = 0.5),
    legend.position = "none",
    plot.margin = margin(10, 10, 10, 10)        # Adjust margins
  )

plot_condition

# Create a separate dataframe with unique years
years <- data.frame(Year = unique(data$Year))

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
            size = 8, fontface = "bold",
            color = ifelse(years$Year == max(years$Year), "white", "#133946")) +

  # Customize the plot appearance
  # scale_y_reverse(breaks = years$Index, labels = years$Year) +  # Reverse y-axis and label with years
  # labs(title = "Yearly Overview") +
  theme_void() +  # Remove axes and gridlines
  theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold")) #+
  #labs(title = " ")

year_legend

final_plot <- year_legend + plot_condition +
  plot_layout(widths = c(2, 10))
final_plot
