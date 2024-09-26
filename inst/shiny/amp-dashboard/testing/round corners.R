# Required Libraries
library(ggplot2)
library(dplyr)
library(ggforce)

shape <- data.frame(
  x = c(1, 0, 0, 1),
  y = c(2019.5, 2019.5, 2020.5, 2020.5)
  )

# Expand and round
ggplot(shape, aes(x = x, y = y)) +
  geom_shape(radius = unit(1, 'cm'))

# Dummy Data
data <- data.frame(
  Year = c(2020, 2021, 2022, 2023, 2024),
  Condition = c("Good", "Poor", "Good", "Very poor", "Very good"),
  Trend = c("Improved", "Deteriorated", "Stable", "Stable", "Improved"),
  Condition_Confidence = c("High", "Limited", "Limited", "Very limited", "High"),
  Trend_Confidence = c("High", "Limited", "High", "Very limited", "High")
)

# Reordering the levels for Condition (Very good on the right)
data$Condition <- factor(data$Condition, levels = c("Very poor", "Poor", "Good", "Very good"))
data$Trend <- factor(data$Trend, levels = c("Improved", "Stable", "Deteriorated"))

# Define color schemes for Condition
condition_colors <- c("Very good" = "#00CC66", "Good" = "#99CC33", "Poor" = "#FF9933", "Very poor" = "#FF3300")

# Define arrows for Trend (Right arrow for 'Improved', Left arrow for 'Deteriorated')
data$Trend_Arrow <- ifelse(data$Trend == "Improved", "\u2192", ifelse(data$Trend == "Deteriorated", "\u2190", ""))

# Adding alpha transparency based on Condition_Confidence
data$alpha_value <- ifelse(data$Condition_Confidence == "High", 1, 
                           ifelse(data$Condition_Confidence == "Limited", 0.7, 0.5))

# Custom Shapes for each tile (rounded rectangle)
shape <- data.frame(
  x = c(0.5, 1, 0.75, 0.25, 0),   # x coordinates for custom shape
  y = c(0, 0.5, 1, 0.75, 0.25)    # y coordinates for custom shape
)

# Function to generate custom shapes for each tile
generate_shape_data <- function(condition, year) {
  # Shift x based on condition index and y based on year
  x_shift <- as.numeric(condition)
  y_shift <- year
  
  # Apply the shifts to create shape coordinates
  shape_transformed <- data.frame(
    x = shape$x + x_shift - 0.5,   # Adjust for centering on the condition
    y = shape$y * 0.9 + y_shift - 0.5 # Adjust y for centering on year
  )
  return(shape_transformed)
}

# Create plot
ggplot() +
  
  # Iterate over the data to generate custom shapes for each condition and year
  geom_shape(data = generate_shape_data("Good", 2020), aes(x = x, y = y), 
             fill = condition_colors["Good"], color = "black", radius = unit(0.3, "cm")) +
  
  geom_shape(data = generate_shape_data("Poor", 2021), aes(x = x, y = y), 
             fill = condition_colors["Poor"], color = "black", radius = unit(0.3, "cm")) +
  
  geom_shape(data = generate_shape_data("Good", 2022), aes(x = x, y = y), 
             fill = condition_colors["Good"], color = "black", radius = unit(0.3, "cm")) +
  
  geom_shape(data = generate_shape_data("Very poor", 2023), aes(x = x, y = y), 
             fill = condition_colors["Very poor"], color = "black", radius = unit(0.3, "cm")) +
  
  geom_shape(data = generate_shape_data("Very good", 2024), aes(x = x, y = y), 
             fill = condition_colors["Very good"], color = "black", radius = unit(0.3, "cm")) +
  
  # Add trend arrows
  geom_text(data = data, aes(x = as.numeric(Condition), y = Year, label = Trend_Arrow), 
            size = 8, color = "black", hjust = 0.5, vjust = -0.5) +
  
  # Add condition labels underneath the tiles (uppercase and bold, smaller size)
  geom_text(data = data, aes(x = as.numeric(Condition), y = Year - 0.6, label = toupper(Condition)), 
            size = 5, color = "black", fontface = "bold") +
  
  # Add horizontal line for 'Stable' trend
  geom_segment(data = subset(data, Trend == "Stable"),
               aes(x = as.numeric(Condition) - 0.1, xend = as.numeric(Condition) + 0.1, y = Year, yend = Year), 
               color = "black", size = 1.2) +
  
  # Set up the y-axis for years
  scale_y_continuous(breaks = seq(2020, 2024, 1)) +
  
  # Axis and titles
  labs(title = "Community Temperature Index",
       x = NULL, y = "Year") +
  
  # Theme settings
  theme_minimal() +
  theme(
    text = element_text(family = "Helvetica"),       # Choose a more infographic font
    axis.text.x = element_blank(),                  # Remove text from the x-axis
    axis.ticks.x = element_blank(),                 # Remove ticks from the x-axis
    axis.text.y = element_text(size = 16),          # Increase size of y-axis text (Year)
    panel.grid.major.y = element_line(color = "grey80"),  # Add grid line for each year
    panel.grid.major.x = element_blank(),           # Remove vertical grid lines
    panel.grid.minor = element_blank(),
    plot.title = element_text(hjust = 0.5, size = 18) # Center title
  )
