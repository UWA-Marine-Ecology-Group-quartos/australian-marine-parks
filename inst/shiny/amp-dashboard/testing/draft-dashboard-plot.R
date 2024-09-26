# Required Libraries
devtools::install_github("hrbrmstr/ggchicklet")

library(ggplot2)
library(dplyr)
library(tidyr)
library(ggforce)
library(ggchicklet)

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

# Define alpha values based on Condition_Confidence
alpha_values <- c("High" = 1, "Limited" = 0.7, "Very limited" = 0.4)

# Plotting with rounded squares, trend arrows, and confidence-dependent fills
# Plotting with rounded rectangles, trend arrows, and confidence-dependent fills
ggplot(data, aes(x = Condition, y = Year)) +
  
  # Adding horizontal grid lines for each year
  geom_hline(yintercept = unique(data$Year), color = "grey80"#, linetype = "solid"
             ) +
  
  # Adding rounded rectangles for Condition with variable alpha based on confidence
  geom_shape(aes(x = as.numeric(Condition), 
                 y = Year, 
                 fill = Condition, 
                 alpha = Condition_Confidence), 
             shape = "roundrect", radius = unit(0.2, "cm"), 
             width = 0.85, height = 0.85, show.legend = TRUE) +
  
  # Alpha values based on Condition_Confidence
  scale_alpha_manual(values = alpha_values, guide = guide_legend(title = "Confidence in Condition")) +
  
  # Adding trend arrows for improving or deteriorating trends (horizontal)
  geom_text(aes(label = Trend_Arrow), size = 10, color = "black", hjust = 0.5, vjust = 0.1) +  # Increased size for bolder arrows
  
  # Adding condition labels underneath the boxes in all capitals
  geom_text(aes(label = toupper(Condition)), vjust = 3, size = 5, color = "black", fontface = "bold") +
  
  # Adding horizontal line for 'Stable' trend
  geom_segment(data = subset(data, Trend == "Stable"),
               aes(x = as.numeric(Condition) - 0.1, xend = as.numeric(Condition) + 0.1, y = Year * 1.00005, yend = Year * 1.00005), 
               color = "black", size = 1.2) +
  
  # Color settings for the conditions
  scale_fill_manual(values = condition_colors) +
  
  # Axis and titles
  labs(title = "Community Temperature Index",
       # y = "Year",
       fill = "Condition",
       size = "Confidence in Condition") +
  
  # Theme settings
  theme_minimal() +
  theme(
    text = element_text(family = "Helvetica"),  # Choose a more infographic font
    axis.text.x = element_blank(),              # Remove x-axis text
    axis.title.x = element_blank(),             # Remove x-axis title
    axis.title.y = element_blank(),             # Remove y-axis title
    axis.ticks.x = element_blank(),             # Remove ticks from the x-axis
    axis.text.y = element_text(size = 14),      # Increase size of y-axis text (Year)
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.title = element_text(hjust = 0.5),
    plot.margin = margin(10, 10, 10, 10)        # Adjust margins
  )

# Required Libraries
library(ggplot2)
library(dplyr)
library(tidyr)
library(ggchicklet)

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

# Plotting with rounded rectangles (chicklet), trend arrows, and confidence-dependent transparency
ggplot(data, aes(x = Condition, fill = Condition, alpha = alpha_value)) +
  
  # Centering the rectangles around the year by adjusting ymin and ymax
  ggchicklet::geom_chicklet(aes(ymin = Year - 0.4, ymax = Year + 0.4), 
                            width = 0.9, radius = grid::unit(8, "pt"), color = "black", show.legend = TRUE) +
  
  ggchicklet:::geom_rrect(
    aes(
      # xmin = store_lower, 
      # xmax = store_upper, 
      ymin = Year - 0.4, 
      ymax = Year + 0.4,
      # fill = rect_color,
      # alpha = rect_alpha
    ),
    # Use relative npc unit (values between 0 and 1)
    # This ensures that radius is not too large for your canvas
    r = unit(0.5, 'npc')
  ) +
  
  # Adding trend arrows for improving or deteriorating trends (horizontal)
  geom_text(aes(label = Trend_Arrow, y = Year), size = 8, color = "black", hjust = 0.5, vjust = 0.1) +
  
  # Adding condition labels underneath the tiles (uppercase and bold, smaller size)
  geom_text(aes(label = toupper(Condition), y = Year - 0.5), size = 4.5, color = "black", fontface = "bold") +
  
  # Adding horizontal line for 'Stable' trend
  geom_segment(data = subset(data, Trend == "Stable"),
               aes(x = as.numeric(Condition) - 0.1, xend = as.numeric(Condition) + 0.1, y = Year, yend = Year), 
               color = "black", size = 1.2) +
  
  # Color settings for the conditions
  scale_fill_manual(values = condition_colors) +
  
  # Alpha settings for confidence levels
  scale_alpha_identity(guide = "legend", name = "Confidence in Condition") +
  
  # Fixing the y-axis to show proper year labels
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


# Required Libraries
library(ggplot2)
library(dplyr)
library(ggforce)

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

# Plotting with rounded rectangles (geom_shape), trend arrows, and confidence-dependent transparency
ggplot(data, aes(x = Condition, fill = Condition, alpha = alpha_value)) +
  
  # Using geom_shape to create rounded rectangles centered on the year
  ggforce::geom_shape(aes(y = Year - 0.4, y = Year + 0.4, xmin = as.numeric(Condition) - 0.45, xmax = as.numeric(Condition) + 0.45), 
                      radius = grid::unit(10, "pt"), color = "black", show.legend = TRUE) +
  
  # Adding trend arrows for improving or deteriorating trends (horizontal)
  geom_text(aes(label = Trend_Arrow, y = Year), size = 8, color = "black", hjust = 0.5, vjust = 0.1) +
  
  # Adding condition labels underneath the tiles (uppercase and bold, smaller size)
  geom_text(aes(label = toupper(Condition), y = Year - 0.6), size = 5, color = "black", fontface = "bold") +
  
  # Adding horizontal line for 'Stable' trend
  geom_segment(data = subset(data, Trend == "Stable"),
               aes(x = as.numeric(Condition) - 0.1, xend = as.numeric(Condition) + 0.1, y = Year, yend = Year), 
               color = "black", size = 1.2) +
  
  # Color settings for the conditions
  scale_fill_manual(values = condition_colors) +
  
  # Alpha settings for confidence levels
  scale_alpha_identity(guide = "legend", name = "Confidence in Condition") +
  
  # Fixing the y-axis to show proper year labels
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
