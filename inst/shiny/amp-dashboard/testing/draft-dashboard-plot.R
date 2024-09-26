# Required Libraries
library(ggplot2)
library(dplyr)
library(tidyr)

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
ggplot(data, aes(x = Condition, y = Year)) +

  # Adding horizontal grid lines for each year
  geom_hline(yintercept = unique(data$Year), color = "grey80", linetype = "dashed") +

  # Adding rounded rectangles for Condition with variable alpha based on confidence
  geom_tile(aes(fill = Condition, alpha = Condition_Confidence), color = "black", width = 0.85, height = 0.85, show.legend = TRUE) +

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
       y = "Year",
       fill = "Condition",
       size = "Confidence in Condition") +

  # Theme settings
  theme_minimal() +
  theme(
    text = element_text(family = "Helvetica"),  # Choose a more infographic font
    axis.text.x = element_blank(),              # Remove x-axis text
    axis.title.x = element_blank(),             # Remove x-axis title
    axis.ticks.x = element_blank(),             # Remove ticks from the x-axis
    axis.text.y = element_text(size = 14),      # Increase size of y-axis text (Year)
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.title = element_text(hjust = 0.5),
    plot.margin = margin(10, 10, 10, 10)        # Adjust margins
  )
