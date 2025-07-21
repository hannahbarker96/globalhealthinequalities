# Load necessary libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(patchwork)

df <- read.csv("data/full_wide_data.csv", header=T, na.strings=c("NA"))

# Filter data for the metrics
data <- df %>% filter(metric %in% c("gdp_pc", "inf_mort", "le_birth", "le_60") 
    , subgroup %in% c("Total", "total"))

data <- data %>% select(-"label", -"subgroup")

colnames(data) <- gsub("^X", "", colnames(data))

pivoted_data <- data %>%
  pivot_longer(
    cols = "1990": "2022",
    names_to = "year",
    values_to = "value"
  )

data <- pivoted_data %>%
  pivot_wider(
    names_from = metric, # Use the values in "Metric" as column headers
    values_from = value  # Use the values in "Value" to populate the new columns
  )

data_1990_2022 <- data %>% filter(year %in% (c("1990", "2022")))

data$year <- as.numeric(data$year)

plot_median_iqr <- function(data, metric, title, y_label) {
summary_data <- data %>%
  group_by(year) %>%
  summarize(
    Median = median(!!sym(metric), na.rm = TRUE),
    Q1 = quantile(!!sym(metric), 0.25, na.rm = TRUE),
    Q3 = quantile(!!sym(metric), 0.75, na.rm = TRUE)
  )

ggplot(summary_data, aes(x = year)) +
  geom_ribbon(aes(ymin = Q1, ymax = Q3), fill = "lightblue", alpha = 0.5) +
  geom_line(aes(y = Median), color = "blue", linewidth = 1) +
  labs(title = title,
       x = "Year",
       y = y_label)
      

  }

plot_1990_2022 <- function(data, metric, title, y_label) {
ggplot(data, aes(x = gdp_pc, y = !!sym(metric), color = as.factor(year))) +
  geom_point(size = 2) +                      # Scatter points
  geom_smooth(method = "lm", se = FALSE) +   # Linear fit lines without confidence intervals
  scale_x_log10() +                          # Apply logarithmic scale to the x-axis
  labs(
    title = title,
    x = "GDP per Capita (USD)",
    y = y_label,
    color = "year"

  ) 


}

p1 <- plot_median_iqr(data, "inf_mort", "Median and IQR of Infant Mortality Rate", "Infant Mortality Rate")

p2 <- plot_1990_2022(data_1990_2022, "inf_mort", "GDP per Capita (USD) vs Infant Mortality Rate", "Infant Mortality Rate")

p3 <- plot_median_iqr(data, "le_birth", "Median and IQR of Life Expectancy at Birth", "Life Expectancy at Birth")

p4 <- plot_1990_2022(data_1990_2022, "le_birth", "GDP per Capita (USD) vs Life Expectancy at Birth", "Life Expectancy at Birth")

p5 <- plot_median_iqr(data, "le_60", "Median and IQR of Life Expectancy at 60", "Life Expectancy at 60")

p6 <- plot_1990_2022(data_1990_2022, "le_60", "GDP per Capita (USD) vs Life Expectancy at 60", "Life Expectancy at 60")

combined_plot <- (p1 + p2) / (p3 + p4) / (p5 + p6) 

ggsave("figure1.png", combined_plot)










