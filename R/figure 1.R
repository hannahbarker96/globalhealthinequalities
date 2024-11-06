# Load necessary libraries
library(dplyr)
library(tidyr)
library(ggplot2)

df <- read.csv("data/full_wide_data.csv", header=T, na.strings=c("NA"))


# Filter data for the metric "le_60"
le_60_data <- df %>% filter(metric == "le_60")

# Reshape data to long format
le_60_long <- le_60_data %>%
  pivot_longer(cols = starts_with("X"), names_to = "Year", values_to = "Value") %>%
  mutate(Year = as.numeric(gsub("X", "", Year)))

# Calculate median and IQR for each year
summary_stats <- le_60_long %>%
  group_by(Year) %>%
  summarise(
    Median = median(Value, na.rm = TRUE),
    Q1 = quantile(Value, 0.25, na.rm = TRUE),
    Q3 = quantile(Value, 0.75, na.rm = TRUE)
  )

print(summary_stats)

# Plot median and IQR
p <- ggplot(summary_stats, aes(x = Year)) +
  geom_ribbon(aes(ymin = Q1, ymax = Q3), fill = "lightblue", alpha = 0.5) +
  geom_line(aes(y = Median), color = "blue", linewidth = 1) +
  labs(title = "Median and Interquartile Range for Life Expectancy aged 60",
       x = "Year",
       y = "Value") +
  theme_minimal()

print(p)

#### LE birth

leb_data <- df %>% filter(metric == "le_birth")

# Reshape data to long format
leb_long <- leb_data %>%
  pivot_longer(cols = starts_with("X"), names_to = "Year", values_to = "Value") %>%
  mutate(Year = as.numeric(gsub("X", "", Year)))

# Calculate median and IQR for each year
summary_stats <- leb_long %>%
  group_by(Year) %>%
  summarise(
    Median = median(Value, na.rm = TRUE),
    Q1 = quantile(Value, 0.25, na.rm = TRUE),
    Q3 = quantile(Value, 0.75, na.rm = TRUE)
  )

print(summary_stats)

# Plot median and IQR
p <- ggplot(summary_stats, aes(x = Year)) +
  geom_ribbon(aes(ymin = Q1, ymax = Q3), fill = "lightblue", alpha = 0.5) +
  geom_line(aes(y = Median), color = "blue", linewidth = 1) +
  labs(title = "Median and Interquartile Range for Life Expectancy at Birth",
       x = "Year",
       y = "Value") +
  theme_minimal()

print(p)

#### IMR 
imr_data <- df %>% filter(metric == "inf_mort")

# Reshape data to long format
imr_long <- imr_data %>%
  pivot_longer(cols = starts_with("X"), names_to = "Year", values_to = "Value") %>%
  mutate(Year = as.numeric(gsub("X", "", Year)))

# Calculate median and IQR for each year
summary_stats <- imr_long %>%
  group_by(Year) %>%
  summarise(
    Median = median(Value, na.rm = TRUE),
    Q1 = quantile(Value, 0.25, na.rm = TRUE),
    Q3 = quantile(Value, 0.75, na.rm = TRUE)
  )

print(summary_stats)

# Plot median and IQR
p <- ggplot(summary_stats, aes(x = Year)) +
  geom_ribbon(aes(ymin = Q1, ymax = Q3), fill = "lightblue", alpha = 0.5) +
  geom_line(aes(y = Median), color = "blue", linewidth = 1) +
  labs(title = "Median and Interquartile Range for Infant Mortality Rate",
       x = "Year",
       y = "Value") +
  theme_minimal()

print(p)