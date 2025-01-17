library(tidyverse)
library(healthequal)


source("R/update_data_from_api.R")

ranked_data <- new_full_data |> 
  filter(!is.na(value)) |>
  mutate(rank = row_number(gdp_pc), .by = c(year, metric, subgroup)) |> 
  arrange(metric, year, rank) |> 
  select(country, year, value, subgroup, metric, pop, rank, label)

out_models <- ranked_data |> 
  nest(data = -c(metric, label, year, subgroup)) |> 
  summarise(
    sii = map(data, ~(sii(.x$value, .x$rank, .x$pop, linear = metric != "inf_mort"))),
    rii = map(data, ~(rii(.x$value, .x$rank, .x$pop, linear = metric != "inf_mort"))),
    .by = c(metric, year, subgroup, label)
  ) |> 
  unnest_wider(c(sii, rii), names_sep = "_") |> 
  left_join(multiplier_vals, by = "metric") |> 
  select(-ends_with("measure")) |> 
  mutate(
    across(starts_with("sii"), ~.x * multiplier),
    across(starts_with("rii"), ~.x ^ multiplier)
  ) 


label_vars <- c(
  le_60 = "Life expectancy at age 60",
  le_birth = "Life expectancy at birth",
  inf_mort = "Infant mortality rate*"
)

multiplier_vals <- tibble::tribble(
  ~metric, ~multiplier,
  "inf_mort", -1,
  "le_60", 1,
  "le_birth", 1
)

out_models |>
  filter(year != 1990) |>
  mutate(axis_labels = label_vars[metric],
         comparison = if_else(subgroup == "Total", "Total", "By sex")) |> 
  ggplot(aes(year, sii_estimate, colour = subgroup, fill = subgroup)) +
  geom_point() +
  geom_line() +
  geom_ribbon(aes(ymin = sii_lowerci, ymax = sii_upperci), alpha = 0.2, colour = NA) +
  facet_grid(axis_labels ~ fct_rev(comparison), scales = "free_y") +
  ylab("Slope Index of Inequality") +
  xlab("Year") +
  theme_minimal() +
  scale_colour_manual(
    "Sex",
    breaks = c("Female", "Male"),
    values = c(
      Female = SPHSUgraphs::sphsu_cols("Leaf", names = FALSE),
      Male = SPHSUgraphs::sphsu_cols("Rust", names = FALSE),
      Total = "black"
    ),
    aesthetics = c("fill", "colour")
  )

out_models |>
  filter(year != 1990) |> 
  mutate(axis_labels = label_vars[metric],
         comparison = if_else(subgroup == "Total", "Total", "By sex")) |> 
  ggplot(aes(year, rii_estimate, colour = subgroup, fill = subgroup)) +
  geom_point() +
  geom_line() +
  geom_ribbon(aes(ymin = rii_lowerci, ymax = rii_upperci), alpha = 0.2, colour = NA) +
  facet_grid(axis_labels ~ fct_rev(comparison), scales = "free_y") +
  ylab("Relative Index of Inequality") +
  xlab("Year") +
  theme_minimal() +
  scale_colour_manual(
    "Sex",
    breaks = c("Female", "Male"),
    values = c(
      Female = SPHSUgraphs::sphsu_cols("Leaf", names = FALSE),
      Male = SPHSUgraphs::sphsu_cols("Rust", names = FALSE),
      Total = "black"
    ),
    aesthetics = c("fill", "colour")
  )

new_full_data |> 
  filter(year %in% c(1991, 2022), subgroup == "Total", metric == "inf_mort", !is.na(gdp_pc), !is.na(value)) |> 
  (\(df) bind_rows(
    max_tib = slice_max(df, value, n = 10, by = year) |> summarise(value = mean(value), .by = year),
    min_tib = slice_min(df, value, n = 10, by = year) |> summarise(value = mean(value), .by = year),
  ))() |> 
  arrange(year, value) |> 
  summarise(
    mort_ratio = exp(diff(log(value))), 
    mort_diff = diff(value),
    .by = year)
