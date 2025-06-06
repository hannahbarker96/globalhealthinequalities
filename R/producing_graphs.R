# Alternative method - using data from api call ---------------------------

source("R/update_data_from_api.R")

new_full_data |> 
  group_by(year) |> 
  filter(metric != "le_60") |> 
  summarise(perc_miss_gdp = sum(is.na(gdp_pc))/n(),
            perc_miss_pop = sum(is.na(pop))/n(),
            perc_miss_outcome = sum(is.na(value))/n()) |> 
  ggplot(aes(year, perc_miss_outcome)) +
  geom_col()

# Looks like 2022 has no data yet

label_vars <- c(
  le_60 = "Life expectancy at age 60",
  le_birth = "Life expectancy at birth",
  inf_mort = "Infant mortality rate*"
)


# SII calculations ---------------------------------------------------------



prop_pop <- new_full_data |> 
  group_by(year, subgroup, metric) |> 
  filter(!is.na(pop), !is.na(gdp_pc), !is.na(value)) |> 
  arrange(gdp_pc, .by_group = TRUE) |> 
  mutate(cumpop = cumsum(pop),
         midpop = (cumpop + lag(cumpop, default = 0))/2,
         prop_midpop = midpop / max(pop))


models <- prop_pop |> 
  nest(data = -c(year, subgroup, metric)) |> 
  mutate(sii_model = map(data, ~lm(value ~ prop_midpop, data = .x)),
         mod_out = map(sii_model, broom::tidy, conf.int = TRUE))

sii_results <- models |> 
  unnest(mod_out) |> 
  filter(term != "(Intercept)") |> 
  ungroup() |> 
  select(
    year,
    metric,
    subgroup,
    sii = estimate,
    sii_se = std.error,
    sii_cl = conf.low,
    sii_cu = conf.high
  ) |> 
  mutate(
    labels = label_vars[metric],
    across(c(sii, sii_cl, sii_cu), ~ if_else(metric == "inf_mort", .x * -1, .x))
    ) 

sii_results|>
  ggplot(aes(year, sii)) +
  geom_point() +
  geom_line() +
  geom_ribbon(aes(ymin = sii_cl, ymax = sii_cu), alpha = 0.2) +
  facet_grid(labels ~ subgroup, scales = "free_y") +
  theme_sphsu_light() +
  labs(caption = "*per 1,000 live births") +
  ylab("Slope Index of Inequality") +
  xlab("Year") +
  theme_sphsu_light()

ggsave("graphs/sii_ribbon.png", dpi = 400, height = 8, width = 12)

## Graphing with colour coding --------------------------------------------

sii_results |> 
  mutate(comparison = if_else(subgroup == "Total", "Total", "By sex")) |> 
  ggplot(aes(year, sii, colour = subgroup, fill = subgroup)) +
  geom_point() +
  geom_line() +
  geom_ribbon(aes(ymin = sii_cl, ymax = sii_cu), alpha = 0.2, colour = NA) +
  facet_grid(labels ~ fct_rev(comparison), scales = "free_y") +
  theme_sphsu_light() +
  labs(caption = "*per 1,000 live births") +
  ylab("Slope Index of Inequality") +
  xlab("Year") +
  theme_sphsu_light() +
  scale_colour_manual(
    "Sex",
    breaks = c("Female", "Male"),
    values = c(
      Female = sphsu_cols("Leaf", names = FALSE),
      Male = sphsu_cols("Rust", names = FALSE),
      Total = "black"
    ),
    aesthetics = c("fill", "colour")
  )

ggsave("graphs/sii_ribbon_sex_colour.png", dpi = 400, height = 8, width = 9)

# RII calculations --------------------------------------------------------

mean_vals <- prop_pop |> 
  group_by(year, metric, subgroup) |> 
  summarise(mean_value = weighted.mean(value, w = pop), .groups = "drop")

mean_vals |> 
  ggplot(aes(year, mean_value)) + 
  geom_point() + 
  facet_grid(metric ~ subgroup, scales = "free_y") 


rii_results <- sii_results |> 
  left_join(mean_vals, by = join_by(year, metric, subgroup)) |> 
  mutate(labels = label_vars[metric]) |> 
  mutate(rii = sii / mean_value,
         rii_se = sii_se / mean_value,
         # cl_rii = conf.low / mean_value,  # Test for same value
         # cu_rii = conf.high / mean_value,
         rii_cl = rii + qnorm(0.025) * rii_se,
         rii_cu = rii + qnorm(0.975) * rii_se)

rii_results |> 
  ggplot(aes(year, rii)) +
  geom_point() +
  geom_line() +
  # geom_linerange(aes(ymin = rii_cl, ymax = rii_cu)) +
  geom_ribbon(aes(ymin = rii_cl, ymax = rii_cu), alpha = 0.2) +
  facet_grid(labels ~ subgroup, scales = "free_y") +
  labs(caption = "*per 1,000 live births") +
  ylab("Relative Index of Inequality") +
  xlab("Year") +
  theme_sphsu_light()

ggsave("graphs/rii_ribbon.png", dpi = 400, height = 8, width = 12)

# Graphing with colour coding ---------------------------------------------

rii_results |> 
  mutate(comparison = if_else(subgroup == "Total", "Total", "By sex")) |> 
  ggplot(aes(year, rii, colour = subgroup, fill = subgroup)) +
  geom_point() +
  geom_line() +
  geom_ribbon(aes(ymin = rii_cl, ymax = rii_cu), alpha = 0.2, colour = NA) +
  facet_grid(labels ~ fct_rev(comparison), scales = "free_y") +
  theme_sphsu_light() +
  labs(caption = "*per 1,000 live births") +
  ylab("Slope Index of Inequality") +
  xlab("Year") +
  theme_sphsu_light() +
  scale_colour_manual(
    "Sex",
    breaks = c("Female", "Male"),
    values = c(
      Female = sphsu_cols("Leaf", names = FALSE),
      Male = sphsu_cols("Rust", names = FALSE),
      Total = "black"
    ),
    aesthetics = c("fill", "colour")
  )

ggsave("graphs/rii_ribbon_sex_colour.png", dpi = 400, height = 8, width = 9)
