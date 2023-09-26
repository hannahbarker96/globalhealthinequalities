# load libraries 
library(tidyverse)
library(SPHSUgraphs)

#load data frame 
df <- read_csv("data/combined_data.csv")


# tidy dataframe ----------------------------------------------------------
label_vars <- c(
  le_60 = "Life expectancy at age 60",
  le_birth = "Life expectancy at birth",
  inf_mort = "Infant mortality rate*"
)


tidy_df <-
  df |>
# df |> filter(!str_detect(`Series Name`, "60")) |> 
  pivot_longer(matches("^\\d{4}"),names_to = "year", values_to = "val", names_pattern = "(\\d{4}).*") |> 
  select(-`Series Code`) |> 
  pivot_wider(names_from = `Series Name`, values_from = val) |> 
  rename(country = `Country Name`, code = `Country Code`, gdp = `GDP per capita (current US$)`, pop = `Population, total`) |> 
  pivot_longer(-c(country, code, year, gdp, pop), names_to = "metric") |> 
  mutate(subgroup = str_extract(metric, "male|female|total") |> replace_na("total") |> str_to_sentence(),
         metric = str_replace(metric, "Mortality rate,", "inf_mort") |> 
           str_replace("Life expectancy ", "le") |> 
           str_replace("at birth", "_birth ") |> 
           str_replace("at age 60", "_60 ") |> 
           str_extract("^\\w*"),
         year = as.integer(year)) 

prop_pop <- tidy_df |> 
  group_by(year, subgroup, metric) |> 
  filter(!is.na(pop), !is.na(gdp), !is.na(value)) |> 
  arrange(gdp, .by_group = TRUE) |> 
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
  select(year, metric, subgroup, sii = estimate, sii_se = std.error, sii_cl = conf.low, sii_cu = conf.high)

sii_results |> 
  mutate(labels = label_vars[metric]) |> 
  ggplot(aes(year, sii)) +
  geom_point() +
  # geom_linerange(aes(ymin = sii_cl, ymax = sii_cu)) +
  geom_ribbon(aes(ymin = sii_cl, ymax = sii_cu), alpha = 0.2) +
  facet_grid(labels ~ subgroup, scales = "free_y") +
  theme_sphsu_light() +
  labs(caption = "*per 1,000 live births") +
  ylab("Slope Index of Inequality") +
  xlab("Year") +
  theme_sphsu_light()

ggsave("graphs/sii_ribbon.png", dpi = 400, height = 8, width = 12)

mean_vals <- prop_pop |> 
  group_by(year, metric, subgroup) |> 
  summarise(mean_value = weighted.mean(value, w = pop), .groups = "drop")

sii_results |> 
  left_join(mean_vals, by = join_by(year, metric, subgroup)) |> 
  mutate(labels = label_vars[metric]) |> 
  mutate(rii = sii / mean_value,
         rii_se = sii_se / mean_value,
         # cl_rii = conf.low / mean_value,
         # cu_rii = conf.high / mean_value,
         rii_cl = rii + qnorm(0.025) * rii_se,
         rii_cu = rii + qnorm(0.975) * rii_se) |> 
  ggplot(aes(year, rii)) +
  geom_point() +
  # geom_linerange(aes(ymin = rii_cl, ymax = rii_cu)) +
  geom_ribbon(aes(ymin = rii_cl, ymax = rii_cu), alpha = 0.2) +
  facet_grid(labels ~ subgroup, scales = "free_y") +
  labs(caption = "*per 1,000 live births") +
  ylab("Relative Index of Inequality") +
  xlab("Year") +
  theme_sphsu_light()

ggsave("graphs/rii_ribbon.png", dpi = 400, height = 8, width = 12)

# Graphing distributions of variables -------------------------------------

tidy_df |> 
  filter(year %in% seq(1992, 2020, by = 5)) |> 
  ggplot(aes(gdp, value)) +
  geom_point() +
  facet_grid(metric~year)

tidy_df |> 
  filter(year == 2020 | year == 1990, metric == "inf_mort", subgroup == "total") |> 
  ggplot(aes(x = metric, y = value)) +
  geom_jitter() +
  facet_wrap(~year)
