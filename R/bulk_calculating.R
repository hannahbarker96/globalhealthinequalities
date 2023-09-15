library(tidyverse)

# load libraries 
library(tidyverse)

#load data frame 
df <- read_csv("data/combined_data.csv")


# tidy dataframe ----------------------------------------------------------


tidy_df <-
  df |>
# df |> filter(!str_detect(`Series Name`, "60")) |> 
  pivot_longer(matches("^\\d{4}"),names_to = "year", values_to = "val", names_pattern = "(\\d{4}).*") |> 
  select(-`Country Code`, -`Series Code`) |> 
  pivot_wider(names_from = `Series Name`, values_from = val) |> 
  rename(country = `Country Name`, gdp = `GDP per capita (current US$)`, pop = `Population, total`) |> 
  pivot_longer(-c(country, year, gdp, pop), names_to = "metric") |> 
  mutate(subgroup = str_extract(metric, "male|female|total") |> replace_na("total"),
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


mod_results <- prop_pop |> 
  nest(data = -c(year, subgroup, metric)) |> 
  mutate(sii_model = map(data, ~lm(value ~ prop_midpop, data = .x)),
         mod_out = map(sii_model, broom::tidy, conf.int = TRUE)) |> 
  unnest(mod_out) |> 
  filter(term != "(Intercept)") |> 
  ungroup() |> 
  select(year, metric, subgroup, estimate, std.error, conf.low, conf.high)

mod_results |> 
  ggplot(aes(year, estimate)) +
  geom_point() +
  geom_linerange(aes(ymin = conf.low, ymax = conf.high)) +
  facet_grid(metric ~ subgroup, scales = "free_y")


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
