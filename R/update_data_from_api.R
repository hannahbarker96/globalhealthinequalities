library(jsonlite)
library(tidyverse)

codes <- read_csv("data/included_countries.csv")

read_worldbank_api <- function(indicator, series) {
  
  data_fetch <- read_json(
    glue::glue("https://api.worldbank.org/v2/country/all/indicator/{indicator}?date=1990:2022&format=json&per_page=9000"),
    simplifyVector = TRUE
  )
  
  as_tibble(data_fetch[[2]]) |>
    transmute(
      country = country$value,
      code = countryiso3code,
      year = date,
      label = indicator$value,
      value = value
    ) |> 
    mutate(variable = series) |> 
    filter(code %in% codes$`Country Code`)
  
}

gdp_pc <- read_worldbank_api("NY.GDP.PCAP.CD", "gdp_pc")

populations <- list(
  pop_female = "SP.POP.TOTL.FE.IN",
  pop_male = "SP.POP.TOTL.MA.IN",
  pop_total = "SP.POP.TOTL"
) |> 
  imap(read_worldbank_api) |> 
  list_rbind()

health_outcomes <- list(
  le_birth__female = "SP.DYN.LE00.FE.IN",
  le_birth__male = "SP.DYN.LE00.MA.IN",
  le_birth__total = "SP.DYN.LE00.IN",
  inf_mort__female = "SP.DYN.IMRT.FE.IN",
  inf_mort__male = "SP.DYN.IMRT.MA.IN",
  inf_mort__total = "SP.DYN.IMRT.IN",
  le_60__female = "SP.DYN.LE60.FE.IN",
  le_60__male = "SP.DYN.LE60.MA.IN"
) |> 
  imap(read_worldbank_api) |> 
  list_rbind()

total_pops <- populations |> 
  filter(variable == "pop_total") |> 
  select(country, code, year, pop = value)

pop_sexes <- populations |> 
  separate(variable, c("metric", "subgroup")) |> 
  filter(subgroup != "total") |> 
  select(country, code, year, subgroup, sub_pop = value) |> 
  mutate(subgroup = str_to_sentence(subgroup))

joined_health_data <- gdp_pc |> 
  select(-label) |> 
  pivot_wider(
    names_from = variable,
    values_from = value
  ) |> 
  left_join(total_pops,
    by = join_by(
      country, code, year
    )
  ) |> 
  left_join(
    health_outcomes,
    by = join_by(
      country, code, year
    )
  ) |> 
  separate(variable, c("metric", "subgroup"), sep = "__") |> 
  mutate(subgroup = str_to_sentence(subgroup))

new_full_data <- joined_health_data |> 
  filter(metric == "le_60") |> 
  left_join(pop_sexes,
            by = join_by(
              country, code, year, subgroup
            )) |> 
  group_by(country, code, year) |> 
  summarise(value = weighted.mean(value, w = sub_pop),
            .groups = "drop") |> 
  mutate(subgroup = "Total", metric = "le_60", label = "Life expectancy at age 60, total (years)") |> 
  left_join(joined_health_data |> 
              select(-c(value, metric, label, subgroup)) |> 
              unique(), 
            by = join_by(country, code, year)) |> 
bind_rows(joined_health_data) |> 
  mutate(year = as.integer(year))
  
