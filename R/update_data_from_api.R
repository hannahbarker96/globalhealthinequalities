library(jsonlite)
library(tidyverse)

all_data <- list(
  gdp_pc = "NY.GDP.PCAP.CD",
  le_birth_fe = "SP.DYN.LE00.FE.IN",
  le_birth_ma = "SP.DYN.LE00.MA.IN",
  le_birth_tot = "SP.DYN.LE00.IN",
  inf_mort_fe = "SP.DYN.IMRT.FE.IN",
  inf_mort_ma = "SP.DYN.IMRT.MA.IN",
  inf_mort_tot = "SP.DYN.IMRT.IN",
  pop_total = "SP.DYN.IMRT.IN",
  pop_fe = "SP.DYN.IMRT.FE.IN",
  pop_ma = "SP.DYN.IMRT.MA.IN"
) |> 
  imap(function(indicator, series) {
    
    data_fetch <- read_json(
      glue::glue("https://api.worldbank.org/v2/country/all/indicator/{indicator}?date=1990:2022&format=json&per_page=9000"),
      simplifyVector = TRUE
    )
    
    as_tibble(data_fetch[[2]]) |>
      transmute(
        country = country$value,
        code = countryiso3code,
        year = date,
        metric = indicator$value,
        value = value
      ) |> 
      mutate(variable = series)
    
  }) |> 
  list_rbind()
