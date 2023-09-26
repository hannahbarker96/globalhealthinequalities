library(jsonlite)

gdp <- read_json(
  "https://api.worldbank.org/v2/country/all/indicator/NY.GDP.PCAP.CD?date=1990:2022&format=json&per_page=9000",
  simplifyVector = TRUE
)

le_birth_fe <- read_json(
  "https://api.worldbank.org/v2/country/all/indicator/SP.DYN.LE00.FE.IN?date=1990:2022&format=json&per_page=9000",
  simplifyVector = TRUE
)

le_birth_ma <- read_json(
  "https://api.worldbank.org/v2/country/all/indicator/SP.DYN.LE00.MA.IN?date=1990:2022&format=json&per_page=9000",
  simplifyVector = TRUE
)

le_birth_tot <- read_json(
  "https://api.worldbank.org/v2/country/all/indicator/SP.DYN.LE00.IN?date=1990:2022&format=json&per_page=9000",
  simplifyVector = TRUE
)

inf_mort_fe <- read_json(
  "https://api.worldbank.org/v2/country/all/indicator/SP.DYN.IMRT.FE.IN?date=1990:2022&format=json&per_page=9000",
  simplifyVector = TRUE
)

inf_mort_ma <- read_json(
  "https://api.worldbank.org/v2/country/all/indicator/SP.DYN.IMRT.MA.IN?date=1990:2022&format=json&per_page=9000",
  simplifyVector = TRUE
)

inf_mort_tot <- read_json(
  "https://api.worldbank.org/v2/country/all/indicator/SP.DYN.IMRT.IN?date=1990:2022&format=json&per_page=9000",
  simplifyVector = TRUE
)

pop_total <- read_json(
  "https://api.worldbank.org/v2/country/all/indicator/SP.DYN.IMRT.IN?date=1990:2022&format=json&per_page=9000",
  simplifyVector = TRUE
)

pop_fe <- read_json(
  "https://api.worldbank.org/v2/country/all/indicator/SP.DYN.IMRT.FE.IN?date=1990:2022&format=json&per_page=9000",
  simplifyVector = TRUE
)

pop_ma <- read_json(
  "https://api.worldbank.org/v2/country/all/indicator/SP.DYN.IMRT.MA.IN?date=1990:2022&format=json&per_page=9000",
  simplifyVector = TRUE
)

list(
  gdp,
  le_birth_fe,
  le_birth_ma,
  le_birth_tot,
  inf_mort_fe,
  inf_mort_ma,
  inf_mort_tot,
  pop_total,
  pop_fe,
  pop_ma
) |>
  map( ~ .x[[2]]) |>
  reduce(bind_rows) |>
  tibble() |>
  transmute(
    country = country$value,
    code = countryiso3code,
    year = date,
    metric = indicator$value,
    value = value
  )


gdp_input[[2]] |> 
  as_tibble() |> 
  select(country) |> 
  mutate(country = country$value) |> 
  count(country)
