#figure 2 

library(tidyverse)
library(ggplot2)
library(dplyr)

df <- read.csv("data/full_wide_data.csv", header=T, na.strings=c("NA"))

# LE by GDP 

df1 <- filter(df, metric == "le_birth" | metric == "gdp_pc" | subgroup == "Total")
head(df1)

df2 <- filter(df, metric == "gdp_pc")
head(df1)

df1$X1990 
df2$X1990 

df3 <- df1 %>% separate(metric, c("le_birth", "gdp_pc"))
head(df3)

# steps: 1) load data 2) filter data into measure, GDP, Total subgroup 3) split columns so GDP an independent column  4) order by GDP 5) generate graph with x = GDP and y = measure 

df_1990 <- df %>% 
select(country, subgroup, metric,X1990)

ordered_df_1990 <- df_1990 |>
pivot_wider(names_from=metric,values_from=X1990)|>
select(country="country",
LE="le_birth",
gdp="gdp_pc",
population = "pop"
)|>
drop_na(LE,gdp,population)|>
mutate(gdp_inequality=r_number(gdp))|>
arrange(gdp_inequality)|>
mutate(cumpop=cumsum(population),
prop_cumpop=cumpop/max(cumpop))

scatter.smooth(x=ordered_df_1990$gdp_inequality , y=ordered_df_1990$LE, main ="gdp_inequality ~ LE")


# IMR by GDP 


# LE_60 by GDP 