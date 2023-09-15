
##########


# load libraries 
library(tidyverse)

#load data frame 
#df <- read.csv("/Users/hannah/FY2/GH project/data sets/final_data_set/simplified_datav3.csv", header=T, na.strings=c("NA"))
df <- read.csv("/Users/hannah/FY2/GH project/data sets/final_data_set/all_data_WBv3.csv", header=T, na.strings=c("NA"))

###### LE at birth, total 

#andys code 
df_1990<-df%>%
 select(Country.Name,Series.Name,X1990)
 
ordered_df_1990<-df_1990|>
 pivot_wider(names_from=Series.Name,values_from=X1990)|>
 select(
 country=Country.Name,
 LE=`Life expectancy at birth, total (years)`,
 gdp=`GDP per capita (current US$)`,
 population=`Population, total`
 )|>
 drop_na(LE,gdp,population)|> 
 mutate(gdp_inequality=row_number(gdp))|>
 arrange(gdp_inequality)|>
 mutate(cumpop=cumsum(population),
   prop_cumpop=cumpop/max(cumpop))

#run a linear regression for life expectancy to give SII http://r-statistics.co/Linear-Regression.html  
scatter.smooth(x=ordered_df_1990$gdp_inequality , y=ordered_df_1990$LE, main ="gdp_inequality ~ LE") #create scatter plot
model <- lm(LE ~ gdp_inequality, data=ordered_df_1990)
print(model)  
#summary(model)

#calculate RII - multiply each country's life expectancy by it population, sum that value across all countries and divide by the total world population
LE_ppln <- mutate(ordered_df_1990, LE*population)
z <- LE_ppln %>% summarise(sum(LE * population, na.rm = TRUE))
y <- LE_ppln %>% summarise(sum(population)) 
z/y #RII 


###### IMR 

#andys code 
df_1990<-df%>%
 select(Country.Name,Series.Name,X1990)
 
ordered_df_1990<-df_1990|>
 pivot_wider(names_from=Series.Name,values_from=X1990)|>
 select(
 country=Country.Name,
 IMR=`Mortality rate, infant (per 1,000 live births)`,
 gdp=`GDP per capita (current US$)`,
 population=`Population, total`
 )|>
 drop_na(IMR,gdp,population)|> 
 mutate(gdp_inequality=row_number(gdp))|>
 arrange(gdp_inequality)|>
 mutate(cumpop=cumsum(population),
   prop_cumpop=cumpop/max(cumpop))

#run a linear regression for IMR to give SII 
scatter.smooth(x=ordered_df_1990$gdp_inequality , y=ordered_df_1990$IMR, main ="gdp_inequality ~ IMR") #create scatter plot
model <- lm(gdp_inequality ~ IMR, data=ordered_df_1990)
print(model)  
#summary(model)

#calculate RII - multiply each country's life expectancy by it population, sum that value across all countries and divide by the total world population
IMR_ppln <- mutate(ordered_df_1990, IMR*population)
z <- IMR_ppln %>% summarise(sum(IMR * population, na.rm = TRUE))
y <- IMR_ppln %>% summarise(sum(population)) 
z/y #RII 

##### life expectancy 60 F - available in years 92,97,02,07,12,17

#andys code 
df_1990<-df%>%
 select(Country.Name,Series.Name,X1990)
 
ordered_df_1990<-df_1990|>
 pivot_wider(names_from=Series.Name,values_from=X1990)|>
 select(
 country=Country.Name,
 LE=`Life expectancy at age 60, female (years)`,
 gdp=`GDP per capita (current US$)`,
 population=`Population, total`
 )|>
 drop_na(LE,gdp,population)|> 
 mutate(gdp_inequality=row_number(gdp))|>
 arrange(gdp_inequality)|>
 mutate(cumpop=cumsum(population),
   prop_cumpop=cumpop/max(cumpop))

#run a linear regression for LE to give SII 
scatter.smooth(x=ordered_df_1990$gdp_inequality , y=ordered_df_1990$LE, main ="gdp_inequality ~ LE") #create scatter plot
model <- lm(gdp_inequality ~ LE, data=ordered_df_1990)
print(model)  
#summary(model)

#calculate RII - multiply each country's life expectancy by it population, sum that value across all countries and divide by the total world population
LE_ppln <- mutate(ordered_df_1990, LE*population)
z <- LE_ppln %>% summarise(sum(LE * population, na.rm = TRUE))
y <- LE_ppln %>% summarise(sum(population)) 
z/y #RII 

#### LE age 60 M

#andys code 
df_1992<-df%>%
 select(Country.Name,Series.Name,X1992)
 
ordered_df_1992<-df_1992|>
 pivot_wider(names_from=Series.Name,values_from=X1992)|>
 select(
 country=Country.Name,
 LE=`Life expectancy at age 60, male (years)`,
 gdp=`GDP per capita (current US$)`,
 population=`Population, total`
 )|>
 drop_na(LE,gdp,population)|> 
 mutate(gdp_inequality=row_number(gdp))|>
 arrange(gdp_inequality)|>
 mutate(cumpop=cumsum(population),
   prop_cumpop=cumpop/max(cumpop))

#run a linear regression for LE to give SII 
scatter.smooth(x=ordered_df_1992$gdp_inequality , y=ordered_df_1992$LE, main ="gdp_inequality ~ LE") #create scatter plot
model <- lm(gdp_inequality ~ LE, data=ordered_df_1992)
print(model)  
#summary(model)

#calculate RII - multiply each country's life expectancy by it population, sum that value across all countries and divide by the total world population
LE_ppln <- mutate(ordered_df_1992, LE*population)
z <- LE_ppln %>% summarise(sum(LE * population, na.rm = TRUE))
y <- LE_ppln %>% summarise(sum(population)) 
z/y #RII 

#### LE birth F 

#andys code 
df_1992<-df%>%
 select(Country.Name,Series.Name,X1992)
 
ordered_df_1992<-df_1992|>
 pivot_wider(names_from=Series.Name,values_from=X1992)|>
 select(
 country=Country.Name,
 LE=`Life expectancy at birth, female (years)`,
 gdp=`GDP per capita (current US$)`,
 population=`Population, total`
 )|>
 drop_na(LE,gdp,population)|> 
 mutate(gdp_inequality=row_number(gdp))|>
 arrange(gdp_inequality)|>
 mutate(cumpop=cumsum(population),
   prop_cumpop=cumpop/max(cumpop))

#run a linear regression for LE to give SII 
scatter.smooth(x=ordered_df_1992$gdp_inequality , y=ordered_df_1992$LE, main ="gdp_inequality ~ LE") #create scatter plot
model <- lm(gdp_inequality ~ LE, data=ordered_df_1992)
print(model)  
#summary(model)

#calculate RII - multiply each country's life expectancy by it population, sum that value across all countries and divide by the total world population
LE_ppln <- mutate(ordered_df_1992, LE*population)
z <- LE_ppln %>% summarise(sum(LE * population, na.rm = TRUE))
y <- LE_ppln %>% summarise(sum(population)) 
z/y #RII 

