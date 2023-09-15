Data visualisation - http://r-statistics.co/ggplot2-Tutorial-With-R.html / https://github.com/rstudio/cheatsheets/blob/main/data-visualization.pdf 

# load libraries
library(ggplot2)

# load data frame
df <- read.csv("/Users/hannah/FY2/GH project/data sets/final_data_set/SII and RII.csv", header=T, na.strings=c("NA"))

head(df)

#create visualisation for SII for LE at birth 
ggplot(data=df, aes(x=Year, y=SII.LE)) + 
geom_point() + 
geom_smooth() + 
labs(title = "Slope index of inequality in life expectancy at birth (years)", x = "Year", y= "Slope Index of Inequality")
ggsave("Slope index of inequality in life expectancy at birth (years) between 1990 and 2020.png", width=5, height=5)

#create visualisation for RII for LE at birth 
ggplot(data=df, aes(x=Year, y=RII)) + 
geom_point() + 
geom_smooth() + 
labs(title = "Relative index of inequality in life expectancy at birth (years)", x = "Year", y= "Relative Index of Inequality")
ggsave("Relative index of inequality in life expectancy at birth (years) between 1990 and 2020.png", width=5, height=5)

# SII and RII for LE at birth on one graph 
ggplot(data=df) +
	geom_point(aes(x=Year, y=SII.LE, col="SII.LE")) +
	geom_smooth(aes(x=Year, y=SII.LE, col="SII.LE")) +
	geom_point(aes(x=Year, y=RII, col="RII")) +
	geom_smooth(aes(x=Year, y=RII, col="RII"))
	scale_color_discrete(name="Legend") +
	labs(title = "SII and RII")
	
ggsave("", width=5, height=5)

#create visualisation for SII for IMR 
ggplot(data=df, aes(x=Year, y=SII.IMR)) + 
geom_point() + 
geom_smooth() + 
labs(title = "Slope index of inequality in infant mortality rate", x = "Year", y= "Slope Index of Inequality")
ggsave("Slope index of inequality in infant mortality rate.png", width=5, height=5)

#create visualisation for RII for IMR 
ggplot(data=df, aes(x=Year, y=RII.IMR)) + 
geom_point() + 
geom_smooth() + 
labs(title = "Relative index of inequality in infant mortality rate", x = "Year", y= "Relative Index of Inequality")
ggsave("Relative index of inequality in infant mortality rate between 1990 and 2020.png", width=5, height=5)

#create visualisation for SII for Life expectancy at age 60 (females)
ggplot(data=df, aes(x=Year, y=SII.LE_60_F)) + 
geom_point() + 
geom_smooth() + 
labs(title = "Slope index of inequality in life expectancy at age 60 (female)", x = "Year", y= "Slope Index of Inequality")
ggsave("Slope index of inequality in life expectancy at age 60 (female) between 1990 and 2020.png", width=5, height=5)

#create visualisation for RII for Life expectancy at age 60 (females)
ggplot(data=df, aes(x=Year, y=RII.LE_60_f)) + 
geom_point() + 
geom_smooth() + 
labs(title = "Relative index of inequality in life expectancy at age 60 (female)", x = "Year", y= "Relative Index of Inequality")
ggsave("Relative index of inequality in life expectancy at age 60 (female) between 1990 and 2020.png", width=5, height=5)

#create visualisation for SII for Life expectancy at age 60 (males)
ggplot(data=df, aes(x=Year, y=SII.LE_60_M)) + 
geom_point() + 
geom_smooth() + 
labs(title = "Slope index of inequality in life expectancy at age 60 (male)", x = "Year", y= "Slope Index of Inequality")
ggsave("Slope index of inequality in life expectancy at age 60 (male) between 1990 and 2020.png", width=5, height=5)

#create visualisation for RII for Life expectancy at age 60 (males)
ggplot(data=df, aes(x=Year, y=RII.LE_60.M)) + 
geom_point() + 
geom_smooth() + 
labs(title = "Relative index of inequality in life expectancy at age 60 (male)", x = "Year", y= "Relative Index of Inequality")
ggsave("Relative index of inequality in life expectancy at age 60 (male) between 1990 and 2020.png", width=5, height=5)

# SII for LE aged 60 both gender on one graph 
ggplot(data=df) +
	geom_point(aes(x=Year, y=SII.LE_60_F, col="SII.LE_60_F")) +
	geom_smooth(aes(x=Year, y=SII.LE_60_F, col="SII.LE_60_F")) +
	geom_point(aes(x=Year, y=SII.LE_60_M), col="SII.LE_60_M")) +
	geom_smooth(aes(x=Year, y=SII.LE_60_M, col="SII.LE_60_M")) +
	scale_color_discrete(name="Legend") +
	labs(title = "SII Life Expectancy aged 60")
ggsave("SII Life Expectancy aged 60.png", width=5, height=5)

#create visualisation for SII for Life expectancy at birth (females)
ggplot(data=df, aes(x=Year, y=SII.LE_F)) + 
geom_point() + 
geom_smooth() + 
labs(title = "Slope index of inequality in life expectancy at birth (female)", x = "Year", y= "Slope Index of Inequality")
ggsave("Slope index of inequality in life expectancy at birth (female) between 1990 and 2020.png", width=5, height=5)

#create visualisation for RII for Life expectancy at birth (females)
ggplot(data=df, aes(x=Year, y=RII.LE_F)) + 
geom_point() + 
geom_smooth() + 
labs(title = "Relative index of inequality in life expectancy at birth (female)", x = "Year", y= "Relative Index of Inequality")
ggsave("Relative index of inequality in life expectancy at birth (female).png", width=5, height=5)

#create visualisation for SII for Life expectancy at birth (males)
ggplot(data=df, aes(x=Year, y=SII.LE_M)) + 
geom_point() + 
geom_smooth() + 
labs(title = "Slope index of inequality in life expectancy at birth (male)", x = "Year", y= "Slope Index of Inequality")
ggsave("Slope index of inequality in life expectancy at birth (male).png", width=5, height=5)

#create visualisation for RII for Life expectancy at birth (males)
ggplot(data=df, aes(x=Year, y=RII.LE_M)) + 
geom_point() + 
geom_smooth() + 
labs(title = "Relative index of inequality in life expectancy at birth (male)", x = "Year", y= "Relative Index of Inequality")
ggsave("Relative index of inequality in life expectancy at birth (male).png", width=5, height=5)

# SII for LE at birth both gender on one graph 
ggplot(data=df) +
	geom_point(aes(x=Year, y=SII.LE_F, col="SII.LE_F")) +
	geom_smooth(aes(x=Year, y=SII.LE_F, col="SII.LE_F")) +
	geom_point(aes(x=Year, y=SII.LE_M), col="SII.LE_M")) +
	geom_smooth(aes(x=Year, y=SII.LE_M, col="SII.LE_M")) 
	scale_color_discrete(name="Legend") 
	labs(title = "SII Life Expectancy at birth")
ggsave("SII Life Expectancy at birth.png", width=5, height=5)
