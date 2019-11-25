setwd("D:/MyWork/Dropbox/Weather")
rm(list = ls())
source("weather_P2.R")

library(ggplot2)

# EXPLORING THE DEPENDENT VARIABLE

# Time series of the daily rain amount, with smoother curve

ggplot(weather, aes(date,rain)) +
  geom_point(aes(colour = rain)) +
  geom_smooth(colour = "blue", size = 1) +
  scale_colour_gradient2(low = "green", mid = "orange",high = "red", midpoint = 20) +
  scale_y_continuous(breaks = seq(0,80,20)) +
  xlab("Date") +
  ylab("Rain (mm)") +
  ggtitle("Daily rain amount")
  
# Histogram of the daily rain amount

ggplot(weather,aes(rain)) + 
  geom_histogram(binwidth = 1,colour = "blue", fill = "darkgrey") +
  scale_x_continuous(breaks = seq(0,80,5)) +
  scale_y_continuous(breaks = seq(0,225,25)) +
  xlab("Rain (mm)") +
  ylab ("Frequency (days)") +
  ggtitle("Daily rain amount distribution")
    
ggplot(subset(weather, rain >= 1),aes(rain)) + 
  geom_histogram(binwidth = 1,colour = "blue", fill = "darkgrey") +
  scale_x_continuous(breaks = seq(0,80,5)) +
  scale_y_continuous(breaks = seq(0,20,2)) +
  xlab("Rain (mm)") +
  ylab ("Frequency (days)") +
  ggtitle("Rain distribution")

# Heavily right-skewed distribution
summary(weather$rain)
# Right-skewness is still there after removing all the dry days
summary(subset(weather, rain >0)$rain)

# Formal calculation of skewness (e1071 package)

library(e1071)
skewness(weather$rain)
skewness(subset(weather, rain >0)$rain)

# Create binary outcome (rained = {Yes,No})

# Number of dry days
nrow(subset(weather, rain == 0))
# Number of days that will also be considered dry days
nrow(subset(weather, rain <1 & rain >0))

# The new binary variable is called "rained"
weather$rained <- ifelse(weather$rain >= 1, "Yes", "No")

# Dry and wet days (absolute)
table(rained = weather$rained)
# Dry and wet days (relative)
prop.table(table(rained = weather$rained))


# ASSOCIATION BETWEEN RAIN AND SEASON OF THE YEAR

# Jitter plot - Rain amount by season
ggplot(weather, aes(season,rain)) +
  geom_jitter(aes(colour=rain), position = position_jitter(width = 0.2)) +
  scale_colour_gradient2(low = "blue", mid = "red",high = "black", midpoint = 30) +
  scale_y_continuous(breaks = seq(0,80,20)) +
  xlab("Season") +
  ylab ("Rain (mm)") +
  ggtitle("Daily rain amount by season")

# Rain amount by season
# {tapply(x,y,z) applies function z to x, for each level of y}
tapply(weather$rain,weather$season,summary)


# Bar plot - dry and wet days by season (relative)
ggplot(weather,aes(season)) +
  geom_bar(aes(fill=rained), position = "fill") +
  geom_hline(aes(yintercept = prop.table(table(weather$rained))["No"]),
             colour = "blue",linetype = "dashed", size = 1) +
  annotate("text",x = 1, y = 0.65, label = "yr. w/o = 0.60", colour = "blue") +
  xlab("Season") +
  ylab ("Proportion") +
  ggtitle("Proportion of days without and with rain, by season")
  
# Dry and wet days by season (relative)
round(prop.table(table(season = weather$season, rained= weather$rained),1),2)

# SERACHING FOR CORRELATIONS 

# Create a new data frame with only the variables than can be numeric
weather.num <- weather[c("rain","l.temp","h.temp","ave.temp","ave.wind","gust.wind",
                         "l.temp.hour","h.temp.hour","gust.wind.hour")]

# Convert the following factor variables to numeric 
weather.num$l.temp.hour <- as.numeric(weather.num$l.temp.hour)
weather.num$h.temp.hour <- as.numeric(weather.num$h.temp.hour)
weather.num$gust.wind.hour <- as.numeric(weather.num$gust.wind.hour)

# Pass the entire data frame to the cor() function to create a big correlation matrix
# Pick only the first row of the matrix [1,] ==> correlation between rain and all the other variables
round(cor(weather.num),2)[1,]

# Let's split the data frame in four parts, one for each season 
weather.num.season <- split(weather.num,weather$season)
# The result is a list...
class(weather.num.season)
# ...with length 4, where...
length(weather.num.season)
# ...each element of the list is a data frame (the seasons), with nine variables
summary(weather.num.season)
# Here are the names of each of the data frames of the list
attributes(weather.num.season)

# *apply family of functions are arguably the most powerful in base R,
# but also the most difficult to master
# {sapply(x,z) applies function z to each element of x}
# First go over the elements of the list and calculate the correlation matrix (all against all)
# For each season, return only the correlation between "rain" and everything else 
sapply(weather.num.season, function (x) round(cor(x)["rain",],2))

# Not going to run it, but here is an alternative that might be easier (or not) to understand
# It actually shows the correlations for each element of the list
# lapply(weather.num.season, function (x) round(cor(x)["rain",],2))

# FACETING

# Amount of rain vs maximum speed, by season
ggplot(weather,aes(gust.wind,rain)) +
  geom_point(colour = "firebrick") +
  geom_smooth(size = 0.75, se = F) +
  facet_wrap(~season) +
  xlab("Maximum wind speed (km/h)") +
  ylab ("Rain (mm)") +
  ggtitle("Amount of rain vs. maximum wind speed, by season")
  
# Using the defaults of the quantiles function returns 4 intervals (quartiles)
quantile(weather$h.temp)
# All we need to do is define the quartiles as the breaks of the cut function and label the intervals
weather$h.temp.quant <- cut(weather$h.temp, breaks = quantile(weather$h.temp),
                            labels = c("Cool","Mild","Warm","Hot"),include.lowest = T)

# The result
table(weather$h.temp.quant)

# Occurrence of rain, by season and daily high temperature 

ggplot(weather,aes(rained,gust.wind)) +
  geom_boxplot(aes(colour=rained)) +
  facet_grid(h.temp.quant~season) +
  xlab("Occurrence of rain") +
  ylab ("Maximum wind speed (km/h)") +
  ggtitle("Occurrence of rain, by season and daily high temperature")


