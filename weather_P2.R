
# Set working directory

setwd("D:/MyWork/Dropbox/Weather")
rm(list = ls())

# Load the data set into R 

weather <- read.csv("weather_2014.csv",sep=";",stringsAsFactors=FALSE)

# Basic data checks

dim(weather)
names(weather)
head(weather)
tail(weather)
summary(weather)
str(weather)

# Check for missing values

# One way
sum(is.na(weather))

# Another way
nrow(weather)
sum(complete.cases(weather))
nrow(weather) == sum(complete.cases(weather))

# Convert character vectors to factors

# Before...
class(weather$season)
summary(weather$season)
weather$season <- factor(weather$season,
                         levels = c("Spring","Summer","Autumn","Winter"))
# After
class(weather$season)
summary(weather$season)


# Using as.factor() when the order doesn't matter or original var. is integer

weather$day <- as.factor(weather$day)
weather$month <- as.factor(weather$month)
weather$dir.wind <- as.factor(weather$dir.wind)

# A closer look at the wind direction variable

# Number of unique values
length(unique(weather$dir.wind))

# Absolute frequency
table(weather$dir.wind)

# Making it relative
rel <- round(prop.table(table(weather$dir.wind))*100,1)
rel

# Bringing some order to the table
sort(rel,decreasing = TRUE)

# Transforming wind direction variable: from 16 to 8 principal winds

# Create a copy from the original variable...
weather$dir.wind.8 <- weather$dir.wind

# ...and then simply recode some of the variables
weather$dir.wind.8 <- ifelse(weather$dir.wind %in%  c("NNE","ENE"), 
                             "NE",as.character(weather$dir.wind.8))

weather$dir.wind.8 <- ifelse(weather$dir.wind %in% c("NNW","WNW"), 
                             "NW",as.character(weather$dir.wind.8))


weather$dir.wind.8 <- ifelse(weather$dir.wind %in% c("WSW","SSW"), 
                             "SW",as.character(weather$dir.wind.8))

weather$dir.wind.8 <- ifelse(weather$dir.wind %in% c("ESE","SSE"), 
                             "SE",as.character(weather$dir.wind.8))

weather$dir.wind.8 <- factor(weather$dir.wind.8,
                         levels = c("N","NE","E","SE","S","SW","W","NW"))


length(unique(weather$dir.wind.8))
table(weather$dir.wind.8)
round(prop.table(table(weather$dir.wind.8))*100,1)
round(prop.table(table(weather$dir.wind.8,weather$season),margin = 2)*100,1)

# Create a date variable based on the day count

first.day <- "2014-01-01"
class(first.day)
first.day <- as.Date(first.day)
class(first.day)

weather$date  <- first.day + weather$day.count - 1
head(weather$day.count)
head(weather$date)

# Create a datetime variable and round to the nearest hour 

# Store date and time as POSIXlt class
l.temp.time.date <- as.POSIXlt(paste(weather$date,weather$l.temp.time))
head(l.temp.time.date)

# Round to the nearest hour
l.temp.time.date <- round(l.temp.time.date,"hours")
head(l.temp.time.date)

# What attributes are stored in the POSIXlt time variable?
attributes(l.temp.time.date)

# Extract the value of the hour attribute as a number
weather$l.temp.hour <- l.temp.time.date [["hour"]]
head(weather$l.temp.hour)

# Lastly, the integer is converted to factor
weather$l.temp.hour <- as.factor(weather$l.temp.hour)
head(weather$l.temp.hour)

# Do the same to the remaining variables
h.temp.time.date <- as.POSIXlt(paste(weather$date,weather$h.temp.time))
gust.wind.time.date <- as.POSIXlt(paste(weather$date,weather$gust.wind.time))

h.temp.time.date <- round(h.temp.time.date,"hours")
gust.wind.time.date <- round(gust.wind.time.date,"hours")

weather$h.temp.hour <- h.temp.time.date [["hour"]]
weather$gust.wind.hour <- gust.wind.time.date [["hour"]]

weather$h.temp.hour <- as.factor(weather$h.temp.hour)
weather$gust.wind.hour <-  as.factor(weather$gust.wind.hour)

# Label the months - Jan...Dec is better than 1...12

weather$month = factor(weather$month,
                       labels = c("Jan","Fev","Mar","Apr",
                                  "May","Jun","Jul","Aug","Sep",
                                  "Oct","Nov","Dec"))

# Create a binary variable is called "rained"
weather$rained <- ifelse(weather$rain >= 1, "Yes", "No")

# Creating 4 categories for the daily high temperature
weather$h.temp.quant <- cut(weather$h.temp, breaks = quantile(weather$h.temp),
                            labels = c("Cool","Mild","Warm","Hot"),include.lowest = T)

