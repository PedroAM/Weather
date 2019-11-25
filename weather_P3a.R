
setwd("D:/MyWork/Dropbox/Weather")
rm(list = ls())
source("weather_P2.R")

library(ggplot2)

# Time series of average daily temperature, with smoother curve

ggplot(weather,aes(x = date,y = ave.temp)) +
  geom_point(colour = "blue") +
  geom_smooth(colour = "red",size = 1) +
  scale_y_continuous(limits = c(5,30), breaks = seq(5,30,5)) +
  ggtitle ("Daily average temperature") +
  xlab("Date") +  ylab ("Average Temperature ( ºC )")

# Same but with varying colour

ggplot(weather,aes(x = date,y = ave.temp)) + 
  geom_point(aes(colour = ave.temp)) +
  scale_colour_gradient2(low = "blue",mid = "green" ,high = "red", midpoint = 16) + 
  geom_smooth(color = "red",size = 1) +
  scale_y_continuous(limits = c(5,30), breaks = seq(5,30,5)) +
  ggtitle ("Daily average temperature") +
  xlab("Date") +  ylab ("Average Temperature ( ºC )")
       

# Distribution of the average temperature by season - density plot

ggplot(weather,aes(x = ave.temp, colour = season)) +
  geom_density() +
  scale_x_continuous(limits = c(5,30), breaks = seq(5,30,5)) +
  ggtitle ("Temperature distribution by season") +
  xlab("Average temperature ( ºC )") +  ylab ("Probability")


# Label the months - Jan...Dec is better than 1...12

weather$month = factor(weather$month,
                       labels = c("Jan","Fev","Mar","Apr",
                                  "May","Jun","Jul","Aug","Sep",
                                  "Oct","Nov","Dec"))

# Distribution of the average temperature by month - violin plot,
# with a jittered point layer on top, with size mapped to amount of rain

ggplot(weather,aes(x = month, y = ave.temp)) +
  geom_violin(fill = "orange") +
  geom_point(aes(size = rain), colour = "blue", position = "jitter") +
  ggtitle ("Temperature distribution by month") +
  xlab("Month") +  ylab ("Average temperature ( ºC )")

# Scatter plot of low vs high daily temperatures, with a smoother curve for each season

ggplot(weather,aes(x = l.temp, y = h.temp)) +
  geom_point(colour = "firebrick", alpha = 0.3) + 
  geom_smooth(aes(colour = season),se= F, size = 1.1) +
  ggtitle ("Daily low and high temperatures") +
  xlab("Daily low temperature ( ºC )") +  ylab ("Daily high temperature ( ºC )")


# We will use the melt function from reshape2 to convert from wide to long form
library(reshape2)

# select only the variables that are needed (day and temperatures) and assign to a new data frame
temperatures <- weather[c("day.count","h.temp.hour","l.temp.hour")]

# Temperature variables in columns
head(temperatures)
dim(temperatures)

# The temperatures are molten into a single variable called l.h.temp 
temperatures <- melt(temperatures,id.vars = "day.count",
                     variable.name = "l.h.temp", value.name = "hour")

# See the difference?
head(temperatures)
tail(temperatures)

# We now have twice the rows. Each day has an observation for low and high temperatures
dim(temperatures)

# Needed to force the order of the factor's level
temperatures$hour <- factor(temperatures$hour,levels=0:23)

# Now we can just fill the colour by the grouping variable to visualise the two distributions 
ggplot(temperatures) +
  geom_bar(aes(x = hour, fill=l.h.temp)) +
  scale_fill_discrete(name="", labels= c("Daily high","Daily low")) +
  scale_y_continuous(limits = c(0,100)) +
  ggtitle ("Low and high temperatures - time of the day") +
  xlab("Hour") +  ylab ("Frequency")
  
