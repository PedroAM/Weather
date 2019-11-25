
setwd("D:/MyWork/Dropbox/Weather")
rm(list = ls())
source("weather_P2.R")
str(weather)

weather$rained <- as.factor(weather$rained)

table(weather$rained)
prop.table(table(weather$rained))

weather$rained.yesterday[2:365] <- as.character(weather$rained[1:364])
weather$rained.yesterday[1] <- "Yes"
weather$rained.yesterday <- as.factor(weather$rained.yesterday)

table(weather$rained,weather$rained.yesterday)
(181+109)/nrow(weather)

library(caTools)


weather$l.temp.hour <-  as.numeric(weather$l.temp.hour)
weather$h.temp.hour <- as.numeric(weather$h.temp.hour)
weather$gust.wind.hour <- as.numeric(weather$gust.wind.hour)

set.seed(123)
index <- sample.split(weather$rained,0.7)

train <- weather[index,]
test <- weather [!index,]

table(train$rained)
prop.table(table(train$rained))

table(train$rained,train$rained.yesterday)
(129+74)/nrow(train)

log.reg <- glm(rained ~ rained.yesterday + season + h.temp + ave.temp +
               gust.wind + dir.wind,
               data = train, family = "binomial")

summary(log.reg)

predLog <- predict(log.reg,test)
table(rained = test$rained,prediction = predLog > 0.5)
(59+29)/nrow(test)

library(rpart)
library(rpart.plot)
library(rattle)

ctree <- rpart(rained ~ rained.yesterday + month + season + l.temp + h.temp + ave.temp + ave.wind +
                     gust.wind + dir.wind + dir.wind.8 + h.temp.hour + l.temp.hour+
                     gust.wind.hour, data = train, method="class")

fancyRpartPlot(ctree)

predTree <- predict(ctree,test,type="class")
table(rained = test$rained,prediction = predTree)
(54+28)/nrow(test)

ctree$cptable

ctreeP <- prune(ctree,cp = 0.05392157)
fancyRpartPlot(ctreeP)

predTreeP <- predict(ctreeP,test,type="class")
table(rained = test$rained,prediction = predTreeP)
(48+36)/nrow(test)

library(randomForest)

set.seed(123)
rf <- randomForest(rained ~ rained.yesterday + month + season + l.temp + h.temp + ave.temp + ave.wind +
                     gust.wind + dir.wind + dir.wind.8 + h.temp.hour + l.temp.hour+
                     gust.wind.hour, data = train, importance = TRUE, ntree=1000)
plot(rf)
pred.rf <- predict(rf,test)
table(rained = test$rained,prediction = pred.rf)
(57+33)/nrow(test)
varImpPlot(rf)
importance(rf)
