
setwd("D:/MyWork/Dropbox/Weather")
rm(list = ls())
source("weather_P2.R")
str(weather)

# For reproducibility; 123 has no particular meaning
set.seed(123)

# randomly pick 70% of the number of observations (365)
index <- sample(1:nrow(weather),size = 0.7*nrow(weather))

# subset weather to include only the elements in the index
train <- weather[index,]

# subset weather to include all but the elements in the index
test <- weather [-index,]

nrow(train)
nrow(test)

library(ggplot2)

# Create a dataframe with train and test indicator...
group <- rep(NA,365)
group <- ifelse(seq(1,365) %in% index,"Train","Test")
df <- data.frame(date=weather$date,rain=weather$rain,group)

# ...and plot it
ggplot(df,aes(x = date,y = rain, color = group)) + geom_point() +
  scale_color_discrete(name="") + theme(legend.position="top")


# Baseline model - predict the mean of the training data
best.guess <- mean(train$rain)

# Evaluate RMSE and MAE on the testing data 
RMSE.baseline <- sqrt(mean((best.guess-test$rain)^2))
RMSE.baseline

MAE.baseline <- mean(abs(best.guess-test$rain))
MAE.baseline

# Create a multiple (log)linear regression model using the training data
lin.reg <- lm(log(rain+1) ~ season +  h.temp + ave.temp + ave.wind + gust.wind +
              dir.wind + as.numeric(gust.wind.hour), data = train)

# Inspect the model
summary(lin.reg)

# What is the effect of the wind variable?
exp(lin.reg$coefficients["gust.wind"])

# Apply the model to the testing data (i.e., make predictions) ...
# (Don't forget to exponentiate the results to revert the log transformation)
test.pred.lin <- exp(predict(lin.reg,test))-1

# ...and evaluate the accuracy
RMSE.lin.reg <- sqrt(mean((test.pred.lin-test$rain)^2))
RMSE.lin.reg

MAE.lin.reg <- mean(abs(test.pred.lin-test$rain))
MAE.lin.reg

# Regression tree

# Needed to grow a tree
library(rpart)
# To draw a beautiful tree
library(rattle)

# rpart function applied to a numeric variable => regression tree
rt <- rpart(rain ~ month + season + l.temp + h.temp + ave.temp + ave.wind +
            gust.wind + dir.wind + dir.wind.8 + as.numeric(h.temp.hour)+
            as.numeric(l.temp.hour)+ as.numeric(gust.wind.hour), data=train)

# Full-grown tree with 8 splits using 6 different variables 
# (Not running the line below - do it to see the tree)
fancyRpartPlot(rt)

# As always, predict and evaluate on the test set
test.pred.rtree <- predict(rt,test)
RMSE.rtree <- sqrt(mean((test.pred.rtree-test$rain)^2))
RMSE.rtree

MAE.rtree <- mean(abs(test.pred.rtree-test$rain))
MAE.rtree

# Check cross-validation results (xerror column)
# It corresponds to 2 splits and cp = 0.088147
printcp(rt)

# Get the optimal CP programmatically...
min.xerror <- rt$cptable[which.min(rt$cptable[,"xerror"]),"CP"]
min.xerror

# ...and use it to prune the tree
rt.pruned <- prune(rt,cp = min.xerror)

# Plot the pruned tree
fancyRpartPlot(rt.pruned)

# Evaluate the new pruned tree on the test set
test.pred.rtree.p <- predict(rt.pruned,test)
RMSE.rtree.pruned <- sqrt(mean((test.pred.rtree.p-test$rain)^2))
RMSE.rtree.pruned

MAE.rtree.pruned <- mean(abs(test.pred.rtree.p-test$rain))
MAE.rtree.pruned

# Random Forest

# Needed to run the algorithm
library(randomForest)

# Convert some factor variables to numeric (train and test sets)
train$h.temp.hour <- as.numeric(train$h.temp.hour)
train$l.temp.hour <- as.numeric(train$l.temp.hour)
train$gust.wind.hour <- as.numeric(train$gust.wind.hour)
test$h.temp.hour <- as.numeric(test$h.temp.hour)
test$l.temp.hour <- as.numeric(test$l.temp.hour)
test$gust.wind.hour <- as.numeric(test$gust.wind.hour)

# For reproducibility; 123 has no particular meaning
# Run this immediately before creating the random forest
set.seed(123)

# Create a random forest with 1000 tress
rf <- randomForest(rain ~ month + season + l.temp + h.temp + ave.temp + ave.wind +
              gust.wind + dir.wind + dir.wind.8 + h.temp.hour + l.temp.hour+
              gust.wind.hour, data = train, importance = TRUE, ntree=1000)

# How many trees are needed to reach the minimum error estimate?
which.min(rf$mse)
# Plot rf to see the estimated error as a function of the number of trees
# (not running it)
# plot(rf)

# Using the importance()  function to calculate the importance of each variable
imp <- as.data.frame(sort(importance(rf)[,1],decreasing = TRUE),optional = T)
names(imp) <- "% Inc MSE"
imp

# As usual, predict and evaluate on the test set
test.pred.forest <- predict(rf,test)
RMSE.forest <- sqrt(mean((test.pred.forest-test$rain)^2))
RMSE.forest

MAE.forest <- mean(abs(test.pred.forest-test$rain))
MAE.forest

# Create a data frame with the error metrics for each method
accuracy <- data.frame(Method = c("Baseline","Linear Regression","Full tree","Pruned tree","Random forest"),
                       RMSE   = c(RMSE.baseline,RMSE.lin.reg,RMSE.rtree,RMSE.rtree.pruned,RMSE.forest),
                       MAE    = c(MAE.baseline,MAE.lin.reg,MAE.rtree,MAE.rtree.pruned,MAE.forest))

# Round the values
accuracy$RMSE <- round(accuracy$RMSE,2)
accuracy$MAE <- round(accuracy$MAE,2)
accuracy

# Create a data frame with the predictions for each method
all.predictions <- data.frame(actual = test$rain,
                              baseline = best.guess,
                              linear.regression = test.pred.lin,
                              full.tree = test.pred.rtree,
                              pruned.tree = test.pred.rtree.p,
                              random.forest = test.pred.forest)

# First six observations
head(all.predictions)

# To melt the columns with the gather() function 
# An alternative to the reshape2 package (see the end of Part3a) 
library(tidyr)

# Gather the prediction variables (columns) into a single row (i.e., wide to long)
# Recall the ggplot2 prefers the long data format
all.predictions <- gather(all.predictions,key = model,value = predictions,2:6)

head(all.predictions)
tail (all.predictions)

# Predicted vs. actual for each model
ggplot(data = all.predictions,aes(x = actual, y = predictions)) + 
  geom_point(colour = "blue") + 
  geom_abline(intercept = 0, slope = 1, colour = "red") +
  geom_vline(xintercept = 23, colour = "green", linetype = "dashed") +
  facet_wrap(~ model,ncol = 2) + 
  coord_cartesian(xlim = c(0,70),ylim = c(0,70)) +
  ggtitle("Predicted vs. Actual, by model")

library(arules)
weather$rained <- as.factor(weather$rained)
rules <- apriori(weather[,c(4,20,21)],
                 appearance = list(rhs=c("rained=Yes","rained=No"),default="lhs"),
                 parameter = list(minlen=2, supp=0.05, conf=0.7))
rules.sorted <- sort(rules, by="lift")
inspect(rules.sorted)


