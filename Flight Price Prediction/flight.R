# Import Pakages ----------------------------------------------------------

library(tidyverse)
library(ggcorrplot)
library(gridExtra)
library(randomForest)
library(caret)
library(mlr)

# Importing Dataset -------------------------------------------------------

library(readxl)
Train <- read_excel("Data_Train.xlsx")
View(Train)

# Understanding Dataset ---------------------------------------------------

str(Train)
summary(Train)
sum(is.na(Train))

# Data Manipulation -----------------------------------------------------

# Droping Cloumnns
Train <- Train[ , !(colnames(Train)) %in% c('Route','Additional_Info')]

# With Categorical Data

Train$Airline = as_factor(Train$Airline)
Train$Source = as_factor(Train$Source)
Train$Destination = as_factor(Train$Destination)
Train$Total_Stops = as_factor(Train$Total_Stops)

# Time and Dates

## 1)
## Converting string into date format
Train$Date_of_Journey = as.Date(Train$Date_of_Journey, "%d/%m/%Y")
## Extracting Day of Week
Train = Train %>% mutate(day_week = lubridate::wday(Train$Date_of_Journey, label = TRUE))

## 2)
## Converting string into date format
Train$Dep_Time <- strptime(Train$Dep_Time, format = '%H:%M')
## Extracting hour from date
Train$Dep_Time_hour = as.factor((lubridate::hour(Train$Dep_Time)))


## 3)
## Extracting only time
Train$Arrival_Time = str_sub(Train$Arrival_Time,1,5)
## Converting string into date format
Train$Arrival_Time <- strptime(Train$Arrival_Time, format = '%H:%M')
## Extracting hour from date
Train$Arrival_Time_hour = as.factor((lubridate::hour(Train$Arrival_Time)))

## 4)
## Duration Between Arival and Departure
Train <- Train %>% mutate(Duration_min = abs(as.numeric(Dep_Time - Arrival_Time))/60)


## Dropping column Date_of_Journey
Train <- Train[ , !(colnames(Train)) %in% c('Date_of_Journey')]
## Dropping column Dep_Time
Train <- Train[ , !(colnames(Train)) %in% c('Dep_Time')]
## Dropping column Arrival_Time
Train <- Train[ , !(colnames(Train)) %in% c('Arrival_Time')]
## Dropping column Duration
Train <- Train[ , !(colnames(Train)) %in% c('Duration')]

# Univariate Analysis -----------------------------------------------------

a1 <- ggplot( Train ,aes(fct_infreq(Train$Airline))) + geom_bar(, fill ='red4') + coord_flip()
a2 <- ggplot( Train ,aes(fct_infreq(Train$Source))) + geom_bar(, fill ='blue4') + coord_flip()
a3 <- ggplot( Train ,aes(fct_infreq(Train$Destination))) + geom_bar(, fill ='orange4') + coord_flip()
a4 <- ggplot( Train ,aes(fct_infreq(Train$Total_Stops))) + geom_bar(, fill ='deeppink4') + coord_flip()
grid.arrange( a1,a2,a3,a4, nrow = 2)

a5 <- ggplot( Train ,aes(fct_infreq(Train$day_week))) + geom_bar(, fill ='coral3') + coord_flip();a5
a6 <- ggplot( Train ,aes(fct_infreq(Train$Dep_Time_hour))) + geom_bar(, fill ='magenta3') + coord_flip()
a7 <- ggplot( Train ,aes(fct_infreq(Train$Arrival_Time_hour))) + geom_bar(, fill ='royalblue1') + coord_flip()
grid.arrange(a6,a7, nrow = 1)

# Spread analysis of Dependent Variable
ggplot( Train, aes(log(Train$Price))) +  geom_area(stat = 'bin', fill ='violetred4')

ggplot( Train, aes(Train$Price), binwidth = 2) + 
  geom_histogram()
  
# Skewness and Kurtosis  
library(moments)
skewness(Train$Price)
kurtosis(Train$Price)

# BiVariate Analysis ------------------------------------------------------

b1 <- ggplot( Train, aes(x = Airline, y= Price, fill = Airline)) 
b11 <- b1 + geom_bar( stat = "summary", fun.y = 'mean') +
    coord_flip();b1

b12 <- b1 + geom_boxplot()+coord_flip();b12




b2 <- ggplot( Train, aes(x = Source, y= Price, fill = Source)) + 
  geom_bar( stat = "summary", fun.y = 'mean') +
  coord_flip();b2

b3 <- ggplot( Train, aes(x = Destination, y= Price, fill = Destination)) + 
  geom_bar( stat = "summary", fun.y = 'mean') +
  coord_flip();b3

b4 <- ggplot( Train, aes(x = Total_Stops, y= Price, fill = Total_Stops)) + 
  geom_bar( stat = "summary", fun.y = 'mean') +
  coord_flip();b4

b5 <- ggplot( Train, aes(x =Train$day_week , y= Price, fill = day_week)) + 
  geom_bar( stat = "summary", fun.y = 'mean') +
  coord_flip();b5

b6 <- ggplot( Train, aes(x =Train$Dep_Time_hour , y= Price, fill = Dep_Time_hour )) + 
  geom_bar( stat = "summary", fun.y = 'mean') +
  coord_flip();b6

b7 <- ggplot( Train, aes(x =Train$Arrival_Time_hour , y= Price, fill = Arrival_Time_hour)) + 
  geom_bar( stat = "summary", fun.y = 'mean') +
  coord_flip();b7










# Test Dataset ------------------------------------------------------

Test <- read_excel("Test_set.xlsx")

# Droping Cloumnns
Test <- Test[ , !(colnames(Test)) %in% c('Route','Additional_Info')]

# With Categorical Data

Test$Airline = as_factor(Test$Airline)
Test$Source = as_factor(Test$Source)
Test$Destination = as_factor(Test$Destination)
Test$Total_Stops = as_factor(Test$Total_Stops)

# Time and Dates

## 1)
## Converting string into date format
Test$Date_of_Journey = as.Date(Test$Date_of_Journey, "%d/%m/%Y")
## Extracting Day of Week
Test = Test %>% mutate(day_week = lubridate::wday(Test$Date_of_Journey, label = TRUE))

## 2)
## Converting string into date format
Test$Dep_Time <- strptime(Test$Dep_Time, format = '%H:%M')
## Extracting hour from date
Test$Dep_Time_hour = as.factor((lubridate::hour(Test$Dep_Time)))


## 3)
## Extracting only time
Test$Arrival_Time = str_sub(Test$Arrival_Time,1,5)
## Converting string into date format
Test$Arrival_Time <- strptime(Test$Arrival_Time, format = '%H:%M')
## Extracting hour from date
Test$Arrival_Time_hour = as.factor((lubridate::hour(Test$Arrival_Time)))

## 4)
## Duration Between Arival and Departure
Test <- Test %>% mutate(Duration_min = abs(as.numeric(Dep_Time - Arrival_Time))/60)


## Dropping column Date_of_Journey
Test <- Test[ , !(colnames(Test)) %in% c('Date_of_Journey')]
## Dropping column Dep_Time
Test <- Test[ , !(colnames(Test)) %in% c('Dep_Time')]
## Dropping column Arrival_Time
Test <- Test[ , !(colnames(Test)) %in% c('Arrival_Time')]
## Dropping column Duration
Test <- Test[ , !(colnames(Test)) %in% c('Duration')]

# Split Dataset ------------------------------------------------------
Train = as.data.frame(Train)
library(dummies)
Train.new = dummies::dummy.data.frame(Train,sep ='.')
Train.new = dummies::dummy.data.frame(Train.new,names = 'day_week',sep ='.')
sum(is.na(Train))
Train = na.omit(Train)
library(caTools)
set.seed(123)
split = sample.split(Train$Price, SplitRatio = 0.8)
train_set = subset(Train, split==TRUE )
valid_set = subset(Train, split==FALSE )

# Random Forest ------------------------------------------------------

library(randomForest)
model_1 <- randomForest( Price ~.,data = train_set,importance = TRUE)

# Predicting on train set
out = function(model_1)
{
  pred_train <- predict(model_1, valid_set, type = "class")
  
  error = valid_set$Price - pred_train
  # Checking classification accuracy
  rmse <- function(error)
  {
    sqrt(mean(error^2))
  }
  rmse(error)
}
out(model_1)

# MLR PAKAGE(Random Forest) ------------------------------------------------------

#library(mlr)

# Creating a task
ml_task <- makeRegrTask(data = train_set,target = 'Price')

# Making a learner
# Regression RandomForest model
rf.lrn <- makeLearner("regr.randomForest",par.vals = list(importance = TRUE))
# To get the list of parameters for any algorithm
# getParamSet("regr.randomForest")

# Define Hyperparameters of model
model_Params <- makeParamSet(
  makeIntegerParam("ntree",lower = 500, upper = 1500),
  makeIntegerParam("mtry",lower = 2, upper = 12),
  makeIntegerParam("maxnodes",lower = 20, upper = 50)
)

# Define model tuning algorithm ~ Random tune algorithm
# Random Search on the space
random_tune <- makeTuneControlRandom(maxit = 10L)

# Create repeated cross validation folds
cv_folds <- makeResampleDesc("CV", iters = 3) # 3 fold cross validation

# Tune model to find best performing parameter settings 
# using random search algorithm
tuned_model <- tuneParams(learner = rf.lrn,
                          task = ml_task,
                          resampling = cv_folds,
                          measures = mlr::rmse,       
                          par.set = model_Params,
                          control = random_tune,
                          show.info = FALSE
                          )

# Apply optimal parameters to model
model <- setHyperPars(learner = rf.lrn,
                      par.vals = tuned_model$x)


# Verify performance on cross validation folds of tuned model
# resample(model,ml_task,cv_folds,measures = list(rsq,mlr::rmse))


rf <- train(learner = rf.lrn,task = ml_task)
rf_pred <- predict(rf,newdata = valid_set)
performance(rf_pred,measures = mlr::rmsle)

Price <- predict(rf,newdata = Test)
submission_file = data.frame(col1 = Price)
colnames( submission_file) <- c('Price')
write.csv(submission_file, file = "C:\\Users\\RISHABH\\Desktop\\sub_file.csv", row.names = FALSE)



############################*** BORUTA Pakage ***########################################

# Boruta Pakage feature selection
library(Boruta)
boruta.train <- Boruta(Price ~ ., data = Train.new, doTrace = 2)

# decision on tentative attributes
final.boruta <- TentativeRoughFix(boruta.train)

# obtain the list of confirmed attributes
getSelectedAttributes(final.boruta, withTentative = F)

# create a data frame of the final result derived from Boruta
boruta.df <- attStats(final.boruta)

rejected_col <- boruta.df %>% select(boruta.df$decision = "Rejected")


####################################################

library(arules)

rules <- apriori(Train,parameter = list(support = .01,conf = 0.8))
rules_conf <- sort(rules, by="confidence",decreasing = TRUE)
p=as.data.frame(inspect(head(rules_conf)))



