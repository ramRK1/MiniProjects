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
Test <- read_excel("Test_set.xlsx")
Test$Price <- NA
All <- rbind(Train,Test)
View(All)

# Understanding Dataset ---------------------------------------------------

str(All)
summary(All)

# Data Manipulation -----------------------------------------------------

# Droping Cloumnns
All <- All[ , !(colnames(All)) %in% c('Route','Additional_Info')]

# With Categorical Data

All$Airline = as_factor(All$Airline)
All$Source = as_factor(All$Source)
All$Destination = as_factor(All$Destination)
All$Total_Stops = as_factor(All$Total_Stops)

# Time and Dates

## 1)
## Converting string into date format
All$Date_of_Journey = as.Date(All$Date_of_Journey, "%d/%m/%Y")
## Extracting Day of Week
All = All %>% mutate(day_week = lubridate::wday(All$Date_of_Journey, label = TRUE))

## 2)
## Converting string into date format
All$Dep_Time <- strptime(All$Dep_Time, format = '%H:%M')
## Extracting hour from date
All$Dep_Time_hour = as.factor((lubridate::hour(All$Dep_Time)))


## 3)
## Extracting only time
All$Arrival_Time = str_sub(All$Arrival_Time,1,5)
## Converting string into date format
All$Arrival_Time <- strptime(All$Arrival_Time, format = '%H:%M')
## Extracting hour from date
All$Arrival_Time_hour = as.factor((lubridate::hour(All$Arrival_Time)))

## 4)
## Duration Between Arival and Departure
All <- All %>% mutate(Duration_min = abs(as.numeric(Dep_Time - Arrival_Time))/60)


## Dropping column Date_of_Journey
All <- All[ , !(colnames(All)) %in% c('Date_of_Journey')]
## Dropping column Dep_Time
All <- All[ , !(colnames(All)) %in% c('Dep_Time')]
## Dropping column Arrival_Time
All <- All[ , !(colnames(All)) %in% c('Arrival_Time')]
## Dropping column Duration
All <- All[ , !(colnames(All)) %in% c('Duration')]

# Univariate Analysis -----------------------------------------------------

a1 <- ggplot( All ,aes(fct_infreq(All$Airline))) + geom_bar( fill ='red4') + coord_flip()
a2 <- ggplot( All ,aes(fct_infreq(All$Source))) + geom_bar( fill ='blue4') + coord_flip()
a3 <- ggplot( All ,aes(fct_infreq(All$Destination))) + geom_bar( fill ='orange4') + coord_flip()
a4 <- ggplot( All ,aes(fct_infreq(All$Total_Stops))) + geom_bar( fill ='deeppink4') + coord_flip()
grid.arrange( a1,a2,a3,a4, nrow = 2)

a5 <- ggplot( All ,aes(fct_infreq(All$day_week))) + geom_bar( fill ='coral3') + coord_flip();a5
a6 <- ggplot( All ,aes(fct_infreq(All$Dep_Time_hour))) + geom_bar( fill ='magenta3') + coord_flip()
a7 <- ggplot( All ,aes(fct_infreq(All$Arrival_Time_hour))) + geom_bar( fill ='royalblue1') + coord_flip()
grid.arrange(a6,a7, nrow = 1)


## Removing ROWS having Price greater than 20,000
All <- subset(All, Price < 20000)
# Spread analysis of Dependent Variable
ggplot( data = All, aes(x=All$Price)) + 
    geom_histogram(fill = "blue4", binwidth = 1000)+
      scale_x_continuous(breaks = seq(0,80000, by =2000))
  
# Skewness and Kurtosis  
library(moments)
skewness(All$Price,na.rm = TRUE)
kurtosis(All$Price, na.rm = TRUE)

# BiVariate Analysis ------------------------------------------------------

b1 <- ggplot( All, aes(x = Airline, y= Price, fill = Airline)) +
        geom_boxplot()+coord_flip();b1

b2 <- ggplot( All, aes(x = Source, y= Price, fill = Source)) +
        geom_boxplot()+coord_flip();b2

b3 <- ggplot( All, aes(x = Destination, y= Price, fill = Destination)) + 
        geom_boxplot()+coord_flip();b3

b4 <- ggplot( All, aes(x = Total_Stops, y= Price, fill = Total_Stops)) + 
        geom_boxplot()+coord_flip();b4

b5 <- ggplot( All, aes(x =All$day_week , y= Price, fill = day_week)) + 
        geom_boxplot()+coord_flip();b5

b6 <- ggplot( All, aes(x =All$Dep_Time_hour , y= Price, fill = Dep_Time_hour )) + 
        geom_boxplot()+coord_flip();b6

b7 <- ggplot( All, aes(x =All$Arrival_Time_hour , y= Price, fill = Arrival_Time_hour)) + 
        geom_boxplot()+coord_flip();b7

b8 <- ggplot( All, aes(y =Duration_min , x= Price)) + 
        geom_point(color="red4") + geom_smooth();b8

######################################################################
All %>% group_by(Airline,Source,Destination) %>% summarise(n())

a <- table(All$Total_Stops,All$day_week)
chisq.test(a)


######################################################################
Price_nor <- normalizeFeatures(All$Price ,method = "standardize")

sum(is.na(All$Price))

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


