# Import Pakages ----------------------------------------------------------
library(tidyverse)
library(readr)
library(gridExtra)
library(randomForest)
library(caret)
library(mlr)

# Importing Dataset -------------------------------------------------------

Train <- read_csv("Train.csv")
Test <- read_csv("Test.csv")

# Understanding Dataset ---------------------------------------------------

str(Train)
summary(Train)

# Data Manipulation ---------------------------------------------------

# 1)
# Convert factor in character [Item_Fat_Content]
Train$Item_Fat_Content <- as_factor( Train$Item_Fat_Content)
# check levels
levels(Train$Item_Fat_Content) 

# replace LF,low fat with Low Fat
levels( Train$Item_Fat_Content)[1:2] <- "Low Fat"

# replace ref with Regular
levels( Train$Item_Fat_Content )[2] <- "Regular"

# 2)
# Since no item product can have zero visibility
## Replace 0 in Item_Visibility by Mean
# Count no of zeros
Train %>% filter( Item_Visibility == 0.0 ) %>% count(n())
# Replace with mean
Train = Train %>% mutate(Item_Visibility = replace(Item_Visibility, 
                                                   which( Item_Visibility == 0.0),
                                                   mean(Train$Item_Visibility)))
# 3)
Train$Item_Type <- factor(Train$Item_Type)
levels(Train$Item_Type)

# 4)
# Get unique years
sort(unique(Train$Outlet_Establishment_Year))
## Replace year with corresponding number 
Train$Outlet_Establishment_Year[Train$Outlet_Establishment_Year == 1985]<- 2018-1985
Train$Outlet_Establishment_Year[Train$Outlet_Establishment_Year == 1987]<- 2018-1987
Train$Outlet_Establishment_Year[Train$Outlet_Establishment_Year == 1997]<- 2018-1997
Train$Outlet_Establishment_Year[Train$Outlet_Establishment_Year == 1998]<- 2018-1998
Train$Outlet_Establishment_Year[Train$Outlet_Establishment_Year == 1999]<- 2018-1999
Train$Outlet_Establishment_Year[Train$Outlet_Establishment_Year == 2002]<- 2018-2002
Train$Outlet_Establishment_Year[Train$Outlet_Establishment_Year == 2004]<- 2018-2004
Train$Outlet_Establishment_Year[Train$Outlet_Establishment_Year == 2007]<- 2018-2007
Train$Outlet_Establishment_Year[Train$Outlet_Establishment_Year == 2009]<- 2018-2009

# 5)
## Convert Outlet_Size into numeric
Train$Outlet_Size <- factor( Train$Outlet_Size)
levels(Train$Outlet_Size)

Train$Outlet_Location_Type <- factor( Train$Outlet_Location_Type)
levels(Train$Outlet_Location_Type)

Train$Outlet_Type <- factor( Train$Outlet_Type)
levels(Train$Outlet_Type)

# Univariate Analysis -----------------------------------------------------

# Spread Analysis Continuous Variables
p41 <- ggplot(Train, aes(Item_Weight)) +
  geom_area(stat = "bin" , fill = "springgreen4", na.rm = TRUE,binwidth = 2)
p42 <- ggplot(Train, aes(Item_MRP)) +
  geom_area(stat = "bin" , fill = "purple4", na.rm = TRUE,binwidth = 5)
p43 <- ggplot(Train, aes(Item_Visibility)) +
  geom_area(stat = "bin" , fill = "darkorange4", na.rm = TRUE)
p44 <- ggplot(Train, aes(Outlet_Establishment_Year)) +
  geom_area(stat = "bin" , fill = "deepskyblue4", na.rm = TRUE,binwidth = 2)

grid.arrange(
  p41,p42,p43,p44, nrow = 2,
  top = "Spread Check")

#********************************************************************
# Spread Analysis Categorical Variables
p51 <- ggplot(Train, aes(Outlet_Type)) +
  geom_bar(fill = "springgreen4", na.rm = TRUE)
p52 <- ggplot(Train, aes(Outlet_Location_Type )) +
  geom_bar(fill = "purple4", na.rm = TRUE)
p53 <- ggplot(Train, aes(Outlet_Size)) +
  geom_bar(fill = "darkorange4", na.rm = TRUE)
p54 <- ggplot(Train, aes(Item_Fat_Content)) +
  geom_bar(fill = "deepskyblue4", na.rm = TRUE)


grid.arrange(
  p51,p52,p53,p54, nrow = 2,
  top = "Spread Check")

#********************************************************************

# Spread Analysis Dependent Variables
ggplot(Train, aes(Item_Outlet_Sales)) +
  geom_area(stat = "bin" , fill = "deepskyblue4", na.rm = TRUE)

# Skewness and Kurtosis  
library(moments)
skewness(Train$Item_Outlet_Sales)
kurtosis(Train$Item_Outlet_Sales)

# BiVariate Analysis ------------------------------------------------------

# Continuous Vs Target Variable
p61 <- ggplot(Train, aes(Train$Item_Weight,Train$Item_Outlet_Sales))+
  geom_point(colour = 'red4') + geom_smooth(method =lm,span =0.8)
  
p62 <- ggplot(Train, aes(Train$Item_Visibility,Train$Item_Outlet_Sales))+
  geom_point(colour = 'purple4') + geom_smooth(method =lm,span =0.8)

p63 <- ggplot(Train, aes(Train$Item_MRP,Train$Item_Outlet_Sales))+
  geom_point(colour = 'navy') + geom_smooth(method =lm,span =0.8)

p64 <- ggplot(Train, aes(Train$Outlet_Establishment_Year,Train$Item_Outlet_Sales))+
  geom_point(colour = 'green4') + geom_smooth(method =lm,span =0.8)

grid.arrange(
  p61,p62,p63,p64, nrow = 2,
  top = "Continuous Vs Target Variable")

#********************************************************************

# Discrete Vs Target Variable
p21 <- ggplot(Train,aes(Train$Item_Fat_Content, Train$Item_Outlet_Sales, fill = Train$Item_Fat_Content)) + 
  geom_boxplot() + coord_flip()
p23 <- ggplot(Train,aes(Train$Outlet_Size, Train$Item_Outlet_Sales, fill = Train$Outlet_Size)) + 
  geom_boxplot() + coord_flip()
p24 <- ggplot(Train,aes(Train$Outlet_Location_Type, Train$Item_Outlet_Sales, fill = Train$Outlet_Location_Type)) + 
  geom_boxplot() + coord_flip()
p25 <- ggplot(Train,aes(Train$Outlet_Type, Train$Item_Outlet_Sales, fill = Train$Outlet_Type)) + 
  geom_boxplot() + coord_flip()

grid.arrange(
  p21,p25,p23,p24, nrow = 2,
  top = "Discrete Vs Target Variable")

p22 <- ggplot(Train,aes(Train$Item_Type, Train$Item_Outlet_Sales, fill = Train$Item_Type)) + 
  geom_boxplot() + coord_flip();p22

# Correlation Matrix ------------------------------------------------------

Train2 = na.omit(Train)
d <- round(cor(Train2[ ,-c(1,3,5,7,9,10,11)]),2)

#Melt data to bring the correlation values in two axis
library(reshape2)
melted_d <- melt(d)

# Creating the Correlation matrix Heatmap
c1 <- ggplot(data = melted_d, aes(x=Var1, y=Var2, fill=value,label= value)) + 
          geom_tile() +
          # Adding color scale
          scale_fill_gradient2(low = "red4",high = "green4",mid ="yellow") +
          # Adding correlation value
          geom_text();c1

# Missing Value Treat ------------------------------------------------------

# Getting missing value percentage
missing_val <- as.data.frame(sapply(Train, function(x) sum(is.na(x))/nrow(Train) ))
# Renaming Column
colnames(missing_val) <- c("percent")
missing_val$col <-as.factor(row.names(missing_val))
missing_val$percent <- subset( missing_val, percent != 0.0)

ggplot(missing_val, aes(y = percent,x = col, fill = "col")) + 
  geom_bar(stat = "identity")

# Since there are more than 28% missing values in Outlet Size, therefore Dropping it.
Train <- Train[,-9]

# Replace Missing vaues in Item_Weight in mean
Train = Train %>% mutate_at(vars(Item_Weight),~ifelse(is.na(.x), mean(.x, na.rm = TRUE), .x))
sum(is.na(Train$Item_Weight))

# Dummy Variables ------------------------------------------------------

# Continuous Variables
set_1 <- Train[, c(1,2,4,6,7,8,11) ]
# standardizing the Variables
set_11 <- normalizeFeatures(Train_trans[,c(2,3,4,6)],method = "standardize")

# Categorical Variables
set_2 <- Train[, c(3,5,9,10) ]
# Creating Dummy Variable using MLR Pakage 
set_22 <- createDummyFeatures(set_2)

# Combining Sets
Train_trans <- bind_cols( set_11, set_22)


########################### SPLIT DATASET ###################################

library(caTools)
set.seed(123)
split = sample.split(Train_trans$Item_Outlet_Sales, SplitRatio = 0.8)
train_set = subset(Train_trans[,-c(1)] , split==TRUE )
valid_set = subset(Train_trans[,-c(1)]  , split==FALSE ) 
###############################*** Random Forest ***####################

library(randomForest)
model_1 <- randomForest( Item_Outlet_Sales ~                    Item_MRP+
                                                      OT.Supermarket.Type1+
                                                Outlet_Establishment_Year+
                                                                   OUT027+
                                                         OT.Grocery.Store+
                                                     OT.Supermarket.Type2+
                                                     OT.Supermarket.Type3+
                                                                   OUT010+
                                                                   OUT018+
                                                               OLT.Tier.3+
                                                                   OUT019+
                                                                  Seafood+
                                                                   OUT045+
                                                              Hard.Drinks+
                                                                   Others+
                                                                   Breads+
                                                     Fruits.and.Vegetables+
                                                               OLT.Tier.2+
                                                              Item_Weight+
                                                               OLT.Tier.1, data = train_set,
                         ntree = 1000,mtry = 10,maxnodes = 38,
                         importance = TRUE)

# Predicting on train set
out = function(model)
{
  pred_train <- predict(model, valid_set, type = "class")
  
  error = valid_set$Item_Outlet_Sales - pred_train
  # Checking classification accuracy
  rmse <- function(error)
  {
    sqrt(mean(error^2))
  }
  rmse(error)
}
out(model_1)
# 911.52


#######################*** IMP Feature ***###################################

feat_imp_df <- importance(model_1) %>% 
  data.frame() %>% 
  mutate(feature = row.names(.)) %>%arrange(desc(X.IncMSE))

# plot Important Feature
ggplot(feat_imp_df, aes(x = fct_reorder(feature,X.IncMSE), 
                        y = log(IncNodePurity), fill = feature)) +
  geom_bar(stat='identity') +
  coord_flip() +
  theme_grey() +
  labs(
    x     = "Feature",
    y     = "Importance",
    title = "Feature Importance: <Model>")

# Training model on top 20 var.

imp_var <- subset(feat_imp_df,X.IncMSE > 0, select = c(feature) )
# 900.126

#######################*** FINAL MODEL ***#################################
model_final = randomForest( Item_Outlet_Sales ~ Item_MRP+
                              OT.Supermarket.Type1+
                              Outlet_Establishment_Year+
                              OUT027+
                              OT.Grocery.Store+
                              OT.Supermarket.Type2+
                              OT.Supermarket.Type3+
                              OUT010+
                              OUT018+
                              OLT.Tier.3+
                              OUT019+
                              Seafood+
                              OUT045+
                              Hard.Drinks+
                              Others+
                              Breads+
                              Fruits.and.Vegetables+
                              OLT.Tier.2+
                              Item_Weight+
                              OLT.Tier.1,
                              data = Train_trans[,-c(1)], 
                            ntree = 500,mtry = 10,maxnodes = 40,
                            importance = TRUE)

Item_Outlet_Sales <- predict(model_final, newdata = Test_trans)


submission_file = data.frame(col1 = Test$Item_Identifier,col2 = Test$Outlet_Identifier,
                             col3 = Item_Outlet_Sales)
colnames( submission_file) <- c("Item_Identifier","Outlet_Identifier","Item_Outlet_Sales")

write.csv(submission_file, file = "C:\\Users\\RISHABH\\Desktop\\sub_file.csv", row.names = FALSE)
#1156

############################*** BORUTA Pakage ***########################################

# Boruta Pakage feature selection
library(Boruta)
boruta.train <- Boruta(Item_Outlet_Sales ~ ., data = train_set, doTrace = 2)

# decision on tentative attributes
final.boruta <- TentativeRoughFix(boruta.train)

# obtain the list of confirmed attributes
getSelectedAttributes(final.boruta, withTentative = F)

# create a data frame of the final result derived from Boruta
boruta.df <- attStats(final.boruta)

# #Taking  predictors
pred_var <- c("Item_Weight",               "Item_Visibility",          
                 "Item_MRP",                  "Outlet_Establishment_Year",
                 "Outlet_Size",               "OLT.Tier.1"  ,             
                 "OLT.Tier.2",                "OLT.Tier.3"   ,            
                 "OT.Supermarket.Type1",      "OT.Supermarket.Type2",     
                 "OT.Supermarket.Type3" ,     "OT.Grocery.Store",         
                 "Seafood",                   "OUT010",                   
                 "OUT013"  ,                  "OUT018" ,                  
                 "OUT019"   ,                 "OUT027"  ,                 
                 "OUT035"    ,                "OUT046"   ,                
                 "OUT049",                     'Item_Outlet_Sales')


############################*** MLR PAKAGE(XGBOOST) ***##################

# *** Setup modelling mlr environment ***

# Define machine learning task
ml_task <- makeRegrTask(data = train_set[,predictors],target = outcomeName)

# Create repeated cross validation folds
cv_folds <- makeResampleDesc("CV", iters = 3) # 3 fold cross validation

# Define model tuning algorithm ~ Random tune algorithm
# Random Search on the space
random_tune <- makeTuneControlRandom(maxit = 100L)

# Define model
# Regression XgBoost model
model <- makeLearner("regr.xgboost")

# To get the list of parameters for any algorithm
# getParamSet("regr.xgboost")

# Define Hyperparameters of model
model_Params <- makeParamSet(
  makeIntegerParam("nrounds",lower=10,upper=20),
  makeIntegerParam("max_depth",lower=1,upper=5),
  makeNumericParam("lambda",lower=0.55,upper=0.60),
  makeNumericParam("eta", lower = 0.001, upper = 0.5),
  makeNumericParam("subsample", lower = 0.10, upper = 0.80),
  makeNumericParam("min_child_weight",lower=1,upper=5),
  makeNumericParam("colsample_bytree",lower = 0.2,upper = 0.8)
)

# Tune model to find best performing parameter settings 
# using random search algorithm
tuned_model <- tuneParams(learner = model,
                          task = ml_task,
                          resampling = cv_folds,
                          measures = mlr::rmse,       
                          par.set = model_Params,
                          control = random_tune,
                          show.info = FALSE
                          )

# Apply optimal parameters to model
model <- setHyperPars(learner = model,
                      par.vals = tuned_model$x)


# Verify performance on cross validation folds of tuned model
# resample(model,ml_task,cv_folds,measures = list(rsq,mse))


xg_train <- train(learner = model,task = ml_task)
xg_pred <- predict(xg_train,newdata = valid_set[,pred_var])
performance(xg_pred,measures = mlr::rmse)

Item_Outlet_Sales <- predict(xg_train, newdata = Test_trans)
submission_file = data.frame(col1 = Test$Item_Identifier,col2 = Test$Outlet_Identifier,
                             col3 = Item_Outlet_Sales)
colnames( submission_file) <- c("Item_Identifier","Outlet_Identifier","Item_Outlet_Sales")

write.csv(submission_file, file = "C:\\Users\\RISHABH\\Desktop\\sub_file.csv", row.names = FALSE)

############################*** MLR PAKAGE(Random Forest) ***###############
#library(mlr)

# Creating a task
ml_task <- makeRegrTask(data = train_set[,pred_var],target = 'Item_Outlet_Sales')

# Making a learner
# Regression RandomForest model
rf.lrn <- makeLearner("regr.randomForest",par.vals = list(ntree = 1000,
                                                          mtry = 10,
                                                          maxnodes = 40,
                                                          importance = TRUE))
# To get the list of parameters for any algorithm
# getParamSet("regr.randomForest")

# Define Hyperparameters of model
model_Params <- makeParamSet(
  makeIntegerParam("ntree",lower = 500, upper = 1200),
  makeIntegerParam("mtry",lower = 3, upper = 12),
  makeIntegerParam("maxnodes",lower = 20, upper = 50)
)

# Define model tuning algorithm ~ Random tune algorithm
# Random Search on the space
random_tune <- makeTuneControlRandom(maxit = 100L)

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
model <- setHyperPars(learner = model,
                      par.vals = tuned_model$x)


# Verify performance on cross validation folds of tuned model
# resample(model,ml_task,cv_folds,measures = list(rsq,mlr::rmse))


rf <- train(learner = rf.lrn,task = ml_task)
rf_pred <- predict(rf,newdata = valid_set)
performance(rf_pred,measures = mlr::rmse)


Item_Outlet_Sales <- predict(rf, newdata = Test_trans[,pred_var])
submission_file = data.frame(col1 = Test$Item_Identifier,col2 = Test$Outlet_Identifier,
                             col3 = Item_Outlet_Sales)
colnames( submission_file) <- c("Item_Identifier","Outlet_Identifier","Item_Outlet_Sales")

write.csv(submission_file, file = "C:\\Users\\RISHABH\\Desktop\\sub_file.csv", row.names = FALSE)

# 1194


############################*** MLR PAKAGE(ExtraTree) ***################
#library(mlr)

# Creating a task
ml_task <- makeRegrTask(data = train_set[,pred_var],target = 'Item_Outlet_Sales')

# Making a learner
# View(listLearners("regr")) #get the list of learners
# Regression ExtraTree model
ef.lrn <- makeLearner("regr.extraTrees")
# To get the list of parameters for any algorithm
# getParamSet("regr.extraTrees")

# Define Hyperparameters of model
model_Params = makeParamSet(
  makeIntegerLearnerParam( "ntree",lower = 100L, upper = 1000L),
  makeIntegerLearnerParam( "mtry", lower = 2L, upper = 12L),
  makeIntegerLearnerParam( "numRandomCuts", lower = 1L, upper = 10L),
  makeIntegerLearnerParam( "nodesize", lower = 10L, upper = 40L)
  )
  
# Define model tuning algorithm ~ Random tune algorithm
# Random Search on the space
random_tune <- makeTuneControlRandom(maxit = 10L)

# Create repeated cross validation folds
cv_folds <- makeResampleDesc("CV", iters = 3) # 3 fold cross validation

# Tune model to find best performing parameter settings 
# using random search algorithm
# options( java.parameters = "-Xmx6g" )
tuned_model <- tuneParams(learner = ef.lrn,
                          task = ml_task,
                          resampling = cv_folds,
                          measures = mlr::rmse,       
                          par.set = model_Params,
                          control = random_tune,
                          show.info = FALSE
                          )

# Apply optimal parameters to model
model <- setHyperPars(learner = model,
                      par.vals = tuned_model$x)


# Verify performance on cross validation folds of tuned model
# resample(model,ml_task,cv_folds,measures = list(rsq,mlr::rmse))


ef <- train(learner = ef.lrn,task = ml_task)
ef_pred <- predict(ef,newdata = valid_set[,pred_var])
performance(ef_pred,measures = mlr::rmse)








