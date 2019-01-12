# Pakages Required 
library(tidyverse)
library(readr)
library(ggcorrplot)
library(gridExtra)
library(randomForest)
library(caret)

# Importing Dataset
Train <- read_csv("Train.csv")
Test <- read_csv("Test.csv")

dataset = dplyr::bind_rows(Train,Test)
####################################################################

dataset$Item_Fat_Content <- factor( dataset$Item_Fat_Content)
dataset$Outlet_Location_Type <- factor( dataset$Outlet_Location_Type)
dataset$Outlet_Size <- factor( dataset$Outlet_Size)
dataset$Outlet_Type <- factor( dataset$Outlet_Type)
dataset$Item_Type <- factor( dataset$Item_Type)

####################################################################

# Data Exploration
summary(dataset)
# dim(Dataset) [dimensions of dataset]
# fix(Dataset) [editing the dataset]

sum( is.na( dataset )) #[count no. of missing value

# *** RAW-DATA  ANALYSIS ***
# PRICE EFFECTS
p11 <- ggplot(dataset, aes(Item_MRP, fill = Outlet_Type)) +
  geom_area(stat = "bin" , na.rm = TRUE)
p12 <- ggplot(dataset, aes(Item_MRP, fill = Outlet_Location_Type)) +
  geom_area(stat = "bin" , na.rm = TRUE)
p13 <- ggplot(dataset, aes(Item_MRP, fill = Outlet_Size)) +
  geom_area(stat = "bin" , na.rm = TRUE)
p14 <- ggplot(dataset, aes(Item_MRP, fill = Item_Fat_Content)) +
  geom_area(stat = "bin" , na.rm = TRUE)


grid.arrange(
  p11,p12,p13,p14, nrow = 2,
  top = "Price Effects")
#********************************************************************
# Weight EFFECTS
p31 <- ggplot(dataset, aes(Item_Weight, fill = Outlet_Type)) +
  geom_area(stat = "bin" , na.rm = TRUE)
p32 <- ggplot(dataset, aes(Item_Weight, fill = Outlet_Location_Type)) +
  geom_area(stat = "bin" , na.rm = TRUE)
p33 <- ggplot(dataset, aes(Item_Weight, fill = Outlet_Size)) +
  geom_area(stat = "bin" , na.rm = TRUE)
p34 <- ggplot(dataset, aes(Item_Weight, fill = Item_Fat_Content)) +
  geom_area(stat = "bin" , na.rm = TRUE)


grid.arrange(
  p31,p32,p33,p34, nrow = 2,
  top = "Weight Effects")

#********************************************************************
# Spread Analysis Continuous Variables
p41 <- ggplot(dataset, aes(Item_Weight)) +
  geom_area(stat = "bin" , fill = "springgreen4", na.rm = TRUE)
p42 <- ggplot(dataset, aes(Item_MRP)) +
  geom_area(stat = "bin" , fill = "purple4", na.rm = TRUE)
p43 <- ggplot(dataset, aes(Item_Visibility)) +
  geom_area(stat = "bin" , fill = "darkorange4", na.rm = TRUE)
p44 <- ggplot(dataset, aes(Outlet_Establishment_Year)) +
  geom_area(stat = "bin" , fill = "deepskyblue4", na.rm = TRUE)


grid.arrange(
  p41,p42,p43,p44, nrow = 2,
  top = "Spread Check")

#********************************************************************
# Spread Analysis Categorical Variables
p51 <- ggplot(dataset, aes(Outlet_Type)) +
  geom_bar(fill = "springgreen4", na.rm = TRUE)
p52 <- ggplot(dataset, aes(Outlet_Location_Type )) +
  geom_bar(fill = "purple4", na.rm = TRUE)
p53 <- ggplot(dataset, aes(Outlet_Size)) +
  geom_bar(fill = "darkorange4", na.rm = TRUE)
p54 <- ggplot(dataset, aes(Item_Fat_Content)) +
  geom_bar(fill = "deepskyblue4", na.rm = TRUE)


grid.arrange(
  p51,p52,p53,p54, nrow = 2,
  top = "Spread Check")

#********************************************************************

# *** BI-VARIATE ANALYSIS ***

# CONTINUOUS V/S CONTINUOUS
dataset2 = na.omit(dataset)
ggcorrplot(cor(dataset2[ ,-c(1,2,4,7,8,9,12)]),method = 'circle',hc.order = T,
           type ='lower',colors = c('red','azure','cadetblue1'),
           ggtheme = ggplot2::theme_grey())

###########################################################

# Missing Values Treatment

# Visual Representation of missing values
library(VIM)
mice_plot <- aggr( dataset[,-c(12)], color = c('blue4','green4'),
                   numbers = TRUE, sortVars = TRUE,
                   labels = names(dataset[,-c(1,7,12)]), cex.axis = .7,
                   gap = 1, ylab = c("Missing Data","Pattern"))

# # Outlet_size vs Item_Outlet_Sales
# q1 <- ggplot( dataset, aes(x =dataset$Item_Weight, y = dataset$Item_Outlet_Sales )) + 
#           geom_point( aes( color = "red4")) + 
#           geom_smooth();q1

library(mice)
imputed_data <- mice(dataset[,-c(12)], m =2, maxit = 50,method = 'pmm')
dataset = mice::complete(imputed_data,2)

sum( is.na( dataset )) #[count no. of missing value

###########################################################

# Feature Engineering
d1 = dataset%>% group_by(Outlet_Establishment_Year) %>% summarise(Count = n()) %>% as.data.frame()
d2 = dataset%>% group_by(Item_Fat_Content) %>% summarise(Count = n())%>% as.data.frame()
d3 = dataset%>% group_by(Item_Type) %>% summarise(Count = n())%>% arrange(Count)%>% as.data.frame()
d4 = dataset%>% group_by(Outlet_Type) %>% summarise(Count = n())%>% as.data.frame()
d5 = dataset%>% group_by(Outlet_Size) %>% summarise(Count = n())%>% as.data.frame()

# *************************************************************************
# Convert factor in character [Item_Fat_Content]

levels(dataset$Item_Fat_Content) # check levels

dataset$Item_Fat_Content <- as.character(dataset$Item_Fat_Content)
library(data.table)
# replace LF,low fat,reg with Low Fat & Regular
data_dt <- data.table(dataset) %>% .[ Item_Fat_Content %in% c("LF","low fat"),Item_Fat_Content := "Low Fat" ]
# Base Method 
# dataset$Item_Fat_Content[ dataset$Item_Fat_Content == "LF" & dataset$Item_Fat_Content == "low fat"] <- "Low Fat"

data_dt <- data_dt %>% .[ Item_Fat_Content == "reg",Item_Fat_Content := "Regular" ]
# Convert into dataframe
dataset = data.frame(data_dt) 
# Into factor
dataset$Item_Fat_Content <- factor( dataset$Item_Fat_Content)

# *************************************************************************

# Create a broad category of Type of Item
library(stringr)

d1 = dataset %>% filter(str_detect(Item_Identifier, '^FD')) %>% mutate(Item_Type_Combined = "Food")
d2 = dataset %>% filter(str_detect(Item_Identifier, '^NC')) %>% mutate(Item_Type_Combined = "Non Consumable")
d3 = dataset %>% filter(str_detect(Item_Identifier, '^DR')) %>% mutate(Item_Type_Combined = "Drinks")
d4 = bind_rows(d1,d2,d3)

# *************************************************************************
# Item Visibility 

# No. of items having visibility zero
i0 = dataset %>% filter( Item_Visibility == 0.0 ) %>% count(n());i0

# Replace with mean
i1 = d4 %>% mutate(Item_Visibility = replace(Item_Visibility, which( Item_Visibility == 0.0),mean(dataset$Item_Visibility)))

# *************************************************************************
dataset_new = i1[,-c(5)]
dataset_new$Item_Type_Combined = as.factor(dataset_new$Item_Type_Combined)

#################################################################

# DATA TRANSFORMATION

# Continuous Variables
set_1 <- dataset_new[, c(1,2,4,5,6,7) ]

# Categorical Variables
set_2 <- dataset_new[, c(3,8,9,10,11) ]

# Converting 
#library(caret)
# 1)
dummies_IFS <- predict(dummyVars(~ Item_Fat_Content, data = set_2), 
                       newdata = set_2)
colnames(dummies_IFS) <- c("IFS.Low_Fat","IFS.Regular")
head(dummies_IFS, n = 3)

# 2)
dummies_OS <- predict(dummyVars(~ Outlet_Size, data = set_2), 
                      newdata = set_2)
colnames(dummies_OS) <- c("OS.High", "OS.Medium", "OS.Small")
head(dummies_OS, n = 3)

# 3)
dummies_OLT <- predict(dummyVars(~ Outlet_Location_Type, data = set_2), 
                       newdata = set_2)
colnames(dummies_OLT) <- c("OLT.Tier 1", "OLT.Tier 2", "OLT.Tier 3")
head(dummies_OLT, n = 3)

# 4)
dummies_OT <- predict(dummyVars(~ Outlet_Type, data = set_2), 
                      newdata = set_2)
colnames(dummies_OT) <- c("OT.Supermarket Type1","OT.Supermarket Type2", "OT.Supermarket Type3","OT.Grocery Store")
head(dummies_OT, n = 3)

# 5)
dummies_IT <- predict(dummyVars(~ Item_Type_Combined, data = set_2), 
                      newdata = set_2)
colnames(dummies_IT) <- c("Drinks","Food","Non Consumable")
head(dummies_IT, n = 3)

set_3 <- bind_cols(data.frame(dummies_IFS),data.frame(dummies_OLT),
                   data.frame(dummies_OS),data.frame(dummies_OT),
                   data.frame(dummies_IT))

dataset_trans <- bind_cols( set_1, set_3)
# pred_col <- data.frame(col1 = Train$Item_Outlet_Sales )
# preprocessParams <- preProcess( pred_col ,method =c("range"))
# pred_trans <- predict(preprocessParams,pred_col)


###############################################################

# DAta standadisation
#library(caret)
dataset_con <- dataset_trans[ , c(1,2,3,4)]
# calculate the pre-process parameters from the dataset
preprocessParams <- preProcess( dataset_con ,method =c("center","scale"))
# transform the dataset using the parameters
dataset_con <- predict(preprocessParams, dataset_con)
str(dataset_con)

dataset_trans_new <- bind_cols(dataset_con,set_3)


###############################################################

dataset_trans_new = dataset_trans_new[,-c(17,18,19)]
# seperating train & test part

data_train = dataset_trans_new[c(0:8523),]
data_test = dataset_trans_new[c(8524:14204),]

# combining columns train set
data_train = cbind(data_train,Train$Item_Outlet_Sales)
# Rename column
colnames(data_train)[17] <- 'Item_Outlet_Sales'

# SPLIT DATASET
library(caTools)
set.seed(123)
split = sample.split(data_train$Item_Outlet_Sales, SplitRatio = 0.8)
train_set = subset(data_train , split==TRUE )
valid_set = subset(data_train , split==FALSE ) 

###############################################################

# Random Forest
library(randomForest)
model_1 <- randomForest( Item_Outlet_Sales ~ ., data = train_set, 
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

##########################################################
feat_imp_df <- importance(model_1) %>% 
  data.frame() %>% 
  mutate(feature = row.names(.)) 

# plot Important Feature
ggplot(feat_imp_df, aes(x = reorder(feature,X.IncMSE), 
                        y = log(IncNodePurity), fill = feature)) +
  geom_bar(stat='identity') +
  coord_flip() +
  theme_grey() +
  labs(
    x     = "Feature",
    y     = "Importance",
    title = "Feature Importance: <Model>")
# x = train_set[,-c(9)]
# y = train_set[,c(9)]
# bestMtry <- tuneRF(x,y, stepFactor = 1.5, improve = 1e-5, ntree = 500)

####################################################################

# Random Search
control <- trainControl(method="repeatedcv", number=10, repeats=3, search="random")
mtry <- ncol(train_set)/3
rf_random <- train(Item_Outlet_Sales~., data=train_set, method="rf", metric="rmse", tuneLength=15, trControl=control)
print(rf_random)
plot(rf_random)

######################################################################

# XGBOOST

library(xgboost)

#put into the xgb matrix format
dtrain = xgb.DMatrix(data =  as.matrix(train_set[,-c(20)]), label = train_set[,c(20)] )
dtest = xgb.DMatrix(data =  as.matrix(valid_set[,-c(20)]), label = valid_set[,c(20)])

# these are the datasets the rmse is evaluated for at each iteration
watchlist = list(train=dtrain, test=dtest)

# try 1 - off a set of paramaters I know work pretty well for most stuff

bst = xgb.train(data = dtrain, 
                max.depth = 8, 
                eta = 0.3, 
                nthread = 2, 
                nround = 1000, 
                watchlist = watchlist, 
                objective = "reg:linear", 
                early_stopping_rounds = 50,
                print_every_n = 500)


bst_slow = xgb.train(data = dtrain, 
                     max.depth=5, 
                     eta = 0.01, 
                     nthread = 2, 
                     nround = 10000, 
                     watchlist = watchlist, 
                     objective = "reg:linear", 
                     early_stopping_rounds = 50,
                     print_every_n = 500)


test_1 =  xgb.DMatrix(data =  as.matrix(data_test))
Item_Outlet_Sales<- predict(bst_slow,test_1)  
#1192
# 






# **********************************************************************

model_final = randomForest( Item_Outlet_Sales ~ ., data = data_train, importance = TRUE)

Item_Outlet_Sales <- predict(model_final, newdata = data_test)


submission_file = data.frame(col1 = Test$Item_Identifier,col2 = Test$Outlet_Identifier,
                             col3 = Item_Outlet_Sales)
colnames( submission_file) <- c("Item_Identifier","Outlet_Identifier","Item_Outlet_Sales")

write.csv(submission_file, file = "C:\\Users\\RISHABH\\Desktop\\sub_file.csv", row.names = FALSE)

# o/p :1172 & rank > 1k
#************************************************************************
# **********************************************************************
