# Importing Pakages
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import seaborn as sns
import warnings
warnings.filterwarnings("ignore")
# when you want graphs in a separate window 
# %matplotlib inline
# when you want an inline plot
# *******************************************************************
# *******************************************************************

## Importing Files
Accident = pd.read_csv("C:/LOCAL_R/Rishabh##/Machine Learning/data SET/dft-accident-data/Accidents0515.csv",sep = ',') 
#Casualties = pd.read_csv("C:/LOCAL_R/Rishabh##/Machine Learning/data SET/accident dataset/Casualties0515.csv") 
#Vehicles0515 = pd.read_csv("C:\LOCAL_R\Rishabh##\Machine Learning\data SET\accident dataset\Vehicles0515.csv",sep = 't') 

# *******************************************************************
# *******************************************************************

# ## Understanding the Data

Accident.head()
# To find the dimensions of the Dataset
Accident.shape
# Find the datatype of variables
Accident.dtypes
# Summary of the Dataset
describe=pd.DataFrame(Accident.describe())

# *******************************************************************
# *******************************************************************

## Univariate Analysis
# Analysing targetx variable
Accident['Accident_Severity'].value_counts(normalize = True).plot.bar()

# ****************************************************************
# Independent Variable (Categorical)
plt.figure(1)
plt.subplot(221)
Accident['Pedestrian_Crossing-Human_Control'].value_counts(normalize = True).plot.bar(figsize=(20,10),title = 'Pedestrian_Crossing-Human_Control')
#plt.show()
plt.subplot(222)
Accident['Pedestrian_Crossing-Physical_Facilities'].value_counts(normalize = True).plot.bar(title = 'Pedestrian_Crossing-Physical_Facilities')
#plt.show()
plt.subplot(223)
Accident['Light_Conditions'].value_counts(normalize = True).plot.bar(title = 'Light_Conditions')
plt.show()
# ****************************************************************
plt.figure(2)
plt.subplot(221)
Accident['Weather_Conditions'].value_counts(normalize = True).plot.bar(figsize=(20,10),title = 'Weather_Conditions')
#plt.show()
plt.subplot(222)
Accident['Road_Surface_Conditions'].value_counts(normalize = True).plot.bar(title = 'Road_Surface_Conditions')
#plt.show()
plt.subplot(223)
Accident['Special_Conditions_at_Site'].value_counts(normalize = True).plot.bar(title = 'Special_Conditions_at_Site')
#plt.show()
plt.subplot(224)
Accident['Carriageway_Hazards'].value_counts(normalize = True).plot.bar(title = 'Carriageway_Hazards')
plt.show()
# ****************************************************************
plt.figure(3)
plt.subplot(221)
Accident['Urban_or_Rural_Area'].value_counts(normalize = True).plot.bar(figsize=(20,10),title = 'Urban_or_Rural_Area')
#plt.show()
plt.subplot(222)
Accident['Did_Police_Officer_Attend_Scene_of_Accident'].value_counts(normalize = True).plot.bar(title = 'Did_Police_Officer_Attend_Scene_of_Accident')
plt.show()

# *******************************************************************
# *******************************************************************
# Independent Variable (Continuous)

plt.figure(1) 
plt.subplot(221) 
sns.distplot(Accident['Police_Force'],hist=False,color="b", kde_kws={"shade": True}); 
plt.subplot(222) 
Accident['Police_Force'].plot.box( patch_artist=True,figsize=(16,5)) 

plt.subplot(223) 
sns.distplot(Accident['Number_of_Vehicles'],hist=False,color="b", kde_kws={"shade": True}); 
plt.subplot(224) 
Accident['Number_of_Vehicles'].plot.box( patch_artist=True,figsize=(16,5)) 
plt.show()

plt.figure(2) 
plt.subplot(221) 
sns.distplot(Accident['Number_of_Casualties'],hist=False,color="b", kde_kws={"shade": True}); 
plt.subplot(222) 
Accident['Number_of_Casualties'].plot.box( patch_artist=True,figsize=(16,5)) 

plt.subplot(223) 
sns.distplot(Accident['Local_Authority_(District)'],hist=False,color="b", kde_kws={"shade": True}); 
plt.subplot(224) 
Accident['Local_Authority_(District)'].plot.box( patch_artist=True,figsize=(16,5)) 
plt.show()

plt.figure(3) 
plt.subplot(221) 
sns.distplot(Accident['2nd_Road_Number'],hist=False,color="b", kde_kws={"shade": True}); 
plt.subplot(222) 
Accident['2nd_Road_Number'].plot.box( patch_artist=True,figsize=(16,5)) 
plt.show()

# *******************************************************************
# *******************************************************************

# Date AND Time

# Converting Date -> DataType
Accident.Date = pd.to_datetime(Accident.Date)
# Extracting Year From Date
Accident.Date[1].year
# Pandas.apply allow the users to pass a function and 
## apply it on every single value of the Pandas series.
Accident['Date_Year'] = Accident.Date.apply(lambda x: x.year)
Accident['Date_Month'] = Accident.Date.apply(lambda x: x.month)


# Converting Time -> DataType
Accident.Time = pd.to_datetime(Accident.Time)
Accident['Time_Hour'] = Accident.Time.apply(lambda x: x.hour)

# Ploting
plt.figure(1) 
plt.subplot(221) 
Accident['Date_Year'].value_counts(normalize = True).plot.bar(figsize=(20,10),title = 'Date_Year')
plt.subplot(222)
Accident['Date_Month'].value_counts(normalize = True).plot.bar(figsize=(20,10),title = 'Date_Month')
plt.subplot(223)
Accident['Day_of_Week'].value_counts(normalize = True).plot.bar(title = 'Day_of_week')
plt.subplot(224)
Accident['Time_Hour'].value_counts(normalize = True).plot.bar(figsize=(20,10),title = 'Time_Hour')
plt.show()

# *******************************************************************
# *******************************************************************

# Bi-Variate Analysis

# Categorical Independent Variable vs Target Variable

#Day_of_Week
Day_of_Week = pd.crosstab(Accident['Day_of_Week'],Accident['Accident_Severity'])
# Percentage wise
for i in range(7):
    Day_of_Week.iloc[i] = Day_of_Week.iloc[i,:] / Day_of_Week.iloc[i].sum()

Day_of_Week.plot(kind="bar", stacked=True, figsize=(4,4))

#Police_Force
Police_Force = pd.crosstab(Accident['Police_Force'],Accident['Accident_Severity'])
# Percentage wise
for i in range(51):
    Police_Force.iloc[i] = Police_Force.iloc[i,:] / Police_Force.iloc[i].sum()
Police_Force.plot(kind="bar", stacked=True, figsize=(4,4))

#Number_of_Vehicles
Number_of_Vehicles = pd.crosstab(Accident['Number_of_Vehicles'],Accident['Accident_Severity'])
# Percentage wise
for i in range(28):
    Number_of_Vehicles.iloc[i] = Number_of_Vehicles.iloc[i,:] / Number_of_Vehicles.iloc[i].sum()

Number_of_Vehicles.plot(kind="bar", stacked=True, figsize=(4,4))

#Number_of_Casualties
Number_of_Casualties = pd.crosstab(Accident['Number_of_Casualties'],Accident['Accident_Severity'])
# Percentage wise
for i in range(28):
    Number_of_Casualties.iloc[i] = Number_of_Casualties.iloc[i,:] / Number_of_Casualties.iloc[i].sum()

Number_of_Casualties.plot(kind="bar", stacked=True, figsize=(4,4))

#Pedestrian_Crossing-Physical_Facilities
Pedestrian_Crossing_Physical_Facilities = pd.crosstab(Accident['Pedestrian_Crossing-Physical_Facilities'],Accident['Accident_Severity'])
# Percentage wise
for i in range(7):
    Pedestrian_Crossing_Physical_Facilities.iloc[i] = Pedestrian_Crossing_Physical_Facilities.iloc[i,:] / Pedestrian_Crossing_Physical_Facilities.iloc[i].sum()

Pedestrian_Crossing_Physical_Facilities.plot(kind="bar", stacked=True, figsize=(4,4))

#Pedestrian_Crossing-Human_Control
Pedestrian_Crossing_Human_Control = pd.crosstab(Accident['Pedestrian_Crossing-Human_Control'],Accident['Accident_Severity'])
# Percentage wise
for i in range(4):
    Pedestrian_Crossing_Human_Control.iloc[i] = Pedestrian_Crossing_Human_Control.iloc[i,:] / Pedestrian_Crossing_Human_Control.iloc[i].sum()

Pedestrian_Crossing_Human_Control.plot(kind="bar", stacked=True, figsize=(4,4))

#Light_Conditions
Light_Conditions = pd.crosstab(Accident['Light_Conditions'],Accident['Accident_Severity'])
# Percentage wise
for i in range(5):
    Light_Conditions.iloc[i] = Light_Conditions.iloc[i,:] / Light_Conditions.iloc[i].sum()

Light_Conditions.plot(kind="bar", stacked=True, figsize=(4,4))

#Weather_Conditions
Weather_Conditions = pd.crosstab(Accident['Weather_Conditions'],Accident['Accident_Severity'])
# Percentage wise
for i in range(10):
    Weather_Conditions.iloc[i] = Weather_Conditions.iloc[i,:] / Weather_Conditions.iloc[i].sum()

Weather_Conditions.plot(kind="bar", stacked=True, figsize=(4,4))

#Road_Surface_Conditions
Road_Surface_Conditions = pd.crosstab(Accident['Road_Surface_Conditions'],Accident['Accident_Severity'])
# Percentage wise
for i in range(6):
    Road_Surface_Conditions.iloc[i] = Road_Surface_Conditions.iloc[i,:] / Road_Surface_Conditions.iloc[i].sum()

Road_Surface_Conditions.plot(kind="bar", stacked=True, figsize=(4,4))

#Special_Conditions_at_Site
Special_Conditions_at_Site = pd.crosstab(Accident['Special_Conditions_at_Site'],Accident['Accident_Severity'])
# Percentage wise
for i in range(9):
    Special_Conditions_at_Site.iloc[i] = Special_Conditions_at_Site.iloc[i,:] / Special_Conditions_at_Site.iloc[i].sum()

Special_Conditions_at_Site.plot(kind="bar", stacked=True, figsize=(4,4))

#Carriageway_Hazards
Carriageway_Hazards = pd.crosstab(Accident['Carriageway_Hazards'],Accident['Accident_Severity'])
# Percentage wise
for i in range(7):
    Carriageway_Hazards.iloc[i] = Carriageway_Hazards.iloc[i,:] / Carriageway_Hazards.iloc[i].sum()

Carriageway_Hazards.plot(kind="bar", stacked=True, figsize=(4,4))

#Urban_or_Rural_Area
Urban_or_Rural_Area = pd.crosstab(Accident['Urban_or_Rural_Area'],Accident['Accident_Severity'])
# Percentage wise
for i in range(3):
    Urban_or_Rural_Area.iloc[i] = Urban_or_Rural_Area.iloc[i,:] / Urban_or_Rural_Area.iloc[i].sum()

Urban_or_Rural_Area.plot(kind="bar", stacked=True, figsize=(4,4))

#1st Road Class
Road_Class = pd.crosstab(Accident['1st_Road_Class'],Accident['Accident_Severity'])
# Percentage wise
for i in range(3):
    Road_Class.iloc[i] = Road_Class.iloc[i,:] / Road_Class.iloc[i].sum()

Road_Class.plot(kind="bar", stacked=True, figsize=(4,4))

#1st Road Number
Road_Number = pd.crosstab(Accident['1st_Road_Number'],Accident['Accident_Severity'])
# Percentage wise
for i in range(7062):
    Road_Number.iloc[i] = Road_Number.iloc[i,:] / Road_Number.iloc[i].sum()

Road_Number.plot(kind="bar", stacked=True, figsize=(4,4))

#Road Type
Road_Type = pd.crosstab(Accident['Road_Type'],Accident['Accident_Severity'])
# Percentage wise
for i in range(6):
    Road_Type.iloc[i] = Road_Type.iloc[i,:] / Road_Type.iloc[i].sum()

Road_Type.plot(kind="bar", stacked=True, figsize=(4,4))

#Speed limit
Speed_limit = pd.crosstab(Accident['Speed_limit'],Accident['Accident_Severity'])
# Percentage wise
for i in range(9):
    Speed_limit.iloc[i] = Speed_limit.iloc[i,:] / Speed_limit.iloc[i].sum()

Speed_limit.plot(kind="bar", stacked=True, figsize=(4,4))

#Junction Detail
Junction_Detail = pd.crosstab(Accident['Junction_Detail'],Accident['Accident_Severity'])
# Percentage wise
for i in range(10):
    Junction_Detail.iloc[i] = Junction_Detail.iloc[i,:] / Junction_Detail.iloc[i].sum()

Junction_Detail.plot(kind="bar", stacked=True, figsize=(4,4))

#Junction_Control
Junction_Control = pd.crosstab(Accident['Junction_Control'],Accident['Accident_Severity'])
# Percentage wise
for i in range(6):
    Junction_Control.iloc[i] = Junction_Control.iloc[i,:] / Junction_Control.iloc[i].sum()

Junction_Control.plot(kind="bar", stacked=True, figsize=(4,4))

#2nd_Road_Class
Road_Class = pd.crosstab(Accident['2nd_Road_Class'],Accident['Accident_Severity'])
# Percentage wise
for i in range(7):
    Road_Class.iloc[i] = Road_Class.iloc[i,:] / Road_Class.iloc[i].sum()

Road_Class.plot(kind="bar", stacked=True, figsize=(4,4))

#Did_Police_Officer_Attend_Scene_of_Accident
Did_Police_Officer_Attend_Scene_of_Accident = pd.crosstab(Accident['Did_Police_Officer_Attend_Scene_of_Accident'],Accident['Accident_Severity'])
# Percentage wise
for i in range(4):
    Did_Police_Officer_Attend_Scene_of_Accident.iloc[i] = Did_Police_Officer_Attend_Scene_of_Accident.iloc[i,:] / Did_Police_Officer_Attend_Scene_of_Accident.iloc[i].sum()

Did_Police_Officer_Attend_Scene_of_Accident.plot(kind="bar", stacked=True, figsize=(4,4))

# Date_Month
Date_Month_Accident = pd.crosstab(Accident.Date_Month,Accident.Accident_Severity)
for i in range(12):
    Date_Month_Accident.iloc[i] = Date_Month_Accident.iloc[i,:]/Date_Month_Accident.iloc[i].sum()

Date_Month_Accident.plot(kind="bar", stacked=True, figsize=(4,4))


# *******************************************************************
# *******************************************************************

# Correlation Matrix
corr = Accident.corr() 

#Generating a mask for the upper triangle
mask = np.zeros_like(corr, dtype=np.bool)
mask[np.triu_indices_from(mask)] = True

#Setting up the matplotlib figure
f, ax = plt.subplots(figsize=(8,8))

#Generating a custom diverging colormap
cmap = sns.diverging_palette(220,10, as_cmap=True)

#Drawing the heatmap with the mask
sns.heatmap(corr, cmap=cmap, square=True,mask=mask, linewidths=.5, cbar_kws={"shrink": .5}, ax=ax)

plt.title("Correlation Matrix")
plt.show()
# *******************************************************************
# *******************************************************************

# Missing Value Treatment

# Count of Missing Values
null_percent = Accident.isnull().sum()/Accident.shape[0]
null_total = Accident.isnull().sum()
missing_value = pd.concat([null_total,null_percent],axis = 1,keys=['null_total','null_percentage'])

# Dropping rows containing any missing values
Accident = Accident.dropna()

# *******************************************************************
# Since in this dataset missing values represent -1 values
#Junction_Control
pd.value_counts(Accident['Junction_Control'].values) # 641392
# Dropping
Accident = Accident[ Accident.Junction_Control != -1 ]


#Weather_Conditions
pd.value_counts(Accident['Weather_Conditions'].values)
# Imputing
Accident.Weather_Conditions[Accident.Weather_Conditions == -1] = 1


# Road_Surface_conditions
pd.value_counts(Accident['Road_Surface_Conditions'].values)
# Dropping
Accident = Accident[ Accident.Road_Surface_Conditions != -1 ]


#Special_Conditions_at_Site
pd.value_counts(Accident['Special_Conditions_at_Site'].values)
# Dropping
Accident = Accident[ Accident.Special_Conditions_at_Site != -1 ]


#Carriageway_Hazards
pd.value_counts(Accident['Carriageway_Hazards'].values)
# Dropping
Accident = Accident[ Accident.Carriageway_Hazards != -1 ]

# *******************************************************************
# *******************************************************************

# Model Building

# Dropping Unecessary Columns
Accident.drop(Accident.columns[[0,1,2,3,4,9,11,13,31]], axis=1, inplace=True)

# Seperating Dependent & Independent Variable
Y = Accident.Accident_Severity
X = Accident.drop(Accident.columns[[1]],axis = 1)

# Dummy Variables
#X=pd.get_dummies(X) 
 
# Splitting Dataset
from sklearn.model_selection import train_test_split
x_train, x_cv, y_train, y_cv = train_test_split(X,Y, test_size =0.3)

# *******************************************************************

# Random Forest
from sklearn.ensemble import RandomForestClassifier
classifier = RandomForestClassifier(n_estimators = 10, criterion = 'entropy', random_state = 0)
classifier.fit(x_train, y_train)



importances = classifier.feature_importances_
std = np.std([tree.feature_importances_ for tree in classifier.estimators_],
             axis=0)
indices = np.argsort(importances)[::-1]

# Print the feature ranking
print("Feature ranking:")

for f in range(X.shape[1]):
    print("%d. feature %d (%f)" % (f + 1, indices[f], importances[indices[f]]))

# Plot the feature importances of the forest
plt.figure()
plt.title("Feature importances")
plt.bar(range(X.shape[1]), importances[indices],
       color="r", yerr=std[indices], align="center")
plt.xticks(range(X.shape[1]), indices)
plt.xlim([-1, X.shape[1]])
plt.show()

# Predicting the Test set results
y_pred = classifier.predict(x_cv)

# Making the Classification report
from sklearn.metrics import classification_report
target_names = ['class 1', 'class 2', 'class 3']
print(classification_report(y_cv, y_pred, target_names=target_names))

# Making the Accuracy Score
from sklearn.metrics import accuracy_score
accuracy_score(y_cv, y_pred)
        
# *******************************************************************

# ExtraTree Classifier
from sklearn.ensemble import ExtraTreesClassifier
                            
Extr = ExtraTreesClassifier(n_estimators=5,n_jobs=4)
Extr.fit(x_train, y_train)

# Predicting the Test set results
y_pred = Extr.predict(x_cv)


# Making the Classification report
from sklearn.metrics import classification_report
target_names = ['class 1', 'class 2', 'class 3']
print(classification_report(y_cv, y_pred, target_names=target_names))

# Making the Accuracy Score
from sklearn.metrics import accuracy_score
accuracy_score(y_cv, y_pred)

# ******************************************************************

# AdaBoost 
from sklearn.tree import DecisionTreeClassifier
from sklearn.ensemble import AdaBoostClassifier

Adab= AdaBoostClassifier(DecisionTreeClassifier(max_depth=3),n_estimators=5)
Adab.fit(x_train, y_train)

# Predicting the Test set results
y_pred = Adab.predict(x_cv)

# Making the Classification report
from sklearn.metrics import classification_report
target_names = ['class 1', 'class 2', 'class 3']
print(classification_report(y_cv, y_pred, target_names=target_names))

# Making the Accuracy Score
from sklearn.metrics import accuracy_score
accuracy_score(y_cv, y_pred)






