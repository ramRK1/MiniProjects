#!/usr/bin/env python
# coding: utf-8

# # Accident Analysis

# ## Importing Pakages

# In[5]:


import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
get_ipython().run_line_magic('matplotlib', 'inline')
import seaborn as sns
import warnings
warnings.filterwarnings("ignore")


# ## Importing Files

# In[8]:


Accident = pd.read_csv("C:/LOCAL_R/Rishabh##/Machine Learning/data SET/accident dataset/Accidents0515.csv") 
#Casualties = pd.read_csv("C:\LOCAL_R\Rishabh##\Machine Learning\data SET\accident dataset\Casualties0515.csv") 
#Vehicles0515 = pd.read_csv("C:\LOCAL_R\Rishabh##\Machine Learning\data SET\accident dataset\Vehicles0515.csv") 


# ## Understanding the Data

# In[9]:


Accident.head()


# In[17]:


Accident.shape


# In[18]:


Accident.dtypes


# In[36]:


d = Accident.shape
d[0]
Accident.isnull().sum()/d[0]


# In[24]:


Accident.describe()


# ## Univariate Analysis

# In[50]:


# Analysing target variable
Accident['Accident_Severity'].value_counts(normalize = True).plot.bar()


# In[71]:


# Independent Variable (Categorical)
plt.figure(1)
plt.subplot(221)
Accident['Pedestrian_Crossing-Human_Control'].value_counts(normalize = True).plot.bar(figsize=(20,10),title = 'Pedestrian_Crossing-Human_Control')
#plt.show()
plt.subplot(222)
Accident['Pedestrian_Crossing-Physical_Facilities'].value_counts(normalize = True).plot.bar(title = 'Pedestrian_Crossing-Physical_Facilities')
#plt.show()
plt.subplot(223)
Accident['Day_of_Week'].value_counts(normalize = True).plot.bar(title = 'Day_of_week')
#plt.show()
plt.subplot(224)
Accident['Light_Conditions'].value_counts(normalize = True).plot.bar(title = 'Light_Conditions')
plt.show()

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

plt.figure(3)
plt.subplot(221)
Accident['Urban_or_Rural_Area'].value_counts(normalize = True).plot.bar(figsize=(20,10),title = 'Urban_or_Rural_Area')
#plt.show()
plt.subplot(222)
Accident['Did_Police_Officer_Attend_Scene_of_Accident'].value_counts(normalize = True).plot.bar(title = 'Did_Police_Officer_Attend_Scene_of_Accident')
plt.show()


# In[86]:


# Independent Variable (Continuous)

plt.figure(1) 
plt.subplot(121) 
sns.distplot(Accident['Police_Force'],hist=False,color="b", kde_kws={"shade": True}); 
plt.subplot(122) 
Accident['Police_Force'].plot.box( patch_artist=True,figsize=(16,5)) 
plt.show()

plt.figure(2) 
plt.subplot(121) 
sns.distplot(Accident['Number_of_Vehicles'],hist=False,color="b", kde_kws={"shade": True}); 
plt.subplot(122) 
Accident['Number_of_Vehicles'].plot.box( patch_artist=True,figsize=(16,5)) 
plt.show()

plt.figure(3) 
plt.subplot(121) 
sns.distplot(Accident['Number_of_Casualties'],hist=False,color="b", kde_kws={"shade": True}); 
plt.subplot(122) 
Accident['Number_of_Casualties'].plot.box( patch_artist=True,figsize=(16,5)) 
plt.show()

plt.figure(4) 
plt.subplot(121) 
sns.distplot(Accident['Local_Authority_(District)'],hist=False,color="b", kde_kws={"shade": True}); 
plt.subplot(122) 
Accident['Local_Authority_(District)'].plot.box( patch_artist=True,figsize=(16,5)) 
plt.show()

plt.figure(5) 
plt.subplot(121) 
sns.distplot(Accident['2nd_Road_Number'],hist=False,color="b", kde_kws={"shade": True}); 
plt.subplot(122) 
Accident['2nd_Road_Number'].plot.box( patch_artist=True,figsize=(16,5)) 
plt.show()


# In[101]:


sns.distplot(Accident[Accident.columns[5]],hist=False,color="b", kde_kws={"shade": True}); 
plt.subplot(122) 
Accident[Accident.columns[5]].plot.box( patch_artist=True,figsize=(16,5)) 
plt.show()


# In[100]:


Accident.columns[0]


# In[88]:


Accident.dtypes

