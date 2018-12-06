# Importing the libraries
import numpy as np
import pandas as pd

# Importing the dataset
dataset = pd.read_csv('train.csv')
# A quick look at the data
dataset.info()
# finding the dimensions
print(dataset.shape)

# Combining Both title and text
dataset['total']=dataset['author']+' '+dataset['title']+' '+dataset['text']

#******************************************************************************
#******************************************************************************

# Cleaning the texts
import re
import nltk
nltk.download('stopwords')
from nltk.corpus import stopwords
from nltk.stem.porter import PorterStemmer
corpus = []
for i in range(20800):
    # REMOVE PUNTUATIONS AND ANY CHARACTER OTHER THAN ALPHABET
    review = re.sub('[^a-zA-Z]', ' ', str(dataset['total'][i]))
    review = review.lower()
    review = review.split()
    # Stemming object
    ps = PorterStemmer()
    # Stemming + removing stopwords
    review = [ps.stem(word) for word in review if not word in \
              set(stopwords.words('english'))]
    review = ' '.join(review)
    corpus.append(review)

#******************************************************************************
#******************************************************************************

# Model 1
    
# Creating the Bag of Words model

from sklearn.feature_extraction.text import CountVectorizer
cv = CountVectorizer(max_features = 14000)
X = cv.fit_transform(corpus).toarray()
Y = dataset.iloc[:,4].values

# Splitting the dataset into the Training set and Test set
from sklearn.model_selection import train_test_split
X_train, X_test, y_train, y_test = train_test_split(X, Y, test_size = 0.20,random_state = 0)

#******************************************************************************
#******************************************************************************

# Model 2

# TF-IDF MODEL

from sklearn.feature_extraction.text import TfidfVectorizer
tf = TfidfVectorizer(analyzer='word', ngram_range=(1,3), min_df = 5)
X =  tf.fit_transform(corpus)
feature_names = tf.get_feature_names()

Y = dataset.iloc[:,4].values

# Splitting the dataset into the Training set and Test set
from sklearn.model_selection import train_test_split
X_train, X_test, y_train, y_test = train_test_split(X, Y, test_size = 0.20,random_state = 0)

# ***Only one of the 2 models should be runned at a time

#******************************************************************************
#******************************************************************************

# *** Applying Machine Learning Technique #1 ***

# Fitting Naive Bayes to the Training set
from sklearn.naive_bayes import MultinomialNB
NB = MultinomialNB()
NB.fit(X_train, y_train)

# Accuracy Score
print('Accuracy of NB classifier on test set:%0.04f'
      %(NB.score(X_test, y_test)))

# 0.8137 tfidf ,min_df = 0
# 0.8837 tfidf ,min_df = 2
# 0.8904 tfidf ,min_df = 5 and n_gram = 1 to 3

# 0.9226 BOW

#*****************************************************************************
#*****************************************************************************

# *** Applying Machine Learning Technique #2 ***

# Fitting LOGISTIC REGRESSION to the Training set
from sklearn.linear_model import LogisticRegression
LR = LogisticRegression(random_state = 0)
LR.fit(X_train, y_train)

# Predicting the Test set results
y_pred = LR.predict(X_test)

# Accuracy Score
print('Accuracy of LR classifier on test set:%0.04f'
      %(LR.score(X_test, y_test)))

# Accuracy of NB classifier on test set:0.9663

#******************************************************************************
#******************************************************************************

# *** Applying Machine Learning Technique #3 ***

from sklearn.ensemble import RandomForestClassifier
Rando= RandomForestClassifier(n_estimators=5)

from pprint import pprint
# Look at parameters used by our current forest
print('Parameters currently in use:\n')
pprint(Rando.get_params())  


classifier = Rando.fit(X_train, y_train)

print('Accuracy of Extratrees classifier on test set: %0.04f'
     %(Rando.score(X_test, y_test)))

# 0.8524

#******************************************************************************

# HYPERPARAMETER OPTIMIZATION --> GRID SEARCH <--

from sklearn.model_selection import GridSearchCV

# parameters for GridSearchCV
param_grid = {"n_estimators": [5,6,7,8],
              "max_depth": [2,3,4],
              "min_samples_split": [2,3,4,5],
              "min_samples_leaf": [1,2,3],
              "max_leaf_nodes": [20, 40,],
              }

grid_search = GridSearchCV(estimator = classifier,
                           param_grid = param_grid,
                           scoring = 'accuracy',
                           cv = 5,
                           n_jobs = -1)

grid_search.fit(X_train,y_train)

best_accuracy = grid_search.best_score_
best_parameters = grid_search.best_params_

print(" BEST ACCURACY IS :%0.04f" %(best_accuracy))
print(" BEST PARAMETERS IS :\n" ,best_parameters)


#******************************************************************************
#******************************************************************************

# *** Applying Machine Learning Technique #4 ***

from sklearn.ensemble import ExtraTreesClassifier
                            
Extr = ExtraTreesClassifier(n_estimators=5,n_jobs=4)

from pprint import pprint
# Look at parameters used by our current forest
print('Parameters currently in use:\n')
pprint(Extr.get_params())

Extr.fit(X_train, y_train)

print('Accuracy of Extratrees classifier on test set: %0.04f'
     %(Extr.score(X_test, y_test)))
# 0.9257

#******************************************************************************
#******************************************************************************

# *** Applying Machine Learning Technique #5 ***

from sklearn.tree import DecisionTreeClassifier
from sklearn.ensemble import AdaBoostClassifier

Adab= AdaBoostClassifier(DecisionTreeClassifier(max_depth=3),n_estimators=5)

from pprint import pprint
# Look at parameters used by our current forest
print('Parameters currently in use:\n')
pprint(Adab.get_params())

Adab.fit(X_train, y_train)

print('Accuracy of Extratrees classifier on test set: %0.04f'
     %(Adab.score(X_test, y_test)))

# 0.9226

#******************************************************************************
#******************************************************************************

# *** Applying Machine Learning Technique #6 ***

# XGBOOST METHOD

import xgboost as xgb
classifier = xgb.XGBClassifier(max_depth=7, 
                               n_estimators=200, 
                               colsample_bytree=0.8, 
                               subsample=0.8, 
                               nthread=10, 
                               learning_rate=0.1)

from pprint import pprint
# Look at parameters used by our current Model
print('Parameters currently in use:\n')
pprint(classifier.get_params())


from scipy.sparse import csc_matrix
# Converting to sparse data and running xgboost
X_train_csc = csc_matrix(X_train)
X_test_csc = csc_matrix(X_test)

classifier.fit(X_train_csc, y_train)
y_pred = classifier.predict(X_test_csc)



from sklearn.metrics import accuracy_score
acc = accuracy_score(y_test, y_pred)
print("Accuracy: %.4f%%" % (acc * 100.0))



#******************************************************************************
#******************************************************************************

# WORD CLOUD
# conda install -c conda-forge wordcloud

import matplotlib.pyplot as plt
from wordcloud import WordCloud, STOPWORDS

stopwords = set(STOPWORDS)
new_words = ['NaN']
new_stopwords=stopwords.union(new_words)
wordcloud = WordCloud(
                          background_color='white',
                          stopwords=new_stopwords,
                          max_words=200,
                          max_font_size=80,min_font_size=25, 
                          random_state=42,
                          width=1100, height=700, margin=0
                         ).generate(str(dataset['total']))


plt.imshow(wordcloud,interpolation='bilinear')
plt.axis("off")
plt.margins(x=0, y=0)
plt.savefig('wc_2.png',dpi = 200)
# plt.show() must be after plt.savefig() as clears the whole thing, 
# so anything afterwards  will happen on a new empty figure.
plt.show()

#******************************************************************************
#******************************************************************************




