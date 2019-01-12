# Importing the libraries
import numpy as np
import pandas as pd

# Importing the dataset
dataset = pd.read_csv('train.csv')
dataset_test = pd.read_csv('test.csv')
# A quick look at the data
#dataset.info()
# finding the dimensions
#print(dataset.shape)

# Combining Both title and text
dataset['total']=dataset['author']+' '+dataset['title']+' '+dataset['text']

dataset_test['total']=dataset_test['author']+' '+dataset_test['title']+' '+dataset_test['text']
dataset_test['label'] = 0
#dataset_test = dataset_test.iloc[[2,3,4,5,6,7],:]
#******************************************************************************
#******************************************************************************

# Cleaning the texts
import re
import nltk
nltk.download('stopwords')
from nltk.corpus import stopwords
from nltk.stem.porter import PorterStemmer
corpus_test = []

# REMOVE PUNTUATIONS AND ANY CHARACTER OTHER THAN ALPHABET
for i in range(5200):
    # REMOVE PUNTUATIONS AND ANY CHARACTER OTHER THAN ALPHABET
    review_test = re.sub('[^a-zA-Z]', ' ', str(dataset_test['total'][i]))
    review_test = review_test.lower()
    review_test = review_test.split()
    # Stemming object
    ps = PorterStemmer()
    # Stemming + removing stopwords
    review_test = [ps.stem(word) for word in review_test if not word in \
              set(stopwords.words('english'))]
    review_test = ' '.join(review_test)
    corpus_test.append(review_test)

#******************************************************************************
#******************************************************************************

# Model 1
    
# Creating the Bag of Words model

from sklearn.feature_extraction.text import CountVectorizer
cv = CountVectorizer(max_features = 14000)
X_train = cv.fit_transform(corpus).toarray()
Y_train = dataset.iloc[:,4].values

from sklearn.feature_extraction.text import CountVectorizer
cv = CountVectorizer()
X_test = cv.fit_transform(corpus_test).toarray()
Y_test = dataset_test.iloc[:,5].values

#******************************************************************************
#******************************************************************************
# *** Applying Machine Learning Technique #1 ***

# Fitting LOGISTIC REGRESSION to the Training set
from sklearn.linear_model import LogisticRegression
LR = LogisticRegression(random_state = 0)
LR.fit(X_train, Y_train)

# Predicting the Test set results
y_pred = LR.predict(X_test)

# Accuracy Score
print('Accuracy of LR classifier on test set:%0.04f'
      %(LR.score(X_test, Y_test)))

