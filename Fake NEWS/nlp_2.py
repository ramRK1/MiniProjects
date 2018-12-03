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
# TRAINING SET
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

# TF-IDF MODEL

from sklearn.feature_extraction.text import TfidfVectorizer
tf = TfidfVectorizer(analyzer='word', ngram_range=(1,3), min_df = 5)
X =  tf.fit_transform(corpus)
feature_names = tf.get_feature_names()

Y = dataset.iloc[:,4].values

# Splitting the dataset into the Training set and Test set
from sklearn.model_selection import train_test_split
X_train, X_test, y_train, y_test = train_test_split(X, Y, test_size = 0.20,random_state = 0)


#******************************************************************************
#******************************************************************************

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

# 0.8983 BOW


#******************************************************************************

from sklearn.ensemble import ExtraTreesClassifier
                            
Extr = ExtraTreesClassifier(n_estimators=5,n_jobs=4)
Extr.fit(X_train, y_train)

print('Accuracy of Extratrees classifier on test set: %0.04f'
     %(Extr.score(X_test, y_test)))
# 0.9257
#******************************************************************************
from sklearn.tree import DecisionTreeClassifier
from sklearn.ensemble import AdaBoostClassifier

Adab= AdaBoostClassifier(DecisionTreeClassifier(max_depth=3),n_estimators=5)
Adab.fit(X_train, y_train)

print('Accuracy of Extratrees classifier on test set: %0.04f'
     %(Adab.score(X_test, y_test)))

# 0.9226
#******************************************************************************
from sklearn.ensemble import RandomForestClassifier
Rando= RandomForestClassifier(n_estimators=5)

Rando.fit(X_train, y_train)

print('Accuracy of Extratrees classifier on test set: %0.04f'
     %(Rando.score(X_test, y_test)))

# 0.9252
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




