import numpy as np
import pandas as pd

# Importing the dataset
dataset_Fake = pd.read_csv('Fake.csv')
dataset_True = pd.read_csv('True.csv')

# Add Columns 
dataset_Fake["label"] = 0
dataset_True["label"] = 1

# Merge by index
dataset = pd.concat([dataset_True, dataset_Fake])
# Reseting the index
dataset = dataset.set_index([[ i for i in range(0,len(dataset))]])

# A quick look at the data
dataset.info()
# finding the dimensions
print(dataset.shape)

# Combining Both title and text
dataset['total']=dataset['title']+' '+dataset['text']

#******************************************************************************
# TRAINING SET
# Cleaning the texts
import re
import nltk
nltk.download('stopwords')
from nltk.corpus import stopwords
from nltk.stem.porter import PorterStemmer
corpus = []
for i in range(44898):
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
    
# MODEL 1    
# Creating BAG OF WORDS MODEL :
    
from sklearn.feature_extraction.text import CountVectorizer
cv = CountVectorizer(max_features = 88000)
X = cv.fit_transform(corpus)

#*****************************************************************************

# MODEL 2
# Creating TF-IDF MODEL :

from sklearn.feature_extraction.text import TfidfVectorizer
tf = TfidfVectorizer(analyzer='word', ngram_range=(1,3), min_df = 2)
X =  tf.fit_transform(corpus)
feature_names = tf.get_feature_names()

#*****************************************************************************

# Should be run after running any of Model 1 and Model 2

Y = dataset.iloc[:,4].values

# Splitting the dataset into the Training set and Test set
from sklearn.model_selection import train_test_split
X_train, X_test, y_train, y_test = train_test_split(X, Y, test_size = 0.20,
                                                    random_state = 0, stratify=Y)

#*****************************************************************************
#*****************************************************************************
# Model Performance Evaluation Metrices

from sklearn.metrics import accuracy_score
from sklearn.metrics import log_loss
from sklearn.metrics import confusion_matrix

# PREDICTION : Accuracy Score
def acc_score(y_test, y_pred ) :
    acc_AS = accuracy_score(y_test, y_pred)
    return print("Accuracy: %.2f%%" % (acc_AS * 100.0))

# PREDICTION : Log-Loss
def log_loss(y_test, y_pred ) :
    acc_LL = log_loss(y_test, y_pred)
    return print("Accuracy: %.2f%%" % (acc_LL * 100.0))   

# PREDICTION : Confusion Matrix
def conf_matrix(y_test, y_pred ) :
    acc = confusion_matrix(y_test, y_pred)
    acc_CM = np.sum(np.diagonal(np.asarray(acc)))/np.sum(acc)
    return print("Accuracy: %.2f%%" % (acc_CM * 100.0))

# acc_score(y_test, y_pred )
# log_loss(y_test, y_pred )
# conf_matrix(y_test, y_pred )

#*****************************************************************************
#*****************************************************************************

# *** Applying Machine Learning Technique #1 ***

# Fitting NAIVE BAYES to the Training set

from sklearn.naive_bayes import MultinomialNB
classifier_1 = MultinomialNB()
classifier_1.fit(X_train, y_train)

# Predicting the Test set results
y_pred = classifier_1.predict(X_test)

# Accuracy of the model
acc_score(y_test, y_pred )

# Accuracy: 95.38%

#******************************************************************************
#******************************************************************************

# *** Applying Machine Learning Technique #3 ***

from sklearn.ensemble import RandomForestClassifier
Rando= RandomForestClassifier(n_estimators=5)

Rando.fit(X_train, y_train)

print('Accuracy of RandomForest classifier on test set: %0.04f'
     %(Rando.score(X_test, y_test)))

# Accuracy of RandomForest classifier on test set: 0.9027 BOW
# Accuracy of RandomForest classifier on test set: 0.8981 tfidf

#******************************************************************************
#******************************************************************************

# *** Applying Machine Learning Technique #4 ***

from sklearn.ensemble import ExtraTreesClassifier
                            
Extr = ExtraTreesClassifier(n_estimators=5,n_jobs=4)
Extr.fit(X_train, y_train)

print('Accuracy of Extratrees classifier on test set: %0.04f'
     %(Extr.score(X_test, y_test)))

# Accuracy of Extratrees classifier on test set: 0.9004 BOW
# Accuracy of Extratrees classifier on test set: 0.9115 tfidf

#******************************************************************************
#******************************************************************************

# WORD CLOUD
# conda install -c conda-forge wordcloud

import matplotlib.pyplot as plt
from wordcloud import WordCloud

wordcloud = WordCloud(
                          background_color='white',
                          
                          max_words=200,
                          max_font_size=80,min_font_size=10, 
                          random_state=42,
                          width=1100, height=700, margin=0
                         ).generate(str(corpus))


plt.imshow(wordcloud,interpolation='bilinear')
plt.axis("off")
plt.margins(x=0, y=0)
plt.savefig('wc_4.png',dpi = 200)
# plt.show() must be after plt.savefig() as clears the whole thing, 
# so anything afterwards  will happen on a new empty figure.
plt.show()

#******************************************************************************
#******************************************************************************










