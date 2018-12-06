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
cv = CountVectorizer(max_features = 40000)
X = cv.fit_transform(corpus).toarray()

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
X_train, X_test, y_train, y_test = train_test_split(X, Y, test_size = 0.20,random_state = 0)

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


#*****************************************************************************
#*****************************************************************************

# *** Applying Machine Learning Technique #2 ***

# Fitting LOGISTIC REGRESSION to the Training set
from sklearn.linear_model import LogisticRegression
classifier_2 = LogisticRegression(random_state = 0)
classifier_2.fit(X_train, y_train)

# Predicting the Test set results
y_pred = classifier_2.predict(X_test)

# Accuracy of the model
acc_score(y_test, y_pred )


#******************************************************************************
#******************************************************************************











