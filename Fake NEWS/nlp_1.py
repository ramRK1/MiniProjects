# Importing the libraries
import numpy as np
import pandas as pd

# Importing the dataset
dataset = pd.read_csv('fake_or_real_news.csv')
# A quick look at the data
dataset.info()
# finding the dimensions
print(dataset.shape)

# Combining Both title and text
dataset['total']=dataset['title']+' '+dataset['text']


#******************************************************************************
#******************************************************************************
#******************************************************************************

# Cleaning the texts
import re
import nltk
nltk.download('stopwords')
from nltk.corpus import stopwords
from nltk.stem.porter import PorterStemmer
corpus = []
for i in range(6335):
    # REMOVE PUNTUATIONS AND ANY CHARACTER OTHER THAN ALPHABET
    review = re.sub('[^a-zA-Z]', ' ', dataset['total'][i])
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
#******************************************************************************
    
# Creating BAG OF WORDS MODEL :
    
from sklearn.feature_extraction.text import CountVectorizer
cv = CountVectorizer(max_features = 40000)
X = cv.fit_transform(corpus).toarray()

#*****************************************************************************

# Creating TF-IDF MODEL :

from sklearn.feature_extraction.text import TfidfVectorizer
tf = TfidfVectorizer(analyzer='word', ngram_range=(1,3), min_df = 2)
X =  tf.fit_transform(corpus)
feature_names = tf.get_feature_names()

#*****************************************************************************

Y = dataset.iloc[:,3].values
# Encoding categorical data
from sklearn.preprocessing import LabelEncoder
labelencoder = LabelEncoder()
Y = labelencoder.fit_transform(Y)

# Splitting the dataset into the Training set and Test set
from sklearn.model_selection import train_test_split
X_train, X_test, y_train, y_test = train_test_split(X, Y, test_size = 0.20,random_state = 0)

#*****************************************************************************
#*****************************************************************************

from sklearn.metrics import accuracy_score
from sklearn.metrics import log_loss
from sklearn.metrics import confusion_matrix

# PREDICTION : Accuracy Score
def acc_score(y_test, y_pred ) :
    acc_AS = accuracy_score(y_test, y_pred)
    return print("Final Accuracy: %0.04f" %(acc_AS))

# PREDICTION : Log-Loss
def log_loss(y_test, y_pred ) :
    acc_LL = log_loss(y_test, y_pred)
    return print("Final Accuracy: %0.04f" %(acc_LL))    

# PREDICTION : Confusion Matrix
def conf_matrix(y_test, y_pred ) :
    acc = confusion_matrix(y_test, y_pred)
    acc_CM = np.sum(np.diagonal(np.asarray(acc)))/np.sum(acc)
    return print("Final Accuracy # Confusion_Matrix: %0.04f" %(acc_CM))

# acc_score(y_test, y_pred )d
# log_loss(y_test, y_pred )
# conf_matrix(y_test, y_pred )

#*****************************************************************************
#*****************************************************************************

# Fitting NAIVE BAYES to the Training set

from sklearn.naive_bayes import MultinomialNB
classifier_1 = MultinomialNB()
classifier_1.fit(X_train, y_train)

# Predicting the Test set results
y_pred = classifier_1.predict(X_test)

# Accuracy of the model
acc_score(y_test, y_pred )
conf_matrix(y_test, y_pred )
# Final Accuracy: 0.8840 BOW Model
# Final Accuracy: 0.8264 tf-idf min_df = 0 & ngram =(1,3)
# Final Accuracy: 0.8350 tf-idf min_df = 2 & ngram =(1,3)

#*****************************************************************************

# Fitting LOGISTIC REGRESSION to the Training set
from sklearn.linear_model import LogisticRegression
classifier_2 = LogisticRegression(random_state = 0)
classifier_2.fit(X_train, y_train)

# Predicting the Test set results
y_pred = classifier_2.predict(X_test)

# Accuracy of the model
acc_score(y_test, y_pred )
# Final Accuracy: 0.9163 BOW Model

#******************************************************************************
#******************************************************************************

# Apply SVD

from sklearn import decomposition
from sklearn.decomposition import TruncatedSVD

svd = decomposition.TruncatedSVD(n_components=200)
svd.fit(X_train)

X_train_svd = svd.transform(X_train)
X_test_svd = svd.transform(X_test)

# Scale the data obtained from SVD. Renaming variable to reuse without scaling.
from sklearn import preprocessing
scale = preprocessing.StandardScaler()
scale.fit(X_train_svd)

X_train_svd_scale = scale.transform(X_train_svd)
X_test_svd_scale = scale.transform(X_test_svd)

#******************************************************************************
# Fitting a simple SVM

from sklearn.svm import SVC

classifier_svm = SVC(C=1.0, probability=True) # since we need probabilities
classifier_svm.fit(X_train_svd_scale, y_train)
y_pred = classifier_svm.predict_proba(X_test_svd_scale)

# Accuracy of the model

log_loss(y_test, y_pred )

# Final Accuracy: 0.4687

#******************************************************************************

# Fitting Logistic Regression to the Training SVD SET
from sklearn.linear_model import LogisticRegression
classifier_1_tfidf = LogisticRegression(random_state = 0)
classifier_1_tfidf.fit(X_train_svd_scale, y_train)

# Predicting the Test set results
y_pred = classifier_1_tfidf.predict(X_test_svd_scale)

# Accuracy of the model

log_loss(y_test, y_pred )

# Final Accuracy: 0.7672

#******************************************************************************

# XGBOOST METHOD

import xgboost as xgb
classifier = xgb.XGBClassifier(max_depth=7, 
                               n_estimators=200, 
                               colsample_bytree=0.8, 
                               subsample=0.8, 
                               nthread=10, 
                               learning_rate=0.1)

from pprint import pprint
# Look at parameters used by our current forest
print('Parameters currently in use:\n')
pprint(classifier.get_params())


from scipy.sparse import csc_matrix
# Converting to sparse data and running xgboost
X_train_csc = csc_matrix(X_train)
X_test_csc = csc_matrix(X_test)

classifier.fit(X_train_csc, y_train)
y_pred = classifier.predict_proba(X_test_csc)

# Accuracy of the model
log_loss(y_test, y_pred )

# Final Accuracy: 0.4558

#********************************************************************
"""
# GRID SEARCH

from sklearn import model_selection, metrics, pipeline
from sklearn.model_selection import GridSearchCV

from sklearn.metrics import log_loss
def logloss(y_test, predictions):
    return log_loss(y_test, predictions)
     

	# create a scoring function
scorer = metrics.make_scorer(logloss, 
                                 greater_is_better=False, needs_proba=True)
#using a pipeline consisting of SVD, scaling and then logistic regression

from sklearn import decomposition ,preprocessing
from sklearn.decomposition import TruncatedSVD
from sklearn.linear_model import LogisticRegression
# Initialize SVD
svd = TruncatedSVD()
# Initialize the standard scaler 
scl = preprocessing.StandardScaler()
# We will use logistic regression here..
lr_model = LogisticRegression()

# Create the pipeline 
clf = pipeline.Pipeline([('svd', svd),
                         ('scl', scl),
                         ('lr', lr_model)])


# grid of parameters:
param_grid = {'svd__n_components' : [120, 180],
              'lr__C': [0.1, 1.0, 10], 
              'lr__penalty': ['l1', 'l2']}    

# Initialize Grid Search Model
model = GridSearchCV(estimator=clf, param_grid=param_grid, scoring=scorer,
                     verbose=10, n_jobs=-1, iid=True, refit=True, cv=2)

# Fit Grid Search Model
model.fit(X_train, y_train)
# we can use the full data here but im only using X_train
print("Best score: %0.3f" % model.best_score_)
print("Best parameters set:")
best_parameters = model.best_estimator_.get_params()

for param_name in sorted(param_grid.keys()):
    print("\t%s: %r" % (param_name, best_parameters[param_name]))
"""
#******************************************************************************
#******************************************************************************
#******************************************************************************

# WORD CLOUD
# conda install -c conda-forge wordcloud

import matplotlib.pyplot as plt
from wordcloud import WordCloud, STOPWORDS

stopwords = set(STOPWORDS)
wordcloud = WordCloud(
                          background_color='white',
                          stopwords=stopwords,
                          max_words=200,
                          max_font_size=80,min_font_size=20, 
                          random_state=42,
                          width=1100, height=700, margin=0
                         ).generate(str(dataset['total']))


plt.imshow(wordcloud,interpolation='bilinear')
plt.axis("off")
plt.margins(x=0, y=0)
plt.savefig('wc_1.png',dpi = 200)
# plt.show() must be after plt.savefig() as clears the whole thing, 
# so anything afterwards  will happen on a new empty figure.
plt.show()

#******************************************************************************
#******************************************************************************
#******************************************************************************

# Word Vectors

# load the GloVe vectors in a dictionary:
# GloVe: Global Vectors for Word Representation
embeddings_index = {}
f = open('glove.840B.300d.txt')
#Common Crawl (840B tokens, 2.2M vocab, cased, 300d vectors, 2.03 GB download)
# link :::: https://nlp.stanford.edu/data/wordvecs/glove.840B.300d.zip\
"""
from tqdm import tqdm
for line in tqdm(f):
    values = line.split()
    word = values[0]
    coefs = np.asarray(values[1:], dtype='float32')
    embeddings_index[word] = coefs
f.close()

print('Found %s word vectors.' % len(embeddings_index))
"""

#******************************************************************************

# LSTM

from keras.models import Model
from keras.layers import LSTM, Activation, Dense, Dropout, Input, Embedding
from keras.optimizers import RMSprop
from keras.preprocessing.text import Tokenizer
from keras.preprocessing import sequence
from keras.utils import to_categorical
from keras.callbacks import EarlyStopping



# RNN
max_words = 2000
max_len = 200
tok = Tokenizer(num_words=max_words)
tok.fit_on_texts(X_train_svd)
sequences = tok.texts_to_sequences(X_train_svd)
sequences_matrix = sequence.pad_sequences(X_train_svd,maxlen=max_len)

def RNN():
    inputs = Input(name='inputs',shape=[max_len])
    layer = Embedding(max_words,50,input_length=max_len)(inputs)
    layer = LSTM(64)(layer)
    layer = Dense(256,name='FC1')(layer)
    layer = Activation('relu')(layer)
    layer = Dropout(0.5)(layer)
    layer = Dense(1,name='out_layer')(layer)
    layer = Activation('sigmoid')(layer)
    model = Model(inputs=inputs,outputs=layer)
    return model


# Call the function and compile the model.
model = RNN()
model.summary()
model.compile(loss='binary_crossentropy',optimizer=RMSprop(),metrics=['accuracy'])

# Fit on the training data.
model.fit(X_train_svd_scale,y_train,batch_size=128,epochs=15,
          validation_split=0.2,callbacks=[EarlyStopping(monitor='val_loss',
                                                        min_delta=0.0001)])

#  Validation Accuracy: 0.5475

# Process the test set data.

test_sequences_matrix = sequence.pad_sequences(X_test_svd_scale,maxlen=max_len)

# Evaluate the model on the test set
# Accuracy of the model
accr = model.evaluate(test_sequences_matrix,y_test)
print('Test set\n  Loss: {:0.3f}\n  Accuracy: {:0.3f}'.format(accr[0],accr[1]))       


#******************************************************************************

from sklearn.ensemble import ExtraTreesClassifier
                            
Extr = ExtraTreesClassifier(n_estimators=5,n_jobs=4)

from pprint import pprint
# Look at parameters used by our current forest
print('Parameters currently in use:\n')
pprint(Extr.get_params())

Extr.fit(X_train, y_train)

score_ETC = Extr.score(X_test, y_test)
print('Accuracy of Extratrees classifier on test set: %0.04f'%(score_ETC))

# Accuracy of Extratrees classifier on test set: 0.8295
#******************************************************************************
from sklearn.tree import DecisionTreeClassifier
from sklearn.ensemble import AdaBoostClassifier

Adab= AdaBoostClassifier(DecisionTreeClassifier(max_depth=3),n_estimators=5)

from pprint import pprint
# Look at parameters used by our current forest
print('Parameters currently in use:\n')
pprint(Adab.get_params())

Adab.fit(X_train, y_train)

score_ABC = Adab.score(X_test, y_test)
print('Accuracy of Extratrees classifier on test set: %0.04f'
     %(score_ABC))

# Accuracy of Extratrees classifier on test set: 0.8224
#******************************************************************************
#******************************************************************************
from sklearn.ensemble import RandomForestClassifier

Rando= RandomForestClassifier(n_estimators=5)

from pprint import pprint
# Look at parameters used by our current forest
print('Parameters currently in use:\n')
pprint(Rando.get_params())

classifier = Rando.fit(X_train, y_train)

score_RFC = Rando.score(X_test, y_test)
print('Accuracy of Extratrees classifier on test set: %0.04f'
     %(score_RFC))


# Accuracy of Extratrees classifier on test set: 0.8137

from sklearn.model_selection import GridSearchCV

# parameters for GridSearchCV
param_grid = {"n_estimators": [5,6,7,8],
              "max_depth": [2,3,4],
              "min_samples_split": [2,3],
              "min_samples_leaf": [1,2,3],
              "max_leaf_nodes": [20, 40],
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

# SENTIMENT ANALYSIS 
from textblob import TextBlob
l=dataset['total'][1]
text=TextBlob(l)
if(text.sentiment.polarity>0 and text.sentiment.subjectivity>0.5):
    print("\n General Opinion(Subjective) with positive Sentiment ")
if(text.sentiment.polarity<0 and text.sentiment.subjectivity>0.5):
    print("\n General Opinion(Subjective) with Negative Sentiment ")
if(text.sentiment.polarity>0 and text.sentiment.subjectivity<0.5):
    print("\n Personal Opinion(Objective) with positive Sentiment ")
if(text.sentiment.polarity<0 and text.sentiment.subjectivity<0.5):
    print("\n Personal Opinion(Objective) with Negative Sentiment ")
if(text.sentiment.polarity==0):
    print("\n neutral")
    
#******************************************************************************
#******************************************************************************
    
    






