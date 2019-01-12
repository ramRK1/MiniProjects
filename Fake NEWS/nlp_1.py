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
y_pred_1 = classifier_1.predict(X_test)

# Accuracy of the model
#acc_score(y_test, y_pred )
#conf_matrix(y_test, y_pred )

from sklearn.metrics import accuracy_score, f1_score, \
precision_score, recall_score, log_loss


Models['Accuracy'][0] = accuracy_score(y_test, y_pred_1)
Models['Precission'][0] =precision_score(y_test, y_pred_1, average="macro")
Models['Recall'][0] =recall_score(y_test, y_pred_1, average="macro")
Models['Log_Loss'][0] =log_loss(y_test, y_pred_1)
Models['F1'][0] =f1_score(y_test, y_pred_1, average="macro")

# Final Accuracy: 0.8840 BOW Model
# Final Accuracy: 0.8264 tf-idf min_df = 0 & ngram =(1,3)
# Final Accuracy: 0.8350 tf-idf min_df = 2 & ngram =(1,3)

#*****************************************************************************
#*****************************************************************************

# *** Applying Machine Learning Technique #2 ***

# Fitting LOGISTIC REGRESSION to the Training set
from sklearn.linear_model import LogisticRegression
classifier_2 = LogisticRegression(random_state = 0)
classifier_2.fit(X_train, y_train)

# Predicting the Test set results
y_pred_2 = classifier_2.predict(X_test)

# Accuracy of the model
#acc_score(y_test, y_pred_2 )
# Final Accuracy: 0.9163 BOW Model

from sklearn.metrics import accuracy_score, f1_score, \
precision_score, recall_score, log_loss


Models['Accuracy'][1] = accuracy_score(y_test, y_pred_2)
Models['Precission'][1] =precision_score(y_test, y_pred_2, average="macro")
Models['Recall'][1] =recall_score(y_test, y_pred_2, average="macro")
Models['Log_Loss'][1] =log_loss(y_test, y_pred_2)
Models['F1'][1] =f1_score(y_test, y_pred_2, average="macro")

#******************************************************************************
#******************************************************************************

# *** Applying Machine Learning Technique #3 ***

# Apply SVD( Singular Value Decomposition )

from sklearn import decomposition
from sklearn.decomposition import TruncatedSVD

svd = decomposition.TruncatedSVD(n_components=200)
svd.fit(X_train)

X_train_svd = svd.transform(X_train)
X_test_svd = svd.transform(X_test)

# Scale the data obtained from SVD. 
# Renaming variable to reuse without scaling.
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
y_pred_3 = classifier_svm.predict(X_test_svd_scale)

# Accuracy of the model
#log_loss(y_test, y_pred_3 )
# Final Accuracy: 0.4687

from sklearn.metrics import accuracy_score, f1_score, \
precision_score, recall_score, log_loss


Models['Accuracy'][2] = accuracy_score(y_test, y_pred_3)
Models['Precission'][2] =precision_score(y_test, y_pred_3, average="macro")
Models['Recall'][2] =recall_score(y_test, y_pred_3, average="macro")
Models['Log_Loss'][2] =log_loss(y_test, y_pred_3)
Models['F1'][2] =f1_score(y_test, y_pred_3, average="macro")

#******************************************************************************

# Fitting Logistic Regression to the Training SVD SET
from sklearn.linear_model import LogisticRegression
classifier_1_tfidf = LogisticRegression(random_state = 0)
classifier_1_tfidf.fit(X_train_svd_scale, y_train)

# Predicting the Test set results
y_pred_3_2 = classifier_1_tfidf.predict(X_test_svd_scale)

# Accuracy of the model
#log_loss(y_test, y_pred_3_2 )
# Final Accuracy: 0.7672

from sklearn.metrics import accuracy_score, f1_score, \
precision_score, recall_score, log_loss


Models['Accuracy'][3] = accuracy_score(y_test, y_pred_3_2)
Models['Precission'][3] =precision_score(y_test, y_pred_3_2, average="macro")
Models['Recall'][3] =recall_score(y_test, y_pred_3_2, average="macro")
Models['Log_Loss'][3] =log_loss(y_test, y_pred_3_2)
Models['F1'][3] =f1_score(y_test, y_pred_3_2, average="macro")

#******************************************************************************
#******************************************************************************
"""
# *** Applying Deep Learning Technique #1 ***

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

"""
#******************************************************************************
#******************************************************************************

# *** Applying Machine Learning Technique #4 ***

from sklearn.ensemble import RandomForestClassifier

Rando= RandomForestClassifier(n_estimators=5)

from pprint import pprint
# Look at parameters used by our current forest
print('Parameters currently in use:\n')
pprint(Rando.get_params())

classifier = Rando.fit(X_train, y_train)
y_pred_4 = classifier.predict(X_test)

#score_RFC = Rando.score(X_test, y_test)
#print('Accuracy of Extratrees classifier on test set: %0.04f' %(score_RFC))
# Accuracy of Extratrees classifier on test set: 0.8137

from sklearn.metrics import accuracy_score, f1_score, \
precision_score, recall_score, log_loss


Models['Accuracy'][4] = accuracy_score(y_test, y_pred_4)
Models['Precission'][4] =precision_score(y_test, y_pred_4, average="macro")
Models['Recall'][4] =recall_score(y_test, y_pred_4, average="macro")
Models['Log_Loss'][4] =log_loss(y_test, y_pred_4)
Models['F1'][4] =f1_score(y_test, y_pred_4, average="macro")


#******************************************************************************
#******************************************************************************

# *** Applying Machine Learning Technique #5 ***

from sklearn.ensemble import ExtraTreesClassifier
                            
Extr = ExtraTreesClassifier(n_estimators=5,n_jobs=4)

from pprint import pprint
# Look at parameters used by our current forest
print('Parameters currently in use:\n')
pprint(Extr.get_params())

classifier = Extr.fit(X_train, y_train)
y_pred_5 = classifier.predict(X_test)

#score_ETC = Extr.score(X_test, y_test)
#print('Accuracy of Extratrees classifier on test set: %0.04f'%(score_ETC))

# Accuracy of Extratrees classifier on test set: 0.8295

from sklearn.metrics import accuracy_score, f1_score, \
precision_score, recall_score, log_loss

Models['Accuracy'][5] = accuracy_score(y_test, y_pred_5)
Models['Precission'][5] =precision_score(y_test, y_pred_5, average="macro")
Models['Recall'][5] =recall_score(y_test, y_pred_5, average="macro")
Models['Log_Loss'][5] =log_loss(y_test, y_pred_5)
Models['F1'][5] =f1_score(y_test, y_pred_5, average="macro")

#******************************************************************************
#******************************************************************************

# *** Applying Machine Learning Technique #6 ***

from sklearn.tree import DecisionTreeClassifier
from sklearn.ensemble import AdaBoostClassifier

Adab= AdaBoostClassifier(DecisionTreeClassifier(max_depth=3),n_estimators=5)

from pprint import pprint
# Look at parameters used by our current forest
print('Parameters currently in use:\n')
pprint(Adab.get_params())

classifier = Adab.fit(X_train, y_train)
y_pred_6 = classifier.predict(X_test)


#score_ABC = Adab.score(X_test, y_test)
#print('Accuracy of Extratrees classifier on test set: %0.04f'%(score_ABC))

# Accuracy of Extratrees classifier on test set: 0.8224
from sklearn.metrics import accuracy_score, f1_score, \
precision_score, recall_score, log_loss


Models['Accuracy'][6] = accuracy_score(y_test, y_pred_6)
Models['Precission'][6] =precision_score(y_test, y_pred_6, average="macro")
Models['Recall'][6] =recall_score(y_test, y_pred_6, average="macro")
Models['Log_Loss'][6] =log_loss(y_test, y_pred_6)
Models['F1'][6] =f1_score(y_test, y_pred_6, average="macro")

#******************************************************************************
#******************************************************************************

# *** Applying Machine Learning Technique #7 ***

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

#y_pred_7 = classifier.predict_proba(X_test_csc)
# Accuracy of the model
#log_loss(y_test, y_pred_7 )
# Final Accuracy: 0.4558

y_pred_7 = classifier.predict(X_test_csc)
# Accuracy of the model
acc_score(y_test, y_pred_7 )
# Final Accuracy: 0.9242

from sklearn.metrics import accuracy_score, f1_score, \
precision_score, recall_score, log_loss


Models['Accuracy'][7] = accuracy_score(y_test, y_pred_7)
Models['Precission'][7] =precision_score(y_test, y_pred_7, average="macro")
Models['Recall'][7] =recall_score(y_test, y_pred_7, average="macro")
Models['Log_Loss'][7] =log_loss(y_test, y_pred_7)
Models['F1'][7] =f1_score(y_test, y_pred_7, average="macro")


#******************************************************************************

# HYPERPARAMETER OPTIMIZATION --> GRID SEARCH <-- # Random Forest
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
    
# WORD CLOUD
# conda install -c conda-forge wordcloud

import matplotlib.pyplot as plt
from wordcloud import WordCloud
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
    
# Comparing ML Algos

df = {'ML Algo' : ['Naive Bayes','Logistic Regression',' SVD-SVM','SVD-LR',
                       'Random_Forest','Extra_tree_classifier','AdaBoost','XGBoost'],
      'Accuracy' :['NaN','NaN','NaN','NaN','NaN','NaN','NaN','NaN'],
      'Precission' :['NaN','NaN','NaN','NaN','NaN','NaN','NaN','NaN'],
      'Recall' :['NaN','NaN','NaN','NaN','NaN','NaN','NaN','NaN'],
      'Log_Loss' :['NaN','NaN','NaN','NaN','NaN','NaN','NaN','NaN'],
      'F1' :['NaN','NaN','NaN','NaN','NaN','NaN','NaN','NaN']
      }
Models = pd.DataFrame(df)

"""
import matplotlib.pyplot as plt
#import seaborn as sns
plt.bar(x="ML Algo", y="Accuracy",data=Models,align='center', alpha=0.5)
plt.xticks(rotation=90)
plt.title('MLA Train Accuracy Comparison')
plt.show()
"""
