#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Fri Aug  2 17:08:03 2024

@author: maddy16
"""

import pandas as pd

messages= pd.read_csv('SMSSpamCollection', sep='\t', names=["label","message"])
messages2= pd.read_csv('SMSSpamCollection', sep='\t', names=["label","message"])
#Data cleaning and preprocessing
import re
import nltk

nltk.download('stopwords')

from nltk.corpus import stopwords
from nltk.stem.porter import PorterStemmer

ps=PorterStemmer()
#Cleaning data for performing the Naive Bayes
corpus=[]
for i in range(0,len(messages)):
    review = re.sub('[^a-zA-Z]', ' ', messages['message'][i])
    review = review.lower()
    review = review.split()
    
    review = [ps.stem(word) for word in review if not word in stopwords.words('english')]
    review = ' '.join(review)
    corpus.append(review)

#Creating BoW
from sklearn.feature_extraction.text import CountVectorizer
cv = CountVectorizer(max_features=2500)#limit max 5000 words to consider(most frequent)
X = cv.fit_transform(corpus).toarray()

y=pd.get_dummies(messages['label'])
y=y.iloc[:,1].values


# Train Test Split

from sklearn.model_selection import train_test_split
X_train, X_test, y_train, y_test = train_test_split(X, y, test_size = 0.20, random_state = 0)

# Training model using Naive bayes classifier

from sklearn.naive_bayes import MultinomialNB
spam_detect_model = MultinomialNB().fit(X_train, y_train)

y_pred=spam_detect_model.predict(X_test)

from sklearn.metrics import confusion_matrix

confusion_m=confusion_matrix(y_test,y_pred)

from sklearn.metrics import accuracy_score
accuracy=accuracy_score(y_test,y_pred)
print(f"The accuracy of the NB model(using stemming and BoW) here is: {round(accuracy*100,2)} %")

#Creating TFIDF

from sklearn.feature_extraction.text import TfidfVectorizer
tv=TfidfVectorizer()
X2=tv.fit_transform(corpus).toarray()


from sklearn.model_selection import train_test_split
X2_train, X2_test, y2_train, y2_test = train_test_split(X2, y, test_size = 0.20, random_state = 0)

# Training model using Naive bayes classifier

from sklearn.naive_bayes import MultinomialNB
spam_detect_model = MultinomialNB().fit(X2_train, y2_train)

y2_pred=spam_detect_model.predict(X2_test)

from sklearn.metrics import confusion_matrix

confusion_m=confusion_matrix(y2_test,y2_pred)

from sklearn.metrics import accuracy_score
accuracy2=accuracy_score(y2_test,y2_pred)
print(f"The accuracy of the NB model(using stemming and TfIDF) here is: {round(accuracy2*100,2)}%")


#Repeating the same using lammetization

import re
from nltk.corpus import stopwords
from nltk.stem import WordNetLemmatizer
nltk.download('wordnet')
# Initialize the lemmatizer
wordnet = WordNetLemmatizer()

# Create an empty list to hold the cleaned and lemmatized text
corpus2 = []

for i in range(0, len(messages)):
    # Remove non-alphabet characters and convert to lowercase
    review2 = re.sub('[^a-zA-Z]', ' ', messages['message'][i])
    review2 = review2.lower()
    
    # Split the text into words
    review2 = review2.split()
    
    # Lemmatize each word and remove stopwords
    review2 = [wordnet.lemmatize(word) for word in review2 if word not in stopwords.words('english')]
    
    # Join the words back into a single string
    review2 = ' '.join(review2)
    
    # Append the processed text to the corpus
    corpus2.append(review2)

#Creating BoW
from sklearn.feature_extraction.text import CountVectorizer
cv2 = CountVectorizer(max_features=5000)#limit max 5000 words to consider(most frequent)
X3 = cv2.fit_transform(corpus2).toarray()

y=pd.get_dummies(messages2['label'])
y=y.iloc[:,1].values


# Train Test Split

from sklearn.model_selection import train_test_split
X3_train, X3_test, y3_train, y3_test = train_test_split(X3, y, test_size = 0.20, random_state = 0)

# Training model using Naive bayes classifier

from sklearn.naive_bayes import MultinomialNB
spam_detect_model = MultinomialNB().fit(X3_train, y3_train)

y3_pred=spam_detect_model.predict(X3_test)

from sklearn.metrics import confusion_matrix

confusion_m=confusion_matrix(y3_test,y3_pred)

from sklearn.metrics import accuracy_score
accuracy3=accuracy_score(y3_test,y3_pred)
print(f"The accuracy of the NB model(using lemmatization and BoW) here is: {round(accuracy3*100,2)} %")

#Creating TFIDF

from sklearn.feature_extraction.text import TfidfVectorizer
tv2=TfidfVectorizer()
X4=tv.fit_transform(corpus2).toarray()


from sklearn.model_selection import train_test_split
X4_train, X4_test, y4_train, y4_test = train_test_split(X4, y, test_size = 0.20, random_state = 0)

# Training model using Naive bayes classifier

from sklearn.naive_bayes import MultinomialNB
spam_detect_model = MultinomialNB().fit(X4_train, y4_train)

y4_pred=spam_detect_model.predict(X4_test)

from sklearn.metrics import confusion_matrix

confusion_m=confusion_matrix(y4_test,y4_pred)

from sklearn.metrics import accuracy_score
accuracy2=accuracy_score(y4_test,y4_pred)
print(f"The accuracy of the NB model(using lemmatization and TfIDF) here is: {round(accuracy2*100,2)}%")


