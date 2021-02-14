# -*- coding: utf-8 -*-
"""
Created on Tue Aug 11 15:56:35 2020

@author: DELL
"""
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
bankfull=pd.read_csv("C:\\Users\\DELL\\Desktop\\New folder\\DATA SCIENCE\\ASSIGNMENTS\\Logistic Regression\\bank-full.csv")
bankfull.head(40)
bankfull.corr()
month= {'jan':1,'feb':2,'mar':3,'apr':4,'may':5,'jun':6,'jul':7,'aug':8,'sep':9,'oct':10,'nov':11,'dec':12}
bankfull.month=[month[item]for item in bankfull.month]
print(bankfull)
import seaborn as sns
get_ipython().run_line_magic('matplotlib','inline')
from sklearn import preprocessing
from sklearn.linear_model import LogisticRegression
from sklearn import metrics
from sklearn import preprocessing
from sklearn.metrics import classification_report
from sklearn.cross_validation import train_test_split 
sns.countplot(x="acceptance",data=bankfull)
pd.crosstab(bankfull.acceptance,bankfull.duration)
sns.countplot(x="age",data=bankfull)
pd.crosstab(bankfull.acceptance,bankfull.age)
sns.countplot(x="education",data=bankfull)
pd.crosstab(bankfull.acceptance,bankfull.education)
sns.countplot(x="balance",data=bankfull)
pd.crosstab(bankfull.acceptance,bankfull.balance)
sns.countplot(x="loan",data=bankfull)
pd.crosstab(bankfull.acceptance,bankfull.loan)
sns.countplot(x="month",data=bankfull)
pd.crosstab(bankfull.acceptance,bankfull.month)
sns.countplot(x="campaign",data=bankfull)
pd.crosstab(bankfull.acceptance,bankfull.campaign)
#boxplot
sns.boxplot(x="acceptance",y="age",data=bankfull,palette="hls")
sns.boxplot(x="acceptance",y="job",data=bankfull,palette="hls")
sns.boxplot(x="marital status",y="age",data=bankfull,palette="hls")
sns.boxplot(x="duration",y="loan",data=bankfull,palette="hls")
sns.boxplot(x="acceptance",y="age",data=bankfull,palette="hls")

bankfull.isnull().sum()
bankfull.shape
Y=bankfull['acceptance']
X=bankfull.drop('acceptance',axis=1)
classifier = LogisticRegression()
classifier.fit(X,Y)
classifier.coef_  
classifier.predict_proba (X) 

y_pred = classifier.predict(X)
bankfull["y_pred"] = y_pred
y_prob = pd.DataFrame(classifier.predict_proba(X.iloc[:,:]))
new_data= pd.concat([bankfull,y_prob],axis=1)

from sklearn.metrics import confusion_matrix
confusion_matrix = confusion_matrix(Y,y_pred)
print (confusion_matrix)
type(y_pred)
accuracy = sum(Y==y_pred)/bankfull.shape[0]
accuracy
pd.crosstab(y_pred,Y)
specificity=39184/(39184+738)
specificity
sensitivity=888/(888+4401)
sensitivity
