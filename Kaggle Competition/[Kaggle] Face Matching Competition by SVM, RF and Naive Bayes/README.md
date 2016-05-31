# Face Matching Competition by Linear SVM, Naive Bayes and Random Forest

### Kaggle project 
*[Face Matching Competition]

[Face Matching Competition]:<https://kaggle.com/join/cs498dfhw3>

Running various models to predict if two faces belong to the same class


###Package Installed:

'''sh
import numpy as np 
import pandas as pd
from sklearn.ensemble import RandomForestClassifier
import csv
from pyflann import *
from sklearn.naive_bayes import GaussianNB
from sklearn import svm
from sklearn.svm import SVC
from sklearn.svm import LinearSVC
'''

###output Files:
Q1_nb.csv : Kaggle Submission File for Q1 naive bayes classifier
Q1_svm.csv : Kaggle Submission File for Q1 linear svm classifier
Q1_rf.csv : Kaggle Submission File for Q1 random forest classifier
Q2.csv : Kaggle Submission File for Q2
Q3.csv : Kaggle Submission File for Q3

###Summary

Performance of Classifiers:
Regarding the low performance of Linear SVM, it could be possible that the reason behind is the data are not linearly separable under the high dimension. Hence, SVM with radial basis function kernel has also been tried, which give much better performance. To implement this classifier, the sklearn.svm module includes Support Vector Machine algorithms is used in Python. In this function, the kernel parameter by default is ‘rbf’. The prediction accuracy on the Evaluation Set from Kaggle is 0.7699 which is much better than the classifier used previously.

Discussion:
Nearest neighbors gives best prediction because it gives 100% prediction accuracy on both the training and testing sets. We also find that the conversion of original feature into difference between the attribute values of face 1 and 2 works well in Naïve Bayes and random forest, but not SVM (both linear and rbf). It is also shown that the converted features result in shorter computation time due to much smaller number of features.