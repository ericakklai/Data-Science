import numpy as np 
import pandas as pd
from sklearn.ensemble import RandomForestClassifier
import csv
from pyflann import *
from sklearn.naive_bayes import GaussianNB
from sklearn import svm
from sklearn.svm import SVC
from sklearn.svm import LinearSVC

filename = '/Users/EricaLai/Desktop/CS498/face/pubfig_dev_50000_pairs.txt'
filename2 = '/Users/EricaLai/Desktop/CS498/face/pubfig_attributes.txt'
filename3 = '/Users/EricaLai/Desktop/CS498/face/pubfig_kaggle_eval.txt'
val1 = '/Users/EricaLai/Desktop/CS498/face/pubfig_kaggle_1.txt'
sol1 = '/Users/EricaLai/Desktop/CS498/face/pubfig_kaggle_1_solution.txt'
val2 = '/Users/EricaLai/Desktop/CS498/face/pubfig_kaggle_2.txt'
sol2 = '/Users/EricaLai/Desktop/CS498/face/pubfig_kaggle_2_solution.txt'
val3 = '/Users/EricaLai/Desktop/CS498/face/pubfig_kaggle_3.txt'
sol3 = '/Users/EricaLai/Desktop/CS498/face/pubfig_kaggle_3_solution.txt'


#Initializing names of output CSV files
SUBMIT_Q1_nb = '/Users/EricaLai/Desktop/CS498/face/Q1_nb.csv'
SUBMIT_Q1_svm = '/Users/EricaLai/Desktop/CS498/face/Q1_svm.csv'
SUBMIT_Q1_rfc = '/Users/EricaLai/Desktop/CS498/face/Q1_rf.csv'
SUBMIT_Q2 = '/Users/EricaLai/Desktop/CS498/face/Q2.csv'
SUBMIT_Q3 = '/Users/EricaLai/Desktop/CS498/face/Q3.csv'

# Training file
alldata = pd.read_csv(filename, header=None,na_values=['NA'],sep='\t',comment='#')
alldata2 = pd.read_csv(filename2,skiprows=1,sep='\t') # for question 2

# Evaluation file
evalset = pd.read_csv(filename3, skiprows = 2,header=None,sep='\t')
x_train = alldata.drop([0], axis=1)
y_train = alldata[0]

## my little playground
##############################################################################
x_first = x_train.iloc[:,0:73]
x_second = x_train.iloc[:,73:147]
x_second.columns = x_first.columns

x_diff = x_first.subtract(x_second)

## function for writing outputting prediction onto Kaggle##
def write_data(filename, fieldnames, data):
	with open(filename, 'w') as csvfile:
		writer = csv.DictWriter(csvfile, fieldnames=fieldnames)
		writer.writeheader()
		for row in data:
			obj = {}
			for i in range(len(fieldnames)):
				obj[fieldnames[i]] = row[i]
			writer.writerow(obj)
##############################################################################
# Question 1

def part_diff(df):
    df_1 = df.iloc[:,0:73]
    df_2 = df.iloc[:,73:146]
    df_2.columns = df_1.columns
    ret = df_1.subtract(df_2)
    return ret

# 1a. Linear SVM

clf_linear = svm.LinearSVC()
clf_linear.fit(x_train, y_train)

def pred_svm(f1,f2):
    print 'Predicting the SVM result of ' + f2
    a = pd.read_csv(f1, header = None, sep = '\t', comment = '#')
    predict_outcomes = clf_linear.predict(a)
    
    b = pd.read_csv(f2, header=0, sep = ',')
    c = b.as_matrix(["Prediction"])
    ans = 0
    for i in range(len(c)):
		if predict_outcomes[i] == c[i][0]:
			ans += 1
    acc = ans*1.0/len(c)
    print (acc)
    return predict_outcomes, acc

#Prediction on Training Set
pred_train_svm= clf_linear.predict(x_train)

accy = 0
for i in range(len(y_train)):
    if y_train[i] == pred_train_svm[i]:
        accy = accy +1
acc_train_svm = float(accy) / float(len(y_train))
print 'train accuracy' , acc_train_svm

#Prediction on Validation Set
pred_val1_svm, acc_val1_svm = pred_svm(val1, sol1)
pred_val2_svm, acc_val2_svm = pred_svm(val2, sol2)
pred_val3_svm, acc_val3_svm = pred_svm(val3, sol3)
#Prediction on Kaggle Set and Output for Submission
predict_eval_svm =clf_linear.predict(evalset)
results = []
for i in range(len(evalset)):
	results.append([i, predict_eval_svm[i]])
write_data(SUBMIT_Q1_svm,['ID','Prediction'], results)



# 1b. Naive Bayes

 
nb = GaussianNB()
nb.fit(x_diff,y_train)


    
    
def pred_nb(f1,f2):
    print 'Predicting the Nayes Bayes result of ' + f2
    a = pd.read_csv(f1, header = None, sep = '\t', comment = '#')
    a = part_diff(a)
    predict_outcomes = nb.predict(a)
    
    b = pd.read_csv(f2, header=0, sep = ',')
    c = b.as_matrix(["Prediction"])
    ans = 0
    for i in range(len(c)):
		if predict_outcomes[i] == c[i][0]:
			ans += 1
    acc = ans*1.0/len(c)
    print (acc)
    return predict_outcomes, acc

#Prediction on Training Set
pred_train_nb= nb.predict(x_diff)

accy = 0
for i in range(len(y_train)):
    if y_train[i] == pred_train_nb[i]:
        accy = accy +1
acc_train_nb = float(accy) / float(len(y_train))
print 'train accuracy' , acc_train_nb   

#Prediction on Validation Set
pred_val1_nb, acc_val1_nb = pred_nb(val1, sol1)
pred_val2_nb, acc_val2_nb = pred_nb(val2, sol2)
pred_val3_nb, acc_val3_nb = pred_nb(val3, sol3)
print 'val accuracy:' , acc_val1_nb,acc_val2_nb, acc_val3_nb
#Prediction on Kaggle Set and Output for Submission
predict_eval_nb = nb.predict(part_diff(evalset) )
results = []
for i in range(len(evalset)):
	results.append([i, predict_eval_nb[i]])
write_data(SUBMIT_Q1_nb,['ID','Prediction'], results)


# 1c. Random Forests
rfc = RandomForestClassifier(n_estimators=800, criterion = 'entropy', n_jobs=5, random_state=0)
rfc.fit(x_diff, y_train)

def pred_rfc(f1, f2):
    print 'Predicting the random forest result of ' + f2
    a = pd.read_csv(f1, header=None, sep = '\t', comment='#')
    a = part_diff(a)
    predict_outcomes = rfc.predict(a)
    b = pd.read_csv(f2, header=0, sep = ',')
    c = b.as_matrix(["Prediction"])
    ans = 0
    for i in range(len(c)):
        if predict_outcomes[i] == c[i][0]:
		ans += 1
    acc = ans*1.0/len(c)
    print (acc)
    return predict_outcomes, acc

#Prediction on Training Set
pred_train_rfc= rfc.predict(x_diff)

accy = 0
for i in range(len(y_train)):
    if y_train[i] == pred_train_rfc[i]:
        accy = accy +1
acc_train_rfc = float(accy) / float(len(y_train))
print 'train accuracy' , acc_train_rfc 

#Prediction on Validation Set
pred_val1_rfc, acc_val1_rfc = pred_rfc(val1, sol1)
pred_val2_rfc, acc_val2_rfc = pred_rfc(val2, sol2)
pred_val3_rfc, acc_val3_rfc = pred_rfc(val3, sol3)

#Prediction on Kaggle Set and Output for Submission
predict_eval_rfc = rfc.predict(part_diff(evalset))
results = []
for i in range(len(evalset)):
	results.append([i, predict_eval_rfc[i]])
write_data(SUBMIT_Q1_rfc,['ID','Prediction'], results)



# Question 2


match = np.array(alldata2.iloc[:,2:75])
data_1_eval = np.array(evalset.iloc[:,0:73])
data_2_eval = np.array(evalset.iloc[:,73:146])
match = match.astype(float)
data1_eval = data_1_eval.astype(float)
data2_eval = data_2_eval.astype(float)

def pred_knn(data1,data2):
    flann = FLANN()
    result1,dists = flann.nn(match,data1,1,algorithm="kmeans",
                        branching=32, iterations=7, checks=16)

    result2,dists = flann.nn(match,data2,1,algorithm="kmeans",
                        branching=32, iterations=7, checks=16)
                        
    pred_nn = []
    for i in xrange(len(data1)):
       if alldata2.iloc[result1[i],0]==alldata2.iloc[result2[i],0]:
        pred_nn.append([i,1])
       else:
        pred_nn.append([i,0])
    return pred_nn


# Accuracy on Training Data
pred_train_nn = pred_knn(np.array(x_train.iloc[:,0:73]).astype(float),np.array(x_train.iloc[:,73:146]).astype(float))
accy = 0
for i in range(len(y_train)):
    if y_train[i] == pred_train_nn[i][1]:
        accy = accy +1
acc_train_q2 = accy / len(y_train)
print 'accuracy' , acc_train_q2   

# Output Prediction on Evaluation Set on Kaggle
pred_eval_nn = pred_knn(data1_eval, data2_eval)
write_data(SUBMIT_Q2,['ID','Prediction'], pred_eval_nn)
        
#Question 3
NL_clf = svm.SVC()
NL_clf.fit(x_train, y_train)

def pred_NL_svm(f1, f2):
    print 'Predicting the rbf svm result of ' + f2
    a = pd.read_csv(f1, header=None, sep = '\t', comment='#')
    predict_outcomes = NL_clf.predict(a)

    b = pd.read_csv(f2, header=0, sep = ',')
    c = b.as_matrix(["Prediction"])
    ans = 0
    for i in range(len(c)):
	if predict_outcomes[i] == c[i][0]:
		ans += 1
    acc = ans*1.0/len(c)
    print (acc)
    return predict_outcomes, acc
    
# Training Error

pred_train_NL_svm= NL_clf.predict(x_train)

accy = 0
for i in range(len(y_train)):
    if y_train[i] == pred_train_NL_svm[i]:
        accy = accy +1
acc_train_NL_svm = float(accy) / float(len(y_train))
print 'train accuracy' , acc_train_NL_svm 

 
# Testing Error on Validation Set
pred_val1_NL_svm, acc_val1_NL_svm = pred_NL_svm(val1, sol1)
pred_val2_NL_svm, acc_val2_NL_svm = pred_NL_svm(val2, sol2)
pred_val3_NL_svm, acc_val3_NL_svm = pred_NL_svm(val3, sol3)

#Prediction on Kaggle Set and Output for Submission
pred_eval_NL_svm = NL_clf.predict(evalset)
results = []
for i in range(len(evalset)):
	results.append([i, pred_eval_NL_svm[i]])
write_data(SUBMIT_Q3,['ID','Prediction'], results)




