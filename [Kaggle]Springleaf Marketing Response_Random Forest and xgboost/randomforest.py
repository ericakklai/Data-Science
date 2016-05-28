# Ka Ki Lai (erica.kklai@gmail.com)
# Dec 15 2015
Random Forest Algorithm for Springleaf Marketing Response

import csv
import numpy as np
import copy
from  sklearn.ensemble import RandomForestClassifier
from sklearn.utils import shuffle

# CONFIG START
TRAIN_CSV = 'train_cleaned.csv'
TEST_CSV = 'test_cleaned.csv'
LABEL_CSV = 'label.csv'
TEST_ID_CSV = 'testid.csv'
SUBMIT_CSV = 'submission_1000_entropy_sqrt.csv'
# CONFIG END

# Read and load Data
def load_data(filename):
	with open(filename, 'rb') as csvfile:
		reader = csv.reader(csvfile, delimiter=',')
		features = reader.next()
		data = []
		for row in reader:
			data.append(row)
	return data


def write_data(filename, fieldnames, data):
	with open(filename, 'w') as csvfile:
		writer = csv.DictWriter(csvfile, fieldnames=fieldnames)
		writer.writeheader()
		for row in data:
			obj = {}
			for i in range(len(fieldnames)):
				obj[fieldnames[i]] = row[i]
			writer.writerow(obj)

print 'Loading ' + TRAIN_CSV
train = load_data(TRAIN_CSV)
print 'Loading ' + LABEL_CSV
label = load_data(LABEL_CSV)
label = [x[0] for x in label]
print 'Loading ' + TEST_CSV
test = load_data(TEST_CSV)
print 'Loading ' + TEST_ID_CSV
testid = load_data(TEST_ID_CSV)
testid = [x[0] for x in testid]

# Random Forest Algorithm
rf = RandomForestClassifier(n_estimators=1000, criterion='entropy', max_features='sqrt', n_jobs=5)

print 'Fitting the random forest'
rf.fit(train, label)

# Predict Result
print 'Predicting the result'
predict_outcomes = rf.predict_proba(test)
results = []
for i in range(len(testid)):
	results.append([testid[i], predict_outcomes[i][1]])
	
# Output
print 'Output' + SUBMIT_CSV
write_data(SUBMIT_CSV, ['ID','target'], results)