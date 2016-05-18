
library(readr)
library(plyr)
library(R.utils)
library(gridExtra)
rm(list=objects())

cat("loading data\n")
load("train.Rda")
load("test.Rda")
train <- read_csv("train.csv", n_max=20000)
test <- read_csv("test.csv", n_max=20000)

#deal with labels and ids
testid = test$ID
label = train$target
save(testid, file="testid.Rda")
save(label,file="label.Rda")
train = subset(train, select=-c(ID, target))
test = subset(test, select=-c(ID))


#separate numeric and non-numeric features
train_numr = train[, sapply(train, is.numeric)]
train_char = train[, sapply(train, is.character)]
test_numr = test[, sapply(test, is.numeric)]
test_char = test[, sapply(test, is.character)]

#standarize NA values of char features
train_char[train_char==-1] = -99999
train_char[train_char==""] = -99999
train_char[train_char=="[]"] = -99999
train_char[is.na(train_char)] = -99999
test_char[test_char==-1] = -99999
test_char[test_char==""] = -99999
test_char[test_char=="[]"] = -99999
test_char[is.na(test_char)] = -99999

cat("creating dummy variables for char features with low freq combining\n")
minfreq = 0.001
train_char_dummy = data.frame(matrix(0,nrow=nrow(train_char), ncol=0));
test_char_dummy = data.frame(matrix(0,nrow=nrow(test_char), ncol=0));
for (i in 1:ncol(train_char)) {
  y = count(train_char,i)
  y$freq = y$freq/nrow(train_char)
  low_freq_col = rep(0, nrow(train_char))
  low_freq_colt = rep(0, nrow(test_char))
  has_low_freq = 0
  for (j in 1:nrow(y)) {
    if (y$freq[j] < minfreq) {
      has_low_freq = 1
      low_freq_col = low_freq_col + as.numeric(train_char[i]==y[j,1])
      low_freq_colt = low_freq_colt + as.numeric(test_char[i]==y[j,1])
    }
    else {
      train_char_dummy = cbind(train_char_dummy, as.numeric(train_char[i]==y[j,1]))
      test_char_dummy = cbind(test_char_dummy, as.numeric(test_char[i]==y[j,1]))
    }
  }
  if (nrow(y)>2 && has_low_freq == 1) {
    train_char_dummy = cbind(train_char_dummy, low_freq_col)
    test_char_dummy = cbind(test_char_dummy, low_freq_colt)
  }
}

cat("creating dummy variables for NA values for numr features\n")
theta = 0.001
train_na_dummy = data.frame(matrix(0,nrow=nrow(train_numr), ncol=0));
test_na_dummy = data.frame(matrix(0,nrow=nrow(test_numr), ncol=0));
for (i in 1:ncol(train_numr)) {
  na_col = is.na(train_numr[i])
  na_colt = is.na(test_numr[i])
  if (length(na_col[na_col])/length(na_col)>theta) {
    train_na_dummy = cbind(train_na_dummy, as.numeric(na_col))
    test_na_dummy = cbind(test_na_dummy, as.numeric(na_colt))
  }
  train_numr[na_col, i] = 0
  test_numr[na_colt, i] = 0
}

#combine numerical, na_dummy and char_dummy features
train = cbind(train_numr, train_char_dummy)
train = cbind(train, train_na_dummy)
test = cbind(test_numr, test_char_dummy)
test = cbind(test, test_na_dummy)

cat("deleting constant features\n")
col_ct = sapply(train, function(x) length(unique(x))==1)
train = train[, !col_ct]
test = test[, !col_ct]

cat("deleting small variance features\n")
theta_var = 0.001
var_ct = sapply(train, function(x) var(x)/(sum(x)/nrow(train))^2<theta_var)
train = train[, !var_ct]
test = test[,!var_ct]

cat("deleting high correlated features\n")
cat(ncol(train))
#this process is very time consuming
theta_cor = 0.001
del = vector(, ncol(train))
for (i in 1:(ncol(train)-1)) {
  cat(i)
  if (!del[i]) {
    for (j in (i+1):ncol(train)) {
      if (!del[j]) {
        if (cor(train[i], train[j])>1-theta_cor) {
          del[j] = TRUE
        }
      }
    }
  }
}
train = train[,!del]
test = test[,!del]

save(train,file="train_cleaned.Rda")
save(test,file="test_cleaned.Rda")

cat("data cleaning completed\n")
