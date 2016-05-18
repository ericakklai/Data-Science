#install.packages("readr")
#install.packages("R.utils")
#install.packages("gridExtra")
#install.packages('plyr')
#install.packages('xgboost')

setwd("~/Documents/Study/STAT542")
library(readr)
library(dplyr)
library(R.utils)
library(ggplot2)
library(gridExtra)
library(plyr)

#train <- read_csv("train.csv")
#save(train,file="train.Rda")
#test <- read_csv("test.csv")
#save(test,file="test.Rda")
rm(list=objects())

train <- read_csv("train.csv", n_max=20000)
y = train$target
# remove the id and target
train = subset(train, select=-c(ID, target))
# get the rowcount
row_count = countLines("train.csv") 
cat("Row count : ", row_count[1], "; Predictor column count : ", ncol(train))
row_count = countLines("test.csv") 
cat("Row count : ", row_count[1], "; Predictor column count : ", ncol(train))

#portion of NA values
length(train[is.na(train)])/(ncol(train)*nrow(train))

#check duplicate rows
nrow(train) - nrow(unique(train))

#check duplicate features
ncol(train) - ncol(unique(train, MARGIN=2))

#look at the columns with only one unique value
col_ct = sapply(train, function(x) length(unique(x)))
cat("Constant feature count:", length(col_ct[col_ct==1]))

#separate numeric and non-numeric features
train_numr = train[, sapply(train, is.numeric)]
train_char = train[, sapply(train, is.character)]
cat("Numerical column count : ", dim(train_numr)[2], 
    "; Character column count : ", dim(train_char)[2])
#explore charactor features
str(lapply(train_char, unique), vec.len = 4)
#standarize NA values
train_char[train_char==-1] = -9999
train_char[train_char==""] = -9999
train_char[train_char=="[]"] = -9999
train_char[is.na(train_char)] == -9999
str(lapply(train_char, unique), vec.len = 4)

#create dummy variables with low-freq combining
train_char_dummy = data.frame(matrix(0,nrow=nrow(train_char), ncol=0));
for (i in 5:5) {
  y = count(train_char,i)
  y$freq = y$freq/nrow(train_char)
  low_freq_col = rep(0, nrow(train_char))
  has_low_freq = 0
  for (j in 1:nrow(y)) {
    if (y$freq[j] < 0.05) {
      has_low_freq = 1
      low_freq_col = low_freq_col + as.numeric(train_char[i]==y[j,1])
    }
    else {
      train_char_dummy = cbind(train_char_dummy, as.numeric(train_char[i]==y[j,1]))
    }
  }
  if (nrow(y)>2 && has_low_freq == 1) {
    train_char_dummy = cbind(train_char_dummy, low_freq_col)
  }
}

#separate the date features
train_date = train_char[,grep("JAN1|FEB1|MAR1", train_char),]
train_char = train_char[, !colnames(train_char) %in% colnames(train_date)]
train_date = sapply(train_date, function(x) strptime(x, "%d%B%y:%H:%M:%S"))
train_date = do.call(cbind.data.frame, train_date)


#look at unique values of each feature
num_ct = sapply(train_numr, function(x) length(unique(x)))
char_ct = sapply(train_char, function(x) length(unique(x)))
date_ct = sapply(train_date, function(x) length(unique(x)))
all_ct = rbind(data.frame(count=num_ct, type="Numerical"), 
               data.frame(count=char_ct, type="Character"), 
               data.frame(count=date_ct, type="Date"))
g1 = ggplot(all_ct, aes(x = count, fill=type)) + 
  geom_histogram(binwidth = 1, alpha=0.7, position="identity") + 
  xlab("Unique values per feature (0-100)")+ theme(legend.position = "none") + 
  xlim(c(0,100)) +theme(axis.title.x=element_text(size=14, ,face="bold"))
g2 = ggplot(all_ct, aes(x = count, fill=type)) +  
  geom_histogram(binwidth = 100, alpha=0.7, position="identity") + 
  xlab("Unique values per feature(101+)")  + xlim(c(101,nrow(train))) +
  theme(axis.title.x=element_text(size=14, ,face="bold"))
grid.arrange(g1, g2, ncol=2)

#look at na values of each feature
num_na = sapply(train_numr, function(x) sum(is.na(x)))
char_na = sapply(train_char, function(x) sum(is.na(x)))
date_na = sapply(train_date, function(x) sum(is.na(x)))
all_na = rbind(data.frame(count=num_na, type="Numerical"), 
               data.frame(count=char_na, type="Character"), 
               data.frame(count=date_na, type="Date"))
#table(all_na)
all_na = data.frame(all_na)
all_na = all_na[all_na$count>0,]
breaks <- c(5,10,50,100,500,1000,2000)
ggplot(all_na, aes(x = count, fill=type)) +  
  geom_histogram(alpha=0.7) + 
  #  scale_y_log10(limits=c(1,2000), breaks=breaks) + 
  scale_x_log10(limits=c(1,20000), breaks=c(breaks,5000,10000,20000)) + 
  labs(title="Histogram of feature count per NA count", size=24, face="bold") +
  theme(plot.title = element_text(size = 16, face = "bold")) +
  xlab("NA Count") + ylab("Feature Count")

#deal with na and unique values in numeric features
train_numr[is.na(train_numr)] = -99999999
length(colnames(train_numr[,sapply(train_numr, function(v) var(v, na.rm=TRUE)==0)]))
