#############################################
#CS 498 Programming Exercise 2.1 Feb 1, 2016#
# Kakilai2# 
#############################################

rm(list=objects())
setwd('/Users/EricaLai/Desktop/CS 498/Assn 1')
library(caret)
library(klaR)
library(ggplot2)

# Read in data for part a)
mydata <-read.csv('pima-indians-diabetes.data.txt', header=FALSE)

# Data for part b) : Adjust 0 values of V3, V4, V6, V8 to 'NA'
mydata_adj = mydata
for (i in c(3,4,6,8))
{ind = mydata_adj[i]==0
mydata_adj[ind,i] = NA
}

# Function of naive bayes for model a and b 

naive_bayes <-function(data){
# Split data
target = data[,9]
features = data[,-9]
data_split <-createDataPartition(y=target, p=.8, list=FALSE)
target_train = target[data_split]
target_test = target[-data_split]
features_train = features[data_split,]
features_test = features[-data_split,]

# Train classifier
target_train_pos = target_train >0
target_train_neg = !target_train >0
p_pos = sum(target_train_pos)/length(target_train)
p_neg = sum(target_train_neg)/length(target_train)
features_train_pos = features_train[target_train_pos,]
features_train_neg = features_train[target_train_neg,]
pos_mean = colMeans(features_train_pos, na.rm = TRUE)
neg_mean = colMeans(features_train_neg, na.rm = TRUE)
pos_sd = sapply(features_train_pos, sd, na.rm=TRUE)
neg_sd = sapply(features_train_neg, sd, na.rm=TRUE)

# Training accuracy
pos_scale = t((t(features_train)-pos_mean)/pos_sd)^2
neg_scale = t((t(features_train)-neg_mean)/neg_sd)^2
logsum_pos<--(1/2)*rowSums(pos_scale, na.rm=TRUE)-sum(log(pos_sd)) + log(p_pos)
logsum_neg<--(1/2)*rowSums(neg_scale, na.rm=TRUE)-sum(log(neg_sd)) + log(p_neg)
pred_train = logsum_pos  >logsum_neg 
accuracy_train = sum(pred_train == target_train_pos)/length(target_train)

# Testing accuracy
target_test_pos = target_test >0
target_test_neg = !target_test >0
pos_scale_test = t((t(features_test)-pos_mean)/pos_sd)^2
neg_scale_test = t((t(features_test)-neg_mean)/neg_sd)^2
logsum_pos_test<--(1/2)*rowSums(pos_scale_test, na.rm=TRUE)-sum(log(pos_sd)) + log(p_pos)
logsum_neg_test<--(1/2)*rowSums(neg_scale_test, na.rm=TRUE)-sum(log(neg_sd)) + log(p_neg)
pred_test = logsum_pos_test  >logsum_neg_test 
accuracy_test = sum(pred_test == target_test_pos)/length(target_test)

return(list(training_accuracy =accuracy_train, testing_accuracy = accuracy_test,target_train = target_train,target_test = target_test, features_train = features_train, features_test =features_test))
}

# Running part model a - d for 8 times
all_train_acc = c()
all_test_acc = c()
for (i in 1:8)
{
# a) and b)
  set.seed(i)
  result_a = naive_bayes(mydata)
  result_b = naive_bayes(mydata_adj)
# c) naive Bayes by caret and klaR and svm using data generated for part a
  features_train_a = result_a$features_train
  features_test_a = result_a$features_test
  target_train_a = as.factor(result_a$target_train)
  target_test_a = result_a$target_test
  mymodel<-train(features_train_a, target_train_a, 'nb', trControl=trainControl(method='cv', number=10))
  pred<-predict(mymodel,newdata=features_test_a)
  result_c = confusionMatrix(data=pred, target_test_a)
# d) svmlight
  svm = svmlight(features_train_a, target_train_a,pathsvm='/Users/EricaLai/Downloads/svm_light_osx.8.4_i7/')
  labels<-predict(svm, features_test_a)
  pre<-labels$class
  result_d = sum(pre==target_test_a)/(sum(pre==target_test_a)+sum(!(pre==target_test_a)))
  
  all_train_acc= rbind(all_train_acc, c(result_a$training_accuracy, result_b$training_accuracy))
  all_test_acc = rbind(all_test_acc, c(result_a$testing_accuracy, result_b$testing_accuracy, result_c$overall[1], result_d ))
}

# Output Result
all_train_acc = rbind(all_train_acc, colMeans(all_train_acc))
all_test_acc = rbind(all_test_acc, colMeans(all_test_acc))
colnames(all_train_acc) = c("a", "b")
colnames(all_test_acc) = c("a","b","c","d")
write.csv(all_train_acc, file = "assn1_all_train_accuracy.csv")
write.csv(all_test_acc, file = "assn1_all_test_accuracy.csv")

plot_acc = data.frame(a = all_test_acc[,1], b = all_test_acc[,2],c= all_test_acc[,3],d = all_test_acc[,4],iteration=1:9)
ggplot(plot_acc,aes(iteration,testing_accuracy))+
  geom_line(aes(y=a,colour="a"))+
  geom_line(aes(y=b,colour="b"))+
  geom_line(aes(y=c,colour="c"))+
  geom_line(aes(y=d,colour="d")) + 
  scale_colour_manual("", breaks = c("a","b","c","d"), values = c("a" = "green", "b" = "red", "c" = "blue", "d" = "orange"))+
  scale_x_continuous(breaks = c(1,3,5,7))+
  ggtitle("Testing Accuracy of Classifiers (a) to (d)")


