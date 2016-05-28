load("~/Desktop/STAT 542/Data.Rdata")
library(MASS)
#Problem 1
#Remove point with very small variance
sd.train=apply(traindata,2,var)
smallvar=which(sd.train<10^(-5))
smallvar #V256 V257 will be removed
traindata=traindata[,-smallvar]
testdata=testdata[,-smallvar]

#Linear Discriminant Analysis
lda.fit=lda(Y~., data=traindata)
#lda.fit
#training error for LDA
lda.predict.train=predict(lda.fit,traindata)
train.err.lda=mean(lda.predict.train$class != traindata$Y)
train.err.lda
#testing error for LDA
lda.predict.test=predict(lda.fit,testdata)
test.err.lda=mean(lda.predict.test$class != testdata$Y)
test.err.lda
# Parametric Naive Bayes using e1071 package
library(e1071)
nb.fit=naiveBayes(as.factor(Y)~.,data=traindata)
#training error for Parametric Naive Bayes 
nb.predict.train=predict(nb.fit,traindata)
nb.predict.train
train.err.nb=mean(nb.predict.train!=traindata$Y)
train.err.nb
#testing error for Parametric Naive Bayes 
nb.predict.test=predict(nb.fit,testdata)
nb.predict.test
test.err.nb=mean(nb.predict.test !=testdata$Y)
test.err.nb
#Non Parametric Naive Bayes by K1aR
library(klaR)
nnb.fit=NaiveBayes(as.factor(Y)~.,data=traindata, usekernel=TRUE)

#training error for Non Parametric Naive Bayes 
nnb.predict.train=predict(nnb.fit,traindata)
#nnb.predict.train$class
train.err.nnb=mean(nnb.predict.train$class !=traindata$Y)
train.err.nnb

#testing error for Non Parametric Naive Bayes 
nnb.predict.test=predict(nnb.fit,testdata)
#nnb.predict.test$class
test.err.nnb=mean(nnb.predict.test$class !=testdata$Y)
test.err.nnb

#Code the parametric Naive Bayes by myself
trainX = traindata[,-1]; 
trainY = as.factor(traindata[,1]);
testX = testdata[,-1];
testY = as.factor(testdata[,1])
p=dim(trainX)[2]; # p: number of predictors
y.levels = levels(trainY)
K= length(y.levels) # number of groups
mymean = matrix(0, K, p)  # save the p means for each of the K groups
mysd = matrix(0, K, p)    # save the p standard errors for each of the K groups
mymean.test=matrix(0,K,p)
mysd.test=matrix(0,K,p)
#training error 
for(k in 1:K){
  mymean[k,] = apply(trainX[trainY == y.levels[k],], 2, mean)
  mysd[k,] = apply(trainX[trainY == y.levels[k],], 2, sd)
}

#cbind(summary(t(mymean)), summary(t(mysd)))
w=mean(trainY==y.levels[1])  
mysd[2,which(mysd[2,]==0)] = 0.000001
tmp1 = colSums((t(trainX) - mymean[1,])^2/(mysd[1,]^2))
tmp2 = colSums((t(trainX) - mymean[2,])^2/mysd[2,]^2)
a=log(mysd[1,]/mysd[2,])

mylogit = log(w/(1-w))- tmp1 + tmp2 - sum(a)

mytrain.pred = rep(8,length(mylogit))
mytrain.pred[mylogit > 0]=3
#table(trainY, mytrain.pred)
train.err.mynb=mean(mytrain.pred !=trainY)
train.err.mynb


#testing error 
for(k in 1:K){
  mymean[k,] = apply(trainX[trainY == y.levels[k],], 2, mean)
  mysd[k,] = apply(trainX[trainY == y.levels[k],], 2, sd)
}

#cbind(summary(t(mymean)), summary(t(mysd)))

w=mean(trainY==y.levels[1])  
mysd[2,which(mysd[2,]==0)] = 0.000001
tmp1 = colSums((t(testX) - mymean[1,])^2/mysd[1,]^2)
tmp2 = colSums((t(testX) - mymean[2,])^2/mysd[2,]^2, na.rm=TRUE)

mylogit = log(w/(1-w))- tmp1 + tmp2 - sum(a)

mytest.pred = rep(8,length(mylogit))
mytest.pred[mylogit > 0]=3
#table(trainY, mytrain.pred)
test.err.mynb=mean(mytest.pred !=testY)
test.err.mynb

#PCA on the data

trainX = traindata[,-1]; 
trainY = as.factor(traindata[,1]);
testX = testdata[,-1];
testY = as.factor(testdata[,1])
pc.x=princomp(trainX,cor=F,score=T)
var.x=apply(pc.x$score,2,var)
tmp = cumsum(var.x)/sum(var.x)
pc.size= sum(tmp < 0.95) +1  #114 PCS used
tr.newx=as.matrix(trainX)%*%pc.x$loadings[,1:pc.size]
test.newx=as.matrix(testX)%*%pc.x$loadings[,1:pc.size]
new.traindata= data.frame(traindata$Y,tr.newx)
new.testdata = data.frame(testdata$Y,test.newx)
colnames(new.traindata)[1]='Y'
colnames(new.testdata)[1]='Y'
new.traindata$Y=as.factor(new.traindata$Y)
new.testdata$Y=as.factor(new.testdata$Y)

# LDA with PCA
lda.fit.pca= lda(Y~.,new.traindata)

#Train Error
lda.predict.train.pca=predict(lda.fit.pca,new.traindata)
train.err.lda.pca=mean(lda.predict.train.pca$class != new.traindata$Y)
train.err.lda.pca

#Test Error
lda.predict.test.pca=predict(lda.fit.pca,new.testdata)
test.err.lda.pca=mean(lda.predict.test.pca$class != testdata$Y)
test.err.lda.pca


#Parametric Naive Bayes with PCA
nb.fit.pca=naiveBayes(as.factor(Y)~.,data=new.traindata)
#training error for Parametric Naive Bayes 
nb.predict.train.pca=predict(nb.fit.pca,new.traindata)
nb.predict.train.pca
train.err.nb.pca=mean(nb.predict.train.pca!=new.traindata$Y)
train.err.nb.pca
#testing error for Parametric Naive Bayes 
nb.predict.test.pca=predict(nb.fit.pca,new.testdata)
nb.predict.test.pca
test.err.nb.pca=mean(nb.predict.test.pca !=new.testdata$Y)
test.err.nb.pca

#Non Parametric Naive Bayes with PCA
nnb.fit.pca=NaiveBayes(as.factor(Y)~.,data=new.traindata, usekernel=TRUE)
nnb.fit.pca
#training error for Non Parametric Naive Bayes 
nnb.predict.train.pca=predict(nnb.fit.pca,new.traindata)
#nnb.predict.train$class
train.err.nnb.pca=mean(nnb.predict.train.pca$class !=new.traindata$Y)
train.err.nnb.pca

#testing error for Non Parametric Naive Bayes 
nnb.predict.test.pca=predict(nnb.fit.pca,new.testdata)
#nnb.predict.test$class
test.err.nnb.pca=mean(nnb.predict.test.pca$class !=new.testdata$Y)
test.err.nnb.pca

#Code the parametric Naive Bayes by myself with PCA
new.trainX = new.traindata[,-1]; 
new.trainY = as.factor(new.traindata[,1]);
new.testX = new.testdata[,-1];
new.testY = as.factor(new.testdata[,1])
p=dim(new.trainX)[2]; # p: number of predictors
y.levels = levels(new.trainY)
K= length(y.levels) # number of groups
mymean.pc = matrix(0, K, p)  # save the p means for each of the K groups
mysd.pc = matrix(0, K, p)    # save the p standard errors for each of the K groups
mymean.pc.test=matrix(0,K,p)
mysd.pc.test=matrix(0,K,p)
#training error 
for(k in 1:K){
  mymean.pc[k,] = apply(new.trainX[new.trainY == y.levels[k],], 2, mean)
  mysd.pc[k,] = apply(new.trainX[new.trainY == y.levels[k],], 2, sd)
}

#cbind(summary(t(mymean)), summary(t(mysd)))
w=mean(new.trainY==y.levels[1])  
mysd.pc[2,which(mysd.pc[2,]==0)] = 0.000001
tmp1 = colSums((t(new.trainX) - mymean.pc[1,])^2/(mysd.pc[1,]^2))
tmp2 = colSums((t(new.trainX) - mymean.pc[2,])^2/mysd.pc[2,]^2)
a=log(mysd.pc[1,]/mysd.pc[2,])

mylogit = log(w/(1-w))- tmp1 + tmp2 - sum(a)

mytrain.pred.pc = rep(8,length(mylogit))
mytrain.pred.pc[mylogit > 0]=3
#table(trainY, mytrain.pred)
train.err.mynb.pc=mean(mytrain.pred.pc !=new.trainY)
train.err.mynb.pc

#testing error 
for(k in 1:K){
  mymean.pc[k,] = apply(new.trainX[new.trainY == y.levels[k],], 2, mean)
  mysd.pc[k,] = apply(new.trainX[new.trainY == y.levels[k],], 2, sd)
}

#cbind(summary(t(mymean)), summary(t(mysd)))

w=mean(new.trainY==y.levels[1])  
mysd.pc[2,which(mysd.pc[2,]==0)] = 0.000001
tmp1 = colSums((t(new.testX) - mymean.pc[1,])^2/mysd.pc[1,]^2)
tmp2 = colSums((t(new.testX) - mymean.pc[2,])^2/mysd.pc[2,]^2, na.rm=TRUE)

mylogit = log(w/(1-w))- tmp1 + tmp2 - sum(a)

mytest.pred.pc = rep(8,length(mylogit))
mytest.pred.pc[mylogit > 0]=3
#table(trainY, mytrain.pred)
test.err.mynb.pc=mean(mytest.pred.pc !=new.testY)
test.err.mynb.pc
# Problem 2 Logistic regression
#(1)Full Model

library(glmnet)
traindata$Y[traindata$Y==3]=0
traindata$Y[traindata$Y==8]=1
glm.fit=glm(Y ~ . ,data=traindata, control=list(maxit=50),family='binomial')
#Train error with Full Model
glm.probs=predict(glm.fit,data=traindata,type="response")
levels(trainY) # By default, the 1st level is the reference level, "3" is coded as 0
glm.pred=rep(3,length(glm.probs))
glm.pred[glm.probs>.5]=8
train.err.lr.full=mean(glm.pred != trainY)
train.err.lr.full
#Test Error with Full Model
testdata$Y[testdata$Y==3]=0
testdata$Y[testdata$Y==8]=1

glm.probs.test=predict(glm.fit,testdata,type="response")
levels(testY) # By default, the 1st level is the reference level, "3" is coded as 0
glm.pred.test=rep(3,length(glm.probs.test))
glm.pred.test[glm.probs.test>.5]=8
test.err.lr.full=mean(glm.pred.test != testY)
test.err.lr.full
#AIC
glm.fit0=glm(Y ~ 1 ,data=traindata, family='binomial' )
forward.AIC =  step(glm.fit0, scope=list(upper=glm.fit, lower=glm.fit0), trace=FALSE, direction="forward")
summary(forward.AIC)
#AIC Train Error
y.probs = predict(forward.AIC, traindata,type="response");
y.pred = rep(3, length(y.probs))
y.pred[y.probs>.5]=8
train.err.aic=mean(y.pred != trainY)
train.err.aic
#AIC Test Error
y.probs.test = predict(forward.AIC, testdata, type="response");
y.pred.test = rep(3, length(y.probs.test))
y.pred.test[y.probs.test>.5]=8
test.err.aic=mean(y.pred.test != testY)
test.err.aic
#BIC
forward.BIC =  step(glm.fit0, scope=list(upper=glm.fit, lower=glm.fit0), trace=1, direction="forward",k=log(1200))
summary(forward.BIC)
#BIC Train Error
y.probs = predict(forward.BIC, traindata, type="response");
y.pred = rep(3, length(y.probs))
y.pred[y.probs>.5]=8
train.err.bic=mean(y.pred != trainY)
train.err.bic
#BIC Test Error
y.probs.test = predict(forward.BIC, testdata, type="response");
y.pred.test = rep(3, length(y.probs.test))
y.pred.test[y.probs.test>.5]=8
test.err.bic=mean(y.pred.test != testY)
test.err.bic
#Lasso
mycv = cv.glmnet(as.matrix(trainX),trainY, alpha=1,family='binomial')
plot(mycv)

mycv.coef=predict(mycv, s=mycv$lambda.1se, type = "coefficients")
print(sum(mycv.coef!=0))

# Train error on Lasso
y.probs.train = predict(mycv,  s=mycv$lambda.1se, newx = as.matrix(trainX), type="response")
y.probs.train
y.pred.train = rep(3,length(y.probs.train))
y.pred.train[y.probs.train>.5]=8
#y.pred.train

train.err.lasso=mean(y.pred.train != trainY)
train.err.lasso
#Test error on Lasso

y.probs.test = predict(mycv,  s=mycv$lambda.1se, newx = as.matrix(testX), type="response")
y.probs.test
y.pred.test = rep(3,length(y.probs.test))
y.pred.test[y.probs.test>.5]=8
#y.pred.test

test.err.lasso=mean(y.pred.test != testY)
test.err.lasso


#Problem 3 SVM based on PCA

#Linear Kernel
svm.linear=svm(as.factor(Y)~.,data=new.traindata,type='C',kernel ="linear",scale=TRUE)
# Train Error for Linear Kernel
y.pred.train.linear.svm= predict(svm.linear,new.traindata)
train.err.linear.svm= mean(y.pred.train.linear.svm!= new.traindata$Y)
train.err.linear.svm
# Test Error for Linear Kernel
y.pred.test.linear.svm=predict(svm.linear,new.testdata)
test.err.linear.svm= mean(y.pred.test.linear.svm!= new.testdata$Y)
test.err.linear.svm


#Quadratic Kernel
svm.quad=tune.svm(as.factor(Y)~., data=new.traindata,type='C',kernel="polynomial",degree=2, gamma = 10^(-2:2))
#Pick gamma=0.1
svm.quad=svm(as.factor(Y)~.,data=new.traindata,type='C',kernel ="polynomial",degree=2 ,gamma=0.1,scale=TRUE)
# Train Error for Quadratic Kernel
y.pred.train.quad.svm= predict(svm.quad,new.traindata)
train.err.quad.svm= mean(y.pred.train.quad.svm!= new.traindata$Y)
train.err.quad.svm
# Test Error for Quadratic Kernel
y.pred.test.quad.svm=predict(svm.quad,new.testdata)
test.err.quad.svm= mean(y.pred.test.quad.svm!= new.testdata$Y)
test.err.quad.svm

#Guassian Kernel
svm.gau=tune.svm(as.factor(Y)~.,data=new.traindata,type='C',kernel ="radial" , gamma= 10^(-2:2), cost =10^(1:2) ,scale=TRUE)
#Pick gamma = 0.01, cost = 10
svm.gau=svm(as.factor(Y)~. ,data=new.traindata,type='C',kernel='radial',gamma=0.01,cost=10)
# Train Error for Gaussian Kernel
y.pred.train.gau.svm= predict(svm.gau,new.traindata)
train.err.gau.svm= mean(y.pred.train.gau.svm!= new.traindata$Y)
train.err.gau.svm
# Test Error for Gaussian Kernel
y.pred.test.gau.svm=predict(svm.gau,new.testdata)
test.err.gau.svm= mean(y.pred.test.gau.svm!= new.testdata$Y)
test.err.gau.svm

#Problem 4 Tree Models
library(tree)
tree.data=tree(as.factor(Y)~.,mindev=0.0005,data=traindata)
cv_tree=cv.tree(tree.data)
cv_tree
par(mfrow=c(1,2))
plot(cv_tree$size ,cv_tree$dev ,type="b")
plot(cv_tree$k ,cv_tree$dev ,type="b")
cv_tree=cv.tree(tree.data, K=10) #prune the tree based on misclassification rate
cv_tree$size[which.min(cv_tree$dev)]
bestsize=9
prune_tree=prune.tree(tree.data,best=9, method = "misclass")
summary(prune_tree)

# Train Error for Decision Tree
y.pred.train.tree= predict(prune_tree,traindata, type="class")
train.err.tree= mean(y.pred.train.tree!= traindata$Y)
train.err.tree
# Test Error for Decision Tree
y.pred.test.tree= predict(prune_tree,testdata, type="class")
test.err.tree= mean(y.pred.test.tree!= testdata$Y)
test.err.tree

print(prune_tree$frame$var)
#Problem 5 randomForest

library(randomForest)
traindata$Y[traindata$Y==3]=0
traindata$Y[traindata$Y==8]=1
testdata$Y[testdata$Y==3]=0
testdata$Y[testdata$Y==8]=1
p= ncol(traindata) - 1
m= round(sqrt(p))
rf.data=randomForest(as.factor(Y)~., data=traindata, mtry=m, importance = T,ntree=500)
rf.data
#Training Error
rf.pred.train=predict(rf.data,traindata)
rf.pred.train
train.err.rf=mean(rf.pred.train != traindata[,1])
train.err.rf
#Testing Error
rf.pred.test=predict(rf.data,testdata)
rf.pred.test
test.err.rf=mean(rf.pred.test!= testdata[,1])
test.err.rf
#Variable Importance Plot
summary(rf.data)
rf.data
varImpPlot(rf.data)
#Problem 6 boosting by gbm
library(gbm)

#Choosing Optimal Number of Observations
boost.data=gbm(Y~.,data=traindata,distribution="adaboost",n.trees=1000,bag.fraction=0.5, shrinkage=0.05,cv.folds=5)
gbm.perf(boost.data, method="cv")
# Number of boosting iterations = 429
#Train Error
boost.pred.train=predict(boost.data,newdata=traindata,n.trees=429, type="response")
boost.pred.train=ifelse(boost.pred.train>0.5,8,3)
train.err.boost=mean(boost.pred.train!=trainY)
train.err.boost

#Test Error
boost.pred.test=predict(boost.data,newdata=testdata,n.trees=429, type="response")
boost.pred.test=ifelse(boost.pred.test>0.5,8,3)
test.err.boost=mean(boost.pred.test!=testY)
test.err.boost

