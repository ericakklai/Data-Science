# 15 Apr, 2016

rm(list=ls())
library(foreign)
library(glmnet)
library(caret)
fname = c("armstrong-2002-v1",
          "armstrong-2002-v2",
          "bhattacharjee-2001",	
          "chowdary-2006",
          "dyrskjot-2003",
          "golub-1999-v1",
          "golub-1999-v2",
          "gordon-2002",
          "laiho-2007",
          "nutt-2003-v1",	
          "nutt-2003-v2",	
          "nutt-2003-v3",	
          "pomeroy-2002-v1",	
          "pomeroy-2002-v2",
          "ramaswamy-2001",	
          "shipp-2002-v1",
          "singh-2002",
          "su-2001",
          "west-2001",	
          "yeoh-2002-v1",
          "yeoh-2002-v2",	
          "alizadeh-2000-v1",	
          "alizadeh-2000-v2",	
          "alizadeh-2000-v3",	
          "bittner-2000",
          "bredel-2005",
          "chen-2002",
          "garber-2001",
          "khan-2001",
          "lapointe-2004-v1",	
          "lapointe-2004-v2",		
          "liang-2005",
          "risinger-2003",
          "tomlins-2006", 
          "tomlins-2006-v2")

flist = rep(0,length(fname))
flist[1:21] =sapply(fname[1:21], function(x){file.path("http://bioinformatics.rutgers.edu/Static/Supplements/CompCancer/Affymetrix",x,paste(x,"_database.txt",sep=""))})
flist[22:35] =sapply(fname[22:35], function(x){file.path("http://bioinformatics.rutgers.edu/Static/Supplements/CompCancer/CDNA",x,paste(x,"_database.txt",sep=""))})

rc = rep(0,length(flist))
acc = rep(0,length(flist))
err = rep(0,length(flist))
feat = rep(0,length(flist))
pred = rep(0,length(flist))
nas = rep(0,length(flist))
##reading in data
dat <- vector(mode = "list", length(flist))
for (i in (1:length(flist))){
  print (i)
  dat[[i]] =  read.table(flist[i], sep="\t")
  }

## remove na values and run the multinomial regression
#nas = c()
for (i in (1:length(flist))){
  print (i)
  datt = t(dat[[i]])
  x = datt[-1,-c(1,2)]
  y = datt[-1,2]
  class(x) = "numeric"
  idx = which(apply(x, 1, function(x){sum(is.na(x))!=0})=='TRUE')
  if (length(idx)>0){
    y = y[-idx]
    x = x[-idx,]
  }

  ## multinomial regression 
  mo = cv.glmnet(x, y,alpha=1, family = 'multinomial', type.measure = 'class', nfolds = 4)

  # we choose min lambda value as the best regularization constant
  rc[i] = mo$lambda.min
  
  #average accuracy over multiple cv trials
  acc[i] = 1-min(mo$cvm) 
  
  #misclassification rate 
  pred = predict(mo,x, type ='class')
  err[i] = 1-sum(y==pred)/length(y) 
 
  #number of genes picked
  feat[i] = mo$glmnet.fit$df[which.min(mo$cvm)]
  nas[i] = length(idx) }

#output result
write.csv(rc, file = "rc.csv", row.names = c(1:35))
write.csv(acc, file = "acc.csv", row.names = c(1:35))
write.csv(err, file = "err.csv", row.names = c(1:35))
write.csv(feat, file = "feat.csv", row.names = c(1:35))

