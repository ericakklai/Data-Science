
library(xgboost)
library(readr)
rm(list=objects())

load("train_cleaned.Rda")
load("label.Rda")

etalist = c(0.03,0.04,0.05)
depthlist = c(12,14,16)
ratiolist = c(0.9,1)
nroundlist = c(400,500,600)

train = cbind(train,label)
train <- train[sample(nrow(train), 20000),]
label = train$label
train = subset(train, select=-c(label))

maxauc = 0
res = list()
bestres = list()
i = 0
for (eta in etalist) {
  for (depth in depthlist) {
    for (ratio in ratiolist) {
      for (nround in nroundlist) {
        i = i+1
        cat(i)
        cat("\n")
        cvres = xgb.cv(data        = data.matrix(train),
                       label       = label,
                       objective   = "binary:logistic",
                       eval_metric = "auc",
                       nfold = 2,
                       eta = eta,
                       nround = nround,
                       max.depth = depth,
                       subsample = ratio,
                       verbose = FALSE)
        res$eta = eta
        res$depth = depth
        res$ratio = ratio
        res$nround = nround
        res$cvres = cvres
        if (cvres$test.auc.mean[length(cvres$test.auc.mean)] > maxauc) {
          maxauc = cvres$test.auc.mean[length(cvres$test.auc.mean)]
          bestres = res
        }
      }
    }
  }
}

save(bestres,file="bestres.Rda")