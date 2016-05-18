
library(xgboost)
library(readr)
rm(list=objects())

load("train_cleaned.Rda")
load("test_cleaned.Rda")
load("label.Rda")
load("testid.Rda")

#sampling
#train <- train[sample(nrow(train), 40000),]

clf <- xgboost(data        = data.matrix(train),
               label       = label,
               nrounds     = 400,
               eta = 0.05,
               max.depth = 12,
               subsample = 0.9,
               objective   = "binary:logistic",
               eval_metric = "auc")

submission <- data.frame(ID=testid)
submission$target <- NA
for (rows in split(1:nrow(test), ceiling((1:nrow(test))/10000))) {
  submission[rows, "target"] <- predict(clf, data.matrix(test[rows,]))
}

write_csv(submission, "xgboost_submission.csv")
