library(readr)

bestrf <- read_csv("best_rf.csv")
bestxgb <- read_csv("best_xgb.csv")

submission <- data.frame(ID=bestrf$ID)
submission[,"target"] = bestrf$target*0.2+bestxgb$target*0.8
write_csv(submission, "xgboost_submission6.3.csv")
