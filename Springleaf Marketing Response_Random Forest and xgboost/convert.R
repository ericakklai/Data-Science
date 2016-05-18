# Ka Ki Lai (e.for.erica@gmail.com)
# Dec 15 2015
# Preparing data for random forest algorithm
# Converting data sample in to .csv to being read into python


library(readr)

load("train_cleaned.Rda")
load("test_cleaned.Rda")
load("label.Rda")
load("testid.Rda")

write_csv(train, "train_cleaned.csv")
write_csv(test, "test_cleaned.csv")

label <- data.frame(ID=label)
write_csv(label, "label.csv")

testid <- data.frame(ID=testid)
write_csv(testid, "testid.csv")

