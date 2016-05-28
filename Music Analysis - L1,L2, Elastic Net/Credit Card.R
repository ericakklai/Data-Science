#Part 2
filename2 = "~/Desktop/machinelearning/default of credit card clients.csv"
cre_dat = read.csv(filename2,header=TRUE, sep=",",skip=1)
cre_x = as.matrix(cre_dat[-c(1,25)])
cre_y = as.matrix(cre_dat[25])

tr_idx = sample(1:nrow(cre_dat),round(0.7*nrow(cre_dat)))
x_train = cre_x[tr_idx,]
y_train = cre_y[tr_idx,]
x_test = cre_x[-tr_idx,]
y_test = cre_y[-tr_idx,]
y_test = as.factor(y_test)

fit = cv.glmnet(x_train, as.factor(y_train), family = "binomial")
fit_lasso = cv.glmnet(x_train, as.factor(y_train), family = "binomial", alpha = 1)
fit_ridge = cv.glmnet(x_train, as.factor(y_train), family = "binomial", alpha = 0)
fit_elastic_net = cv.glmnet(x_train, as.factor(y_train), family = "binomial", alpha = 0.5)

plot(fit)
plot(fit_lasso,main="Lasso")
plot(fit_ridge,main="Ridge")
plot(fit_elastic_net,main="Elastic Net")

accuracy = function(a,b){
  sum(a==b)/length(a)
}

olr = predict(fit,x_test,type="class")
lasso = predict(fit_lasso,s=fit_lasso$lambda.min,x_test,type="class")
ridge = predict(fit_ridge,s=fit_ridge$lambda.min,x_test,type="class")
elastic_net = predict(fit_elastic_net,s=fit_lasso$lambda.min,x_test,type="class")

accuracy(olr,y_test)
accuracy(lasso,y_test)
accuracy(ridge,y_test)
accuracy(elastic_net,y_test)







