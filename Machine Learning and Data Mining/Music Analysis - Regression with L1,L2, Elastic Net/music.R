# Apr 18, 2016

rm(list = ls())
#read data and train-test splitting
filename1 = "~/Desktop/machinelearning/Geographical Original of Music/default_plus_chromatic_features_1059_tracks.txt"
dat <- read.csv(filename1, header=FALSE)
n = dim(dat)[1]
train_idx = sample(1:n,round(0.7*n))

train = dat[train_idx,]
test = dat[-train_idx,]
lat = train[,117]
long = train[,118]
xtrain = as.matrix(train[,1:116])
lat_test = test[,117]
long_test = test[,118]
xtest = as.matrix(test[,1:116])
#straightforward
sum1 = lm (lat~xtrain)
summary(sum1)

sum2 = lm(long~xtrain)
summary(sum2)

# remove duplicated column
rm_duplicate = function(x){
  d = matrix(0,dim(x)[1],dim(x)[2])
  for (i in 1:dim(x)[1]){
    d[i,] = duplicated(x[i,])
  }

  x[,which(apply(d,2,sum)!=nrow(x))]
}

x_new = rm_duplicate(xtrain)
dim(x_new)
fit_new_lat = lm(lat~x_new)
summary(fit_new_lat)
fit_new_long = lm(long~x_new)
summary(fit_new_long)

pred_lat = predict(fit_new_lat,as.data.frame(x_new))
pred_long = predict(fit_new_long,as.data.frame(x_new))

plot(lat,pred_lat)
plot(long,pred_long)


lat2 = lat - min(lat) + 1
long2 = long - min(long) + 1

library(MASS)
#boxcox for latitude
bc = boxcox(lat2~x_new)
lamb = bc$x[which.max(bc$y)] # best lambda
lat_bc_lamb = (lat2^lamb-1)/lamb
model_bc_lat_lamb = lm(lat_bc_lamb~x_new)
summary(model_bc_lat_lamb)

#boxcox for long
bc_long = boxcox(long2~x_new)
lamb_long = bc_long$x[which.max(bc_long$y)] # best lambda
long_bc_lamb = (long2^lamb_long-1)/lamb_long
model_bc_long_lamb = lm(long_bc_lamb~x_new)
summary(model_bc_long_lamb)


library(glmnet)
for (i in 0:4) {
  assign(paste("fit_lat", i, sep=""), cv.glmnet(x_new,lat, type.measure="mse", 
                                                alpha=i/4,family="gaussian"))
}

plot(fit_lat4,main="Lasso")
plot(fit_lat0,main="Ridge")
plot(fit_lat1,main="Elastic net1")
plot(fit_lat2,main="Elastic net2")
plot(fit_lat3,main="Elastic net3")


#predict on xtest lat
xtest =  rm_duplicate(xtest) 
beta = coef(fit_new_lat)

pred_ols_lat = cbind(rep(1,nrow(xtest)),as.matrix(xtest))%*%beta
pred_lasso_lat = predict(fit_lat4,s=fit_lat4$lambda.min,xtest)
pred_ridge_lat = predict(fit_lat0,s=fit_lat0$lambda.min,xtest)
pred_elnet1_lat = predict(fit_lat1,s=fit_lat1$lambda.min,xtest)
pred_elnet2_lat = predict(fit_lat2,s=fit_lat2$lambda.min,xtest)
pred_elnet3_lat = predict(fit_lat3,s=fit_lat3$lambda.min,xtest)

mse_ols_lat = mean((pred_ols_lat-lat_test)^2)
mse_lasso_lat = mean((pred_lasso_lat-lat_test)^2)
mse_ridge_lat = mean((pred_ridge_lat-lat_test)^2)
mse_elnet1_lat = mean((pred_elnet1_lat-lat_test)^2)
mse_elnet2_lat = mean((pred_elnet2_lat-lat_test)^2)
mse_elnet3_lat = mean((pred_elnet3_lat-lat_test)^2)

#compare box-cox and unbox-cox MSE
trans_back = function(x,y){
  x = sapply(x,function(a){max(a,1)})
  (x*lamb+1)^(1/lamb)+min(y)-1
}
beta_bc = coef(model_bc_lat_lamb)
pred_ols_lat_bc = trans_back(cbind(rep(1,nrow(xtest)),as.matrix(xtest))%*%beta_bc,lat)
mse_ols_lat_bc = mean((pred_ols_lat_bc-lat_test)^2)



#longtitude 
for (i in 0:4) {
  assign(paste("fit_long", i, sep=""), cv.glmnet(x_new,long, type.measure="mse", 
                                                alpha=i/4,family="gaussian"))
}

plot(fit_long4,main="Lasso")
plot(fit_long0,main="Ridge")
plot(fit_long1,main="Elastic net1")
plot(fit_long2,main="Elastic net2")
plot(fit_long3,main="Elastic net3")

beta_long = coef(fit_new_long)
pred_ols_long = cbind(rep(1,nrow(xtest)),as.matrix(xtest))%*%beta_long
pred_lasso_long = predict(fit_long4,s=fit_long4$lambda.min,xtest)
pred_ridge_long = predict(fit_long0,s=fit_long0$lambda.min,xtest)
pred_elnet1_long = predict(fit_long1,s=fit_long1$lambda.min,xtest)
pred_elnet2_long = predict(fit_long2,s=fit_long2$lambda.min,xtest)
pred_elnet3_long = predict(fit_long3,s=fit_long3$lambda.min,xtest)

mse_ols_long = mean((pred_ols_long-long_test)^2)
mse_lasso_long = mean((pred_lasso_long-long_test)^2)
mse_ridge_long = mean((pred_ridge_long-long_test)^2)
mse_elnet1_long = mean((pred_elnet1_long-long_test)^2)
mse_elnet2_long = mean((pred_elnet2_long-long_test)^2)
mse_elnet3_long = mean((pred_elnet3_long-long_test)^2)



