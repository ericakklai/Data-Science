# STAT 425 Final Project : Forecast use of a city bikeshare system
# Ka Ki Lai (kakilai2)
# Dec 16, 2015

rm(list = ls())
library(lubridate)
library(ggplot2)
library(dplyr) 
library(corrplot)
library(randomForest)
library(scales)

setwd("/Users/EricaLai/Desktop/STAT 425/Final Project")

#Data Input
train = read.csv("/Users/EricaLai/Desktop/STAT 425/Final Project/train.csv")
test = read.csv("/Users/EricaLai/Desktop/STAT 425/Final Project/test.csv")

# Exploratory Data Analysis
dim(train)
dim(test)

names(train)
names(test)

summary(train)
summary(test)

str(train)
str(test)


#Exploratory Data Analysis (EDA)


# Add some new features, such as hour of the day, time, day
train$hour  = hour(train$datetime)
train$times = as.POSIXct(strftime(ymd_hms(train$datetime), format="%H:%M:%S"), format="%H:%M:%S")
train$jitter_times = train$times+minutes(round(runif(nrow(train),min=0,max=59)))
train$day = wday(train$datetime, label=TRUE)

# begin on Monday (coded as 1) and end 
# on Sunday (coded as 7)
train$day = ifelse(wday(train$datetime)==1, 7, wday(train$datetime)-1)
train$day = as.factor(train$day)
train$month = as.factor(month(train$datetime))
levels(train$day)=c("Mon", "Tue", "Wed", "Thurs", "Fri", "Sat", "Sun")

x_axis = "jitter_times"
y_axis = "count"
color  = "temp" 

ggplot(train[train$workingday==1,], aes_string(x=x_axis, y=y_axis, color=color)) +
  geom_point(position=position_jitter(w=0.0, h=0.4)) +
  theme_light(base_size=20) +
  xlab("Hour of the Day") +
  scale_x_datetime(breaks = date_breaks("4 hours"), labels=date_format("%I:%M %p")) + 
  ylab("Number of Bike Rentals") +
  scale_colour_gradientn("Temp (°C)", colours=c("#5e4fa2", "#3288bd", "#66c2a5", "#abdda4", "#e6f598", "#fee08b", "#fdae61", "#f46d43", "#d53e4f", "#9e0142")) +
  ggtitle("Bike Rentals on Workdays") +
  theme(plot.title=element_text(size=18))

ggplot(train[train$workingday==0,], aes_string(x=x_axis, y=y_axis, color=color)) +
  geom_point(position=position_jitter(w=0.0, h=0.4)) +
  theme_light(base_size=20) +
  xlab("Hour of the Day") +
  scale_x_datetime(breaks = date_breaks("4 hours"), labels=date_format("%I:%M %p")) + 
  ylab("Number of Bike Rentals") +
  scale_colour_gradientn("Temp (°C)", colours=c("#5e4fa2", "#3288bd", "#66c2a5", "#abdda4", "#e6f598", "#fee08b", "#fdae61", "#f46d43", "#d53e4f", "#9e0142")) +
  ggtitle("Bike Rentals on Weekends/Holidays") +
  theme(plot.title=element_text(size=18))

color  = "humidity" 
ggplot(train[train$workingday==1,], aes_string(x=x_axis, y=y_axis, color=color)) +
  geom_point(position=position_jitter(w=0.0, h=0.4)) +
  theme_light(base_size=20) +
  xlab("Hour of the Day") +
  scale_x_datetime(breaks = date_breaks("4 hours"), labels=date_format("%I:%M %p")) + 
  ylab("Number of Bike Rentals") +
  scale_colour_gradientn("humidity", colours=c("#5e4fa2", "#3288bd", "#66c2a5", "#abdda4", "#e6f598", "#fee08b", "#fdae61", "#f46d43", "#d53e4f", "#9e0142")) +
  ggtitle("Bike Rentals on Workdays") +
  theme(plot.title=element_text(size=18))

# You can change "color" to other numerical variables and repeat the above plots
color  = "windspeed" 
ggplot(train[train$workingday==1,], aes_string(x=x_axis, y=y_axis, color=color)) +
  geom_point(position=position_jitter(w=0.0, h=0.4)) +
  theme_light(base_size=20) +
  xlab("Hour of the Day") +
  scale_x_datetime(breaks = date_breaks("4 hours"), labels=date_format("%I:%M %p")) + 
  ylab("Number of Bike Rentals") +
  scale_colour_gradientn("Windspeed", colours=c("#5e4fa2", "#3288bd", "#66c2a5", "#abdda4", "#e6f598", "#fee08b", "#fdae61", "#f46d43", "#d53e4f", "#9e0142")) +
  ggtitle("Bike Rentals on Workdays") +
  theme(plot.title=element_text(size=18))




#Display the rental for X1-by-X2, where X1: day (of the week) and X2: hour. 


# Calculate the average count for each day/time, store in a dataframe
hourly = group_by(train, day, hour)
day_hour_counts = summarise(hourly, count=mean(count))
day_hour_counts[1:5,]

# plot heat mat with ggplot for wday
ggplot(day_hour_counts, aes(x = hour, y = day)) + 
  geom_tile(aes(fill = count)) + 
  scale_fill_gradient(name="Average Counts", low="white", high="green") + 
  theme(axis.title.y = element_blank())

# Display the rental for X1-by-X2, 
# X1: season 
# X2: hour
hourly = group_by(train, season, hour)
day_hour_counts = summarise(hourly, count=mean(count))

# plot heat mat with ggplot for season
ggplot(day_hour_counts, aes(x = hour, y = season)) + 
  geom_tile(aes(fill = count)) + 
  scale_fill_gradient(name="Average Counts", low="white", high="pink") +
  scale_y_continuous(breaks=c(1, 2, 3, 4),
                     labels=c("Spring", "Summer", "Fall", "Winter")) + 
  theme(axis.title.y = element_blank())

# plot heat mat with ggplot for month
hourly = group_by(train, month, hour)
day_hour_counts = summarise(hourly, count=mean(count))


ggplot(day_hour_counts, aes(x = hour, y = month)) + 
  geom_tile(aes(fill = count)) + 
  scale_fill_gradient(name="Average Counts", low="white", high="blue") +
  theme(axis.title.y = element_blank())

#ggplot(day_hour_counts, aes(x=hour, y=count)) +
 # geom_bar(aes(fill = as.factor(season)), position = "dodge", stat="identity")

hourly = group_by(train, workingday, hour)
workingday_hour_counts = summarise(hourly, count=mean(count))
ggplot(workingday_hour_counts, aes(x=hour, y=count)) +
  geom_bar(aes(fill = as.factor(workingday)), position = "dodge", stat="identity")

#Create Some New Variables
DataFeature = function(data) {
  old.features = c("holiday",
                   "season",
                   "workingday",
                   "weather",
                   "temp",
                   "atemp",
                   "humidity",
                   "windspeed");
  newdata = data[, old.features];
  newdata$weather = as.factor(newdata$weather)
  newdata$season = as.factor(newdata$season)
  levels(newdata$season)=c("Spring", "Summer", "Fall", "Winter")
  
  newdata$hour= as.factor(hour(data$datetime))
  newdata$year = as.factor(year(data$datetime))
  newdata$month = as.factor(month(data$datetime))
  
  newdata$wday = ifelse(wday(data$datetime)==1, 7, wday(data$datetime)-1)
  newdata$wday = as.factor(newdata$wday)
  levels(newdata$wday)=c("Mon", "Tue", "Wed", "Thurs", "Fri", "Sat", "Sun")
  return(newdata)
}


train = read.csv("train.csv")
test = read.csv("test.csv")

Newtrain = DataFeature(train)
Newtrain$lcount = log(train$count +1)
Newtrain$lcasual = log(train$casual +1)
Newtrain$lregistered = log(train$registered +1)
Newtest = DataFeature(test)

#Model 1: A Simple Model
#Predict by the average counts from the same month, the same hour of the day, 
#and the same day of the week.
submission = data.frame(datetime=test$datetime, 
                        hour=Newtest$hour, 
                        wday=Newtest$wday)


for (i_year in unique(Newtest$year)){
  for (i_month in unique(Newtest$month)) {
    cat("Year: ", i_year, "\tMonth: ", i_month, "\n")
    trainSubset = filter(Newtrain, year==i_year, month==i_month)
    by_wday_hour = group_by(trainSubset, wday, hour)
    wday_hour_lcounts = summarise(by_wday_hour, lcounts=mean(lcount))
    
    testLocs = Newtest$year ==i_year & Newtest$month == i_month
    tmp = submission[testLocs, ]
    tmp = inner_join(tmp, wday_hour_lcounts)
    submission[testLocs, "count"] = exp(tmp$lcounts)-1
  }
}
submission=submission[, c(1, 4)]
write.csv(submission, file = "Submission1_simple_model.csv", row.names=FALSE)



#Model 2:  A Simple Linear Regression Model with AIC

submission = data.frame(datetime=test$datetime, 
                        hour=Newtest$hour, 
                        wday=Newtest$wday)

for (i_year in unique(year(ymd_hms(test$datetime)))) {
  for (i_month in unique(month(ymd_hms(test$datetime)))) {
    cat("Year: ", i_year, "\tMonth: ", i_month, "\n")
    testLocs = year(ymd_hms(test$datetime))==i_year & month(ymd_hms(test$datetime))==i_month
    trainLocs = ymd_hms(train$datetime) <= min(ymd_hms(test$datetime[testLocs]))
    if (i_year == 2011) {
    myfit = lm(lcount ~ workingday + temp + atemp + humidity +hour+ wday +windspeed, data=Newtrain[trainLocs,])
    AIC = step(myfit, data = Newtrain[trainLocs,], direction = "both") }
    else {
      myfit = lm(lcount ~ workingday + temp + atemp + humidity +hour+ wday +windspeed + season, data=Newtrain[trainLocs,])
      AIC = step(myfit, data = Newtrain[trainLocs,], direction = "both") 
    }
    mypred.count = exp(predict(AIC, Newtest[testLocs,]))-1
    mypred.count[mypred.count < 0] = 0; 
    submission[testLocs, "count"] = mypred.count
  }
}
submission=submission[, c(1, 4)]
write.csv(submission, file = "Submission1_simple_LS_model_AIC_season.csv", row.names=FALSE)



# Model 3 Random Forest on lcount

submission = data.frame(datetime=test$datetime, 
                        hour=Newtest$hour, 
                        wday=Newtest$wday)

for (i_year in unique(year(ymd_hms(test$datetime)))) {
  for (i_month in unique(month(ymd_hms(test$datetime)))) {
    cat("Year: ", i_year, "\tMonth: ", i_month, "\n")
    testLocs = year(ymd_hms(test$datetime))==i_year & month(ymd_hms(test$datetime))==i_month
    trainLocs = ymd_hms(train$datetime) <= min(ymd_hms(test$datetime[testLocs]))
    rfModel = randomForest(lcount~holiday + season + workingday + weather + temp + atemp + humidity + windspeed + hour + wday+year, data = Newtrain[trainLocs,], ntree = 500, mtry = 7)
    yhat = exp(predict(rfModel, newdata=Newtest[testLocs,])) - 1 
    yhat[yhat < 0] = 0; 
    submission[testLocs, "count"] = yhat
  }
}
submission=submission[, c(1, 4)]
write.csv(submission, file = "Submission5ind_rf_LS_mtry7ntree100_model.csv", row.names=FALSE)

# Model 4 Random Forest on lcasual and lregistered

submission = data.frame(datetime=test$datetime, 
                        hour=Newtest$hour, 
                        wday=Newtest$wday)

for (i_year in unique(year(ymd_hms(test$datetime)))) {
  for (i_month in unique(month(ymd_hms(test$datetime)))) {
    cat("Year: ", i_year, "\tMonth: ", i_month, "\n")
    testLocs = year(ymd_hms(test$datetime))==i_year & month(ymd_hms(test$datetime))==i_month
    trainLocs = ymd_hms(train$datetime) <= min(ymd_hms(test$datetime[testLocs]))
    data = Newtrain[trainLocs,]
    
    
    rfModel = randomForest(lcasual~holiday + season + workingday + weather + temp + atemp + humidity + windspeed + hour + wday+year, data = data, ntree = 500, nodesize=11,  mtry=7 )#,  sampsize=sampleSize)#,  importance=TRUE,corr.bias=TRUE, mtry=6 ) 
    yhat1 = exp( predict(rfModel, newdata=Newtest[testLocs,]) ) - 1
    
    rfModel = randomForest(lregistered~holiday + season + workingday + weather + temp + atemp + humidity + windspeed + hour + wday+year, data = data, ntree = 500, nodesize=11,  mtry=7 )#,  sampsize=sampleSize)#,  importance=TRUE,corr.bias=TRUE, mtry=6 ) 
    yhat2 = exp( predict(rfModel, newdata=Newtest[testLocs,]) ) - 1
    
    yhat = yhat1 + yhat2
    yhat[yhat < 0] = 0;
    
    submission[testLocs, "count"] = yhat
  }
}

submission=submission[, c(1, 4)]
write.csv(submission, file = "Submission5ind_rf_LS_c_a 500mtry6_model.csv", row.names=FALSE)

