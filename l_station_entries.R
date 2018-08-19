############################################################
##  II: Predicting entries at the l stations              ##    
##  Apoorva Bhide                                         ##
##  18 August 2018                                        ##
############################################################
#1: Wrangle data into correct format
setwd('~/R Work/chicago_transit_data/')
station_entries <- read.csv('cta-ridership-l-station-entries-daily-totals.csv', header = TRUE, stringsAsFactors = FALSE)
station_entries$date <- as.Date(station_entries$date)
#This is too much data. Restricting to 2012-2018...
station_entries <- station_entries[station_entries$date >= '2012-01-01',]
unique(station_entries$stationname)
#There are 148 stations. Wow, that's a lot. How are the total visits distributed?
entries_by_station <- aggregate(rides ~ stationname, station_entries, sum)

#Find out the busy stations on weekdays vs weekends
avg_traffic_weekday <- aggregate(rides ~ stationname, station_entries[station_entries$daytype == 'U',], mean)
avg_traffic_weekend <- aggregate(rides ~ stationname, station_entries[station_entries$daytype == 'W'| station_entries$daytype == 'A',], mean)
avg_traffic <- merge(avg_traffic_weekday, avg_traffic_weekend, by = 'stationname')
plot(x = avg_traffic$rides.x, y = avg_traffic$rides.y, xlab = 'Rides on weekdays', ylab = 'Rides on weekends')
#Wider distribution than bus routes, but practically all stations have more rides on weekends 

station_entries$month <- strftime(station_entries$date, "%m")
station_entries$weekday <- strftime(station_entries$date, "%a")

#Divide into train and test
sample_size <- floor(0.75 * nrow(station_entries))
set.seed(2017)
train_index <- sample(seq_len(nrow(station_entries)), size = sample_size)
station_train <- station_entries[train_index,]
station_test <- station_entries[-train_index,]

library(caret)
train_control<- trainControl(method="cv", number=3, savePred=TRUE)

#Convert categorical variables to factors
station_train$stationname <- as.factor(station_train$stationname)
station_train$daytype <- as.factor(station_train$daytype)
station_train$month <- as.factor(station_train$month)
station_train$weekday <- as.factor(station_train$weekday)
#Repeat for test
station_test$stationname <- as.factor(station_test$stationname)
station_test$daytype <- as.factor(station_test$daytype)
station_test$month <- as.factor(station_test$month)
station_test$weekday <- as.factor(station_test$weekday)

#Training linear model with cross-validation
lin_model <- train(rides ~ stationname + daytype + month + weekday,
                   data = station_train, trControl = train_control, method="lm")
summary(lin_model)

station_test$predicted_rides <- predict(lin_model,station_test)
mse.train <- mean(lin_model$residuals ^ 2) 
mse.test  <- sum((station_test$predicted_rides - station_test$rides)^2)/(nrow(station_entries)-length(station_train)-2)

#Regular linear model, works much faster
lm_model <- lm(rides ~ stationname + daytype + month + weekday,
               data = station_train)
summary(lm_model)

station_test$predicted_rides <- predict(lm_model,station_test)
mse.train <- mean(lm_model$residuals ^ 2) 
mse.test  <- sum((station_test$predicted_rides - station_test$rides)^2)/(nrow(station_entries)-length(station_train)-2)

print(length(which(station_test$predicted_rides < 0))/nrow(station_test))
#The weird thing - 6.7% of the predicted values are negative.

#Let's try modeling the log of rides
lm_model <- lm(log(rides + 1) ~ stationname + daytype + month + weekday,
               data = station_train)
summary(lm_model)
