############################################################
##  I: Forecasting bus ridership by route                 ##    
##  Apoorva Bhide                                         ##
##  16 August 2018                                        ##
############################################################
#1. Wrangle data into time series
setwd('~/R Work/chicago_transit_data/')
rides_by_route <- read.csv('cta-ridership-bus-routes-daily-totals-by-route.csv', header = TRUE, stringsAsFactors = FALSE)
rides_by_route$date <- as.Date(rides_by_route$date)

rides_by_route$week <- strftime(rides_by_route$date, '%W')
rides_by_route$year <- strftime(rides_by_route$date, '%Y')
rides_by_week <- aggregate(rides ~ route + week + year, rides_by_route, sum)

ts_79_train <- ts(rides_by_week$rides[rides_by_week$route == 79], frequency = 365.25/7, start = c(2001,1), end = c(2016,12))
ts_79_test <- ts(rides_by_week$rides[rides_by_week$route == 79], frequency = 365.25/7, start = c(2017,1))
plot(ts_79_train)
plot(ts_79_test)
library(forecast)
plot(forecast(ts_79_train))

#Let's do a simple forecast with a naive method
f1 <- meanf(ts_79_train, h = 64)
f2 <- rwf(ts_79_train, h = 64)
f3 <- rwf(ts_79_train, drift = TRUE, h = 64)

plot(f2)
accuracy(f1)
accuracy(f1, ts_79_test)
accuracy(f2, ts_79_test)
accuracy(f3, ts_79_test)

f_ets <- stlf(ts_79_train, h = 64)
plot(f_ets)
accuracy(f_ets, ts_79_test)

auto_79 <- auto.arima(ts_79_train)
f_arima <- forecast(auto_79, h = 64)
plot(f_arima)
accuracy(f_arima, ts_79_test)

#At this level of granularity, the accuracy is not really good. Trying month level forecast...
rides_by_route$month <- strftime(rides_by_route$date, '%m')
rides_by_month <- aggregate(rides ~ route + month + year, rides_by_route, sum)

ts_79_train_m <- ts(rides_by_month$rides[rides_by_month$route == 79], frequency = 12)
ts_79_test_m <- ts(rides_by_month$rides[rides_by_month$route == 79], frequency = 12)
plot(ts_79_train_m)
plot(ts_79_test_m)
library(forecast)
plot(forecast(ts_79_train))

#Let's do a simple forecast with a naive method
f1 <- meanf(ts_79_train, h = 64)
f2 <- rwf(ts_79_train, h = 64)
f3 <- rwf(ts_79_train, drift = TRUE, h = 64)

plot(f2)
accuracy(f1)
accuracy(f1, ts_79_test)
accuracy(f2, ts_79_test)
accuracy(f3, ts_79_test)

f_ets <- stlf(ts_79_train, h = 64)
plot(f_ets)
accuracy(f_ets, ts_79_test)

auto_79 <- auto.arima(ts_79_train)
f_arima <- forecast(auto_79, h = 64)
plot(f_arima)
accuracy(f_arima, ts_79_test)
