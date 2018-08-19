############################################################
##  I: Forecasting bus ridership by route                 ##    
##  Apoorva Bhide                                         ##
##  16 August 2018                                        ##
############################################################
setwd('~/R Work/chicago_transit_data/')
rides_by_route <- read.csv('cta-ridership-bus-routes-daily-totals-by-route.csv', header = TRUE, stringsAsFactors = FALSE)
rides_by_route$date <- as.Date(rides_by_route$date)

rides_by_route$month <- strftime(rides_by_route$date, '%m')
rides_by_route$year <- strftime(rides_by_route$date, '%Y')
rides_by_month_train <- aggregate(rides ~ route + month + year, rides_by_route[rides_by_route$date < '2017-01-01',], sum)
rides_by_month_test <- aggregate(rides ~ route + month + year, rides_by_route[rides_by_route$date >= '2017-01-01',], sum)

route_79_train <- rides_by_month_train[rides_by_month_train$route == 79,]
route_79_test <- rides_by_month_test[rides_by_month_test$route == 79,]
plot(ts(route_79_train$rides))

#Basic Arima
ts_79_train <- ts(route_79_train$rides, frequency = 12, start = 2001)
plot(forecast(ts_79_train))
plot(hw(ts_79_train,h = 15))
ts_79_train <- diff(ts_79_train)


ts_79_test <- diff(ts(route_79_test$rides))
plot(ts_79_train)
acf(ts_79_train)
pacf(ts_79_train)
autoarima <- auto.arima(ts_79_train)
prediction <- forecast(autoarima, h = 15)
i <- 0
j <- 0
k <- 0
models <- data.frame()
for(i in 1:3)
{
  for(j in 0:1)
  {
    for(k in 1:4)
    {
      arima_model <- arima(ts_79_train,order = c(i,j,k))
      models <- rbind(models,data.frame(cbind(i,j,k,AIC(arima_model))))
    }
  }
}
#ARIMA(2,1,4) gives the best with the data
model <- arima(ts_79_train, c(2,1,4))
acf(resid(model))
Box.test(resid(model),type="Ljung-Box")
#There is autocorrelation in the residuals. Hmmm.
#Trying ARIMA(3,0,3) instead
model_2 <- arima(ts_79_train, c(3,0,3))
acf(resid(model_2))
Box.test(resid(model_2),type="Ljung-Box")
#This one looks good. Ljung-Box gives a p-value of 0.8572
plot(forecast(model_2,h = 15))
model_2_fc <- forecast(model_2,h = 15) 

accuracy(model_2_fc$mean, as.numeric(ts_79_test))

auto_fc <- forecast(autoarima,h = 15)
accuracy(auto_fc$mean, as.numeric(ts_79_test))
