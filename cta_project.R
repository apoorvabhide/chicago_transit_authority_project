############################################################
##  I: Forecasting bus ridership by route                 ##    
##  Apoorva Bhide                                         ##
##  16 August 2018                                        ##
############################################################
setwd('~/R Work/chicago_transit_data/')
rides_by_route <- read.csv('cta-ridership-bus-routes-daily-totals-by-route.csv', header = TRUE, stringsAsFactors = FALSE)
rides_by_route$date <- as.Date(rides_by_route$date)

#Generally, what are the busy routes?
routes <- aggregate(rides ~ route, rides_by_route, sum)
#Maybe weekday busy routes are different from weekend busy routes
rides_by_route_12_18 <- rides_by_route[rides_by_route$date >= "2012-01-01" & rides_by_route$date <= "2018-03-31",]
route_9 <-rides_by_route_12_18[rides_by_route_12_18$route == 9,]
plot(ts((diff(rides_by_route_12_18$rides[rides_by_route_12_18$route == 9]))))

#What is the average traffic on every route?
avg_traffic_weekday <- aggregate(rides ~ route, rides_by_route_12_18[rides_by_route_12_18$daytype == 'U',], mean)
avg_traffic_weekend <- aggregate(rides ~ route, rides_by_route_12_18[rides_by_route_12_18$daytype == 'W'| rides_by_route_12_18$daytype == 'A',], mean)
avg_traffic <- merge(avg_traffic_weekday, avg_traffic_weekend, by = 'route')
plot(x = avg_traffic$rides.x, y = avg_traffic$rides.y, xlim = c(0,25000), ylim = c(0,25000), xlab = 'Rides on weekdays', ylab = 'Rides on weekends')

#Create year-wise split of routes
rides_by_route_12_18$year <- strftime(rides_by_route_12_18$date, format = '%Y')
route_yr <- aggregate(rides ~ route + year, rides_by_route_12_18, sum)
library(reshape)
route_year <- cast(route_yr, route ~ year)
write.csv(route_year,'route_year.csv')
route_year <- route_year[,-c(3:7)]
route_year$diff <- route_year$`2012` - route_year$`2018`
route_year$diff <- route_year$diff/route_year$`2012`

route_year <- na.omit(route_year)
plot(x = route_year$`2012`, y = route_year$diff, xlab = 'Rides in 2012', ylab = 'Difference in rides, 2012-2018', ylim = c(0.4,1))
hist(route_year$diff)

#Only keeping the top 10 routes by total ridership between 2012-2018
routes_top <- aggregate(rides ~ route, rides_by_route_12_18, sum)
routes_top <- head(routes_top[order(-routes_top$rides),], 10)

#The busiest route in terms of volume seems to be route 79
route_79 <- rides_by_route_12_18[rides_by_route_12_18$route == 79,]
#First, reserve validation set as Feb 2018 and test as March 2018
validation_79 <- route_79[route_79$date >= '2018-02-01' & route_79$date <= '2018-02-28',]
test_79 <- route_79[route_79$date >= '2018-03-01' & route_79$date <= '2018-03-31',]
#Let's only consider data from 2015 onwards
route_79 <- route_79[route_79$date>='2015-01-01' & route_79$date <= '2018-01-31',]

plot(forecast(msts(route_79$rides, seasonal.periods = c(7,365.25))))

plot(forecast(ts(route_79$rides)))
#There is a downward trend in the data, and it also has more variance at the beginning than later
ts_79 <- msts(route_79$rides, seasonal.periods = c(7, 365.25))
ts_79 <- diff(log(ts_79))
plot(ts_79)

library(forecast)
#Let's make a forecast, using simple exponential smoothing
ses_79 <- tbats(ts_79)
feb_pred <- forecast(ses_79)
plot(feb_pred)
validation_79 <- ts(diff(log(validation_79$rides)))
plot(ts(validation_79))

mse <- function()
{
  error <- mean((feb_pred - validation_79)^2)
  print(error)
}
mse()

library(astsa)
acf(ts_79)
#
pacf(ts_79)
