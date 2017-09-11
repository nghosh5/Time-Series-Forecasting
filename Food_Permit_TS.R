## Load libraries and data 
#= 
library(readr)
library(lubridate)
library(xts)
library(tidyr)
library(dplyr)
library(zoo)
library(forecast)

fd <- read_csv(file.choose())
fd_backup<-fd
colnames(fd)[1] <- "Inspection_ID"
View(fd)
colnames(fd)[11] <- "Inspection_Date"
View(fd)
fd1<-subset(fd,select=c(Inspection_ID, Inspection_Date))
class(fd1$Inspection_Date)
fd1$Inspection_Date<-as.Date(fd1$Inspection_Date,format='%m/%d/%Y')
class(fd1$Inspection_Date)
class(fd1$Inspection_ID)
fd1$year_month<- as.yearmon(fd1$Inspection_Date)
fd1$year <- year(fd1$Inspection_Date)

View(fd1)
fd1_monthly <- fd1 %>% filter(year > 2009 & year < 2017) %>% group_by(year_month) %>%tally() %>% arrange(year_month)
colnames(fd1_monthly)[2]<-'No_of_Inspections'

# Convert to time series data 
fd1_ts <- ts(fd1_monthly[,2], start=c(2010,1), end=c(2016,12), freq=12)
# Or, using xts 
fd1_monthly$year_month <- as.yearmon(fd1_monthly$year_month, "%b %y") # notice the format option. it follows R's format. 
# convert to xts object 
fd1_xts <- xts(fd1_monthly$No_of_Inspections, order.by=fd1_monthly$year_month)
periodicity(fd1_xts)

# Plot individual time components 
par(mfrow=c(1,2))
monthplot(fd1_ts) # by month 
seasonplot(fd1_ts, year.labels=T, col=seq(1:11)) # by season 
# First, quarters 
fd1$year_quarter = as.yearqtr(fd1$Inspection_Date)
fd_quarterly <- fd1%>% filter(year > 2009 & year < 2017)%>% group_by(year_quarter) %>%tally() %>% arrange(year_quarter) # notice the mutate verb. 
colnames(fd_quarterly)[2]<-'No_of_Inspections'
fd_qxts <- xts(fd_quarterly$No_of_Inspections, order.by=as.yearqtr.default(fd_quarterly$year_quarter))
periodicity(fd_qxts)
plot(fd_qxts) 
# Yearly
View(fd1)
#Add year column
fd1$year <- year(fd1$Inspection_Date)
fd_yearly <- fd1%>% filter(year > 2009 & year < 2017)%>% group_by(year) %>%tally() %>% arrange(year) # notice the mutate verb. 
colnames(fd_yearly)[2]<-'No_of_Inspections'
fd_yts <- ts(fd_yearly$No_of_Inspections, start=c(2010), end=c(2016), freq=1) 
fd_yxts <- as.xts(fd_yts)
plot(fd_yxts)

# combine plots 
par(mfrow=c(1,3))
plot(fd1_ts) #monthly
plot(fd_qxts) #quarterly
plot(fd_yxts)#yearly
#additive
dec_fd <- decompose(fd1_ts, type="additive") 
plot(dec_fd) #
not bad! 
# periodic
mod_stl <- stl(fd1_xts,  s.window="periodic", t.window=4, robust=T) 
plot(mod_stl)
mod_stl1 <- stl(fd1_xts,  s.window=12, t.window=1, robust=T)
mod_stl2 <- stl(fd1_xts,  s.window=1, t.window=12, robust=T)
plot(mod_stl1)
plot(mod_stl2, col="blue")
#stl works only on additive models

# For multiplicative models, we simply used a log transformation of all TS components 
lfd_xts <- log(fd1_xts)
plot(fd1_xts)
plot(lfd_xts) # not a big difference. likely not a multiplicative series, but for illustration: 
mod_stl3 <- stl(lfd_xts, s.window="period")
plot(mod_stl3)
mod_stl4 <- stl(lfd_xts, s.window=1, t.window=12, robust=T)
plot(mod_stl4) # better? we will explore measures of model fit later on 
# Note: to convert decomposed values back to non-logged values 
exp(mod_stl3$time.series) 

########################

# FORECASTING 
#- 
# Naive forecasting = last data value, or last observation from the same season 
naive_fd <- meanf(fd1_ts, h=5)
names(naive_fd) 
head(naive_fd$fitted)
head(fd1_ts)
head(naive_fd$residuals)
# reasonable? 
plot(naive_fd) # h = number of periods 
# increase number of forecast periods 
plot(meanf(fd1_ts, h=12)) # as said, very naive, and likely not very useful 


# Drift = last data value + mean change in data 
plot(naive(fd1_ts, h=12)) # still not ideal 

# Seasonal naive
snaive_fd1 <- snaive(fd1_ts, h=24) 
head(snaive_fd1$fitted) # hmmm 
snaive_fd1$fitted # used the first 12 observations to build the model 
fd1_ts
snaive_fd1$residuals
plot(snaive_fd1) # considerably better
# Comes in various "flavors," from the simplest addressing only constant level, 
# to triple exponential (Holt-Winters) that addresses level, trend and season. 
# For all three components, select model types with three letters: 
# A = additive
# M = multiplicative
# N = none
# Z = automatic selection # YAY! We don't need to guess. The algorithm will do it for us. 

# Simple
simple_fd <- ets(fd1_ts, model="ANN") # Additive noise; No trend; No seasonal  
plot(simple_fd) 
accuracy(simple_fd) 
qqnorm(simple_fd$residuals)
qqline(simple_fd)
forecast(simple_fd, 15) # argument for k periods forecast, with 80% and 95% CI.  
plot(forecast(simple_fd, 15))

# Tripple 
# using ets: Error, Trend, Seasonality
fd1_ets <- ets(fd1_ts) 
# Takes a while becasuse it is fitting various models, looking for the smallest resids. 
qqnorm(fd1_ets$residuals)
qqline(fd1_ets$residuals) # excellent for the most part, but there's an outlier 
accuracy(fd1_ets) 
plot(fd1_ets) 
plot(forecast(fd1_ets, 10), col="steelblue", lwd=2)
#ARIMA modeling
#= 
# Autoregressive integrated moving average (ARIMA) 
# Main idea: model based on function of recent value and prediction resids. 
# To do so, we first need to convert the ts to become STATIONARY 
# = stabilizing" the mean of the series to smooth out the trend 

# Differencing 
# compute the differences between consecutive data points to get stationarity 
# First, check for trend

ndiffs(fd1_ts) # none. date seems stationary. 
adf.test(fd1_ts) # confirmed! 

fd_arima <- auto.arima(fd1_ts) # may take a while 
plot(forecast(fd_arima, 10)) 

accuracy(fd_arima)
qqnorm(fd_arima$residuals)
qqline(fd_arima$residuals) # reasonable  
# test for autocorrelation different from 0
Box.test(fd_arima$residuals, type="Ljung-Box") # might be ok after all 
