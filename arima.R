library(forecast)

ts.data<-read.csv("TractorSales.csv")
str(ts.data)
ts.data<-ts(data=ts.data[,2],start=c(2003,1),frequency = 12)
decomp.tractor<-decompose(ts.data)
plot(decomp.tractor)
####we can see all the factors 
plot(ts.data,xlab="years",ylab="tractor sales")
summary(ts.data)
###Lets make the data stationary by differencing
dif1.sales<-diff(ts.data,differences = 1)
plot.ts(dif1.sales)
adf.test(ts.data,alternative = "stationary",k=0)
#########
arima.auto.tractor<-auto.arima(ts.data)
arima.auto.tractor
accuracy(arima.auto.tractor)
####this is auto arima
plot(forecast.Arima(arima.auto.tractor,h=10))
##########################
##1.lag analysis
lag.plot(ts.data,lags = 9,do.lines = F)
acf(ts.data)
pacf(ts.data)

##2.Differnce the data to make it stationary over the mean
plot(diff(ts.data))

##3.log data to make data stationary over variance
plot(log10(ts.data))



######kpss test is a better test for adf.test in which low p value indicates non-stationarity in data
####


kpss.test(log10(ts.data))
ns <- nsdiffs(log10(ts.data))
if(ns > 0) {
  data_star <- diff(log10(ts.data),lag=frequency(log10(ts.data)),differences=ns)
} else {
  data_star <- log10(ts.data)
}
nd <- ndiffs(log10(ts.data))
if(nd > 0) {
  data_star <- diff(data_star,differences=nd)
}
###order is 1
####differnce the log data to make it stationary on mean and variance
plot(diff(log10(ts.data)),ylab="differenced log tractor sales",col="steelblue")

####get the order in acf and pacf plot visually 
#### acf gives AR factor while pacf gives order for MA
acf(diff(log10(ts.data)))###0
pacf(diff(log10(ts.data)))###1

###Model is p,d,q=0,1,1

arima.model<-auto.arima(log10(data),trace = F,approximation = F)
summary(arima.model)

#### summary shows that our model p,d,q is same as we estimated from the plot
prediction<-predict(arima.model,n.ahead = 36)
prediction
accuracy(arima.model)
####check for rmse and mape
plot(ts.data,xlim=c(2004,2018),ylim=c(1,1600),xlab="year",ylab="tractor sales")
lines(10^(prediction$pred),col="steelblue")
lines(10^(prediction$pred+2*prediction$se),col="orange")
lines(10^(prediction$pred-2*prediction$se),col="magenta")


Box.test(arima.model$residuals,lag = 20,type = "Ljung-Box")
plot(arima.model$residuals)
acf(arima.model$residuals)
#####there is no autocorelation as p value is more than 0.5
###so we are good to go as no information to extract




