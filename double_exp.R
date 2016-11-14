library(forecast)
skirts <- scan("http://robjhyndman.com/tsdldata/roberts/skirts.dat",skip=5)
head(skirts)
sk.series<-ts(skirts,start = c(1886))
plot(sk.series)

skirt.forecast<-HoltWinters(sk.series,gamma=F)
skirt.forecast
####alpha=0.83,beta=1
###this values show how the series is dependent on recent values
skirt.forecast$SSE
plot(skirt.forecast)
### we can see the forecasted values quite agree but still they lag behind to a certain level
skirt.holt.forecast<-forecast.HoltWinters(skirt.forecast,h=19)
plot(skirt.holt.forecast)
acf(skirt.holt.forecast$residuals,lag.max = 20,na.action = na.omit)
###at lag5 it exceeds the bound
Box.test(skirt.holt.forecast$residuals, lag=20, type="Ljung-Box")
###pvalue less than 0.5 shows we are good
plot(skirt.holt.forecast$residuals)
### residuals seem good to go


#############################################
