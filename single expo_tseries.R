library(forecast)
########################################################
########SIMPLE EXPONENTIAL SMOOTHING####################
########################################################
rain <- scan("http://robjhyndman.com/tsdldata/hurst/precip1.dat",skip=1)
rainseries<-ts(rain,start = c(1813))
plot.ts(rainseries,col="steelblue")
#####data shows variance is constant about 25
rainseries_forecast<-HoltWinters(rainseries,beta = F,gamma = F)
rainseries_forecast
####alpha is given by 0.024
####forecasts are shown only from period of 1813-1912
rainseries_forecast$fitted

### this shows the fitted(forecasted values for the same time period)
plot(rainseries_forecast)
#### forecasted values are red in colour
####measuring accuracy(sse)
rainseries_forecast$SSE
rain.forecast<-forecast.HoltWinters(rainseries_forecast,h=8)
rain.forecast
plot.forecast(rain.forecast,col="steelblue")
#####the inteval for 80% and 95% is given 
###lets check if we can improve upon this
acf(rain.forecast$residuals,lag.max = 20,na.action = na.omit)
###We can autocorrelation is at 3 touching significant bounds
###now checking for non-zero correlations in the insample forecast we do ljung-box test
Box.test(rain.forecast$residuals,lag=20,type="Ljung-Box")
###pvalue 0.6 and stats=17.4 

plot.ts(rain.forecast$residuals)
### we can check for pattern whether the forecast errors are normally distributed or not
### so the foreacasted values are not autocorelated and the model can't be improved more
 
















