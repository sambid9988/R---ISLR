library(MASS)
library(ISLR)

#Simple Linear Regression
names(Boston)
plot(medv~lstat,Boston,pch=20,main="Scatter Plot")
fit1<-lm(medv~lstat,data=Boston)
fit1
summary(fit1)
#regression line
abline(fit1,col="blue",lwd=2)
names(fit1)
#gives the confident interval
confint(fit1)
predict(fit1,data.frame(lstat=c(5,10,25)),interval="confidence")##we can have prediction interval also
plot(predict(fit1),residuals(fit1))#to check any corelation or linearity between error terms
plot(predict(fit1),rstudent(fit1))#rstudent for outliers
plot(hatvalues(fit1))##leverage statistics
which.max(hatvalues(fit1))

##Multiple Linear Regression

fit2<-lm(medv~lstat+age,data=Boston)#age is significant
summary(fit2)
fit3<-lm(medv~.,data=Boston)
##age is not significant, that means there are lot of other terms which are correlated to age
summary(fit3)
plot(fit3)
par(mfrow=c(2,2))
fit4<-update(fit3,~.-age-indus)
summary(fit4)

####non_linear terms and interactions
fit5<-lm(medv~lstat*age,Boston)
#interaction term lstat+age+lstat*age
summary(fit5)
fit6<-lm(medv~lstat+I(lstat^2),Boston)
#polynomial term
summary(fit6)




attach(Boston)
par(mfrow=c(1,1))
plot(medv~lstat)
points(lstat,fitted(fit6),col="red",pch=20)
fit7<-lm(medv~poly(lstat,4))
points(lstat,fitted(fit7),col="blue",pch=20)



#qualitative predictors

fix(Carseats)
names(Carseats)
summary(Carseats)
cfit<-lm(Sales~.+Income:Advertising,Carseats)
summary(cfit)
contrasts(Carseats$ShelveLoc)


####Writing a R function
regplot<-function(x,y,....){
  fit<-lm(x,y)
  plot(x,y)
  abline(fit,col="red")
}
attach(Carseats)
regplot(Price,Sales,xlab="lstat",ylab="sales")
