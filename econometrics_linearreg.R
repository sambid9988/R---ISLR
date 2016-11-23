library(ggplot2)
library(HH)

reg_data<-read.csv("regression_auto.csv",stringsAsFactors = F)
####view of the data
(reg_data)
####checking for missing values column wise
colSums(is.na(reg_data))

###understanding the structure of data
str(reg_data)

####EDA
###one varibale out of 8 is categorical

table(reg_data$repairs)

reg_data$make<-as.factor(reg_data$make)
reg_data$foreign<-as.factor(reg_data$foreign)
reg_data$repairs<-as.factor(reg_data$repairs)
reg_data<-reg_data[,-3]
######dependent variable is mpg


ggplot(data=reg_data,aes(price))+geom_density()
ggplot(data=reg_data,aes(x=foreign,y=mpg,color=foreign))+geom_boxplot()
ggplot(data=reg_data,aes(x=repairs,y=mpg,color=repairs))+geom_boxplot()


#####fitting the model
lm_model<-lm(mpg~.,data = reg_data)
summary(lm_model)
#####F statistics is significant, R^2 is 0.76 means 76% of variation is explained by our regression line
ggplot(reg_data,aes(x=length,y=mpg))+geom_point()+geom_smooth(method="lm",se=F)

confint(lm_model,levle=0.95)
#### this gives confidence intervals around our coefficeints
###
vif(lm_model)
### checking mutlicollinearity using vif which is one of the essential assumption of vif
