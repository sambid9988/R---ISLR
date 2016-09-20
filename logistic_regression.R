library(ISLR)
attach(Smarket)
fix(Smarket)
colSums(is.na(Smarket))##check for no of missing values

summary(Smarket)
View(cor(Smarket[,-9]))
pairs(Smarket)
###split data into training dataset and testing dataset
training<-subset(Smarket,Year<2005)
View(training)
testing<-subset(Smarket,Year>=2005)
direction_testing<-Direction[Year>=2005]

###fit logistic regression model
stock_model<-glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume,data=training,family = binomial)
summary(stock_model)


##now testing 
model_pred_probs<-predict(stock_model,testing,type = "response")
model_pred_Direction<-rep("Down",252)
model_pred_Direction[model_pred_probs>0.5]<-"Up"

##create confustion matrix to check accuracy of model
table(model_pred_Direction,direction_testing)
mean(model_pred_Direction!=direction_testing)
