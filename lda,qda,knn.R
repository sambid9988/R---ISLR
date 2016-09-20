
###lda model
library(MASS)
lda_model<-lda(Direction~Lag1+Lag2,data=training)
summary(lda_model)
lda_predict<-predict(lda_model,testing)
names(lda_predict)
lda_predict_direction<-lda_predict$class
lda_predict_direction
##creating a confusion matrix

table(lda_predict_direction,direction_testing)
mean(lda_predict_direction!=direction_testing)



###qda model
qda_model<-qda(Direction~Lag1+Lag2,data=training)
qda_predict<-predict(qda_model,testing)
names(qda_predict)
qda_predict_direction<-qda_predict$class


table(qda_predict_direction,direction_testing)
mean(qda_predict_direction!=direction_testing)###40.7 misclassification error


###KNN
library(class)
std_data<-scale(Smarket[,c(2,3)])
training_data<-subset(std_data,Year<2005)
testing_data<-subset(std_data,Year>=2005)
training_direction<-Direction[Year<2005]

knn_prediction<-knn(training_data, testing_data, training_direction, 3)
table(knn_prediction,direction_testing)
mean(knn_prediction!=direction_testing)###46.4 misclassification error

