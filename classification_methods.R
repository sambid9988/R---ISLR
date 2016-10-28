


library(caret)
library(ggplot2)
library(mlbench)
library(pROC)
library(caretEnsemble)
library(kernlab)

data("Sonar")
View(Sonar)
dim(Sonar)
str(Sonar)
set.seed(1)
intrain<-createDataPartition(Sonar$Class,p=0.7,list = F)
training<-Sonar[intrain,]
testing<-Sonar[-intrain,]
dim(testing)
mycontrol<-trainControl(method = "cv",number=5,verboseIter = T)
model_preds <-train(Class~.,data=training,method="",tuneLength=5,trControl=mycontrol,metric="Accuracy")
model_preds
prediction<-predict(model_preds,newdata=testing)
confusionMatrix(prediction,testing$Class)

#####Sonardataset#########
########################
######GBM accuracy-85%
######SVM(radial) accuracy-90%,svmpoly=83.87%
######adaptive boosting takes time-85.4%
######neuralnet82%-deepnet-50%
####xbglinear=80.6
####xgbtree=88%(kaswwwwww)
#####c5.0=77%
#####oblique trees 83.8%,randomforest=83%
#######mars 80.6
######dwdPoly=80.6,dwdRadial=83%
####randomGLM slowest method till now with accuracy 75%
#####svmRadialWeights accuracy 87%
###ssn(stabised nearest neighbour classifier)-80%(quite fast)
###self organising maps-85%###very fast also
####randdomferns-82%medium fast
###ownn-82.2%(snn)
####Multi-Layer Perceptron, with multiple layers-82%
########Greedy Prototype Selection(fast)-80.6%

