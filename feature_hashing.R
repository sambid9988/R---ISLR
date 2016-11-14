library(RCurl)
library(FeatureHashing)
library(glmnet)
binData <- getBinaryURL("https://archive.ics.uci.edu/ml/machine-learning-databases/00296/dataset_diabetes.zip",
                        ssl.verifypeer=FALSE)
conObj <- file("dataset_diabetes.zip", open = "wb")
writeBin(binData, conObj)
# don't forget to close it
close(conObj)
files <- unzip("dataset_diabetes.zip")
diabetes <- read.csv(files[1], stringsAsFactors = FALSE)
str(diabetes)
dim(diabetes)
####50variables and 101766 obs
###EDA
diabetes<-subset(diabetes,select=-c(encounter_id,patient_nbr))
#transforming all ? to zeroes
diabetes[diabetes=="?"]<-NA
###remove zero variance
diabetes <- diabetes[sapply(diabetes, function(x) length(levels(factor(x,exclude=NULL)))>1)]

###prep outcome variable 
diabetes$readmitted<-ifelse(diabetes$readmitted=="<30",1,0)


outcome<-"readmitted"
diabetes_hash<-diabetes
predictornames<-setdiff(names(diabetes_hash),outcome)

####change all NA's to 0
diabetes_hash[is.na(diabetes_hash)]<-0

set.seed(1)
split<-sample(nrow(diabetes_hash),floor(0.5*nrow(diabetes_hash)))
train<-diabetes_hash[split,]
test<-diabetes_hash[-split,]

####try large values for 
train_hashed<-hashed.model.matrix(~.,data =train[,predictornames],hash.size = 2^12,transpose = F )
train_hashed<-as(train_hashed,"dgCMatrix")
test_hashed<-hashed.model.matrix(~.,data=test[,predictornames],hash.size = 2^12,transpose = F)
test_hashed<-as(test_hashed,"dgCMatrix")
glmnet_model<-cv.glmnet(train_hashed,train[,outcome],family="binomial",type.measure = "auc")

####now prediction
predict.glm<-predict(glmnet_model,test_hashed,s="lambda.min")
auc(test[,outcome],predict.glm)
####0.64 AUC



