library(data.table)
library(ggplot2)
library(xgboost)
library(ROSE)
library(caret)
library(doParallel)
cl <- makeCluster(4)
registerDoParallel(cl)
###load the data
train<-fread("train.csv",na.strings = c(""," ","?","NA",NA))
test<-fread("test.csv",na.strings = c(""," ","?","NA",NA))
###look at the data
str(train);dim(train)
str(test);dim(test)
head(train);names(train)
###this is our target variable with two levels
unique(test$income_level)

####recode the outcome 
train[,income_level:=ifelse(income_level=="-50000",0,1)]
test[,income_level:=ifelse(income_level=="-50000",0,1)]

table(train$veterans_benefits)

#### we can see majority are 0 about 95% values and 1 about 5%
View(new_train)
factors<-c(2:5,7:16,20:29,31:38,40,41)
numericals<-setdiff(1:41,factors)

train[,(factors):=lapply(.SD,factor),.SDcols=factors]
train[,(numericals):=lapply(.SD,as.numeric),.SDcols=numericals]
test[,(factors):=lapply(.SD,factor),.SDcols=factors]
test[,(numericals):=lapply(.SD,as.numeric),.SDcols=numericals]
####lets separate into categorical and numerical it will help i further analysis
cat_train<-train[,factors,with=F]
cat_test<-test[,factors,with=F]

num_train<-train[,numericals,with=F]
num_test<-test[,numericals,with=F]
rm(train,test)

####Lets start with numerical data
summary(num_train)

#####################EDA###########################
##################################################
###################################################
###checking for missing values.Hence no missing values in numerical data
colSums(is.na(num_train))

hplot<-function(a){
  ggplot(data=num_train,aes(x=a,y=..density..))+
    geom_histogram(fill="darkblue",color="pink",alpha=0.5,bins =sqrt(nrow(num_train)))+geom_density()
    
}

hplot(num_train$age)
hplot(num_train$weeks_worked_in_year)
hplot(num_train$wage_per_hour)
hplot(num_train$capital_losses)
summary(num_train)
###############most of them have a skewed distribution 
###############
qplot(data=num_train,x=num_train$capital_gains,y=num_train$wage_per_hour,color=cat_train$income_level,geom="point")

####qplot show numerical data are less diverse/less unique values w.r.t our data size
#####barchart for eda of categorical data

bplot<-function(b){
  ggplot(cat_train,aes(x=b,fill=income_level))+geom_bar(position = "dodge",color="black")+scale_fill_brewer(palette = "Accent")+theme(axis.text.x =element_text(angle  = 60,hjust = 1,size=10))

}

bplot(cat_train$class_of_worker)
###requires binning
bplot(cat_train$hispanic_origin)
####requires binning
bplot(cat_train$occupation_code)
####requires binning
bplot(cat_train$education)
bplot(cat_train$fill_questionnaire_veteran_admin)
####seems like most cat.variables have low freq and requires binning

###lets check for correlation
lm.model<-lm((as.numeric(cat_train$income_level)-1)~.,data=num_train)
summary(lm.model)
vif(lm.model)
####no collinearity between variables
####lets check for missing values in categorical data
colSums(is.na(cat_train))
###removing variables where frequency of missing values is higher
cat_train<-subset(cat_train,select=-c(migration_msa,migration_reg,migration_within_reg,migration_sunbelt))
cat_test<-subset(cat_test,select=-c(migration_msa,migration_reg,migration_within_reg,migration_sunbelt))


#####
cat_train$hispanic_origin<-addNA(cat_train$hispanic_origin)
cat_train$country_father<-addNA(cat_train$country_father)
cat_train$country_mother<-addNA(cat_train$country_mother)
cat_train$country_self<-addNA(cat_train$country_self)
cat_train$state_of_previous_residence<-addNA(cat_train$state_of_previous_residence)

cat_test$hispanic_origin<-addNA(cat_test$hispanic_origin)
cat_test$country_father<-addNA(cat_test$country_father)
cat_test$country_mother<-addNA(cat_test$country_mother)
cat_test$country_self<-addNA(cat_test$country_self)
cat_test$state_of_previous_residence<-addNA(cat_test$state_of_previous_residence)
#######missing value imputation is done
#combine factor levels with less than 5% values
#train
for(i in names(cat_train)){
  p <- 5/100
  ld <- names(which(prop.table(table(cat_train[[i]])) < p))
   levels(cat_train[[i]])[levels(cat_train[[i]]) %in% ld] <- "Other"
}

#test
for(i in names(cat_test)){
  p <- 5/100
  ld <- names(which(prop.table(table(cat_test[[i]])) < p))
  levels(cat_test[[i]])[levels(cat_test[[i]]) %in% ld] <- "Other"
}


####check how can we bin data
num_train[,.N,age][order(age)]

num_train[,age:=cut(x=age,breaks = c(0,25,65,90),include.lowest = T,labels=c("young","adult","old"))]
num_train[,age:=factor(age)]
num_test[,age:=cut(x=age,breaks=c(0,25,65,90),include.lowest = T,labels = c("young","adult","old"))]
num_test[,age:=factor(age)]

#####Binning numeric variables now
num_train[,wage_per_hour:=ifelse(wage_per_hour==0,"Zero","MorethanZero")][,wage_per_hour:=as.factor(wage_per_hour)]
num_train[,capital_gains:=ifelse(capital_gains==0,"Zero","MorethanZero")][,capital_gains:=as.factor(capital_gains)]
num_train[,capital_losses:=ifelse(capital_losses==0,"Zero","MorethanZero")][,capital_losses:=as.factor(capital_losses)]
num_train[,dividend_from_Stocks:=ifelse(dividend_from_Stocks==0,"Zero","MorethanZero")][,dividend_from_Stocks:=as.factor(dividend_from_Stocks)]


num_test[,wage_per_hour:=ifelse(wage_per_hour==0,"Zero","MorethanZero")][,wage_per_hour:=as.factor(wage_per_hour)]
num_test[,capital_gains:=ifelse(capital_gains==0,"Zero","MorethanZero")][,capital_gains:=as.factor(capital_gains)]
num_test[,capital_losses:=ifelse(capital_losses==0,"Zero","MorethanZero")][,capital_losses:=as.factor(capital_losses)]
num_test[,dividend_from_Stocks:=ifelse(dividend_from_Stocks==0,"Zero","MorethanZero")][,dividend_from_Stocks:=as.factor(dividend_from_Stocks)]



train.data<-cbind(num_train,cat_train)
test.data<-cbind(num_test,cat_test)
####removing unwanted files

rm(num_train,cat_train,num_test,cat_test)

####
train.rose<-ROSE(income_level~.,data=train.data,seed = 123)$data
table(train.rose$income_level)
#####removing zero variance
train.rose<- train.rose[sapply(train.rose, function(x) length(levels(factor(x,exclude=NULL)))>1)]

str(train.rose);str(test.data)
########
sparse_rose.train<-sparse.model.matrix(income_level~.-1,data=train.rose)
sparse_rose.test<-sparse.model.matrix(income_level~.-1,data=test.data)

params <- list()
params$objective <- "binary:logistic"
params$eta <- 0.1
params$max_depth <- 6
params$subsample <- 0.9
params$colsample_bytree <- 0.9
params$min_child_weight <- 2
params$eval_metric <- "auc"
target<-as.numeric(train.rose$income_level)-1
model_xgb<-xgb.cv(params = params,data=sparse_rose.train,label=target,nrounds = 300,nfold = 5,early.stop.round = 30,prediction = T)

prediction<-predict(model_xgb,test.data)

#########


