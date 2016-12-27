#This is a dataset of a giant insurance company that sells various insurance products to its customers. 
#Through the marketing efforts of the company, a database of leads is generated. A lead refers to a prospective customer. 
#The call center team of the company then calls the lead at various times in a day to sell products.
#However, the company incurs very high costs to call all the leads and still has very low
#conversion rate (leads that actually buy the product). To minimize the costs, 
#the company wants to develop a model to predict the probability to convert. 

library(data.table)#faster data manipulation
library(dplyr)##data manipulation
library(ggplot2)# data visualisation
library(lubridate)# date manipulation
library(caret)###for one-hot encoding,confusion matrix and 
library(VIM)# knn imputation
library(ROSE)### handling class imbalance
library(HH)### to check vif
library(caTools)##ROC curve
set.seed(123)

insurance_data<-fread("INSURANCE_UPDATED.csv",na.strings = c(""," ","NA","Null","NULL",NA))
View(insurance_data);dim(insurance_data);summary(insurance_data)
str(insurance_data)
#252355     29
# checking missing values column wise
colSums(is.na(insurance_data))

# This function normalizes the column names.
# INPUT -- Table with messed up column names.
# OUTPUT -- Table with proper column names.
#--------------------------------------------

proper_feature_names <- function(input_table){
  
  colnames(input_table) <- tolower(colnames(input_table))
  
  colnames(input_table) <- gsub('([[:punct:]])|\\s+','_',colnames(input_table))
  
  while (any(grepl("__",colnames(input_table),fixed = TRUE)) == TRUE){
    colnames(input_table) <- gsub("__","_",colnames(input_table),fixed = TRUE) 
  }
  
  colnames(input_table) <- gsub("\\*$", "",colnames(input_table))
  
  return(input_table)
}

insurance_data<-proper_feature_names(insurance_data)
names(insurance_data)

# Removing variables that have above 50% missing values
cols.chosen<-c("marital_status","education","university","professional_skills","previous_workplace","current_workplace","transaction_method","transaction_size","close_date","answer_time")
insurance_data[,(cols.chosen):=NULL]



###################################################################
##############UNI/BI VARIATE ANALYSIS##############################
#################################################################
insurance_data[,prop.table(table(sale_closed))]
#output variable, as we can see there is a high imbalance in classes,
# we need to oversample/undersample/synthetic sampling(SMOTE)

length(unique(insurance_data$lead_id))
length(unique(insurance_data$customer_id))
length(unique(insurance_data$agent_id))
#3 of them have less unique values , that means they are repeated

insurance_data[,prop.table(table(calls_answered_by_lead))]
# calls answered by lead variable
insurance_data[,prop.table(table(age_yrs_))]
table(insurance_data$age_yrs_)
#from summary we got that age is zero which is not possible ,
#and there can be a way to impute missing values using customer_id
insurance_data[,prop.table(table(email_domain))]
length(unique(insurance_data$email_domain))
table(insurance_data$email_domain)
### values are less unique , we can use a frequency term
insurance_data[,prop.table(table(sex))]
###sex mising values can be predicted from customer_id ? lets see
length(unique(insurance_data$city))
table(insurance_data$city)
city<-insurance_data[,.N,city][order(-city)]

###less unique city values,we can binarize less than 5% frequent cities
insurance_data[,prop.table(table(annual_income))]
table(insurance_data$annual_income)

####binning the variables with less than 5% frequency,missing values/0 preiction using customer_id
insurance_data[,prop.table(table(source_where_was_the_lead_captured_from_))]
table(insurance_data$source_where_was_the_lead_captured_from_)
###binning the variables with less than 5%
insurance_data[,prop.table(table(tabacco))]
####binarize product,and less than 5 % frequent terms bin in "others" category
insurance_data[,prop.table(table(product))]

###function to plot continous and categorical variables
tr <- function(a){
  ggplot(data = insurance_data, aes(x= a, y=..density..)) + 
    geom_histogram(fill="blue",color="red",alpha = 0.5,bins =100) +
    geom_density()            
}            
###nasty skewed data ,we can normalise but we have to take a look
tr(insurance_data$attemp_number)
insurance_data$attemp_number<-as.factor(insurance_data$attemp_number)

###function to plot categorical variables
all_bar <- function(i){
  ggplot(insurance_data,aes(x=i,fill=insurance_data$sale_closed))+geom_bar(position = "dodge",  color="black")+scale_fill_brewer(palette = "Pastel1")+theme(axis.text.x =element_text(angle  = 60,hjust = 1,size=10))
}
all_bar(as.factor(insurance_data$product))
all_bar(as.factor(insurance_data$tabacco))
all_bar(as.factor(insurance_data$source_where_was_the_lead_captured_from_))
####missing value treatment######
#####visit time,date and attempt number missing values can be put 0 (as no response from customer)
insurance_data[,visit_date:=ifelse(is.na(visit_date)==T,"0",visit_date)]
insurance_data[,visit_time:=ifelse(is.na(visit_time)==T,"0",visit_time)]
insurance_data[,attemp_number:=ifelse(is.na(attemp_number)==T,0,attemp_number)]
#####age variable missing,so first we will create a table and then merge with main table
age_mis<-insurance_data[,.(age.mean=mean(age_yrs_,na.rm = T)),by=customer_id]
###merging table age_mis and insurance_data
insurance_data<-merge(insurance_data,age_mis,all.x = T,by="customer_id")
insurance_data$age.mean<-round(insurance_data$age.mean)
insurance_data$age.mean[is.nan(insurance_data$age.mean)]<-NA
#KNN imputation for missing values(always choose k odd, for tie break within neigbours)
insurance_data<-kNN(insurance_data,variable = c("age.mean"),k=11)
insurance_data$age.mean_imp<-NULL
insurance_data$age_yrs_<-NULL
####lets check if we can bin age variable(seems like we can )
ggplot(all_data,aes(x=age.mean,y=income_avg,color=sale_closed))+geom_point()
vroom<-insurance_data[,.N,age.mean][order(age.mean)]
boxplot(all_data$age.mean)
tr(insurance_data$age.mean)
buyers<-insurance_data%>%
  select(age.mean,sale_closed)
buyers<-buyers[which(sale_closed=="Yes")]

####sex variable missing,getting the missing values using customer_id, and then KNN imputation
sex_mis<-insurance_data[,.(customer_id,sex)]
sex_mis<-na.omit(sex_mis)
sex_mis<-subset(sex_mis,!duplicated(sex_mis$customer_id))
insurance_data$sex<-NULL
###merging tables ,but still about 34 % values are missing, we can delete this variable as collecting data is not possible
insurance_data<-merge(insurance_data,sex_mis,by="customer_id",all.x=T)
all_bar(as.factor(insurance_data$sex))
#insurance_data$sex<-NULL
###annual income missing,getting the missing values using customer id ,and then KNN imputation
income_mis<-insurance_data[,.(income_avg=mean(annual_income,na.rm = T)),by=customer_id]
income_mis$income_avg<-round(income_mis$income_avg)
insurance_data<-merge(insurance_data,income_mis,by="customer_id",all.x = T)
###knn impute for missing values
insurance_data<-kNN(insurance_data,variable = c("income_avg"),k=11)
###nasty skewed data, most of them 0,making it a factor
tr(insurance_data$income_avg)
insurance_data$income_avg<-as.factor(insurance_data$income_avg)

#insurance_data$income_avg_imp<-NULL
#insurance_data$annual_income<-NULL

tabacco_mis<-insurance_data[,.(customer_id,tabacco)]
tabacco_mis<-na.omit(tabacco_mis)
tabacco_mis<-subset(tabacco_mis,!duplicated(tabacco_mis$customer_id))
insurance_data$tabacco<-NULL
###merging tables now,stll 44% values missing, we can create a new variable on this to see if any trend follows
insurance_data<-merge(insurance_data,tabacco_mis,by="customer_id",all.x = T)

###########################################################################
#########################DATA MANiPULATION################################
#########################Feature Engineering#############################

# Lets start with missing values first
#there are lot of values missing in tabbaco, ther may be a trend followling,so a new variable na created
insurance_data[,tabbacco_NA:=ifelse(sapply(insurance_data$tabacco,is.na)==T,1,0)]

####counts for customer_id,lead_id,agent_id,email_domain created(label encoding)
insurance_data[,customer_visits:=.N,by=customer_id]
insurance_data$customer_id<-NULL
insurance_data[,lead_counts:=.N,by=lead_id]
insurance_data$lead_id<-NULL
insurance_data[,agent_counts:=.N,by=agent_id]
insurance_data$agent_id<-NULL

###date/time manipulation
insurance_data$lead_creation_date<-as.Date(insurance_data$lead_creation_date,"%d-%b-%y")
insurance_data$lead_assigned_date<-as.Date(insurance_data$lead_assigned_date,"%d-%b-%y")
insurance_data$product<-as.factor(insurance_data$product)

###factor the variables 
insurance_data$calls_answered_by_lead<-as.factor(insurance_data$calls_answered_by_lead)
insurance_data$minutes_on_a_call<-as.POSIXct(insurance_data$minutes_on_a_call,format="%H:%M:%S")
insurance_data$email_domain<-tolower(insurance_data$email_domain)
insurance_data$email_domain<-as.factor(insurance_data$email_domain)
insurance_data$city<-tolower(insurance_data$city)
insurance_data$city<-as.factor(insurance_data$city)
insurance_data$source_where_was_the_lead_captured_from_<-as.factor(insurance_data$source_where_was_the_lead_captured_from_)
insurance_data$sale_closed<-as.factor(insurance_data$sale_closed)

#####there seems to be lot of missing values in this variable tabbaco,
####making the missing values unavailable
for (i in seq_along(insurance_data)) set(insurance_data, i=which(is.na(insurance_data[[i]])), j=i, value="Unavailable")
insurance_data$tabacco<-as.factor(insurance_data$tabacco)

#####binning age variable(0-25,25-60,60-above)
insurance_data[,age.mean:=cut(x=age.mean,breaks = c(0,25,60,99),include.lowest = T,labels = c("young","adult","old"))]

####visit date seems to similar with lead_assigned_date
insurance_data$visit_date<-NULL
names(insurance_data)

####feature engineering
insurance_data[,":="(lead_date_diff=as.numeric(lead_assigned_date-lead_creation_date),
                     lead_assigned_month=month(lead_assigned_date),
                     lead_assigned_wday=wday(lead_assigned_date),
                     min_on_a_call=minute(minutes_on_a_call),
                     second_on_a_call=second(minutes_on_a_call),
                     visit_hour=hour(visit_time))]

insurance_data[,seconds_total_on_call:=min_on_a_call*60+second_on_a_call]
insurance_data$min_on_a_call<-NULL
insurance_data$second_on_a_call<-NULL

###taking alook at seconds_total
tr(insurance_data$seconds_total_on_call)
insurance_data$seconds_total_on_call<-as.factor(insurance_data$seconds_total_on_call)
###deleting date columns
cols.chosen<-c("lead_creation_date","lead_assigned_date","minutes_on_a_call","visit_time")                   
insurance_data[,(cols.chosen):=NULL]

#####assigning zeros to the values missing (as no response from customer)
insurance_data[is.na(insurance_data)]<-0
names(insurance_data)
str(insurance_data)
factvars<-c("product","attemp_number","calls_answered_by_lead","email_domain","city","source_where_was_the_lead_captured_from_","age.mean","tabacco","income_avg","seconds_total_on_call")
ins_fact<-subset(insurance_data,select = factvars)
numvars<-setdiff(names(insurance_data),factvars)
ins_num<-subset(insurance_data,select = numvars)

##########function to convert 5% less frequent values to 1
for(i in names(ins_fact)){
  p <- 5/100
  ld <- names(which(prop.table(table(ins_fact[[i]])) < p))
  levels(ins_fact[[i]])[levels(ins_fact[[i]]) %in% ld] <- "Other"
}
dim(ins_fact)
dim(ins_num)
str(ins_fact)
View(ins_fact)
#write.csv(insurance_data,"insurance_complete.csv",row.names = F)

####one hot encoding
dmy<-dummyVars("~.",data=ins_fact)
trnsf<-data.frame(predict(dmy,newdata = ins_fact))

####combing all data numerical and one-hot encoded factorial
all_data<-cbind(trnsf,ins_num)

####splitting into training data and testing data
intrain<-createDataPartition(y=all_data$sale_closed,p=0.7,list = F)
training_data<-all_data[intrain,]
testing_data<-all_data[-intrain,]

####as we dont know which sampling will do better for us, we till try all types of sampling under,over,both and syntheic sampling
all_data_over<-ovun.sample(sale_closed~.,data=training_data,method = "over",N=352530)$data
all_data_under<-ovun.sample(sale_closed~.,data=training_data,method="under",N=768)$data
all_data_both<-ovun.sample(sale_closed~.,data=training_data,method="both",N=176649,p=0.5,seed=2)$data
all_data_rose1<-ROSE(sale_closed~.,data=training_data,seed=1)$data

table(all_data_rose$sale_closed)
str(all_data_rose)
##############################################
##############################################
###PREDICTION$$$$
##ROSE MoDEL########
####################


##Dropping feature:=lead_date_diff,city.mumbai,product.Term,seconds_total_on_call.Other
##income_avg.3 ,visit_month,email_domain.Other,lead_assigned_wday,income_avg.7
#tabacco.Unavailable,tabacco.No
all_data_rose$sale_closed<-as.numeric(all_data_rose$sale_closed)-1
all_data_rose<-subset(all_data_rose,select = -tabacco.No)
testing_data_rose<-testing_data[,names(all_data_rose)]
names(all_data_rose)
names(testing_data_rose)
lm_model<-lm(sale_closed~.,data=all_data_rose)
summary(lm_model)
vif_lm<-vif(lm_model)
View(data.frame(vif_lm))

###vif high value:=calls_answered_by_lead.No,attemp_number.Other,seconds_total_on_call.Other
###lead_counts,calls_answered_by_lead.Yes,
glm_model<-glm(as.factor(sale_closed)~.,data=all_data_rose,family=binomial(link=logit))
predict1<-predict(glm_model,newdata=testing_data_rose,type="response")
p_class<-ifelse(predict1>0.5,"Yes","No")
confusionMatrix(testing_data$sale_closed,p_class)
##accuracy 96%
colAUC(predict1,testing_data_rose$sale_closed,plotROC = T)
roc.curve(testing_data_rose$sale_closed,predict1,add.roc = T,col=2,lty=2)
##AUC 0.984 
ks.test(testing_data_rose$sale_closed,predict1)

##############################
###Both-Model################
###############################
###dropping variable:=calls_answered_by_lead.No,seconds_total_on_call.Other
#tabacco.Unavailable,seconds_total_on_call.0 ,visit_month,attemp_number.Other
#income_avg.0,visit_hour,attemp_number.0,lead_date_diff ,calls_answered_by_lead.Yes
#tabacco.No,lead_assigned_wday,city.Other,city.new.delhi,city.mumbai,income_avg.1
##source_where_was_the_lead_captured_from_.opicle,email_domain.Other,source_where_was_the_lead_captured_from_.Other 
#product.Term,product.Other,email_domain.gmail.com
all_data_both$sale_closed<-as.numeric(all_data_both$sale_closed)-1
all_data_both<-subset(all_data_both,select = -email_domain.gmail.com)
testing_data_both<-testing_data[,names(all_data_both)]
names(testing_data_both)
names(all_data_both)
lm_model2<-lm(sale_closed~.,data=all_data_both)
summary(lm_model2)
###our linear model gives NA because of linear dependent variables
## Correlation matrix with p-values
cor.prob <- function (X, dfr = nrow(X) - 2) {
  R <- cor(X, use="pairwise.complete.obs")
  above <- row(R) < col(R)
  r2 <- R[above]^2
  Fstat <- r2 * dfr/(1 - r2)
  R[above] <- 1 - pf(Fstat, 1, dfr)
  R[row(R) == col(R)] <- NA
  R
}
flattenSquareMatrix <- function(m) {
  if( (class(m) != "matrix") | (nrow(m) != ncol(m))) stop("Must be a square matrix.") 
  if(!identical(rownames(m), colnames(m))) stop("Row and column names must be equal.")
  ut <- upper.tri(m)
  data.frame(i = rownames(m)[row(m)[ut]],
             j = rownames(m)[col(m)[ut]],
             cor=t(m)[ut],
             p=m[ut])
}

View(flattenSquareMatrix(cor.prob(all_data_both)))### check correaltion
alias(lm_model2)### check linear dependent variables
vif_lm2<-vif(lm_model2)# vif values
View(data.frame(vif_lm2))
str(testing_data_both)
glm_model1<-glm(as.factor(sale_closed)~.,data=all_data_both,family=binomial(link="logit"))
predict2<-predict(glm_model1,testing_data_both,type="response")
p_class2<-ifelse(predict2>0.5,"Yes","No")
confusionMatrix(testing_data_both$sale_closed,p_class2)
####accuracy 95%
colAUC(predict2,testing_data_both$sale_closed,plotROC = T)
roc.curve(testing_data_both$sale_closed,predict2,add.roc = T,col=3,lty=1)
###auc 0.97

##############################################
###########Undersampling#################
#############################################
#Dropping variables:=income_avg.4,lead_assigned_month,income_avg.7,source_where_was_the_lead_captured_from_.Google_ppc
#visit_month,vist_day ,email_domain.gmail.com,email_domain.rediffmail.com ,email_domain.yahoo.com
#age.mean.adult,age.mean.Other,source_where_was_the_lead_captured_from_.Other
##city.mumbai,agent_counts,city.Other,product.Other,product.Health,lead_assigned_wday
####visit_hour,product.Term,attemp_number.Other,source_where_was_the_lead_captured_from_.adchakra
##email_domain.Other,income_avg.3,income_avg.Other ,source_where_was_the_lead_captured_from_.OMG
all_data_under$sale_closed<-as.numeric(all_data_under$sale_closed)-1
all_data_under<-subset(all_data_under,select =-calls_answered_by_lead.No)
testing_data_under<-testing_data[,names(all_data_under)]
lm_model3<-lm(sale_closed~.,data =all_data_under)
summary(lm_model3)
vif(lm_model3)
glm_model2<-glm(sale_closed~.,data=all_data_under,family = binomial(link="logit"))
predict3<-predict(glm_model2,testing_data_under,type="response")
p_class3<-ifelse(predict3>0.5,"Yes","No")
confusionMatrix(testing_data_under$sale_closed,p_class3)
###94 % accuracy
colAUC(predict3,testing_data_under$sale_closed,plotROC = T)
roc.curve(testing_data_under$sale_closed,predict3,add.roc = T,col=5,lty=5)
#auc 0.979
