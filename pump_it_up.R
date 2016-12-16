
library(data.table)
library(dplyr)
library(ggplot2)
library(lubridate)

train<-fread("4910797b-ee55-40a7-8668-10efd5c1b960.csv",na.strings = c(""," ","?","NA",NA))
output<-fread("0bf8bc6e-30d0-4c50-956a-603fc693d966.csv",na.strings = c(""," ","?","NA",NA))
test<-fread("702ddfc5-68cd-4d1d-a0de-f5f566f76d91.csv",na.strings = c(""," ","?","NA",NA))
####look at the data
str(train)
str(test)
View(train);View(output)
dim(train)
dim(test)
###checking for output variable

unique(output$status_group)
round(prop.table(table(output$status_group))*100)
c<-list(train,test)
combin<-rbindlist(c)
output<-subset(output,select =status_group)
combin<-bind_cols(combin,output)
colSums(is.na(combin))
dim(combin);str(combin);View(combin)
length(unique(combin$id))
##write.csv(combin,"combin.csv",row.names = F)

####analyzing varibles
combin[,prop.table(table(funder))]
length(unique(combin$funder))
####put 0 and 1 binarize the data(if funding is done or not)
combin[,prop.table(table(installer))]
length(unique(combin$installer))
####put 0 and 1 binazrize the data(if organisation installed or not)
combin[,prop.table(table(wpt_name))]
length(unique(combin$wpt_name))
###delete this column wpt_name
combin[,prop.table(table(basin))]
##binarize basin (one hot encoding)
combin[,prop.table(table(subvillage))]
###delete subvillage
combin[,prop.table(table(region))]
###one hot encoding
combin[,prop.table(table(region_code))]
####keep it discrete(may be one hot encoding or delete)
combin[,prop.table(table(district_code))]
###apply function for less than 5% as others
combin[,prop.table(table(lga))]
###delete lga
combin[,prop.table(table(ward))]
###delete ward
combin[,prop.table(table(public_meeting))]
####one hot encode (0,1)
###delete recorded by
combin[,prop.table(table(scheme_management))]
#####binarize less than 5% variables ,na bin
####delete scheme_name
combin[,prop.table(table(permit))]
###one hot encode 0,1
combin[,prop.table(table(extraction_type))]
####binarize less than 5% variables
combin[,prop.table(table(extraction_type_group))]
####binarize less than 5% variables
combin[,prop.table(table(extraction_type_class))]
###binarize less than 5% variables
combin[,prop.table(table(management))]
###binarize less than 5%variables
combin[,prop.table(table(management_group))]
###binarize 5% less variables
combin[,prop.table(table(payment))]
###keeping like the same way as it is
combin[,prop.table(table(payment_type))]
###keeping like the way it is
combin[,prop.table(table(water_quality))]
###binarize water quality 5%
combin[,prop.table(table(quality_group))]
###binarise water quality 5%
combin[,prop.table(table(quantity))]
###keep like that
combin[,prop.table(table(quantity_group))]
###delete quantity_group
combin[,prop.table(table(source))]
###delete source
combin[,prop.table(table(source_type))]
####binarize for 5% variables 
combin[,prop.table(table(source_class))]
###Keep like that
combin[,prop.table(table(waterpoint_type))]
####delete waterpoint_type
combin[,prop.table(table(waterpoint_type_group))]
###binarize for 5% less variables
####function to plot continous variables
tr <- function(a){
  ggplot(data = combin, aes(x= a, y=..density..)) + 
    geom_histogram(fill="blue",color="red",alpha = 0.5,bins =100) +
    geom_density()            
}  
summary(combin$num_private)
tr(combin$amount_tsh)
###amount_tsh binarize(0,1)
tr(combin$gps_height)
summary(combin$gps_height)
boxplot(combin$gps_height)
###outlier treatment with 2634 as height
tr(combin$longitude)
tr(combin$latitude)
###k-means clustering
tr(combin$num_private)
###delete num_private
tr(combin$population)
summary(combin$population)
####converting to 0 and 1 in population(less than 500)
tr(combin$construction_year)
summary(combin$construction_year)
###construction year values which are zero convert them to 1959 (recency of the yoc)
all_bar <- function(i){
  ggplot(train,aes(x=i,fill=output$status_group))+geom_bar(position = "dodge",  color="black")+scale_fill_brewer(palette = "Pastel1")+theme(axis.text.x =element_text(angle  = 60,hjust = 1,size=10))
}

all_bar(as.factor(train$basin))
View(combin)
colSums(is.na(combin))

#######
combin<-subset(combin,select=-c(id,wpt_name,subvillage,lga,ward,recorded_by,scheme_name,quantity_group,source,waterpoint_type))
#######
names(combin)
str(combin)

numcols<-c(1,4,6,7,8,13,17)

factcols<-setdiff(1:30,numcols)
combin[,(factcols):=lapply(.SD,factor),.SDcols=factcols]
########################################################
#################DATA MANIPULATION#####################
######################################################

combin$amount_tsh<-as.factor(ifelse(combin$amount_tsh==0,0,1))
combin$date_recorded<-as.Date(combin$date_recorded)
combin<-mutate(combin,yearrecord_recency=difftime("2014-01-01",combin$date_recorded))
combin$yearrecord_recency<-as.integer(combin$yearrecord_recency)
combin<-mutate(combin,record_month=month(date_recorded),record_year=year(date_recorded),record_day=wday(date_recorded))
combin<-subset(combin,select = -date_recorded)
combin$funder<-as.factor(ifelse(combin$funder==0,0,1))
combin$gps_height<-ifelse(combin$gps_height>2634,2634,combin$gps_height)
combin<-subset(combin,select=-installer)
combin<-subset(combin,select=-num_private)

levels(combin$public_meeting)[levels(combin$public_meeting)==T]<-1
levels(combin$public_meeting)[levels(combin$public_meeting)==F]<-0

levels(combin$permit)[levels(combin$permit)==T]<-1
levels(combin$permit)[levels(combin$permit)==F]<-0
combin$construction_year<-ifelse(combin$construction_year==0,1950,combin$construction_year)
combin<-mutate(combin,year_constrecency=2014-construction_year)
combin<-mutate(combin,populate=ifelse(population==0 | population==1,1,0))
combin_fact<-combin%>%
  select(amount_tsh,funder,basin,region,region_code,district_code,public_meeting,permit,scheme_management,permit,extraction_type,extraction_type_group,extraction_type_class,management,management_group,payment,payment_type,water_quality,quality_group,quantity,source_type,source_class,waterpoint_type_group)
combin_num<-combin%>%
  select(gps_height,longitude,latitude,population,construction_year,yearrecord_recency,record_month,record_day,record_year,populate,year_constrecency)
View(combin_fact)
colSums(is.na(combin_num))
colSums(is.na(cluster))

#write.csv(combin_num,"combin_num.csv",row.names = F)
#write.csv(combin_fact,"combin_fact.csv",row.names = F)
names(combin)
cluster<-combin%>%
  select(amount_tsh,gps_height,longitude,latitude,population)
str(cluster)
#rm(combin)
#rm(zero_var)
cluster$population<-scale(cluster$population)
cluster$amount_tsh<-scale(cluster$amount_tsh)
cluster$gps_height<-scale(cluster$gps_height)
#cluster<-cluster[sample(nrow(cluster)),]
library(factoextra)
fviz_nbclust(cluster[1:5000,],kmeans,method="wss")
###optimal clusters 3
kmeans_clus<-kmeans(cluster,3,nstart = 20)
kmeans_clus$size
fviz_cluster(kmeans_clus, data =cluster, frame.type = "convex")+
  theme_minimal()
cluster_no<-data.frame(kmeans_clus$cluster)
cluster<-bind_cols(cluster,cluster_no=cluster_no)
combin_num<-bind_cols(combin_num,cluster_no)

####function
for(i in names(combin_fact)){
  p <- 5/100
  ld <- names(which(prop.table(table(combin_fact[[i]])) < p))
  levels(combin_fact[[i]])[levels(combin_fact[[i]]) %in% ld] <- "Others"
}
str(combin_fact)
View(combin_fact)
colSums(is.na(combin_fact))
levels(combin_fact$funder)[levels(combin_fact$funder=="Others")]<-0
combin_fact$funder<-as.numeric(combin_fact$funder)-1
combin_fact$public_meeting<-as.numeric(combin_fact$public_meeting)-1
combin_fact$permit<-as.numeric(combin_fact$permit)-1
combin_fact$scheme_management<-addNA(combin_fact$scheme_management)
combin_fact$amount_tsh<-as.numeric(combin_fact$amount_tsh)-1
dmy<-dummyVars("~.",data=combin_fact)
trsnf<-data.frame(predict(dmy,newdata=combin_fact))
View(trsnf)
names(trsnf)
rm(cluster_no)
complete_data<-bind_cols(combin_num,trsnf)
str(output)
output$status_group<-as.factor(output$status_group)
complete_data<-bind_cols(complete_data,output)
write.csv(x_train,"x_train.csv",row.names = F)
write.csv(x_test,"x_test.csv",row.names = F)
#####xgboost modelling

x_target<-as.numeric(output$status_group)
x_train<-complete_data[1:59400,]
x_test<-complete_data[59400:nrow(complete_data),]
names(x_train)
####removing features using xgb.importance
x_train<-subset(x_train,select = -region_code.3)
x_test<-subset(x_test,select = -region_code.3)
xgtrain<-xgb.DMatrix(data=as.matrix(x_train[,1:108]),label=x_target,missing=NA)
xgtest<-xgb.DMatrix(data=as.matrix(x_test[,1:108]),missing=NA)


params<-list()
params$objective="multi:softmax"
params$booster<-"gbtree"
params$eta<-0.1
params$num_class<-4
params$max_depth<-12
params$subsample <- 0.9
params$colsample_bytree <- 0.8
params$min_child_weight <- 12
params$eval_metric <- "merror"

model_xgb_cv <- xgb.cv(params=params, xgtrain, nrounds = 500, nfold = 5, early.stop.round = 30, prediction = TRUE,set.seed(123))

min.merror.idx = which.min(model_xgb_cv$dt[, test.merror.mean])
min.merror.idx
model_xgb <- xgb.train(params = params, xgtrain, nrounds = 176)
pred<-predict(model_xgb,xgtest)
vimp<-xgb.importance(model=model_xgb,feature_names = colnames(x_train))
View(vimp)
xgb.plot.importance(importance_matrix = vimp)
submit<-data.table(status_group=pred)
submit[submit==1]<-"functional"
submit[submit==2]<-"functional needs repair"
submit[submit==3]<-"non functional"
write.csv(submit,"submit.csv",row.names = F)

###########rank 141 

###function to understand the correlation
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
corMasterlist<-flattenSquareMatrix(cor.prob(x_train))
View(corMasterlist)












