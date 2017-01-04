library(data.table)
library(ggplot2)
library(caret)
library(xgboost)
library(gbm)
library(Hmisc)
library(mice)
library(corrgram)
dengue_train<-fread("dengue_features_train.csv",na.strings = c(""," ","?","NA",NA))
dengue_test<-fread("dengue_features_test.csv",na.strings = c(""," ","?","NA",NA))
dengue_output<-fread("dengue_labels_train.csv",na.strings = c(""," ","?","NA",NA))
dim(dengue_test);dim(dengue_train)
str(dengue_test);str(dengue_train)

### lot of missing values it seems
dengue_test$total_cases<-mean(dengue_output$total_cases)
dengue_train<-merge(dengue_train,dengue_output,all.x = T,by=c("city","year","weekofyear"))
c<-list(dengue_train,dengue_test)
combin1<-rbindlist(c)
rm(c)
dim(combin)
str(combin)
write.csv(combin,"combin.csv",row.names = F)
####UNIvARIATE ANALYSIS
colSums(is.na(combin))
combin[,prop.table(table(city))]

###checkng correlation
corrgram(combin, order=TRUE, lower.panel=panel.shade,
         upper.panel=panel.pie, text.panel=panel.txt,
         main="Car Milage Data in PC2/PC1 Order")
corrgram(combin,
         main="Iris data with example panel functions",
         lower.panel=panel.pts, upper.panel=panel.conf,
         diag.panel=panel.density)





ggplot(combin,aes(as.factor(city),total_cases,fill=as.factor(city)))+geom_bar(stat="identity")

#factor city
combin[,prop.table(table(year))]
table(combin$year)
ggplot(combin,aes(year,total_cases))+geom_point(color="red")
### we can rank them with frequency keeping variables, year recency
length(unique(combin$weekofyear))
ggplot(combin,aes(weekofyear,total_cases))+geom_point(color="green")
#### we can rank weeks here

###function to graph variable for continous variables        
tr <- function(a){
  ggplot(data = combin, aes(x= a, y=..density..)) + 
    geom_histogram(fill="blue",color="red",alpha = 0.5,bins =100) +
    geom_density()            
}  

tr(combin$ndvi_ne)# nice normal data
tr(combin$ndvi_nw)# nice normal data
tr(combin$ndvi_sw)# nice normal data
tr(combin$ndvi_se)# nice normal data
tr(combin$precipitation_amt_mm)# highly skewed data
tr(combin$reanalysis_air_temp_k)# nice normal data
tr(combin$reanalysis_avg_temp_k)
tr(combin$reanalysis_dew_point_temp_k)
tr(combin$reanalysis_max_air_temp_k)
tr(sqrt(combin$total_cases))


#############Missing value treatment/Data manipulation

colSums(is.na(combin))
mice_mod<-mice(combin,method = "rf",m=5,seed=500,maxit = 50)
mice_output<-complete(mice_mod)
combin$reanalysis_sat_precip_amt_mm<-mice_output$reanalysis_sat_precip_amt_mm


combin$city<-as.factor(combin$city)
combin$city<-as.numeric(combin$city)-1
combin$week_start_date<-ymd(combin$week_start_date)
combin$week_start_date<-day(combin$week_start_date)
str(combin)
training_data<-combin[1:1456,]
testing_data<-combin[1457:1872,]
testing_data$total_cases<-NULL

mycontrol<-trainControl(method="cv",number=10,verboseIter = T)
rf_model<-train(total_cases~.,data=training_data,method="rf",importance=T,trControl=mycontrol,metric="RMSE")
plot(rf_model)
fm<-rf_model$finalModel
varImpPlot(fm)
glm_ridge_model<-train(total_cases~.,data=training_data,method="enet",preProcess=c("center","scale"),metric = "RMSE")
coef(glm_ridge_model$finalModel)
pred<-predict(glm_ridge_model,testing_data)
p<-data.frame(pred)
varImp(glm_ridge_model)
write.csv(p,"pred_glmnet.csv",row.names = F)

names(combin)
x_features<-c("year","weekofyear","ndvi_ne",
              "ndvi_nw","ndvi_sw","precipitation_amt_mm","reanalysis_air_temp_k",
              "reanalysis_avg_temp_k","reanalysis_dew_point_temp_k","reanalysis_max_air_temp_k",
              "reanalysis_min_air_temp_k","reanalysis_precip_amt_kg_per_m2",
             "reanalysis_specific_humidity_g_per_kg","reanalysis_tdtr_k",
              "station_avg_temp_c","station_diur_temp_rng_c","station_max_temp_c","station_min_temp_c",
              "station_precip_mm","year_count")

#week_start_date,week_count,ndvi_nw,city

x_target<-training_data$total_cases

xgtrain<-xgb.DMatrix(data=as.matrix(training_data[,x_features]),label=x_target)
xgtest<-xgb.DMatrix(data=as.matrix(testing_data[,x_features]))

## xgboost
params <- list()
params$objective <- "reg:linear"
params$eta <- 0.1
params$max_depth <- 5
params$subsample <- 0.8
params$colsample_bytree <- 0.9
params$min_child_weight <- 2
params$eval_metric <- "rmse"
model_xgb_cv <- xgb.cv(params=params, xgtrain, nrounds = 400, nfold = 5, early.stop.round = 30, prediction = TRUE,set.seed=123)
model_xgb <- xgb.train(params = params, xgtrain, nrounds =367)

pred <- predict(model_xgb, xgtest)

vimp <- xgb.importance(model = model_xgb, feature_names = x_features)
View(vimp)
xgb.plot.importance(vimp)
submit<-data.frame(outcome=pred)
write.csv(pred,"predxgb.csv")


dim(testing_data)
zeros <- rep(0, 416)

#--------------------------------------------
# I will fit 50 models.
# The predictions are averaged out.
# So this is simply an ensemble of boosters.
#--------------------------------------------

control <- 50

for (i in 1:control){
  
  bst <- xgboost(data = xgtrain,
                 label = x_target,
                 eta = 0.1,
                 max_depth = 5,
                 subsample = 0.8,
                 colsample_bytree = 0.9,
                 nrounds = 235,
                 objective = "reg:linear",
                 eval_metric = "rmse",
                 maximize = FALSE)
  
  yhat <- predict(bst,xgtest)
  zeros <- zeros + yhat
}

zeros <- zeros/control
submission<-data.frame(zeros,stringsAsFactors = F)
write.csv(submission,"sub.xgb.csv",row.names = F)









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

View(flattenSquareMatrix(cor.prob(combin)))

#write.csv(combin,"combined.csv",row.names = F)

#combin<-read.csv("combined.csv")





gbm_model<-gbm.fit(
  x=training_data[,x_features],
  y=x_target,
  distribution="gaussian",
  n.trees = 1000,
  shrinkage = 0.01,
  interaction.depth = 4,
  n.minobsinnode = 10,
  bag.fraction = 0.8,
  verbose = T
  
  
  )
summary(gbm_model)
gbm.perf(gbm_model)
gbm_pred<-predict(gbm_model,testing_data[,x_features],n.trees=475,type="response")

write.csv(data.frame(gbm_pred),"gbm_pred.csv",row.names = F)

















