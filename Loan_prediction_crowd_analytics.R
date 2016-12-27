library(data.table)
library(ggplot2)
library(dplyr)
library(caret)
library(gmodels)
cat_train<-fread("CAX_Startup_Train.csv",na.strings = c(""," ","?","NA",NA))
cat_test<-fread("CAX_Startup_Test.csv",na.strings = c(""," ","?","NA",NA))

colSums(is.na(cat_all))
str(cat_train);str(cat_test)
dim(cat_train);dim(cat_test)
cat_test$Dependent<-NULL
all_data<-all_data%>%
  select(-Dependent,everything())
cat_all<-bind_rows(cat_train,cat_test)
dim(cat_all);names(cat_all);str(cat_all)
View(cat_all)
######################################
########EDA#########################
#################################
###univariate
table(cat_all$Company_Location)
###dummy 3 var
table(cat_all$Company_raising_fund)
####delete company_raising_fund
table(cat_all$Company_Industry_count)
###dummy 3 var
table(cat_all$Company_mobile_app)
#####delete company_mobile_app
table(cat_all$Company_investor_count_seed)
###binarize less than 5% Company_investor_count_seed
table(cat_all$Company_investor_count_Angel_VC)
#####binarize less than 5 % compan_investor_count_angel_vc
table(cat_all$Company_cofounders_count)
#####binarize less than 5%
table(cat_all$Company_advisors_count)
####binarize less than 5%
table(cat_all$Company_senior_team_count)
###keep numerical
table(cat_all$Company_top_Angel_VC_funding)
####dummy(0,1)
table(cat_all$Company_repeat_investors_count)
###binarize less than 5%
table(cat_all$Founders_top_company_experience)
###dummy 2 var
table(cat_all$Founders_previous_company_employee_count)
###dummy 3 var
table(cat_all$Founders_startup_experience)
###dummy 2 var
table(cat_all$Founders_big_5_experience)
###dummy 2 var
table(cat_all$Company_business_model)
###dummy 3
table(cat_all$Founders_experience)
###dummy 3
table(cat_all$Founders_global_exposure)
####dummy 2
table(cat_all$Founders_Industry_exposure)
###dummy 3
table(cat_all$Founder_education)
###dummy 4
table(cat_all$Founder_university_quality)
###dummy 4
table(cat_all$Founders_Popularity)
###dummy 2
table(cat_all$Founders_fortune1000_company_score)
###binarize less than 5%
table(cat_all$Founders_profile_similarity)
###dummy 4
table(cat_all$Founders_publications)
###dummy 3
table(cat_all$Founders_skills_score)
ggplot(cat_all,aes(Founders_skills_score))+geom_histogram(bins = 20)
boxplot(cat_all$Founders_skills_score)
###continous variable
table(cat_all$Founders_Entrepreneurship_skills_score)
ggplot(cat_all,aes(Founders_Entrepreneurship_skills_score))+geom_histogram(bins=30)
boxplot(cat_all$Founders_Entrepreneurship_skills_score)
###outlier treatment (spatial sign)
table(cat_all$Founders_Operations_skills_score)
###binarize 0 and 1
table(cat_all$Founders_Engineering_skills_score)
####binarize 0 and 1(ordered)
####outlier spatial-sign
####outlier spatial-sign
table(cat_all$Founders_Operations_skills_score)
####0 and 1 binarize
cat_all[,prop.table(table(Founders_Marketing_skills_score))]
table(cat_all$Founders_Marketing_skills_score)
####continous variable (0,1)
table(cat_all$Founders_Leadership_skills_score)
###0,1 binarize
table(cat_all$Founders_Data_Science_skills_score)
###same binarize 0,1
table(cat_all$Founders_Business_Strategy_skills_score)
###binarize 3groups or keeping continous
table(cat_all$Founders_Product_Management_skills_score)
###binarize 0,1
table(cat_all$Founders_Sales_skills_score)
####binarize 0,1
table(cat_all$Founders_Domain_skills_score)
###binarize 0,1
table(cat_all$Company_incubation_investor)
###binarize 0,1 oridnal
table(cat_all$Company_competitor_count)
###binarize 0,1
table(cat_all$Company_1st_investment_time)
boxplot(cat_all$Company_avg_investment_time)
##continous
table(cat_all$Company_avg_investment_time)
boxplot(cat_all$Company_avg_investment_time)
###continous
table(cat_all$Company_crowdsourcing)
####binarize 1,0
table(cat_all$Company_crowdfunding)
###binarize 1,0
table(cat_all$Company_big_data)
####binarize 1,0
table(cat_all$Company_analytics_score)
####ordinal 0,1 binary
table(cat_all$Company_Product_or_service)
####binarize 3 groups
table(cat_all$Company_subscription_offering)
###binarize 2 groups
table(cat_all$Founder_highest_degree_type)
###binarize 4 groups
table(cat_all$Company_difficulty_obtaining_workforce)
####ordinal 4 3 groups
table(cat_all$Company_Founder_Patent)
###2 binarize  (0,1)
str(all_data)
cat_all<-select(cat_all,-CAX_ID)
####Data Manipulation
colSums(is.na(testing_data))
testing_data$Dependent<-NULL
numvars<-c(9,26,27,29,33,39,40)
factvars<-setdiff(1:50,numvars)
cat_all[,(factvars):=lapply(.SD,factor),.SDcols=factvars]
cat_num<-cat_all[,numvars,with=F]
cat_fact<-cat_all[,factvars,with=F]
#####function to binarize less than 5% frequenct variables
for(i in names(cat_fact)){
  p <- 5/100
  ld <- names(which(prop.table(table(cat_fact[[i]])) < p))
  levels(cat_fact[[i]])[levels(cat_fact[[i]]) %in% ld] <- "Other"
}




str(cat_fact)
str(cat_num)
View(cat_fact)
names(cat_fact)
###ordering company_difficulty_obtaining_workforce
cat_fact<-transform(cat_fact,Company_difficulty_obtaining_workforce=factor(Company_difficulty_obtaining_workforce,levels =c("Low","Medium","High"),ordered = T ))
x<-data.frame(model.matrix(~cat_fact$Company_difficulty_obtaining_workforce))
cat_fact$diff_obt_work_l<-x$cat_fact.Company_difficulty_obtaining_workforce.L
cat_fact$diff_obt_work_q<-x$cat_fact.Company_difficulty_obtaining_workforce.Q
cat_fact$Company_difficulty_obtaining_workforce<-NULL
####ordering company_analytics
cat_fact<-transform(cat_fact,Company_analytics_score=factor(Company_analytics_score,levels = c("0","1","2","3"),ordered = T))
y<-data.frame(model.matrix(~cat_fact$Company_analytics_score))
cat_fact$analtics_score_l<-y$cat_fact.Company_analytics_score.L
cat_fact$analtics_score_q<-y$cat_fact.Company_analytics_score.Q
cat_fact$analtics_score_c<-y$cat_fact.Company_analytics_score.C
cat_fact$Company_analytics_score<-NULL
####company_incubation investor
cat_fact<-transform(cat_fact,Company_incubation_investor=factor(Company_incubation_investor,levels =c("No","Yes"),ordered = T ))
z<-data.frame(model.matrix(~cat_fact$Company_incubation_investor))
cat_fact$incubation_invest_l<-z$cat_fact.Company_incubation_investor.L
cat_fact$Company_incubation_investor<-NULL
####rounding skill scores
cat_fact$Founders_DOmain_skills_score<-cat_all$Founders_Domain_skills_score
cat_fact$Founders_DOmain_skills_score<-round(cat_fact$Founders_DOmain_skills_score)
cat_fact$Founders_Sales_skills_score<-cat_all$Founders_Sales_skills_score
cat_fact$Founders_Sales_skills_score<-round(cat_fact$Founders_Sales_skills_score)
cat_fact$Founders_Product_Management_skills_score<-round(cat_all$Founders_Product_Management_skills_score)
cat_num$Founders_Business_Strategy_skills_score<-round(cat_num$Founders_Business_Strategy_skills_score)
cat_fact$Founders_Data_Science_skills_score<-round(cat_all$Founders_Data_Science_skills_score)
cat_fact$Founders_Leadership_skills_score<-round(cat_all$Founders_Leadership_skills_score)
cat_fact$Founders_Marketing_skills_score<-round(cat_all$Founders_Marketing_skills_score)
cat_fact$Founders_Operations_skills_score<-round(cat_all$Founders_Operations_skills_score)
cat_num$Founders_Entrepreneurship_skills_score<-round(cat_num$Founders_Entrepreneurship_skills_score)
cat_num$Founders_skills_score<-round(cat_num$Founders_skills_score)
cat_fact<-transform(cat_fact,Founders_publications=factor(Founders_publications,levels = c("None","Few","Many"),ordered = T))
z<-data.frame(model.matrix(~cat_fact$Founders_publications))    
cat_fact$Founders_publication_l<-z$cat_fact.Founders_publications.L
cat_fact$Founders_publication_q<-z$cat_fact.Founders_publications.Q                    
cat_fact<-transform(cat_fact,Founders_profile_similarity=factor(Founders_profile_similarity,levels = c("None","Low","Medium","High"),ordered = T))
z<-data.frame(model.matrix(~cat_fact$Founders_profile_similarity))
cat_fact$Founders_profile_similarity_l<-z$cat_fact.Founders_profile_similarity.L
cat_fact$Founders_profile_similarity_q<-z$cat_fact.Founders_profile_similarity.Q
cat_fact$Founders_profile_similarity_c<-z$cat_fact.Founders_profile_similarity.C
cat_fact<-transform(cat_fact,Founder_university_quality=factor(Founder_university_quality,levels=c("0","1","3","4"),ordered=T))
z<-data.frame(model.matrix(~cat_fact$Founder_university_quality))
cat_fact$Founder_university_quality_l<-z$cat_fact.Founder_university_quality.L
cat_fact$Founder_university_quality_q<-z$cat_fact.Founder_university_quality.Q
cat_fact$Founder_university_quality_c<-z$cat_fact.Founder_university_quality.C
cat_fact$Founders_profile_similarity<-NULL
cat_fact$Founder_university_quality<-NULL
cat_fact<-transform(cat_fact,Founders_experience=factor(Founders_experience,levels = c("Other","Medium","High"),ordered = T))
z<-data.frame(model.matrix(~cat_fact$Founders_experience))
cat_fact$Founders_experience<-as.factor(cat_all$Founders_experience)
cat_fact$Founders_Industry_exposure<-as.factor(cat_all$Founders_Industry_exposure)
z<-data.frame(model.matrix(~cat_fact$Founders_previous_company_employee_count))
cat_fact<-transform(cat_fact,Founders_previous_company_employee_count=factor(Founders_previous_company_employee_count,levels = c("Small","Medium","Large"),ordered = T))
cat_fact$Founders_previous_company_employee_count_l<-z$cat_fact.Founders_previous_company_employee_count.L
cat_fact$Founders_previous_company_employee_count_q<-z$cat_fact.Founders_previous_company_employee_count.Q
cat_fact$Founders_previous_company_employee_count<-NULL
cat_fact$Company_competitor_count<-cat_all$Company_competitor_count
cat_fact$Company_cofounders_count<-cat_all$Company_cofounders_count



summary(lm_model)
str(training_data)
str(testing_data)
testing_data$Dependent<-NULL

dmy<-dummyVars("~ Company_Location+Company_Industry_count+Company_investor_count_seed+
               Company_investor_count_Angel_VC+Company_advisors_count+
               Company_top_Angel_VC_funding+ Company_repeat_investors_count+
               Founders_top_company_experience+Founders_startup_experience+
               Founders_big_5_experience+Company_business_model+Founders_experience+
              Founders_global_exposure+Founders_global_exposure+Founders_Industry_exposure+
               Founder_education+Founders_Popularity+Founders_fortune1000_company_score+
               Company_crowdsourcing+Company_crowdfunding+Company_big_data+Company_Product_or_service+
               Company_subscription_offering+Founder_highest_degree_type+Company_Founder_Patent",data=all_data)

trnsf<-data.frame(predict(dmy,newdata=all_data))
str(trnsf)
all_data<-subset(all_data,select = -c(Company_Location,Company_Industry_count,Company_investor_count_seed,
                                        Company_investor_count_Angel_VC,Company_advisors_count,
                                        Company_top_Angel_VC_funding,Company_repeat_investors_count,
                                        Founders_top_company_experience,Founders_startup_experience,
                                        Founders_big_5_experience,Company_business_model,Founders_experience,
                                        Founders_global_exposure,Founders_global_exposure,Founders_Industry_exposure,
                                        Founder_education,Founders_Popularity,Founders_fortune1000_company_score,
                                        Company_crowdsourcing,Company_crowdfunding,Company_big_data,Company_Product_or_service,
                                       Company_subscription_offering,Founder_highest_degree_type,Company_Founder_Patent))



all_data<-bind_cols(all_data,trnsf)
all_data<-all_data%>%
  select(-Dependent,everything())
all_data<-bind_cols(cat_num,cat_fact)
View(all_data)
training_data<-slice(all_data,1:234)
dim(training_data);dim(testing_data)
testing_data<-slice(all_data,235:nrow(all_data))

str(cat_num)
names(training_data)
str(training_data)
View(training_data)
all_data$Company_Industry_count<-NULL
all_data$Company_investor_count_seed<-NULL
all_data$Founders_experience <-NULL
all_data$Founders_Industry_exposure<-NULL
all_data<-bind_cols(all_data,trnsf)
summary(cat_num)
boxplot(cat_num$Founders_skills_score)
boxplot(cat_num$Founders_Entrepreneurship_skills_score)
boxplot(cat_num$Founders_Engineering_skills_score)
boxplot(cat_num$Founders_Business_Strategy_skills_score)
boxplot(cat_num$Company_1st_investment_time)
boxplot(cat_num$Company_avg_investment_time)
View(cor(cat_num))
ggplot(data=cat_num,aes(x=cat_num$Founders_Engineering_skills_score,y=cat_fact$Dependent,color=cat_fact$Dependent))+geom_point()
cat_num$Company_1st_investment_time<-ifelse(cat_num$Company_1st_investment_time>85,85,cat_num$Company_1st_investment_time)
cat_num$Company_avg_investment_time<-ifelse(cat_num$Company_avg_investment_time>90,90,cat_num$Company_avg_investment_time)
cat_num$Founders_Entrepreneurship_skills_score<-ifelse(cat_num$Founders_Entrepreneurship_skills_score==22,27.5,cat_num$Founders_Entrepreneurship_skills_score)
cat_num$Company_senior_team_count<-cat_all$Company_senior_team_count
cat_num$Company_senior_team_count<-ifelse(cat_num$Company_senior_team_count>12,12,cat_num$Company_senior_team_count)
cat_num$Founders_Business_Strategy_skills_score<-ifelse(cat_num$Founders_Business_Strategy_skills_score>46.875,46.875,cat_num$Founders_Business_Strategy_skills_score)

training_data<-subset(training_data,select =-Company_advisors_count.2)
testing_data<-subset(testing_data,select=-Company_advisors_count.2)
###dropping feature using statistical significance-Founders_top_company_experience.No,
####Company_advisors_count.0 ,Founders_fortune1000_company_score.0,Founders_fortune1000_company_score.1
####Company_advisors_count.Other,Company_Industry_count.Few,Founders_Business_Strategy_skills_score
###Company_Industry_count.single ,Founders_Industry_exposure.High,Founders_Industry_exposure.Low
#####Company_business_model.B2B,Founders_profile_similarity_c,Founders_experience.High
####Founders_Operations_skills_score,diff_obt_work_q,incubation_invest_l
###Founder_highest_degree_type.Management ,Founders_Data_Science_skills_score
###Company_cofounders_count,Founders_global_exposure.No,Founders_global_exposure.Yes
####Founder_education.PhD,Founders_fortune1000_company_score.0.5,Founders_Entrepreneurship_skills_score
###Company_Location.Europe,Founders_Marketing_skills_score,Company_investor_count_Angel_VC.0
#####
dim(training_data)
dim(testing_data)
testing_data$Dependent<-NULL
#View(all_data)
#training_data<-slice(all_data,1:234)
#dim(training_data);dim(testing_data)
#testing_data<-slice(all_data,235:nrow(all_data))
View(training_data)
lm_model<-lm((as.numeric(Dependent)-1)~.,data=training_data)
summary(lm_model)
vif(lm_model)
#####
glm_model<-glm(Dependent~.,data=training_data,family = binomial(link="logit"))
summary(glm_model)
prediction<-predict(glm_model,testing_data,type="response")
pred<-ifelse(prediction>0.5,1,0)
write.csv(data.frame(pred),"pred.csv",row.names = F)
