install.packages("plm")
library(plm)
panel_wage<-read.csv("panel_wage.csv",stringsAsFactors = F)
str(panel_wage)
View(panel_wage)

pdata<-plm.data(panel_wage,index=c("id","t"))

###pooled ols
pooling<-plm(lwage~exp+wks+exp2,data=pdata,model ="pooling")
summary(pooling)

####between each other
between<-plm(lwage~exp+wks+exp2+ed,data=pdata,model="between")
summary(between)

plmtest(pooling)


####firstdiff eachother
firstdiff<-plm(lwage~exp+wks+exp2+ed,data=pdata,model="fd")
summary(firstdiff)

#### we can see exp and ed removed 

####fixed effect estimator
fixed<-plm(lwage~exp+wks+exp2+ed,data=pdata,model="within")
summary(fixed)

###Random effect estimator
random<-plm(lwage~exp+wks+exp2+ed,data=pdata,model="random")
summary(random)


###LM test to find a better model
plmtest(pooling)
pFtest(pooling,fixed)

####Hausman test to find better model
phtest(random,fixed)
####we can see one model is inconsistent we should go with random test
