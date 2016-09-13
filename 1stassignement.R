college<-read.csv("College.csv",header = T)
college
head(college)
dim(college)
fix(college)
rownames(college) = college[,1]
fix(college)
college<-college[,-1]
fix(college)
#i
summary(college)
#ii
pairs(college[,1:10],col="green")
#iii
attach(college)
college<-read.csv("College.csv",header = T)
class(college)
plot(college$Outstate,college$Private)
boxplot(college$Outstate,college$Private,xlab="outstate",ylab="private")
#iv
nrow(college)
Elite<-rep("No",nrow(college))
Elite[college$Top10perc>50]<-"yes"
Elite<-as.factor(Elite)
college<-data.frame(college,Elite)
summary(college$Elite)
plot(college$Elite,college$Outstate)
#v
par(mfrow=c(2,2))
hist(college$Apps)
hist(college$Accept,col = 2)
hist(college$perc.alumni,col=3,breaks = 10)
hist(college$Expend,breaks = 100)
#relation between variable
dev.off()
plot(college$Grad.Rate,college$Outstate,col="blue")
plot(college$Accept, college$S.F.Ratio)
plot(college$Top10perc, college$Grad.Rate)
identify(college$Top10perc,college$Grad.Rate,name)


