ufo<-read.csv("scrubbed.csv",stringsAsFactors = F)
library(ggplot2)
library(qdap)### character cleaning
library(syuzhet)## getting sentiment
library(dplyr)###
library(lubridate)##for date manipulations
library(forecast)#for time series
####gettting a sample of comments
ufo_sentiments<-sample(ufo$comments,1000)

ufo_sentiments= gsub("[^[:ascii:]]|[[:punct:]]", "", ufo_sentiments, perl=TRUE)
###punctuatuions and ascii characters
ufo_sentiments= gsub("[ \t]{2,}","", ufo_sentiments)
####tab space removing
ufo_sentiments<-replace_abbreviation(ufo_sentiments)
ufo_sentiments<-replace_contraction(ufo_sentiments)
ufo_sentiments<-replace_symbol(ufo_sentiments)
ufo_sentiments<-tolower(ufo_sentiments)
ufo_sentiments<-rm_stopwords(ufo_sentiments,Top200Words,unlist=T)
#######from qdap package works on character only

##### sentiment analysis
my_sentiment<-get_nrc_sentiment((ufo_sentiments))
Sentimentscores<-data.frame(colSums(my_sentiment[,]))
names(Sentimentscores)<-"Score"
SentimentScores<-cbind("sentiment"=rownames(Sentimentscores),Sentimentscores)
rownames(SentimentScores)<-NULL
ggplot(data=SentimentScores,aes(x=sentiment,y=Score))+geom_bar(aes(fill=sentiment),stat = "identity")+
  theme(legend.position="none")+
  xlab("Sentiment")+ylab("score")+ggtitle("UFO-WOW!!!!!")
########seeems like people feel positive about UF0's
########################################################
#####
####################################################

####mapping ufo's according to shape in various places(world map)
ufo$latitude<-as.numeric(ufo$latitude)
ufo_map<-borders("world",colour = "black",fill="blue")
map_ufo_data<-ggplot(ufo)+ufo_map+geom_point(aes(x=ufo$longitude, y=ufo$latitude,color=shape),shape=18) +
  theme(legend.position = "top")+
  ggtitle("UFOs")




################################TIME SERIES ANALYSIS ON UFO presence in future

####stripping date from date-time column
date_data<-format(as.POSIXct(strptime(ufo$datetime,"%m/%d/%Y %H:%M",tz="")),format="%m/%d/%Y")
time<-format(as.POSIXct(strptime(ufo$datetime,"%m/%d/%Y %H:%M",tz="")),format="%H:%M")
year_data<-mdy(date_data)
year_data<-year(year_data)
yeardata<-data.frame(year_data)
###grouping date data with no of ufo's seen per year

yeardata_ufo<-yeardata%>%
  group_by(year_data)%>%
  summarise(total=n())%>%
  arrange(desc(total))
####getting series of data
year_data_ufo<-subset(year_data_ufo,year_data>1940)
year_data_ufo<-arrange(year_data_ufo,year_data)
ts.ufo<-ts(data=year_data_ufo[,2],start=c(1941),frequency = 1)
ggplot(data=year_data_ufo,aes(x=year_data,y=total))+
  geom_line(linetype="dashed",color="orange",size=1)+
  geom_point(color="green",size=1)+ylab("UFFFFFOS")

##seems like from late 90's the trend was increasing but now in recent years is has decreased 

arima.ufo<-auto.arima(ts.ufo,trace = F,approximation = F)####ARIMA(2,1,2)
accuracy(arima.ufo)
summary(arima.ufo)#### get MAPE and RSME
pred<-forecast(arima.ufo,h=5)
autoplot(pred)+ylab("UFO FORECAST")
###################################
###################################

ufo$country<-as.factor(ufo$country)
country_wise_ufo<-ufo%>%
  group_by(country)%>%
  summarise(total=n())%>%
  arrange(desc(total))
levels(country_wise_ufo$country) <- c("NA","Australia","Canada","Germany","Great Britain","US")
ggplot(country_wise_ufo,aes(x=country,y=total))+
  geom_bar(stat="identity",color="blue",fill="pink")+
  theme_minimal()
######################
#######################shape wise distribution
ufo$shape<-as.factor(ufo$shape)
shape_wise_dist<-ufo%>%
  group_by(shape)%>%
  summarise(total=n())%>%
  arrange(desc(total))
ggplot(data=shape_wise_dist,aes(x=shape,y=total))+
  geom_bar(stat = "identity",color="white",fill="red")

#####many people saw light

######which shape stayed 

ufo$duration..seconds.<-as.integer(ufo$duration..seconds.)
ufo$duration..seconds.[is.na(ufo$duration..seconds.)]<-0
shape_duration<-ufo%>%
  group_by(shape)%>%
  summarise(total=mean(duration..seconds.))
levels(shape_duration$shape)[[1]]<-"strangelight"

ggplot(data=shape_duration,aes(x=shape,y=shape_duration$shape))+
  geom_bar(stat="identity",col="blue")
#############

