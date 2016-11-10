library(ggplot2)
library(dplyr)
library(tm)
library(plyr)
library(SnowballC)
library(wordcloud)
library(httr)
library(twitteR)
library(Rstem)
library(sentiment)
library(ROAuth)
library(base64enc)
library(stringr)
library(syuzhet)
download.file(url="http://curl.haxx.se.ca/cacert.pem",destfile = "cacert.pem")
requestURL<-"https://api.twitter.com/oauth/request_token"
accessURL <- "https://api.twitter.com/oauth/access_token"
authURL <- "https://api.twitter.com/oauth/authorize"
consumerkey<-"xxxxxxxxxxxxxxxx"
consumersecret<-"xxxxxxxxxxxxxxxxxxx"
my_oauth<-OAuthFactory$new(consumerKey=consumerkey,consumerSecret=consumersecret,requestURL=requestURL,accessURL=accessURL,authURL=authURL)
my_oauth$handshake(cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl"))
save(my_oauth, file="oauth_token.Rdata")
#######################
#####################
load("oauth_token.Rdata")
accessToken="xxxxxxxxxxxxxxxxxxxxxxxxxxxx"
accessSecret="xxxxxxxxxxxxxxxxxxxxx"
setup_twitter_oauth(consumer_key=consumerkey, consumer_secret=consumersecret,
                    access_token=accessToken, access_secret=accessSecret)



################
some_tweets<-searchTwitter("Trump",n=5000,since="2016-01-01",lang="en")
length(some_tweets)


Trump<-ldply(some_tweets,function(t) t$toDataFrame())
write.csv(Trump,"Trump.csv")


####get the text
some_txt<-sapply(some_tweets,function(x) x$getText())


####let's clean people name ,RT,etc
.
some_txt1<-gsub("(RT|via)((?:\\b\\w*@\\w+)+)","",some_txt)

#####let's clean html links

some_txt2<-gsub("http[^[:blank:]]+","",some_txt1)

########let's remove people names

some_txt3<-gsub("@\\w+","",some_txt2)

########let's remove punctuations

some_text4<-gsub("[[:punct:]]"," ",some_txt3)

######let's remove number (alphanumeric)

some_text5<-gsub("[^[:alnum:]]"," ",some_text4)
some_text5<-gsub("RT","",some_text5)

###########

write.csv(some_text5,"tweets4.csv")

############################################################

#####creating corpus now
####removing puncutations,stop words etc

some_text8<-Corpus(VectorSource(some_text5))
some_text8<-tm_map(some_text8, removePunctuation)
some_text8<-tm_map(some_text8,content_transformer(tolower))
some_text8<-tm_map(some_text8,removeWords,stopwords("english"))
some_text8<-tm_map(some_text8,stripWhitespace)

########build word cloud

pal<-brewer.pal(8,"Dark2")
#####you can change brewer pal options

wordcloud(some_text8,min.freq = 10,max.words =Inf,scale=c(8,.2),rot.per=.15,random.order = F,color=pal)

############now the analysis(sentiment)

mysentiment<-get_nrc_sentiment((some_text5))
####used to classify sentiment scores
Sentimentscores<-data.frame(colSums(mysentiment[,]))
names(Sentimentscores)<-"Score"
View(Sentimentscores)
SentimentScores<-cbind("sentiment"=rownames(Sentimentscores),Sentimentscores)
rownames(SentimentScores)<-NULL
ggplot(data=SentimentScores,aes(x=sentiment,y=Score))+geom_bar(aes(fill=sentiment),stat = "identity")+
  theme(legend.position="none")+
  xlab("Sentiment")+ylab("score")+ggtitle("Total sentiment based on scores")


