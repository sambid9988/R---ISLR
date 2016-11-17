library(dplyr)
library(ggplot2)
ipl<-read.csv("deliveries.csv")
matches<-read.csv("matches.csv")
###########
str(ipl)

batting_runs<-aggregate(total_runs~batsman,data=ipl,sum)
batting_runs<-arrange(batting_runs,desc(total_runs))
###highest runs by players
ggplot(batting_runs[1:10,],aes(x=batsman,y=total_runs))+
  geom_bar(stat="identity",fill="tomato3")+
  scale_x_discrete("Top 10 batsman")+
  scale_y_continuous("Runs",breaks=seq(1000,5000,200)) +
  theme(axis.text.x=element_text(angle=75,vjust = 0.5))

batting_teamruns<-aggregate(total_runs~batting_team,data=ipl,sum)
####batting total by teams
ggplot(batting_teamruns,aes(x=batting_team,y=total_runs,color=batting_team))+
  geom_bar(stat="identity",fill="paleturquoise2")+
  theme(axis.text.x=element_text(angle=45,vjust = 0.5))+ 
  geom_text(aes(label=total_runs), vjust=1.6, color="purple", position = position_dodge(0.9), size=3.5)
                                                            
###batsman 6's and 4's
batsman_4s<-ipl%>%
            select(batsman,batsman_runs)%>%
            filter(batsman_runs==4)%>%
            group_by(batsman)%>%
            count()%>%arrange(desc(n))

batsman_6s<-ipl%>%
  select(batsman,batsman_runs)%>%
  filter(batsman_runs==6)%>%
  group_by(batsman)%>%
  count()%>%arrange(desc(n))
###top 10 highest 4's
ggplot(batsman_4s[1:10,],aes(x=batsman,y=n,color=batsman))+
  geom_bar(stat="identity",fill="violetred2")+
  geom_text(aes(label=n), vjust=1, color="white", position = position_dodge(0.9), size=3.5)+
  xlab("Batsman")+ylab("4S")
###top 10highest 6's
ggplot(batsman_6s[1:10,],aes(x=batsman,y=n,color=batsman))+
  geom_bar(stat="identity",fill="yellowgreen")+
  geom_text(aes(label=n), vjust=1, color="black", position = position_dodge(0.9), size=3.5)+
  xlab("Batsman")+ylab("6s")



########################################################################################
str(matches)
View(matches)
season_matches<-matches%>%
          group_by(season)%>%
          summarise(total=n())
season_matches$season<-as.factor(season_matches$season)      
ggplot(season_matches,aes(x=season,y=total))+
  geom_bar(stat="identity",aes(fill=season),alpha=0.5)+theme_get()+
  geom_text(aes(label=total),vjust=1,position=position_dodge(1))+ggtitle("Seasonwise Matches")

###################top 10 venues
venue_matches<-matches%>%
      group_by(venue)%>%
      summarise(total=n())%>%
      arrange(desc(total))



ggplot(venue_matches[1:10,],aes(x=venue,y=total))+
  geom_bar(stat="identity",fill="blueviolet")+coord_flip()+
  geom_text(aes(label=total))+theme(axis.text.x=element_text(angle=90,vjust = 0.3))+ggtitle("Top 10 Venues")+theme_minimal()

##################team matches played

team_matches1<-matches%>%
          group_by(team1)%>%
        summarise(total1=n())%>%
        arrange(desc(team1))
team_matches2<-matches%>%
        group_by(team2)%>%
      summarise(total2=n())%>%
    arrange(desc(team2))
team_matches<-cbind(team_matches1,team_matches2)
View(team_matches)
team_matches<-team_matches%>%
  mutate(total=total1+total2)%>%
  select(team1,total)


ggplot(team_matches,aes(x=team1,y=total))+
  geom_bar(stat="identity",fill="sienna")+theme_dark()+
  geom_text(aes(label=total))+
  theme(axis.text.x=element_text(angle=45,vjust = 0.3))+
ggtitle("Matches per Team")


################man of the match
man_of_match<-matches%>%
            group_by(player_of_match)%>%
            summarise(total=n())%>%
            arrange(desc(total))

ggplot(data=man_of_match[1:10,],aes(x=player_of_match,y=total))+
    geom_bar(stat="identity",width=0.7,fill="lawngreen")+
  geom_text(aes(label=total), vjust=1.6, color="white", size=3.5)+
  theme(axis.text.x=element_text(angle=45,vjust = 0.3))+
  theme_minimal()+ylab("MOF")+ggtitle("players by man of the match")


#####team-wise wins
team_high_wins<-matches%>%
        group_by(winner)%>%
        summarise(total=n())%>%
        arrange(desc(total))


labels<-round(team_high_wins$total/sum(team_high_wins$total)*100,1)
labls<-paste(team_high_wins$winner,labels)


pie(team_high_wins$total,main="Team_Wins",
    col.main="Red",
    labels =labls,col=rainbow(14),cex=0.7)

######
str(ipl)
dismisal<-ipl%>%
      group_by(player_dismissed)%>%
      summarise(total=n())%>%
      arrange(desc(total))
dismisal<-dismisal[-1,]

            

