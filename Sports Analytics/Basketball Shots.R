load("roster.rda")
load("shots.rda")
link<-paste("http://stats.nba.com/stats/shotchartdetail?","SeasonType=Regular+Season&", "TeamID=0&","PlayerID=203498&","GameID=&","Outcome=&", "Location=&","Month=0&","SeasonSegment=&","DateFrom=&", "DateTo=&","OpponentTeamID=0&","VsConference=&", "VsDivision=&","Position=&","RookieYear=&", "GameSegment=&","Period=0&","LastNGames=0&","ContextMeasure=FGA&", "PlayerPosition=",sep="")
link
shots<-readLines(link)
shots<-jsonlite::fromJSON(shots)
names(shots)
shots1<-as.data.frame(shots$resultSets$rowSet[[1]])
colnames(shots1)<-shots$resultSets$headers[[1]]
shots1[,c(3,8:10,17:21)]<-apply(shots1[,c(3,8:10,17:21)],2,as.numeric)
str(shots1)
k<-summary(shots1$SHOT_MADE_FLAG)
as.numeric(k[2]/k[1])
shots1$SHOT_MADE_FLAG<-factor(shots1$SHOT_MADE_FLAG)

data<-data.frame(player="sample",fgp=123,attempts=123,made=123)
total<-data.frame(player="sample",fgp=123,attempts=123,made=123)
data$player<-as.character(data$player)
total$player<-as.character(total$player)
data$fgp<-as.numeric(data$fgp)
total$fgp<-as.numeric(total$fgp)
data$attempts<-as.numeric(data$attempts)
total$attempts<-as.numeric(total$attempts)
data$made<-as.numeric(data$made)
total$made<-as.numeric(total$made)
for(i in 1:nrow(roster)){
link_new<-paste("http://stats.nba.com/stats/shotchartdetail?","SeasonType=Regular+Season&", "TeamID=0&",paste("PlayerID=",as.character(roster$PERSON_ID[i]),"&",sep=""),"GameID=&","Outcome=&", "Location=&","Month=0&","SeasonSegment=&","DateFrom=&", "DateTo=&","OpponentTeamID=0&","VsConference=&", "VsDivision=&","Position=&","RookieYear=&", "GameSegment=&","Period=0&","LastNGames=0&","ContextMeasure=FGA&", "PlayerPosition=",sep="")
shots<-readLines(link_new)
shots<-jsonlite::fromJSON(shots)
shots1<-as.data.frame(shots$resultSets$rowSet[[1]])
colnames(shots1)<-shots$resultSets$headers[[1]]
data$player<-unique(shots1$PLAYER_NAME)
shots1$SHOT_MADE_FLAG<-factor(shots1$SHOT_MADE_FLAG)
k<-summary(shots1$SHOT_MADE_FLAG)
data$fgp<-as.numeric(k[2]/(k[2]+k[1]))
data$attempts<-as.numeric(k[2]+k[1])
data$made<-as.numeric(k[2])
total<-rbind(total,data)
}
total<-total[-1,]



shots1$YEAR<-substr(as.character(shots1$GAME_DATE),1,4)
shots1$SHOT_MADE_FLAG<-factor(shots1$SHOT_MADE_FLAG)
summary(shots1$SHOT_MADE_FLAG)


library(SportsAnalytics270)
data("nba2009_2016")
View(nba2009_2016)
shots1<-shots1[shots$YEAR<2017,]
library(ggplot2)
library(dplyr)
ggplot(shots1,aes(x=SHOT_ZONE_AREA))+geom_bar()+ggtitle("smt")
FG_PCT<-shots1%>%group_by(SHOT_ZONE_AREA)%>%summarise(FG_PCT=mean(SHOT_MADE_FLAG))
ggplot(FG_PCT,aes(x=SHOT_ZONE_AREA,y=FG_PCT))+geom_bar(stat="identity")+ggtitle("smt else")
shots2<-as.data.frame(shots$resultSets$rowSet[[2]])
colnames(shots2)<-shots$resultSets$headers[[2]]
head(shots2,5)
shots2$FG_PCT<-as.numeric(as.character(shots2$FG_PCT))
FG_PCT_av<-shots2%>%group_by(SHOT_ZONE_AREA)%>%summarise(FG_PCT=mean(FG_PCT))
ggplot(FG_PCT,aes(x=SHOT_ZONE_AREA,y=FG_PCT))+geom_bar(stat="identity")+geom_line(data=FG_PCT_av,aes(factor(SHOT_ZONE_AREA),y=FG_PCT,group=1),colour="cyan",size=2)+ggtitle("smt else")
ggplot(shots1,aes(LOC_X,LOC_Y))+geom_point()

theme<-theme(axis.title.x=element_blank(), axis.text.x=element_blank(),
             axis.ticks.x=element_blank(),
             axis.title.y=element_blank(),
             axis.text.y=element_blank(),
             axis.ticks.y=element_blank())+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(colour ="black",fill=NA,size=2))
ggplot(shots1,aes(LOC_X,LOC_Y))+geom_point()+theme
ggplot(shots1,aes(LOC_X,LOC_Y))+geom_point(aes(colour=EVENT_TYPE),alpha=0.5)+theme
