#############################
#######Winning percentage####
#############################
#### Logistic Regression ####
#############################

shot<-read.csv("shot_logs.csv")
shot = shot[shot$TOUCH_TIME > 0,]

# Shot Result = FGM
shot$GAME_CLOCK = as.character(shot$GAME_CLOCK)
shot$GAME_CLOCK = gsub(":", ".", shot$GAME_CLOCK)
shot$GAME_CLOCK = as.numeric(shot$GAME_CLOCK)

# New variable: psychological influence
shot$Clutch = "No"
shot$Clutch[shot$GAME_CLOCK <= 1 & shot$PERIOD >= 4 & abs(shot$FINAL_MARGIN) <= 5] = "Yes"
shot$Clutch = factor(shot$Clutch)

str(shot)

# Diverse shots
shot$SHOT_TYPE[shot$DRIBBLES <= 1 & shot$SHOT_DIST  > 4] = 'Catch and Shoot'
shot$SHOT_TYPE[shot$DRIBBLES <= 1 & shot$SHOT_DIST <= 4] = 'Cut'
shot$SHOT_TYPE[shot$DRIBBLES > 1 & shot$SHOT_DIST  <= 4] = 'Drive'
shot$SHOT_TYPE[shot$DRIBBLES > 4] = 'ISO/Post up'
shot$SHOT_TYPE[shot$DRIBBLES > 20] = 'Long ISO'
shot$SHOT_TYPE[shot$DRIBBLES <=1 & shot$PTS_TYPE == 3] = 'Spot Up Three'
shot$SHOT_TYPE = factor(shot$SHOT_TYPE)

summary(shot$SHOT_TYPE)

#View(shot_train)
model <- glm(SHOT_RESULT~ LOCATION + W + FINAL_MARGIN + SHOT_NUMBER + PERIOD + GAME_CLOCK  
             + DRIBBLES + TOUCH_TIME + SHOT_DIST + CLOSE_DEF_DIST + Clutch + SHOT_TYPE, family = "binomial", data = shot)

summary(model)
# INtercept, FINAL_MARGIN, SHOT_NUMBER, PERIOD, DRIBBLES, TOUCH_TIME, SHOT_DIST, CLOSE_DEF_DIST, SHOT_TYPE - Cut, Drive

# Train x Test (predictions)
shot_train <- shot[1:70000,]
shot_test <- shot[70001:124711,]

model_train2 <- glm(SHOT_RESULT~ FINAL_MARGIN + SHOT_NUMBER + PERIOD   
                    + DRIBBLES + TOUCH_TIME + SHOT_DIST + CLOSE_DEF_DIST + SHOT_TYPE, family = "binomial", data = shot_train)


model_train4 <- glm(SHOT_RESULT~  FINAL_MARGIN + SHOT_NUMBER + PERIOD  
                    + DRIBBLES + TOUCH_TIME + SHOT_DIST + CLOSE_DEF_DIST + Clutch + SHOT_TYPE, family = "binomial", data = shot_train)

## Predictions ##
fitted.results <- predict(model_train2, newdata = shot_test, type='response')
fitted.results <- ifelse(fitted.results > 0.5,1,0)
fitted.results <- as.data.frame(fitted.results)
fitted.results <- na.omit(fitted.results)
misClasificError <- mean(fitted.results != shot_test$FGM)
print(paste('Accuracy',1-misClasificError))
# Accuracy 0.48408777317844

fitted.results4 <- predict(model_train4, newdata = shot_test, type='response')
fitted.results4 <- ifelse(fitted.results4 > 0.5,1,0)
fitted.results4 <- as.data.frame(fitted.results4)
fitted.results4 <- na.omit(fitted.results4)
misClasificError4 <- mean(fitted.results4 != shot_test$FGM)
print(paste('Accuracy',1-misClasificError4))
# Accuracy 0.48399875372769




#######################
#### RANDOM FOREST ####
#######################

nbaData <- read.csv("NBA data.csv")
nbaData <- nbaData[-c(25,48,77,103,128,151,178,205,228,255,281,306,331,354,381,404,460,483,508,534,561,584,613), ] 
nbaData <- nbaData[,-c(20,25)]

library(dplyr)
nbaTeam1 <- read.csv("NBA stat team 1.csv")
nbaTeam2 <-read.csv("NBA stat team2.csv")

nbaTeam2 <- nbaTeam2 %>% mutate(WinPct = nbaTeam2$W/82)

nbaFull<-left_join(nbaData, nbaTeam2[,27:28], by=c("Tm"="ABR"))

library(rpart)
library(rpart.plot)
library(randomForest)
library(ggplot2)
nbaFullSubset <- nbaFull
set.seed(1)
nbaFullSubset$MP <- as.numeric(nbaFullSubset$MP)
#View(nbaFull)
nbaFull <- na.omit(nbaFull)
nbaFull$PER <- as.numeric(nbaFull$PER)
nbaFull$TS. <- as.numeric(nbaFull$TS.)
nbaFull$ORB. <- as.numeric(nbaFull$ORB.)
nbaFull$DRB. <- as.numeric(nbaFull$DRB.)
nbaFull$TRB. <- as.numeric(nbaFull$TRB.)
nbaFull$AST. <- as.numeric(nbaFull$AST.)
nbaFull$STL. <- as.numeric(nbaFull$STL.)
nbaFull$BLK. <- as.numeric(nbaFull$BLK.)
nbaFull$TOV. <- as.numeric(nbaFull$TOV.)
nbaFull$OWS <- as.numeric(nbaFull$OWS)
nbaFull$DWS <- as.numeric(nbaFull$DWS)
nbaFull$WS <- as.numeric(nbaFull$WS)
nbaFull$WS.48 <- as.numeric(nbaFull$WS.48)
nbaFull$OBPM <- as.numeric(nbaFull$OBPM)
nbaFull$DBPM <- as.numeric(nbaFull$DBPM)
nbaFull$BPM <- as.numeric(nbaFull$BPM)
nbaFull$VORP <- as.numeric(nbaFull$VORP)



set.seed(1)
formula <- as.formula(WinPct ~ PER + TS. + ORB. + DRB. + TRB. + AST. + STL. + BLK. + TOV. + OWS + DWS + WS + WS.48 + OBPM + DBPM + BPM + VORP)
random_forest <- randomForest(formula, data=nbaFull, mtry=4, ntree=10000, importance=TRUE)
importance(random_forest)
print(random_forest)

jpeg('randomforestplot.jpg')
varImpPlot(random_forest)

data <- data.frame(type=rownames(importance(random_forest)), importance(random_forest), check.names=F)
data$type <-reorder(data$type, data$`%IncMSE`)

jpeg('ggplot_RandomForest.jpg')
ggplot(data=data, aes(x=type, y=`%IncMSE`)) + geom_bar(stat='identity') + geom_hline(yintercept=abs(min(importance(random_forest)[])), col=2, linetype='dashed') + coord_flip()

# the best predictors DWS (Defensive Box Plus/Minus) and WS.48 (Win Shares Per 48 Minutes) best predic the win for a team.
# Which is sort of anomaly WS (Win Shares) and OWS (Offensive Win Shares) should have done better.


# Now observe the same prediction with linear model.
fitLinear<-lm(WinPct ~ PER + TS. + ORB. + DRB. + TRB. + AST. + STL. + BLK. + TOV. + OWS + DWS + WS + WS.48 + OBPM + DBPM + BPM + VORP, data=nbaFull)
summary(fitLinear)
# ORB., STL., OWS, DWS, WS.48 are the major predictors (p-value < 0.05)


library(plyr)
data <- nbaFull
data$MP <- as.numeric(data$MP)
cumMP <- by(data = data$MP, INDICES = data$Tm, FUN = sum)
cumMP_df <- data.frame(Team = names(cumMP), cumMP = as.numeric(cumMP))
colnames(data)[5] <- "Team"
data <- merge(x=data, y=cumMP_df, by = "Team")
data$WinPctMP <- with(data, WinPct * (MP/cumMP))

library(reshape)
library(colorspace)
library(scales)

datanew<-data[order(data[,30],decreasing=TRUE),]
datanew<- head(datanew,15)
datanew[,c(2,3,9,12,13,14,15)] <- NULL
data_melt<-melt(datanew)
data_melt <- ddply(data.m, .(variable), transform,rescale=rescale(value))

library(ggplot2)
#jpeg('Heatmap_top15.jpg')
ggplot(data_melt, aes(data_melt$variable, data_melt$Player)) + geom_tile(aes(fill = rescale), colour = "white") + scale_fill_gradient(low = "white", high = "steelblue")

###############################
#3 point percentage prediction#
###############################

### clear all environment data before proceeding.
rm(list=ls())
# install.packages("plotly")
# install.packages("magrittr")
library(magrittr)
library(plotly)
library(MASS)
library(plyr)
library(dplyr)

traditional<-read.csv("main-stats.csv")
usage<-read.csv("usage.csv")

allstats <- merge(traditional, usage, by=c("PLAYER"))

allstats$X3PMiss <- apply(allstats[,c('X3PA','X3PM')],1,function(x) {x[1]-x[2]})
allstats$Cross <- apply(allstats[,c('MIN.x','X.3PA')],1,function(x) {x[1]*x[2]})

### removing all players who attempted less than 10 3ptrs during the season
allstats<-allstats[!(allstats$X3PA<10),]

###plots showing total shot count based on usage and minutes played
p1<-plot_ly(allstats, x=~MIN.x, y=~USG., z=~X3PM,
            marker=list(size=5,
                        color='rgba(0, 255, 0, 255)'))%>%
  add_trace(name="Shots Made", mode='markers')
p2<-plot_ly(allstats, x=~MIN.x, y=~USG., z=~X3PMiss,
            marker=list(size=5,
                        color='rgba(255, 0, 0, 255)'))%>%
  add_trace(name="Shots Missed", mode='markers')

subplot(p1,p2) %>%
  layout(scene=list(
    xaxis = list(title="Minutes played"), 
    yaxis = list(title="Player Usage %"), 
    zaxis=list(title="Shot Count"))
  )

### model for predicting number of made shots
model1 <- glm.nb(formula=X3PM ~ X.3PA + MIN.x + Cross + USG., data=allstats, init.theta = 1e-6, control=glm.control(trace=FALSE,maxit=100))

### model for predicting number of missed shots
model2 <- glm.nb(formula=X3PMiss ~ X.3PA + MIN.x + Cross + USG., data=allstats, init.theta = 1e-6, control=glm.control(trace=FALSE,maxit=100))

summary(model1)
summary(model2)

results <- as.data.frame(cbind(fitted(model1), fitted(model2), model1$y, model2$y))

### adding predicted and actual percentages
results$predictedX<-results$V1/(results$V1+results$V2)
results$actualX<-results$V3/(results$V3+results$V4)

### creating final dataframe
final<-data.frame(allstats$PLAYER, results$predictedX, results$actualX)
colnames(final) <- c('player', 'predictedX', 'actualX')

### calculating mean squared error
sqrErr<-sum((final$predictedX - final$actualX)^2)/nrow(final)

### graphing predicted 3pt% vs actual 3pt%
graphData<-sample_n(final, 10)
graphData<-droplevels(graphData)
plot_ly(graphData, x = ~player, y = ~predictedX, type = 'bar', name = 'Predicted') %>%
  add_trace(y = ~actualX, name = 'Actual') %>%
  layout(yaxis = list(title = 'Count'), barmode = 'group')


###############################
#FGP Empirical Bayes Estimate##
###############################
library(ggplot2)
library(dplyr)
library(ggthemes)
################################################
##Data scraping and preprocessing###############
#wouldn't recomend running the code(takes long)#
################################################
# data<-data.frame(player="sample",fgp=123,attempts=123,made=123)
# total<-data.frame(player="sample",fgp=123,attempts=123,made=123)
# data$player<-as.character(data$player)
# total$player<-as.character(total$player)
# data$fgp<-as.numeric(data$fgp)
# total$fgp<-as.numeric(total$fgp)
# data$attempts<-as.numeric(data$attempts)
# total$attempts<-as.numeric(total$attempts)
# data$made<-as.numeric(data$made)
# total$made<-as.numeric(total$made)
# for(i in 1:nrow(roster)){
#   link_new<-paste("http://stats.nba.com/stats/shotchartdetail?","SeasonType=Regular+Season&", "TeamID=0&",paste("PlayerID=",as.character(roster$PERSON_ID[i]),"&",sep=""),"GameID=&","Outcome=&", "Location=&","Month=0&","SeasonSegment=&","DateFrom=&", "DateTo=&","OpponentTeamID=0&","VsConference=&", "VsDivision=&","Position=&","RookieYear=&", "GameSegment=&","Period=0&","LastNGames=0&","ContextMeasure=FGA&", "PlayerPosition=",sep="")
#   shots<-readLines(link_new)
#   shots<-jsonlite::fromJSON(shots)
#   shots1<-as.data.frame(shots$resultSets$rowSet[[1]])
#   colnames(shots1)<-shots$resultSets$headers[[1]]
#   data$player<-unique(shots1$PLAYER_NAME)
#   shots1$SHOT_MADE_FLAG<-factor(shots1$SHOT_MADE_FLAG)
#   k<-summary(shots1$SHOT_MADE_FLAG)
#   data$fgp<-as.numeric(k[2]/(k[2]+k[1]))
#   data$attempts<-as.numeric(k[2]+k[1])
#   data$made<-as.numeric(k[2])
#   total<-rbind(total,data)
# }
#######################################################
##Saved the scraped data in the rda below##############
#######################################################
load("fgp.rda")
total<-na.omit(total)
ggplot(total,aes(fgp))+geom_histogram(bins=27)+xlab("FG% Average For All Players")+ggtitle("FG% Distribution Across Players")+theme_economist_white()
ggplot(total,aes(fgp,y=..density..))+geom_histogram(bins=27)+geom_density(col="blue")+xlab("FG% Average For All Players")+ggtitle("FG% Distribution Across Players")+theme_economist_white()

m <- MASS::fitdistr(total$fgp, dbeta,
                    start = list(shape1 = 1, shape2 = 10))
alpha0 <- m$estimate[1]
beta0 <- m$estimate[2]
total <- total %>%
  mutate(eb_estimate = (made + alpha0) / (attempts + alpha0 + beta0))

total<-total[order(total$eb_estimate,decreasing = T),]
head(total,n=5)
total<-total[order(total$eb_estimate),]
head(total,n=5)
