library(readxl)
library(leaflet)
library(ggplot2)
library(dplyr)
library(reshape2)
library(caret)
library(maptools)
library(maps)
library(geosphere)
library(htmltools)
library(ggthemes)
library(plotly)
library(shiny)
library(shinydashboard)
library(leaflet)



airport<-read.csv("global_airports.csv")
#As we needed to fill the map by the number of airports for each country, we needed to add that variable 
#and assign corresponding number to each country
#for that we traversed the whole dataset and added the numbers
#however this worked very slowly, so we decided to export the dataset and donot run the loop every time
#see below the loop that has been used
#map<-mutate(map,num="NA")
#map
#num<-airport%>% group_by(country) %>%tally()
#num$country<-as.character(num$country)
#for (i in 1:length(map$region)){
#  for(j in 1:length(num$country)){
#    if(map$region[i]==num$country[j])
#      map$num[i]<-num$n[j]
#  }
# if(map$region[i]=="USA")
#  map$num[i]<-1323
# if(map$region[i]=="Democratic Republic of the Congo")
#   map$num[i]<-38
# if(map$region[i]=="Democratic Republic of the Congo")
#   map$num[i]<-38
#write.csv(map,file="Dmap.csv")
#}

dmap<-read.csv("Dmap.csv")
dmap$num<-as.numeric(dmap$num)

#########################################################
#### Customizing the data for Airline Network Plots #####
#########################################################
#map("state")
xlim <- c(-171.738281, -56.601563)
ylim <- c(12.039321, 71.856229)
airports <- read.csv("http://datasets.flowingdata.com/tuts/maparcs/airports.csv", header=TRUE)
flights <- read.csv("http://datasets.flowingdata.com/tuts/maparcs/flights.csv", header=TRUE, as.is=TRUE)
flights$airline[flights$airline=="AA"]<-"American Airlines"
flights$airline[flights$airline=="OO"]<-"Skywest Airlines"
flights$airline[flights$airline=="HA"]<-"Hawaiian Airlines"
flights$airline[flights$airline=="CO"]<-"Continental Airlines"
flights$airline[flights$airline=="AS"]<-"Alaska Airlines"
flights$airline[flights$airline=="B6"]<-"JetBlue Airlines"
flights$airline[flights$airline=="DL"]<-"Delta Airlines"
flights$airline[flights$airline=="EV"]<-"Atlandtic Southswest Airlines"
flights$airline[flights$airline=="F9"]<-"Frontier Airlines"
flights$airline[flights$airline=="FL"]<-"Air Tran Airlines"
flights$airline[flights$airline=="MQ"]<-"American Eagle Airlines"
flights$airline[flights$airline=="UA"]<-"United Airlines"
flights$airline[flights$airline=="US"]<-"US Airlines"
flights$airline[flights$airline=="WN"]<-"Southwest Airlines"
flights$airline[flights$airline=="XE"]<-"ExpressJet Airlines"
flights$airline[flights$airline=="YV"]<-"Mesa Airlines"




#############################
#### Dataset of Flights #####
#############################
delta<-read.csv("DeltaAirFlights.csv")
delta<-mutate(delta,Airline="Delta Airlines")
alaska<-read.csv("AlaskaAirFligths.csv")
alaska<-mutate(alaska,Airline="Alaska Airlines")
amair<-read.csv("AmericanAirFlights.csv")
amair<-mutate(amair,Airline="American Airlines")
haw<-read.csv("HawaiianAirFlights.csv")
haw<-mutate(haw,Airline="Hawaiian Airlines")
unair<-read.csv("UnitedAirFlights.csv")
unair<-mutate(unair,Airline="United Airlines")
virgin<-read.csv("VirginAmericaFlights.csv")
virgin<-mutate(virgin,Airline="Virgin America")
yearflights<-rbind(delta,alaska,amair,haw,unair,virgin)

#########################
##Dataset of Passengers##
#########################
deltap<-read.csv("DeltaAirPass.csv")
deltap<-mutate(deltap,Airline="Delta Airlines")
alaskap<-read.csv("AlaskaPass.csv")
alaskap<-mutate(alaskap,Airline="Alaska Airlines")
amairp<-read.csv("AmericanAirPass.csv")
amairp<-mutate(amairp,Airline="American Airlines")
hawp<-read.csv("HawaiianAirPass.csv")
hawp<-mutate(hawp,Airline="Hawaiian Airlines")
unairp<-read.csv("UnitedAirPass.csv")
unairp<-mutate(unairp,Airline="United Airlines")
virginp<-read.csv("VirginAmericaPass.csv")
virginp<-mutate(virginp,Airline="Virgin America")
passengers<-rbind(deltap,alaskap,amairp,hawp,unairp,virginp)

#######################
#########Joining#######
#######################
airlines<-yearflights %>% inner_join(passengers, by = c("Year","Month","Airline"))
names(airlines)<-c("Year","Month","DomesticFlights","InternationalFlights","TotalFlights","Airline","DomesticPassengers","InternationalPassengers","TotalPassengers")
airlines$Month<-factor(airlines$Month)
#Removing TOTAL Rows
airlines<-filter(airlines, airlines$Month!="TOTAL")
airlines$Month<-factor(airlines$Month)
airlines$Month<-factor(airlines$Month,levels=c(1,2,3,4,5,6,7,8,9,10,11,12),labels=c("January","February","March","April","May","June","July","August","September","October","November","December"))



####### International Airports
airportsdot <- read.csv("https://raw.githubusercontent.com/jpatokal/openflights/master/data/airports.dat", header=F)
colnames(airportsdot) <- c("ID", "name", "city", "country", "IATA_FAA", "ICAO", "lat", "lon", "altitude", "timezone", "DST")
#View(airportsdot)

# getting the routs data
routes<- read.csv("https://raw.githubusercontent.com/jpatokal/openflights/master/data/routes.dat", header=F)
colnames(routes) <- c("airline", "airlineID", "sourceAirport", "sourceAirportID", "destinationAirport", "destinationAirportID", "codeshare", "stops", "equipment")
#View(routes)


library(plyr)
departures <- ddply(routes, .(sourceAirportID), "nrow")
names(departures)[2] <- "flights"
arrivals <- ddply(routes, .(destinationAirportID), "nrow")
names(arrivals)[2] <- "flights"

#add the info on departing and arriving flights to the airports dataset
airportD <- merge(airportsdot, departures, by.x = "ID", by.y = "sourceAirportID")
airportA <- merge(airportsdot, arrivals, by.x = "ID", by.y = "destinationAirportID")

# create the data set containing both departures and arrivals
airportD$type <- "departures"
airportA$type <- "arrivals"
airportDA <- rbind(airportD, airportA)

# The data to be used for airport network plotting
routesair<-merge(routes, airportsdot, by.x = "sourceAirport", by.y = "IATA_FAA") 



# A linear model that predicts the average ticket price for US domestic airlines, we wanted to make this model dynamic 
# however we couldn't manage

#################
##Linear Model###
#################
#getting the data for each quarter (1995-2016)
fair1<-read.csv("fair1.csv")
names(fair1)<-c("Year","Q1Price")
fair2<-read.csv("fair2.csv")
names(fair2)<-c("Year","Q2Price")
fair3<-read.csv("fair3.csv")
names(fair3)<-c("Year","Q3Price")
fair4<-read.csv("fair4.csv")
names(fair4)<-c("Year","Q4Price")
#joining the data
avfair<-inner_join(fair1,fair2,by="Year")
avfair<-inner_join(avfair,fair3,by="Year")
avfair<-inner_join(avfair,fair4,by="Year")
avfair<-mutate(avfair,average=(Q1Price+Q2Price+Q3Price+Q4Price)/4)

fit<-lm(avfair$average~avfair$Year+avfair$Q1Price)
fit
predfair<-predict(fit)
predfair
qplot(avfair$average,predfair,xlab="Observations",ylab="Predictions")+geom_abline(slope =1,intercept = 0)
