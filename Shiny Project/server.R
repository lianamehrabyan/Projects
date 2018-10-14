shinyServer(function(input, output, session) {

  output$airport <- renderLeaflet({
 #  dataAir<-airport
   #if(input$Country) 
    # dataAir<-airport %>% filter(country==input$Country) 
    
    leaflet(airport) %>% addTiles() %>% addMarkers(
      clusterOptions = markerClusterOptions(), popup = paste("Country: ", airport$country, "<br>",
                                                                          "City: ", airport$city, "<br>",
                                                                        "Airport name: ", airport$name, "<br>",
                                                                       "Code :", airport$iata_faa, "<br>"))
  })
  
  ## For ggploty use this
  # We used this plot for showing how many airports the country has when you hovered above, but as it was time and memory 
  # consuming we decided to use just the dynamic map. #### The code written below ####
  # output$heatmap <- renderPlotly({
  #   ggplotly(  ggplot(dmap,aes(x=long,y=lat)) + geom_polygon(aes(group=group,fill= num))+scale_fill_gradient(name="Number of Airports")+ylab("Latitude")+xlab("Longitute")
  #                +theme_igray() + geom_point(aes(text=region, size=num), colour="white", alpha=1/2) )
  # })
  
  output$heatmap <- renderPlot({
    ggplot(dmap,aes(x=long,y=lat)) + geom_polygon(aes(group=group,fill= num))+scale_fill_gradient(name="Number of Airports")+ylab("Latitude")+xlab("Longitute")+theme_igray()
  
    })
  # plotting the airport network for chosen airport
  output$airportnetwork <- renderPlot({
    map("world", col="#f2f2f2", fill=TRUE, bg="white", lwd=0.05)
    fsub1 <- routesair[routesair$sourceAirport == input$Aiport,]
    for (j in 1:length(fsub1$sourceAirport)) {
      air11 <- routesair[routesair$sourceAirport == fsub1[1,]$sourceAirport,]
      air2 <- routesair[routesair$destinationAirport == fsub1[j,]$destinationAirport,]

      inter <- gcIntermediate(c(air11[1,]$lon, air11[1,]$lat), c(air2[1,]$lon, air2[1,]$lat), n=100, addStartEnd=TRUE)

      lines(inter, col="black", lwd=0.8)
    }
  })
  
  # plotting the airline network for chosen airline
  output$airnetwork <- renderPlot({
     pal <- colorRampPalette(c("#f2f2f2", "black"))
     colors <- pal(100)
    
    map("world", col="#f2f2f2", fill=TRUE, bg="white", lwd=0.05, xlim=xlim, ylim=ylim)
    
    fsub <- flights[flights$airline == input$Airline,]
    fsub <- fsub[order(fsub$cnt),]
    maxcnt <- max(fsub$cnt)
    for (j in 1:length(fsub$airline)) {
      air1 <- airports[airports$iata == fsub[j,]$airport1,]
      air2 <- airports[airports$iata == fsub[j,]$airport2,]
      
      inter <- gcIntermediate(c(air1[1,]$long, air1[1,]$lat), c(air2[1,]$long, air2[1,]$lat), n=100, addStartEnd=TRUE)
      colindex <- round( (fsub[j,]$cnt / maxcnt) * length(colors) )
      
      lines(inter, col=colors[colindex], lwd=0.8)
      
    }
  })
  
  # Worldwide airports
  output$airportnet <-renderPlot({
    world<-map_data("world")
     g<- ggplot(world,aes(x=long,y=lat))+geom_polygon(aes(group=group))
     g<-g+ggtitle("World Map")+geom_jitter(data=airportsdot, aes(x=lon, y=lat, colour="coral1"), show.legend = F, size=1) 
     g
  })
  
  # Codes for plotting annual statistics
  output$totalflights <- renderPlot({
    yairlines<-filter(airlines,Year== input$Year)
    ggplot(yairlines,aes(x = Month, y = TotalFlights, group = Airline, color = Airline)) +
      geom_line() + 
      geom_point(size = 1.1) + 
      theme_economist() +
      scale_fill_hc()
    
  })
  output$totalflights2 <- renderPlot({
    yairlines<-filter(airlines,Year==input$Year)
    ggplot(yairlines,aes(x = Month, y = DomesticFlights, group = Airline, color = Airline)) +
      geom_line() + 
      geom_point(size = 1.1) + 
      theme_economist() +
      scale_fill_hc()
    
  })
  
  output$totalflights3 <- renderPlot({
    yairlines<-filter(airlines,Year==input$Year)
    ggplot(yairlines,aes(x = Month, y = InternationalFlights, group = Airline, color = Airline)) +
      geom_line() + 
      geom_point(size = 1.1) + 
      theme_economist() +
      scale_fill_hc()
    
  })
  output$totalpass <- renderPlot({
    yairlines<-filter(airlines,Year==input$Year)
    ggplot(yairlines,aes(x = Month, y = TotalPassengers, group = Airline, color = Airline)) +
      geom_line() + 
      geom_point(size = 1.1) + 
      theme_economist() +
      scale_fill_hc()
  })
  output$totalpass2 <- renderPlot({
    yairlines<-filter(airlines,Year==input$Year)
    ggplot(yairlines,aes(x = Month, y = DomesticPassengers, group = Airline, color = Airline)) +
      geom_line() + 
      geom_point(size = 1.1) + 
      theme_economist() +
      scale_fill_hc()
  })
  output$totalpass3 <- renderPlot({
    yairlines<-filter(airlines,Year==input$Year)
    ggplot(yairlines,aes(x = Month, y = InternationalPassengers, group = Airline, color = Airline)) +
      geom_line() + 
      geom_point(size = 1.1) + 
      theme_economist() +
      scale_fill_hc()
  })
  
  # data for the airports 
  output$airTable <- renderDataTable({
   
   airport<-airport[airport$city==input$City & airport$country==input$Country,]
  })
  

 
  # the code below lets us see only the cities of the chosen country in select tabs
  output$City<-renderUI({
   allCity<-airport$city
    # if(as.logical(as.numeric(input$Country))) {
    a<-input$Country
    
    selectizeInput("City", "City", choices = unique(sort(airport$city[airport$country==a])))
   
    # } else {selectizeInput("City", "City", choices = a)}
    
  })
  
  output$Name<-renderUI({
    a<-input$City
    selectizeInput("Name", "Name", choices = unique(sort(airport$name[airport$city==a])))
  })
  
  
})

