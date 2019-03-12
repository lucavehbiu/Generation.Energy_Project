#prepapre the data
set.seed(100)
data <- boxtel_data[sample.int(nrow(boxtel_data), 10000),]

# will be drawn last and thus be easier to see
data %>% dplyr:: select(-pot_tj) -> data

data <- gather(data, starts_with("pot"), key = "Years", value = "pred")



#server

server <- function(input, output, session) {
  
  ## Interactive Map ###########################################
  
  # Create the map
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles(
        urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
        attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
      ) %>%
      setView(5.32977,51.592484, zoom = 13) 
    
  })
  
  # A reactive expression that returns the set of zips that are
  # in bounds right now
  filterData <- reactive({
    
    data2 <-  data %>% dplyr:: filter(Years == input$year)
    
  })
  
  # Precalculate the breaks we'll need for the two histograms
  
  output$histCentile <- renderPlot({
    # If no zipcodes are in view, don't plot
    
    hist(filterData()$pred,
         breaks = 40,
         main = "Distribution of Energy (TJ)",
         xlab = "Percentile",
         col = '#00DD00',
         xlim = c(0, 0.3),
         border = 'white')
  })
  
  #bullshit plot about altitude and blah bllah
  output$scatterCollegeIncome <- renderPlot({
    
    filterData() %>% ggplot(aes(hoogtevers, filterData()$pred)) +
      geom_point(color = "lightblue") + geom_smooth() + theme(panel.background = element_blank())
    
  })
  
  # This observer is responsible for maintaining the circles and legend,
  # according to the variables the user has chosen to map to color and size.
  
  
  
  colorpal <- reactive({
    colorNumeric(input$colors, filterData()$pred)
  })
  
  
  observe({
    
    pal <- colorpal()
    
    
    leafletProxy("map", data = filterData()) %>%
      clearShapes() %>%
      addCircles(radius = ~10^pred/10, weight = 1, color = "#777777",
                 fillColor = ~pal(pred), 
                 fillOpacity = 0.7, 
                 popup = ~paste(pred)) %>% 
      addLegend("bottomleft", pal=pal, values= ~pred, title= "Energy Demand",
                layerId="colorLegend")
  })
  
  
  
  
}

shinyApp(ui, server)

