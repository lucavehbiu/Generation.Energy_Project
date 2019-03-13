#Load Libraries
library(shiny)
library(shinydashboard)
library(shinythemes)
library(plotly)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(scales)
library(sf)
library(rgdal)
library(RColorBrewer)
library(leaflet)
library(spdep)


#load objects
load('data.RDA')    #final data (working horse)
load('new.RDA')     #gementees with geometries
load('nature.RDA')  #natural resource data recoded
load('population.RDA') #population data recoded


#list of municipalities names
nmss <- as.vector(final_data$Gemeentena)

#list of region names
region <- as.vector(final_data$Res_regio)

#list of projection year variables
year <- c("2015AO" = "prj15AO", "2020AO" = "prj20AO",  
          "2025AO" =  "prj25AO","2030AO" = "prj30AO",  
          "2040AO" = "prj40AO", "2050AO" = "prj50AO",
          "2015NB" =  "prj15NB", "2020NB" = "prj20NB",  
          "2025NB" = "prj25NB","2030NB" = "prj30NB",  
          "2040NB" = "prj40NB", "2050NB" = "prj50NB")

#list of energy sources
energy <- as.vector(final_data$AO50)

#recode data to make it look fancier
final_data$AO50 <- recode(final_data$AO50, 
                          "e_won50AO_tj" = "E_home",
                          "e_cdv50AO_tj" = "E_commerce",
                          "e_pdv50AO_tj" = "E_public",
                          "e_indbwn50AO_tj" = "E_industrie",
                          "e_lbv50AO_tj" = "E_nature",
                          "w_won50AO_tj" = "W_home",
                          "w_cdv50AO_tj" = "W_commerce",
                          "w_pdv50AO_tj" = "W_public",
                          "w_indbwn50AO_tj" = "W_industrie",
                          "w_lbv50AO_tj" = "W_nature")




#UI Interface ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

ui <- dashboardPage( skin = "black",
                    
                    #UI header
                    dashboardHeader(
                      dropdownMenu(type = "messages",
                                   messageItem(
                                     from = "Generation Energy",
                                     message = "Current projections of NL"
                                   ),
                                   messageItem(
                                     from = "New User",
                                     message = "How do I register?",
                                     icon = icon("question"),
                                     time = "13:45"
                                   ),
                                   messageItem(
                                     from = "Support",
                                     message = "The new server is ready.",
                                     icon = icon("life-ring"),
                                     time = "2014-12-01"
                                   )
                      ),
                        title = "Energy Demand - NL",
                        titleWidth = 260,
                        tags$li(class = "dropdown",
                                tags$a(href= "http://generation.energy/", 
                                       target="_blank", 
                                       tags$img(height = "25px", 
                                                alt="SNAP Logo", 
                                                src="https://media.licdn.com/dms/image/C4E0BAQEXSlDASA-1Lw/company-logo_400_400/0?e=1559174400&v=beta&t=OohUisMRQJXWuZoN31OYR5joBBA5l1IGuahWzR9fOR0")
                                ))
                    ),

                    #Dashboard sidebar
                    dashboardSidebar(tags$head(tags$style("#histoPlot{height:60vh !important;}}")),
                                     width = 260,

                                     
                      sidebarMenu(style = " 
                                  overflow: visible;
                                  width: 260px;", #styling
                                  
                        #inputs for region         
                        selectizeInput(inputId = 'region', 
                                    label = 'Select Region:',
                                    choices = sort(region),
                                    selected = "Metropoolregio Eindhoven "),
                        
                        #updated input within server (Region-City)
                        htmlOutput(outputId = "regioServer"),
                        
                        #inputs for projection year
                        selectInput(inputId = "yearSelection",
                                    label = "Select Projection year:",
                                    choices = sort(year)),
                        
                        #inputs for kind of energy usage
                        selectInput(inputId = 'energy',
                                    label = 'Select kind of energy:',
                                    choices = sort(final_data$AO50),
                                    selected = F),
                        helpText("  Scroll Down"),
                        
                        #numeric input for histogram
                        sliderInput('histo',
                                     'Choose Granularity',
                                     value = 100,
                                     min = 3,
                                     max = 300))),
                    

                     #Dashboard body
                     dashboardBody(
                       tags$header(tags$style(HTML(' 
                                                 .main-header .logo {
                                                 font-family: "Raleway";
                                                 font-size: 24px;
                                                 position: fixed; 
                                                 overflow: visible;
                                                 width: 260px;
                                                 }'))),
                       tags$style(type="text/css",
                                  ".shiny-output-error { visibility: hidden; }",
                                  ".shiny-output-error:before { visibility: hidden; }"
                       ),
                       
                       
                        #first row of boxes
                        fluidRow(
                          splitLayout(cellWidths = c('50%', '50%'),
                                      
                                    column(12,
                                           box(status = "success",
                                        width = '100%',
                                        title = "Regio EnergieMix",
                                        solidHeader = T,
                                        height = '100%',
                                      plotlyOutput("plotRegio"))),
                                      
                                   
                                      box(status = "success",
                                          width = '100%',
                                          title = "Regio EnergieMix",
                                          solidHeader = T,
                                          height = '100%',
                                        leafletOutput("plotMap"))
                                      
                                    )),
                       
                        #second row of boxes
                        fluidRow(splitLayout(cellWidths = c('50%', '32%', '18%'),
                                             
                          column(12, 
                                 box(width = '100%', 
                              title = "Gemeente EnergieMix",
                              status = "success",
                              solidHeader = T,
                              height = 460,
                            plotlyOutput("plotGemeente"))),
                          
                          column(12, 
                                 box(width = '100%',
                              status = "success",
                              solidHeader = T,
                              title = "Gemeente",
                              plotlyOutput("plotNature"),
                              height = 460)),
                          
                          #third column containg Value Boxes
                          column(12,
                          valueBoxOutput("popBox", width = 24),

                          valueBoxOutput("densBox", width  =20),
                          
                          valueBoxOutput("ageBox", width = 20),
                          
                          valueBoxOutput("surfaceBox", width = 20)
                          ))),
                       
                       
                        #Third row of boxes
                        fluidRow(column(12, 
                                        box(width = '100%',
                                            height = '100%',
                                 status = "success",
                                 title = "Energy potential distriubtion per source/gemeente/region",
                                 solidHeader = T,
                                 plotlyOutput("histoPlot"))))
                       
                      
                       
                       )) #brackets for closing header and body


#Server side~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

server <- function(input, output) {
  
  #Server input for city of a region
  output$regioServer <- renderUI({
    
    ultimo <- final_data %>% filter(Res_regio == input$region)
    
    radioButtons(inputId = "citySelection",
                label = "Select city from selected Region:",
                choices = sort(unique(ultimo$Gemeentena)),
                selected = "Asten")
    
  })
  
  
  
  #pie chart for regions
  output$plotRegio <- renderPlotly({
    
    p <-  final_data %>%   
      gather(starts_with("prj"), 
             key = "Year", 
             value = "projections") %>% 
      filter(Res_regio == input$region,
             Year == input$yearSelection) %>% 
      select(projections, Year, AO50) %>% 
      group_by(AO50) %>% 
      summarise(shuma = sum(projections)) %>% 
      mutate(fraction = shuma / sum(shuma))
    #choose color theme
    colors = rev(colorRampPalette(brewer.pal(10, "RdBu"))(10))
    
    
    
    #plot the plotly chart
    p %>% 
      plot_ly(labels = ~AO50, 
              values = ~shuma, 
              textinfo = "none", 
              marker = list(colors = ~colors), sort = F) %>%  
      add_pie(hole = 0.75, 
              pull = 0.06, 
              direction = "clockwise") %>% 
      layout(title = input$region,  
             showlegend = T,
             titlefont = list(family = "Raleway",
                              size = 18,
                              color = "gray"),
             legend = list(orientation = "v",
                           font = list(size = 14, color = "gray")),
             annotations = list(x = 1.3, y = -0.12, 
                                text = "Source: Generation.Energy (2019)", 
                                showarrow = F, 
                                xref='paper', 
                                yref='paper', 
                                xanchor='right', 
                                yanchor='auto', 
                                xshift=0, 
                                yshift=0,
                                font=list(size=14, color="gray"))) 

  })
  
  #google maps plot
  output$plotMap <- renderLeaflet({
    
    #set the view to netherlands
    map_centre = st_centroid(world %>% filter(name_long == "Netherlands")) %>% 
      st_coordinates()
    
    #choose palettes for it
    factpal <- colorFactor("red", new$Gemeentena)
    
    #plot dat beautiful map
    leaflet() %>% addProviderTiles(providers$Stamen.TonerBackground,
                                   options = providerTileOptions(noWrap = T)) %>% 
      addProviderTiles(providers$Stamen.Watercolor, 
                       options = providerTileOptions(opacity = 0.2)) %>% 
      setView(lng = map_centre[, "X"], map_centre[, "Y"], zoom = 7) %>% 
      addPolygons(data = new %>% filter(Res_regio == input$region), 
                  fill = ~Res_regio, stroke = F, label = input$region) %>% 
      addPolygons(data = new %>% filter(Res_regio == input$region,
                                        Gemeentena == input$citySelection),
                  label = input$citySelection,
                  fillOpacity = 1,
                  fillColor = ~factpal(Gemeentena),
                  popup = ~paste(GM_Code)) 
    
  })
  
  #Bar chart for gemeentes
  output$plotGemeente <- renderPlotly({

    
  g <- final_data %>% 
    gather(starts_with("prj"), 
           key = "Year", 
           value = "projections") %>% 
    filter(Gemeentena == input$citySelection,
      Year == input$yearSelection) %>% 
    mutate(Percentage = round((projections/ sum(projections)), 
                            digits = 2) *100) %>% 
    group_by(baseYearAO = substr(AO50, 1, 2)) %>% 
    mutate(Score = sum(projections)) %>% 
    ggplot(aes(Gemeentena, Percentage, fill = AO50)) +
      geom_bar(stat = "identity", position = "dodge") +
      guides(fill = guide_legend(title = element_blank()), reverse = T) +
      theme(
            axis.ticks.y = element_blank(), 
            axis.line.x = element_line(color = "gray"),
            panel.background = element_blank(),
            axis.text.y = element_blank(),
            legend.title = element_blank(),
            legend.text = element_text(colour = "gray",
                                       size = 9),
            legend.direction = "vertical") +
      scale_y_continuous() +
    scale_fill_brewer(palette = "RdBu", direction = -1) +
    stat_summary(fun.y = sum, aes(label = ..y.., group = Score), 
                 geom = "text", 
                 position = position_dodge(width = .95), 
                 color = c("red", "blue"))
  
  
  ggplotly(g) %>% layout(xaxis = list(title = ""),
                         yaxis = list(title = input$yearSelection)) 
    
  })
    
    
    #natural resources pie chart
    output$plotNature <- renderPlotly({
    
      g3 <- nature %>%   
        filter(Gemeentena == input$citySelection,
               resource != "Semi-bebouwd") %>%
        arrange(resource) %>% 
        mutate(fraction = surface / sum(surface)) 
      
      colors2 <- c('rgb(235, 116, 0)',
                   'rgb(199, 149, 0)',
                   'rgb(0, 85, 128)',
                   'rgb(27, 77, 62)',
                   'rgb(102, 76, 40)',
                   'rgb(211, 211, 211')

       g3 %>% 
          plot_ly(labels = ~resource, values = ~fraction, 
                  sort = F, 
                  textinfo = "none",
                  marker = list(colors = ~colors2)) %>%  
          add_pie(hole = 0.82, pull = 0.05, direction = "clockwise") %>% 
          layout(title = paste("Natural Resources",input$citySelection),  
                 showlegend = T,
                 titlefont = list(family = "Raleway",
                                  size = 18,
                                  color = "gray"),
                 legend = list(orientation = "v",
                               xanchor = "center",
                               y = 0.5,
                               x = 0.5,
                               font = list(size = 14, color = "gray")),
                 annotations = 
                   list(x = 1, y = -0.125, text = "Source: CBS (2016)", 
                        showarrow = F, xref='paper', yref='paper', 
                        xanchor='right', yanchor='auto', xshift=0, yshift=0,
                        font=list(size=14, color="gray")))
    })
    
    #value box outputs - population
    output$popBox <- renderValueBox({
      regio_pop %>% 
        filter(Gemeentena == input$citySelection) -> test
      
      valueBox(test$Population, "Population", icon = icon("male"),
               color = "olive")
    })
    
    #population density
    output$densBox <- renderValueBox({
      regio_pop %>% 
        filter(Gemeentena == input$citySelection) -> test2
      
      valueBox(test2$Density, "Density per km²", 
               icon = icon("building"),
               color = "olive")
      
    })
    
    #energy density for square km
    output$ageBox <- renderValueBox({
      
        e_dens = merge(final_data, regio_pop, by = "Gemeentena") %>% 
          gather(starts_with("prj"), 
                 key = "Year", 
                 value = "projections") %>% 
          filter(Gemeentena == input$citySelection,
                 Year == input$yearSelection) %>% 
          mutate(Energy_per_km = round(sum(projections)/Density), 3) -> test3
      
      valueBox(paste(test3$Energy_per_km, "TJ"), "Energy per km² ",
               icon = icon("solar-panel"),
               color = "olive")
      
      
    })
    
    #for surface by gementeena
    output$surfaceBox <- renderValueBox({
      
      new %>% 
        mutate(Shape__Are = round(Shape__Are/1000000, 1)) %>% 
        filter(Gemeentena == input$citySelection) -> neww
      
      valueBox(paste(neww$Shape__Are, "km²"), "Surface",
               icon = icon("earth"),
               color = "olive")
      
    })
    
    
    
    #histogram of energy sources distribution
    output$histoPlot <- renderPlotly({
      
        ggplot() + 
        geom_histogram(data = final_data %>% 
                         gather(starts_with("prj"), 
                                key = "Year", 
                                value = "projections") %>% 
                         filter(Year == input$yearSelection,
                                AO50 == input$energy), 
                       aes(projections), 
                       bins = input$histo, fill = "gray") +
        geom_histogram(data = final_data %>% 
                         gather(starts_with("prj"), 
                                key = "Year", 
                                value = "projections") %>% 
                         filter(Year == input$yearSelection,
                                AO50 == input$energy,
                                Res_regio == input$region), 
                       aes(projections, fill = Res_regio), 
                       bins = input$histo) +
        geom_histogram(data = final_data %>% 
                         gather(starts_with("prj"), 
                                key = "Year", 
                                value = "projections") %>% 
                         filter(Year == input$yearSelection,
                                AO50 == input$energy,
                                Res_regio == input$region,
                                Gemeentena == input$citySelection), 
                       aes(projections, fill = Gemeentena), bins = input$histo) +
        theme(panel.background = element_blank(),
                    legend.position = c(0.7, 0.9),
                    legend.direction = "horizontal",
                    legend.text = element_text(size = 30, colour = "gray"),
              axis.ticks = element_blank(),
              axis.text = element_text(size = 15, colour = "gray"),
              axis.title.x = element_text(size = 20, colour = "gray"),
              axis.title.y = element_text(size = 20, colour = "gray"),
              legend.title = element_blank()) +
        scale_fill_manual(values = c("red", "blue"), element_blank()) -> h
      
     ggplotly(h) %>% 
       layout(
              showlegend = T,
              titlefont = list(family = "Raleway",
                               size = 18,
                               color = "gray"),
              legend = list(x = 0.66, y = 0.95,
                            font = list(size = 26, color = "gray"),
                            title = "mhonkrekarin"),
              annotations = list(x = 2, y = -0.1, 
                                 text = "Source: Generation.Energy", 
                                 showarrow = F, 
                                 xref='paper', 
                                 yref='paper', 
                                 xanchor='right', 
                                 yanchor='auto', 
                                 xshift=0, 
                                 yshift=0,
                                 font=list(size=14, color="gray")),
              xaxis = list(title = paste("Energy Demand (TJ)", input$energy)),
              yaxis = list(title = "Frequency"))
      
    })
    
    
    
  
}

shinyApp(ui, server)

