library(shiny)
library(shinydashboard)
library(shinythemes)
library(plotly)

#load save RDA objects
load("bashk.RDA")   #AO projections (automation)
load("bashkNB.RDA") #NB projections (after savings)
load("before_merging.RDA")  #un-gathered final data
load("un_gathered.merged.RDA")  #merged

load('data.RDA')    #gather final data (working horse)

load('mapping.RDA') #area of NL fortified and merge (Geemente + Regio)

load('NL_map.RDA') #map of NL from google

load('nature.RDA')  #natural resource data (to be recoded)

#list of all variables
nms <- names(final_data)

#list of municipalities names
nmss <- as.vector(final_data$Gemeentena)

#list of region names
region <- as.vector(final_data$Res_regio)

#list of projection year variables
year <- c("prj15AO",  "prj20AO",  "prj25AO", "prj30AO",  "prj40AO", "prj50AO",
          "prj15NB",  "prj20NB",  "prj25NB", "prj30NB",  "prj40NB", "prj50NB")

#list of energy sources
energy <- as.vector(final_data$AO20)




#UI Interface ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

ui <- dashboardPage( skin = "black",
                    
                    #UI header
                    
                    dashboardHeader(
                        title = "Energy Demand - NL",
                        titleWidth = 260,
                        tags$li(class = "dropdown",
                                tags$a(href= "http://generation.energy/", target="_blank", 
                                       tags$img(height = "25px", 
                                                alt="SNAP Logo", 
                                                src="https://media.licdn.com/dms/image/C4E0BAQEXSlDASA-1Lw/company-logo_400_400/0?e=1559174400&v=beta&t=OohUisMRQJXWuZoN31OYR5joBBA5l1IGuahWzR9fOR0")
                                ))
                    ),

                    #Dashboard sidebar
                    dashboardSidebar(width = 260,
                      sidebarMenu(style = "position: fixed; 
                                  overflow: visible;
                                  width: 260px;",
                        selectizeInput(inputId = 'region', 
                                    label = 'Select Region:',
                                    choices = sort(region),
                                    selected = "Metropoolregio Eindhoven "),
                        
                        htmlOutput(outputId = "regioServer"),
                        
                        
                        selectInput(inputId = "yearSelection",
                                    label = "Select Projection year:",
                                    choices = sort(year)),
                      
                        selectInput(inputId = 'energy',
                                    label = 'Select kind of energy:',
                                    choices = sort(final_data$AO20),
                                    selected = F),
                        helpText("Scroll Down"),
                        
                        sliderInput('histo',
                                     'Choose Granularity',
                                     value = 100,
                                     min = 3,
                                     max = 300))),
                    
                       
                    
                    
                    
                     #Dashboard body
                     dashboardBody(
                       tags$head(tags$style(HTML(' 
                                              .main-header .logo {
                                                 font-family: "Raleway";
                                                 font-size: 24px;
                                                 position: fixed; 
                                                 overflow: visible;
                                                 width: 260px;
                                                 }'))),
                       

                       
                        fluidRow(
                          splitLayout(cellWidths = c('50%', '50%'),
                                  column(12, 
                                    box(status = "success",
                                        width = '100%',
                                        title = "Regio EnergieMix",
                                        solidHeader = T,
                                        height = 455,
                                      plotlyOutput("plotRegio"))),
                                    column(12, box(width = '100%',
                                        solidHeader = T,
                                        height = 455,
                                        title = "Map of Regio/Geemente",
                                        status = "success",
                                      plotOutput("plotMap"))))),
                       

                        fluidRow(splitLayout(cellWidths = c('50%', '32%', '18%'),
                                             
                          column(12, box(width = '100%', 
                              title = "Gemeente EnergieMix",
                              status = "success",
                              solidHeader = T,
                              height = 455,
                            plotOutput("plotGemeente"))),
                          column(12, box(width = '100%',
                                         status = "success",
                              solidHeader = T,
                              title = "Gemeente",
                              plotlyOutput("plotNature"),
                              height = 455)),
                          column(12,
                                 valueBox(1*125000 , "Population", icon = icon("male"),
                                   width = 20, color = "olive"),
                          
                          valueBox(paste(1*845, "per m2") , "Density", icon = icon("building"),
                                   width = 20, color = "olive"),
                          valueBox(1*43 , "Average Age", icon = icon("hotel"),
                                   width = 20, color = "olive"))
                          )),
                       

                        fluidRow(column(12, box(width = '100%',
                                                status = "success",
                                 title = "Energy potential distriubtion per source/gemeente/region",
                                 solidHeader = T,
                                 plotOutput("histoPlot"))))))



server <- function(input, output) {
  
  #Server inputs
  
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
      gather(starts_with("prj"), key = "Year", value = "projections") %>% 
      filter(Res_regio == input$region,
             Year == input$yearSelection) %>% 
      group_by(Res_regio) %>% 
      arrange(desc(AO40)) %>% 
      mutate(fraction = projections / sum(projections)) %>% 
      mutate(ymax = cumsum(fraction),
             ymin = c(0, head(ymax, n = -1))) 
    
    colors = colorRampPalette(brewer.pal(10, "PuOr"))(100)[100*p$ymax]
    
    
    p %>% 
      plot_ly(labels = ~AO40, values = ~ymax, textinfo = "none", 
              marker = list(colors = ~colors), name = "luca") %>%  
      add_pie(hole = 0.82, pull = 0.06, direction = "clockwise") %>% 
      layout(title = input$region,  showlegend = T,
             titlefont = list(family = "Agency FB",
                              size = 20,
                              color = "gray"),
             legend = list(orientation = "v",
                           xanchor = "center",
                           y = 0.5,
                           x = 0.5,
                           font = list(size = 9, color = "gray")),
             annotations = 
               list(x = 1, y = -0.1, text = "Source: Generation.Energy", 
                    showarrow = F, xref='paper', yref='paper', 
                    xanchor='right', yanchor='auto', xshift=0, yshift=0,
                    font=list(size=15, color="gray")))
    


    
    #title for pie chart of regions
  
    
  })
  
  #google maps plot
  output$plotMap <- renderPlot({
    
    
    ggmap(sfMap) +
      geom_polygon(data = area.fort %>% filter(Res_regio == input$region), 
                   aes(fill = Res_regio, x = long, y = lat, group = group),
                   colour = "white",
                   alpha = 0.9,
                   size = 0.2) +
      geom_polygon(data = area.fort %>% filter(Res_regio == input$region,
                                               Gemeentena == input$citySelection),
                   aes(fill = Gemeentena, x = long, y = lat, group = group),
                   size = 0.4) +
      scale_fill_manual(values = c("blue", "orange")) +
      labs(x = element_blank(),
           y = element_blank()) +
      theme(axis.ticks = element_blank(),
            axis.text = element_blank(),
            legend.title = element_blank(),
            legend.text = element_text(size = 18, colour = "gray"),
            legend.direction = "horizontal",
            legend.position = c(0.25, 0.965)) +
      coord_equal()  -> g
    
    grid.arrange(g,
                 bottom = textGrob("Source: Generation Energy",
                                   x = 1,
                                   hjust = 1, gp = gpar(fontface = 3L, fontsize = 13)))
    

    
    
  })
  
  
 
  
  #final_data[which(final_data$prj40AO > 0),]
  #Bar chart for gemeentes
  output$plotGemeente <- renderPlot({
    
  g <- final_data %>% 
    gather(starts_with("prj"), key = "Year", value = "projections") %>% 
    filter(Res_regio == input$region,
      Gemeentena == input$citySelection,
      Year == input$yearSelection) %>% 
    mutate(fraction = round((projections/ sum(projections)), digits = 2) *100) %>% 
    ggplot(aes(Gemeentena, fraction, fill = AO40)) +
      geom_bar(stat = "identity", position = "dodge") +
      labs(title = input$citySelection,
        x = element_blank(), 
        y = element_blank()) +
      guides(fill = guide_legend(title = element_blank(), reverse = T)) +
      theme(title = element_text(colour = "gray",
                                 size = 15),
            axis.ticks.y = element_blank(), 
            axis.line.x = element_line(color = "gray"),
            panel.background = element_blank(),
            axis.text.y = element_blank(),
            axis.text.x = element_text(size = 20, colour = "gray"),
            legend.title = element_text(colour = "gray",
                                        size = 20),
            legend.text = element_text(colour = "gray",
                                       size = 18),
            legend.direction = "horizontal",
            legend.position = c(0.68, 0.97)) +
      scale_fill_brewer(palette = "PuOr",
                        labels = c("E_cdv",
                                   "E_indbdwn",
                                   "E_lbv",
                                   "E_pdv",
                                   "E_rest",
                                   "E_won",
                                   "W_cdv",
                                   "W_indbdwn",
                                   "W_lbv",
                                   "W_pdv",
                                   "W_rest")) + 
      geom_text(aes(label = paste0(fraction, '%')), 
                position=position_dodge(width=0.9), 
                vjust= -0.2,
                hjust = 0.9,
                size = 4) +
      scale_y_continuous() +
    coord_flip() 
  g
   
    
  })
    
    
    #natural resources pie chart
    output$plotNature <- renderPlotly({
      
      
      g3 <- nature %>%   
        filter(Gemeentena == input$citySelection) %>% 
        mutate(fraction = surface / sum(surface)) %>% 
        arrange(desc(resource)) %>% 
        mutate(ymax = cumsum(fraction),
               ymin = c(0, head(ymax, n = -1))) 
        
        
        colors = colorRampPalette(brewer.pal(9, "Greens"))(100)[100*g3$ymax]
        
        
       g3 %>% 
          plot_ly(labels = ~resource, values = ~ymax, textinfo = "none", 
                  marker = list(colors = ~colors)) %>%  
          add_pie(hole = 0.82, pull = 0.05) %>% 
          layout(title = paste("Natural Resources",input$citySelection),  showlegend = T,
                 titlefont = list(family = "Agency FB",
                                  size = 20,
                                  color = "gray"),
                 legend = list(orientation = "v",
                               xanchor = "center",
                               y = 0.5,
                               x = 0.5,
                               font = list(size = 16, color = "gray")),
                 annotations = 
                   list(x = 1, y = -0.1, text = "Source: CBS", 
                        showarrow = F, xref='paper', yref='paper', 
                        xanchor='right', yanchor='auto', xshift=0, yshift=0,
                        font=list(size=15, color="gray")))
        
        
      
    })
    
    #histogram of energy sources distribution
    
    output$histoPlot <- renderPlot({
      
      
        ggplot() + 
        geom_histogram(data = final_data %>% 
                         gather(starts_with("prj"), key = "Year", value = "projections") %>% 
                         filter(Year == input$yearSelection,
                                AO20 == input$energy), 
                       aes(projections), bins = input$histo) +
        geom_histogram(data = final_data %>% 
                         gather(starts_with("prj"), key = "Year", value = "projections") %>% 
                         filter(Year == input$yearSelection,
                                AO20 == input$energy,
                                Res_regio == input$region), 
                       aes(projections, fill = Res_regio), bins = input$histo) +
        geom_histogram(data = final_data %>% 
                         gather(starts_with("prj"), key = "Year", value = "projections") %>% 
                         filter(Year == input$yearSelection,
                                AO20 == input$energy,
                                Res_regio == input$region,
                                Gemeentena == input$citySelection), 
                       aes(projections, fill = Gemeentena), bins = input$histo) +
        theme(panel.background = element_blank(),
                    legend.position = c(0.85, 0.9),
                    legend.direction = "horizontal",
                    legend.text = element_text(size = 30, colour = "gray"),
              axis.ticks = element_blank(),
              axis.text = element_text(size = 25, colour = "gray"),
              axis.title.x = element_text(size = 20, colour = "gray")) +
        labs(x = "Energy Potential",
             y = element_blank()) +
        scale_fill_manual(values = c("blue", "orange"), element_blank()) -> h
      
      grid.arrange(h,
                   bottom = textGrob("Source: Generation Energy",
                                     x = 1,
                                     hjust = 1, gp = gpar(fontface = 3L, fontsize = 13)))
      

      
      
    })
    
    
  
  
}

shinyApp(ui, server)

