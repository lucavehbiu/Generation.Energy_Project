library(shiny)
library(shinydashboard)
library(shinythemes)
library(plotly)

#load save RDA objects
load("bashk.RDA")   #AO projections (automation)
load("bashkNB.RDA") #NB projections (after savings)
load("before_merging.RDA")  #un-gathered final data
load("un_gathered.merged")  #merged

load('data.RDA')    #gather final data (working horse)

load('mapping.RDA') #area of NL fortified and merge (Geemente + Regio)

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




#UI Interface ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

ui <- dashboardPage(
                    
                    #UI header
                    
                    dashboardHeader(
                        title = "Energy Demand Potential",
                        tags$li(a(href = 'http://generation.energy/',
                                  img(src = '0.jpg',
                                      title = "Generation.Energy", height = "30px")),
                                class = "dropdown")),

                    #Dashboard sidebar
                    dashboardSidebar(
                        selectizeInput(inputId = 'region', 
                                    label = 'Select Region:',
                                    choices = sort(region),
                                    selected = "Metropoolregio Eindhoven "),
                        
                        selectInput(inputId = "yearSelection",
                                    label = "Select Projection year:",
                                    choices = sort(year)),
                        
                        htmlOutput(outputId = "regioServer")),
                    
                     #Dashboard body
                     dashboardBody(
                        fluidRow(splitLayout(cellWidths = c("60%", "40%"),
                                    
                                    box(width = "100%",
                                      plotOutput("plotRegio")),
                                    
                                    box(width = "100%",
                                      plotOutput("plotMap")))),
                        
                        fluidRow(splitLayout(cellWidths = c('60%', '40%'),
                                             
                          box(width = "100%",
                                     plotOutput("plotGemeente")),
                          
                          box(width = "100%",
                              plotOutput("plotNature"))))
                          
                        ))
                    
                    



server <- function(input, output) {
  
  #Server inputs
  
  output$regioServer <- renderUI({
    
    ultimo <- final_data %>% filter(Res_regio == input$region)
    
    selectInput(inputId = "citySelection",
                label = "Select city from selected Region:",
                choices = sort(ultimo$Gemeentena))
    


    
  })
  
  
  #pie chart for regions
  output$plotRegio <- renderPlot({
    
    g2 <- final_data %>%   
      gather(starts_with("prj"), key = "Year", value = "projections") %>% 
      filter(Res_regio == input$region,
             Year == input$yearSelection) %>% 
      group_by(Res_regio) %>% 
      mutate(fraction = projections / sum(projections)) %>% 
      mutate(ymax = cumsum(fraction),
             ymin = c(0, head(ymax, n = -1))) %>% 
      arrange(desc(ymax)) %>% 
      ggplot(aes(fill = AO40, ymax = ymax, ymin = ymin, xmax = 4, xmin = 3)) +
      geom_rect() +
      coord_polar(theta = "y") +
      xlim(c(0, 4)) +
      labs(y = element_blank(), x = element_blank()) +
      guides(fill = guide_legend(title = "Opwekmix", reverse = T)) +
      theme(axis.ticks = element_blank(), axis.text = element_blank(), 
            panel.background = element_blank()) +
      scale_fill_brewer(palette = "PuOr")
    
    #title for pie chart of regions
    tg2 <- grobTree(
                   textGrob(input$region, 
                            y= -0.3, 
                            vjust= 0.8, 
                            gp = gpar(fontsize= 40, face=4, col="gray")),
                   cl="titlegrob")
    
    heightDetails.titlegrob <- function(x) 
      do.call(sum,lapply(x$children, grobHeight))
    
    
    grid.arrange(g2, top = tg2)
    
  })
  
  
  output$plotMap <- renderPlot({
    
    area.fort %>%  
      ggplot(aes(long, lat, group = group)) +
      geom_polygon(aes( 
        fill = Res_regio == input$region)) +
      # geom_polygon(aes(x = long, y = lat, group = group, fill = Gemeentena == "Eindhoven")) +
      geom_path() +
      coord_equal() +
      guides(fill = guide_legend(element_blank())) +
      theme(axis.ticks = element_blank(),
            axis.text = element_blank())
    
    
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
      labs(
        x = element_blank(), 
        y = element_blank()) +
      guides(fill = guide_legend(title = "Opwekmix", reverse = T)) +
      theme(axis.ticks = element_blank(), 
            panel.background = element_blank(),
            axis.text.y = element_blank()) +
      scale_fill_brewer(palette = "Spectral") + 
      geom_text(aes(label = paste0(fraction, '%')), 
                position=position_dodge(width=0.9), 
                vjust= -0.2) +
      scale_y_continuous() +
    coord_flip() 
  
    tg <- grobTree(textGrob("Energievraag en OPWEKMIX", 
                            y= 0.8, 
                            vjust= 0.8, 
                            gp = gpar(fontsize=17, 
                                      face=2, 
                                      col="lightblue")),
                   textGrob(input$citySelection, 
                            y= -0.3, 
                            vjust= 1.2, 
                            gp = gpar(fontsize= 40, face=4, col="gray")),
                   cl="titlegrob")
    
    heightDetails.titlegrob <- function(x) 
      do.call(sum,lapply(x$children, grobHeight))
    
    
    grid.arrange(g, top = tg)
    
    
    #natural resources pie chart
    output$plotNature <- renderPlot({
      
      
      g3 <- nature %>%   
        filter(Gemeentena == input$citySelection) %>% 
        mutate(fraction = surface / sum(surface)) %>% 
        arrange(desc(fraction)) %>% 
        mutate(ymax = cumsum(fraction),
               ymin = c(0, head(ymax, n = -1))) %>% 
        ggplot(aes(fill = resource, ymax = ymax, ymin = ymin, xmax = 4, xmin = 3)) +
        geom_rect() +
        coord_polar(theta = "y") +
        xlim(c(0, 5)) +
        labs(y = element_blank(), x = element_blank()) +
        guides(fill = guide_legend(title = "Natural resources:")) +
        theme(axis.ticks = element_blank(), axis.text = element_blank(), 
              panel.background = element_blank()) +
        scale_fill_brewer(palette = "Greens")
      
      tg3 <- grobTree(
        textGrob(paste0(input$citySelection, "-", "Natural resources"), 
                 y= -0.3, 
                 vjust= 0.8, 
                 gp = gpar(fontsize= 40, face=4, col="gray")),
        cl="titlegrob")
      
      heightDetails.titlegrob <- function(x) 
        do.call(sum,lapply(x$children, grobHeight))
      
      
      grid.arrange(g3, top = tg3) #plot the title with the chart
      
    })
    
    
    
    
    
    
    
    
  })
  
  
}

shinyApp(ui, server)

