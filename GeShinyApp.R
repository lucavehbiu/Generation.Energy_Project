library(shiny)
library(shinydashboard)
library(shinythemes)
library(plotly)


#list of all variables
nms <- names(final_data)

#list of municipalities names
nmss <- as.vector(final_data$Gemeentena)

#list of region names
region <- as.vector(final_data$Res_regio)

#list of projection year variables
year <- c("baseYearAO", "prj15AO",  "prj20AO",  "prj25AO", "prj30AO",  "prj40AO", "prj50AO")




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
                        selectInput('region', 
                                    'Select Region:',
                                    choices = sort(region),
                                    selected = "`Regio Wes Brabant`"),
                        
                        selectInput(inputId = "yearSelection",
                                    label = "Select Projection year:",
                                    choices = sort(nms)),
                        
                        htmlOutput(outputId = "regioServer")
                        #htmlOutput(outputId = "yearSelection")
                        
               
                        
                        
                        
                      ),
                    
                     #Dashboard body
                     dashboardBody(
                        fluidRow(
                                    
                                    box(width = "100%",
                                      plotOutput("plot2")
                                      
                                      ),
                                    
                                    box(width = "100%",
                                      plotOutput("plot3")
                                    )
                                    
                                    ))
                    
                    )



server <- function(input, output) {
  
  #Server inputs
  
  output$regioServer <- renderUI({
    
    ultimo <- final_data %>% filter(Res_regio == input$region)
    
    selectInput(inputId = "citySelection",
                label = "Select city from selected Region:",
                choices = sort(ultimo$Gemeentena))
    


    
  })
  
  #plots
  
  output$plot2 <- renderPlot({
    
    #portion %>% gather(ends_with("50AO_tj"), key = "AO50", value = "prj50AO") %>% 
    g2 <- final_data %>%   
      filter(Res_regio == input$region) %>% 
      group_by(Res_regio) %>% 
      mutate(fraction = prj40AO / sum(prj40AO)) %>% 
      mutate(ymax = cumsum(fraction),
             ymin = c(0, head(ymax, n = -1))) %>% 
      ggplot(aes(fill = AO40, ymax = ymax, ymin = ymin, xmax = 4, xmin = 3)) +
      geom_rect() +
      coord_polar(theta = "y") +
      xlim(c(0, 4)) +
      labs(y = element_blank(), x = element_blank()) +
      guides(fill = guide_legend(title = "Energy sources")) +
      theme(axis.ticks = element_blank(), axis.text = element_blank(), 
            panel.background = element_blank()) +
      scale_fill_brewer(palette = "Set3")
    
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
  
#Bar chart  
  output$plot3 <- renderPlot({
    
  g <- final_data %>% 
    filter(Res_regio == input$region,
      Gemeentena == input$citySelection) %>% 
    mutate(fraction = round((prj40AO/ sum(prj40AO)), digits = 2) *100) %>% 
    ggplot(aes(Gemeentena, fraction, fill = AO40)) +
      geom_bar(stat = "identity", position = "dodge") +
      labs(
        x = element_blank(), 
        y = element_blank()) +
      guides(fill = guide_legend(title = "Energy sources", reverse = T)) +
      theme(axis.ticks = element_blank(), 
            panel.background = element_blank(),
            axis.text.y = element_blank()) +
      scale_fill_brewer(palette = "Spectral") + 
      geom_text(aes(label = paste0(fraction, '%')), 
                position=position_dodge(width=0.9), 
                vjust= -0.2) +
      scale_y_continuous() +
    coord_flip()
    
    tg <- grobTree(textGrob(input$region, 
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
    
    
    
    
    
    
  })
  
  
}

shinyApp(ui, server)

