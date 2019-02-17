library(shiny)
library(shinydashboard)
library(shinythemes)
library(plotly)


nms <- names(test)

nmss <- as.vector(pjes$Gemeentena)

#UI Interface ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

ui <- dashboardPage(
                    
                    #UI header
                    
                    dashboardHeader(
                        title = "Energy Distribution - All administrative levels"),

                    #Dashboard sidebar
                    dashboardSidebar(
                      
                        selectInput(inputId = "citySelection",
                                    label = "Select city:",
                                    choices = sort(unique(nmss))),
                        selectInput('color', 'Color', choices = nms, selected = "AO40"),
                        
                        selectInput(inputId = "yearSelection",
                                    label = "Select year",
                                    choices = sort(nms)),
                        
                        htmlOutput(outputId = "projFilter")
                        
                        
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
  
  output$projFilter <- renderUI({
    
    #data <- portion %>% gather(ends_with("50AO_tj"), key = "AO50", value = "prj50AO") %>% 
      #filter(Gemeentenaam == input$citySelection)
    
    selectInput('y', 'Y', choices = lali, selected = "prj40AO")
    
    
    
  })
  
  
  #plots
  
  output$plot2 <- renderPlot({
    
    #portion %>% gather(ends_with("50AO_tj"), key = "AO50", value = "prj50AO") %>% 
    test %>%   
      filter(Gemeentena == input$citySelection) %>% 
      mutate(fraction = prj40AO / sum(prj40AO)) %>% 
      arrange(fraction) %>% 
      mutate(ymax = cumsum(fraction),
             ymin = c(0, head(ymax, n = -1))) %>% 
      ggplot(aes(fill = AO40, ymax = ymax, ymin = ymin, xmax = 4, xmin = 3)) +
      geom_rect() +
      coord_polar(theta = "y") +
      xlim(c(0, 4)) +
      labs(y = element_blank(), x = input$citySelection, title = "Energy Potential Distribution (TJ) - 2050 AO") +
      guides(fill = guide_legend(title = "Energy sources")) +
      theme(axis.ticks = element_blank(), axis.text = element_blank(), panel.background = element_blank()) +
      scale_fill_brewer(palette = "Spectral")
    
    
    
    
  })
  
  
  output$plot3 <- renderPlot({
    pjes %>% gather(ends_with("40AO_tj"), key = "AO40", value = "prj40AO") %>% 
      select(Gemeentena, prj40AO, AO40) %>% 
      filter(Gemeentena == input$citySelection) %>% 
      mutate(fraction = round((prj40AO/ sum(prj40AO)), digits = 2)) -> test
    
  g <- test %>%   ggplot(aes(Gemeentena, fraction, fill = AO40)) +
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
                vjust= -0.2,
                data = subset(test, fraction > 0)) +
      scale_y_continuous(labels = percent) +
    coord_flip()
    
    tg <- grobTree(textGrob("Energy Potential Distribution (TJ) - 2050 AO", 
                            y= 0.8, vjust=1, gp = gpar(fontsize=13, face=2, col="gray")),
                   textGrob(input$citySelection, y= -0.3, vjust= 0, gp = gpar(fontsize= 40, face=4, col="gray")),
                   cl="titlegrob")
    
    heightDetails.titlegrob <- function(x) do.call(sum,lapply(x$children, grobHeight))
    
    
    grid.arrange(g, top = tg)
    
    
    
    
    
    
  })
  
  
}

shinyApp(ui, server)

