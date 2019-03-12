library(leaflet)
library(shiny)
library(shinydashboard)

# Choices for drop-downs
vars <- c("Current year" = "pot_tj2014",
          "Future year" = "pot_tj2050")


ui <- navbarPage("Boxtel", id="nav",
           
           tabPanel("Interactive map",
                    div(class="outer",
                        
                        tags$head(
                          # Include our custom CSS
                          includeCSS("styles.css"),
                          includeScript("gomap.js")
                        ),
                        
                        # If not using custom CSS, set height of leafletOutput to a number instead of percent
                        leafletOutput("map", width="100%", height="100%"),
                        
                        # Shiny versions prior to 0.11 should use class = "modal" instead.
                        absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                      draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
                                      width = 330, height = "auto",
                                      
                                      h2("Explore Boxtel"),
                                      
                                      
                                      selectInput("year", "Years", vars, selected = "Current year"),
                                      
                                      selectInput("colors", "Color Scheme",
                                                  rownames(subset(brewer.pal.info, 
                                                                  category %in% c("seq", "div"))),

                                      ),
                                      
                                      plotOutput("histCentile", height = 200),
                                      plotOutput("scatterCollegeIncome", height = 250)
                        ),
                        
                        tags$div(id="cite",
                                 'Data compiled through: ', 
                                 tags$em('Klimaat Monitor, 2014–2050'), ' by Generation.Energy')
                    )
           ))

           