# libraries & data load ----

library(shiny)
library(shinydashboard)
library(tidyverse)
library(stringr)
library(shinyWidgets)
library(wesanderson)
library(plotly)

# import ------

# data
data <- read_rds("data/clean_data.rds")
data_attend <- read_rds("data/attendance_data.rds") 


# skill lists
load("data/skill_lists.RData")

# user interface ----

ui <- dashboardPage(
  
  # ui header ----
  
  dashboardHeader(
    title = "Skill list performance"
  ),
  
  # ui sidebar ----
  
  dashboardSidebar(
    
    selectInput(inputId = "courseSelection",
                label = "Select course:",
                choices = sort(unique(data$course))
    ),
    
    
    # checking dynamic selectInputs
    htmlOutput(outputId = "cityServer"),
    htmlOutput(outputId = "groupServer"),
    htmlOutput(outputId = "sheetServer"),
    
    # select the skill group
    selectInput(inputId = "skillGroupSelection",
                label = "Select the skill group: ",
                choices = sort(unique(data$skill_group))
    ),
    
    # student selection
    htmlOutput(outputId = "studentServer")
    
  ),
  
  
  # ui body -----
  
  dashboardBody(
    tags$style(type = "text/css", "#map {height: calc(100vh - 80px) !important;}"),
    fluidRow(
      
      # group chart
      box(
        title = "Group selected vs. requested level",
        width = 7,
        plotOutput(outputId = "box1", width = "100%")
      ),
      
      # group tab box for top 5 skills
      tabBox(
        width = 5,
        tabPanel(
          title = "Group worst skills (req.lvl = 100 points)",
          tableOutput(outputId = "groupTable1")
        ),
        tabPanel(
          title = "Group best skills (req.lvl = 100 points)",
          tableOutput(outputId = "groupTable2")
        ),
        tabPanel(
          title = "Group boxplot",
          plotlyOutput(outputId = "groupBoxplot")
        )
      )
    ),
    
    # STUDENT ANALYSIS
    
    # student assistance
    fluidRow(
      box(
        title = "Student attendance",
        width = 8,
        plotlyOutput(outputId = "attendanceChart",
                   width = "100%",
                   height = 250)
      ),
      htmlOutput(outputId = "attendanceServer"),
      htmlOutput(outputId = "justUnattendanceServer"),
      htmlOutput(outputId = "unattendanceServer")
    ),
      
    # student skill bar chart
    fluidRow(
      box(
        title = "Student evolution",
        width = 14,
        plotOutput(outputId = "box2", 
                     width = "100%",
                     height = 300)
      )
    ),
    
    # student skill table
    fluidRow(
      box(
        title = "Students skill list (WORST to BEST)",
        width = 14,
        dataTableOutput(outputId = "table2")
      )
    )
    
  )
  
)



# server ------

server <- function(input, output) {
  
  # server input select ----
  
  output$cityServer <- renderUI({
    data_available <- data %>% filter(course == input$courseSelection)
    
    selectInput(inputId = "citySelection",
                label = "Select city:",
                choices = sort(unique(data_available$city)))
  })
  output$groupServer <- renderUI({
    data_available <- data %>% filter(city == input$citySelection,
                                      course == input$courseSelection)
    
    selectInput(inputId = "groupSelection",
                label = "Select group:",
                choices = sort(unique(data_available$group)))
  })
  output$sheetServer <- renderUI({
    data_available <- data %>% filter(city == input$citySelection,
                                      group == input$groupSelection,
                                      course == input$courseSelection)
    
    selectInput(inputId = "sheetSelection",
                label = "Select evaluation sheet:",
                choices = sort(unique(data_available$sheet_name), decreasing = T))
  })
  output$studentServer <- renderUI({
    data_available <- data %>% filter(city == input$citySelection,
                                      group == input$groupSelection,
                                      sheet_name == input$sheetSelection,
                                      course == input$courseSelection)
    
    selectInput(inputId = "studentSelection",
                label = "Select student:",
                choices = sort(unique(data_available$student_name)))
  })
  
  
  # body plots ----
  
  # group performance
  output$box1 <- renderPlot({
    data %>% 
      filter(group == input$groupSelection, 
             sheet_name == input$sheetSelection,
             skill_group == input$skillGroupSelection, 
             course == input$courseSelection) %>%
      skill_filter(sheet_name = input$sheetSelection,
                   module1_list, module2_list, energy_list, wifi_list, final_list) %>% 
      group_by(skill_type) %>%
      summarise(Group_lvl = median(my_level),
                Requested_lvl = median(lvl_required)) %>% 
      gather(key = skill_grouped, value = marks, Group_lvl, Requested_lvl) %>% 
      ggplot(aes(x = skill_type, marks, fill = skill_grouped)) +
        geom_col(position = "dodge") +
        scale_fill_brewer(palette="Paired") +
        coord_flip() + aes(x=reorder(skill_type, marks)) +
        theme(axis.title.y = element_blank(), 
              axis.text.y.left = element_text(size = 12))
    
  })
  
  # group boxplot
  output$groupBoxplot <- renderPlotly({
    data %>%
      filter(sheet_name == input$sheetSelection, 
             group == input$groupSelection,
             skill_group == input$skillGroupSelection, 
             course == input$courseSelection) %>%
      skill_filter(sheet_name = input$sheetSelection,
                   module1_list, module2_list, energy_list, 
                   wifi_list, final_list) %>% 
      mutate(student_marks = round((my_level/
                                      lvl_required)*100)) %>%
      ggplot(aes(y = student_marks, x = group)) +
        geom_boxplot() + 
        geom_jitter(aes(color = student_name),alpha = 0.35)-> p1
    
    ggplotly(p1)
    
  })
  
  # student attendance line
  output$attendanceChart <- renderPlotly({
    
    data_attend %>%
      filter(CITY == input$citySelection, 
             GROUP == input$groupSelection,
             STUDENT == input$studentSelection) %>%
      ggplot(aes(x = day)) +
        geom_line(aes(y = Yes, colour = "Yes"), alpha = 0.3) +
        geom_smooth(aes(y = Yes, colour = "Yes"), se = F) +
        geom_smooth(aes(y = Justified, colour = "Justified"), se = F) +
        geom_smooth(aes(y = Unjustified, colour = "Unjustified"), se = F) +
        scale_colour_manual("", 
                          breaks = c("Yes", "Justified", "Unjustified"),
                          values = c("Yes" = "blue", 
                                     "Justified" = "darkgoldenrod1", 
                                     "Unjustified" = "red")) -> p2
    ggplotly(p2)
    })
  
  # student attendance box
  output$attendanceServer <- renderUI({
    
    # attendance box
    data_attend %>% 
      filter(CITY == input$citySelection,
             GROUP == input$groupSelection,
             STUDENT == input$studentSelection) %>%
      summarise(sum(Yes, na.rm = T)/
                  sum(!is.na(Yes))) -> temp
    tresh_hold <- 0.80
    
    if (temp[[1]] >= tresh_hold) {
      infoBox("Attendance percentage:", 
              paste(round(temp[[1]]*100,0),"%"), 
              icon = icon("far fa-clock"), 
              fill = TRUE, color = "green")
    } else if (temp[[1]] >= 0.75) {
      infoBox("Attendance percentage:", 
              paste(round(temp[[1]]*100,0),"%"), 
              icon = icon("far fa-clock"), 
              fill = TRUE, color = "yellow")
    } else {
      infoBox("Attendance percentage:", 
              paste(round(temp[[1]]*100,0),"%"), 
              icon = icon("far fa-clock"), 
              fill = TRUE, color = "red")
    }
    
  })
  
  output$justUnattendanceServer <- renderUI({
    # justified attendance box
    data_attend %>% 
      filter(CITY == input$citySelection,
             GROUP == input$groupSelection,
             STUDENT == input$studentSelection) %>%
      summarise(sum(Justified, na.rm = T)/
                  sum(!is.na(Justified))) -> temp_unj
    
    infoBox("Justified unattendance perc:", 
            paste(round(temp_unj[[1]]*100,0),"%"), 
            icon = icon("far fa-clock"), 
            fill = F, color = "yellow")
    
  })
  
  
  output$unattendanceServer <- renderUI({
    # unjustified attendance box
    data_attend %>% 
      filter(CITY == input$citySelection,
             GROUP == input$groupSelection,
             STUDENT == input$studentSelection) %>%
      summarise(sum(Unjustified, na.rm = T)/
                  sum(!is.na(Unjustified))) -> temp_unj
    
    infoBox("Unjustified unattendance perc:", 
            paste(round(temp_unj[[1]]*100,0),"%"), 
            icon = icon("far fa-clock"), 
            fill = F, color = "red")
    
  })
  
  # student performance
  output$box2 <- renderPlot({
    data %>% 
      filter(student_name == input$studentSelection,
             skill_group == input$skillGroupSelection, 
             course == input$courseSelection) %>%
      select(skill_type, sheet_name, my_level) %>% 
      dplyr::rename(student_level = my_level) %>%
      filter(sheet_name != "begin") %>% 
      ggplot(aes(x = skill_type, y = student_level, fill = sheet_name)) +
        geom_col(position = "dodge") +
        scale_y_continuous(limits = as.integer(c(0, 10))) +
        scale_fill_brewer(palette="Paired") +
        theme(axis.text.x = element_text(angle=45, vjust = 1, 
                                         hjust=1, size = 12)) +
        xlab(label = NULL)
  })
  
  # data tables ----
  
  # group 5 BEST skills 
  
  output$groupTable2 <- renderTable({
    data %>%
      filter(sheet_name == input$sheetSelection, 
             group == input$groupSelection,
             skill_group == input$skillGroupSelection, 
             course == input$courseSelection
      ) %>%
      skill_filter(sheet_name = input$sheetSelection,
                   module1_list, module2_list, energy_list, 
                   wifi_list, final_list) %>% 
      group_by(skill_type, skill) %>% 
      summarise(Best_skills = round((median(my_level)/
                                       median(lvl_required))*100)) %>% 
      select(skill_type, skill, Best_skills) %>% 
      arrange(desc(Best_skills)) %>%
      head(10)
  })
  
  # group 5 WORST skills 
  
  output$groupTable1 <- renderTable({
    data %>%
      filter(sheet_name == input$sheetSelection, 
             group == input$groupSelection,
             skill_group == input$skillGroupSelection, 
             course == input$courseSelection
      ) %>% 
      skill_filter(sheet_name = input$sheetSelection,
                   module1_list, module2_list, energy_list, 
                   wifi_list, final_list) %>% 
      group_by(skill_type, skill) %>% 
      summarise(Worst_skills = round((median(my_level)/
                                        median(lvl_required))*100)) %>%
      select(skill_type, skill, Worst_skills) %>%
      arrange(Worst_skills) %>% 
      head(10)
  })
  
  # student data table skills
  output$table2 <- renderDataTable({
    data %>%
      filter(student_name == input$studentSelection, 
             sheet_name == input$sheetSelection,
             skill_group == input$skillGroupSelection, 
             course == input$courseSelection) %>% 
      skill_filter(sheet_name = input$sheetSelection,
                   module1_list, module2_list, energy_list, 
                   wifi_list, final_list) %>% 
      mutate(skill_performance = round(my_level/
                                         lvl_required*100),0) %>% 
      select(skill_type, skill, skill_performance) %>% 
      arrange(skill_performance)
  }, options = list(aLengthmenu = c(10,25,50),
                    iDisplayLength = 10)
  ) 
  
}


# shiny app ----

shinyApp(ui, server)

