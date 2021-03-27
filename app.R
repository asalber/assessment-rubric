# Author: Alfredo Sánchez Alberca (asalber@ceu.es)

library(shiny)
library(tidyverse)
library(shinyjs)
library(shinythemes)
library(knitr)
library(kableExtra)

# Define UI for application that draws a histogram
ui <- fluidPage(theme = shinytheme("cerulean"),
    useShinyjs(), 
    # Application title
    titlePanel("Assesment from rubric"),
    navbarPage(title = "",
        # Data loading menu
        tabPanel("LOAD DATA", 
                 fileInput("file",
                           "Choose CSV files from directory",
                           multiple = F,
                           accept=c('text/csv', 
                                    'text/comma-separated-values,text/plain', 
                                    '.csv')),
                 # Data table
                 dataTableOutput("dataTable")),
        # Grades computation menu
        tabPanel("GRADES", 
                 downloadButton("downloadGrades", "Download Grades", class = "btn-primary"),
                 # Grades table
                 dataTableOutput("gradesTable"),
        ),
        # Student assessment report
        tabPanel("REPORTS", 
                 flowLayout(uiOutput("selectStudent", inline = T),  htmlOutput("grade", inline = T)),
                 # Assessment table
                 tableOutput("studentReport"),
                 column(12, align="center", plotOutput('boxplot', width = "800px"), offset = 0),
                 h3("Comments"),
                 textOutput("comments")
        )
    )
)

# Define server logic required to draw a histogram

server <- function(input, output) {
    
    dataset <- reactive({
        inFile <- input$file
        if (is.null(inFile))
            return(NULL)
        data <- read.csv(inFile$datapath)
        
        # Extract the weights of the questions
        weights <- data %>% 
            filter(Name == "Weight") %>%
            select(-c(Name, Email, Taken, Comments)) %>%
            pivot_longer(cols = everything(), names_to = "Item", values_to = "Weight")
        # Pivot data table
        data <- data %>% 
            filter(Name != "Weight") %>% 
            pivot_longer(cols = -c(Name, Email, Taken, Comments), names_to = "Item", values_to = "Assessment") %>%
            # Combine with weights
            left_join(weights, by = "Item")
    })
    
    # Compute grades
    grades <- reactive({
        req(input$file)
        data <- dataset()
        # Compute the grades
        grades <- data %>%
            # Replace NAs by 0
            replace_na(list(Assessment = 0)) %>%
            # Add a new columns with the score
            mutate(Score = if_else(Taken == "Y", Weight * Assessment, NaN)) %>%
            # Group by student
            group_by(Name) %>%
            # Compute the grade
            summarise(Grade = round(sum(Score) / sum(Weight) * 10,1))
        return(grades)
    })
    
    # Show loaded data
    output$dataTable <- renderDataTable({
        dataset()
    })
    
    # Compute grades
    output$gradesTable <- renderDataTable(
        grades(), 
        options = list(
            pageLength = 20, autoWidth = TRUE
        )
    )
    
    # Downdoad grades
    output$downloadGrades <- downloadHandler(
        filename = "grades.csv",
        content = function(file) {
            write.csv(grades(), file, row.names = F)
        }
    )
   
    output$selectStudent <- renderUI(
        selectInput("student","Select student", choices= 
                        as.character(unique(unlist(dataset()[["Name"]]))))
    )
    
    data.student <- eventReactive(input$student, {
        data <- dataset()
        data.student <- data %>% filter(Name == input$student) %>%
            # Convert Achieved to numeric
            mutate(Assessment = as.numeric(Assessment)) %>%
            # Replace NAs by 0
            replace_na(list(Assessment = 0)) %>%
            # Add a new columns with the score
            mutate(Score = Weight * Assessment) %>%
            # Add a new column with Achivement
            mutate(Achieved = recode(Assessment, "0" = "No", "0.5" = "Partially", "1.0" = "Yes"))
        return(data.student) 
    })
    
    output$grade <- renderUI({
        req(input$student)
        grades <- grades()
        grade <- grades %>% filter(Name == input$student) %>% pull(Grade)
        h3(paste("Grade: ", round(grade, 1)))
    })
    
    output$studentReport <- function() {
        req(input$student)
        data.student <- data.student()
        data.student %>% 
            mutate(Achieved = cell_spec(Achieved, "html", color = if_else(Achieved == "Yes", "green", if_else(Achieved == "Partially", "orange", "red")))) %>%
            select(Item, Weight, Achieved, Score) %>%
            kable(format = "html", escape = F, align = c("l", "c", "c", "c")) %>% 
            kable_styling(bootstrap_options = c("striped"), full_width = F)
    }
    
    output$boxplot <- renderPlot({
        req(input$student)
        grades <- grades()
        grade <- grades %>% filter(Name == input$student) %>% pull(Grade)
        boxplot(grades$Grade, horizontal = T, col = rgb(5, 161, 230, maxColorValue = 255), main = "Distribution of scores")
        text(x = grade, y = 1.1, labels = "You")
        points(grade, 1, col = "red", pch = 19)
    })
    
    output$comments <- renderText({
        req(input$student)
        grades <- grades()
        grade <- grades %>% filter(Name == input$student) %>% pull(Grade)
        data.student <- data.student()
        paste(data.student$Comments[1], if_else(grade<4, "You did not pass, but do not be discouraged because you can retake this exam in the ordinary examination. If you need some help, do not hesitate to ask for a tutorial.", ifelse(grade<5, "You do not need to retake this exam, but it is recommended that you take it again on the ordinary examination to improve your score.", "Congratulations! You pass.")))
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
