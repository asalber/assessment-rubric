#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
#library(knitr)
library(tidyverse)
library(shinyjs)
library(shinythemes)
#library(DT)


# Define UI for application that draws a histogram
ui <- fluidPage(theme = shinytheme("cerulean"),
    useShinyjs(), 
    # Application title
    titlePanel("Assesment from rubric"),
    navbarPage(title = "",
        tabPanel("LOAD DATA", 
                 fileInput("file",
                           "Choose CSV files from directory",
                           multiple = F,
                           accept=c('text/csv', 
                                    'text/comma-separated-values,text/plain', 
                                    '.csv')),
                 dataTableOutput("dataTable")),
        tabPanel("GRADES", 
                 fluidRow(
                     actionButton(inputId = "computeGrades", "Compute grades", icon("paper-plane")),
                     hidden(downloadButton("downloadGrades", "Download Grades", class = "btn-primary"))
                 ),
                 dataTableOutput("gradesTable"),
                 
        ),
        tabPanel("REPORTS", 
                 uiOutput("selectStudent"),
                 tableOutput("studentReport"),
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
    grades <- eventReactive(input$computeGrades, {
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
            pageLength = 20
        )
    )
    
    # Downdoad grades
    output$downloadGrades <- downloadHandler(
        filename = "grades.csv",
        content = function(file) {
            write.csv(grades(), file, row.names = F)
        }
    )
    
    # Show download button
    observeEvent(input$computeGrades, {
        show("downloadGrades")
    })
    
    # Show select input box for students
    output$selectStudent <- renderUI(
        selectInput("student","Select student", choices= 
                        as.character(unique(unlist(dataset()[["Name"]]))))
    )
    
    report <- eventReactive(input$student, {
        data <- dataset()
        data.student <- data %>% filter(Name == input$student) %>% select(Item, Weight, Assessment)
        return(data.student)
    })
    
    output$studentReport <- renderTable({
        req(input$student)
        report()
    })
        
        
    # output$distPlot <- renderPlot({
    #     # generate bins based on input$bins from ui.R
    #     x    <- faithful[, 2]
    #     bins <- seq(min(x), max(x), length.out = input$bins + 1)
    # 
    #     # draw the histogram with the specified number of bins
    #     hist(x, breaks = bins, col = 'darkgray', border = 'white')
    # })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
