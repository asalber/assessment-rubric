# Author: Alfredo Sánchez Alberca (asalber@ceu.es)

library(shiny)
library(tidyverse)
library(shinyjs)
library(shinythemes)
library(knitr)
library(kableExtra)
library(e1071)
library(DT)

# Define UI for application that draws a histogram
ui <- fluidPage(theme = shinytheme("cerulean"),
                useShinyjs(), 
                # Application title
                titlePanel("Rubric evaluation"),
                navbarPage(title = NULL,
                           # Rubric creation menu
                           tabPanel("RUBRIC",
                                    fluidRow(
                                        # Exam name
                                        column(6, textInput('examName', 'Exam name', value = paste0("Exam-", Sys.Date()))),
                                        # Load items
                                        column(6, fileInput("items.file",
                                                            "Select a CSV file with the rubric items",
                                                            multiple = F,
                                                            accept=c('text/csv', 
                                                                     'text/comma-separated-values,text/plain', 
                                                                     '.csv'),
                                                            buttonLabel = "Explore...",
                                                            width = '400px'))
                                    ),
                                    textOutput("text1"),
                                    # Items table
                                    DT::dataTableOutput("itemsTable"),
                                    # Rubric summary table
                                    tableOutput("itemsSummary"),
                                    # Download rubric items
                                    uiOutput("downloadRubricButton")
                           ),
                           tabPanel("STUDENTS",
                                    # Load students
                                    fileInput("students.file",
                                              "Select a CSV file with the students (Blackboard format).",
                                              multiple = F,
                                              accept=c('text/csv', 
                                                       'text/comma-separated-values,text/plain', 
                                                       '.csv'),
                                              buttonLabel = "Explore...",
                                              width = '500px'),
                                    
                                    # Students table
                                    DT::dataTableOutput("studentsTable"),
                                    # Download rubric items
                                    uiOutput("downloadRubricTemplateButton")
                           ),
                           
                           # Data loading menu
                           tabPanel("CORRECTION", 
                                    fileInput("assessment.file",
                                              "Select a CSV file with the rubric correction",
                                              multiple = F,
                                              accept=c('text/csv', 
                                                       'text/comma-separated-values,text/plain', 
                                                       '.csv'),
                                              buttonLabel = "Explore...",
                                              width = '400px'),
                                    # Data table
                                    dataTableOutput("dataTable")),
                           # Grades computation menu
                           tabPanel("GRADES",
                                    h2('Grades'),
                                    # Grades table
                                    dataTableOutput("gradesTable"),
                                    # Download grades button
                                    downloadButton("downloadGrades", "Download grades", class = "btn-primary"),
                           ),
                           # Statistics
                           tabPanel("STATISTICS",
                                    h2('Descriptive Statistics'),
                                    fluidRow(
                                        # Statistics
                                        column(2, tableOutput("stats")),
                                        # Histogram
                                        column(8, align="center", plotOutput('histogram', width = "800px"), offset = 0)
                                    )
                           ),
                           # Student assessment report
                           tabPanel("REPORTS", 
                                    # Download reports button
                                    downloadButton("downloadReports", "Download reports", class = "btn-primary"),
                                    flowLayout(
                                        # Select input student
                                        uiOutput("selectStudent", inline = T),  
                                        # Grade
                                        htmlOutput("grade", inline = T)),
                                    # Assessment table
                                    tableOutput("studentReport"),
                                    column(12, align="center", plotOutput('boxplot', width = "800px"), offset = 0),
                                    htmlOutput("comments")
                           )
                )
)

# Define server logic required to draw a histogram

server <- function(input, output) {
    # Encoding
    encoding.items <- reactive({
        encoding <- unlist(guess_encoding(input$items.file$datapath))[1]
        return(encoding)
    })
    
    # Load items data set
    data.items <- reactive({
        inFile <- input$items.file
        if (is.null(inFile))
            return(NULL)
        # Get the encoding of the file (specially for Windows systems)
        encoding <- encoding.items()
        if (encoding == "ISO-8859-1")
            data <- read_csv2(inFile$datapath, locale = locale(encoding = encoding))
        else 
            data <- read_csv(inFile$datapath, locale = locale(encoding = encoding))
        return(data)
    })
    
    # Show text and download button
    observeEvent(input$items.file, {
        output$text1 <- renderText('Select the items for the rubric.')
        output$downloadRubricButton <- renderUI(
            downloadButton("downloadRubric", "Download rubric", class = "btn-primary")
        )
    })
    
    # Show items table
    output$itemsTable <- renderDataTable(data.items(), 
                                         options = list(
                                             language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json'),
                                             pageLength = 10, 
                                             autoWidth = FALSE))
    
    selected.items <- eventReactive(input$itemsTable_rows_selected, {
        data.items()[input$itemsTable_rows_selected, ]
    })
    
    # Items summary
    output$itemsSummary <- renderTable({
        if (length(input$itemsTable_rows_selected)){
            selected.items() %>% 
                group_by(Chapter) %>%
                summarise(`Cumulative Weight`= sum(Weight))
        }
    })
    
    # Download rubric
    output$downloadRubric <- downloadHandler(
        filename = paste0(input$examName, "-rubric.csv"),
        content = function(file) {
            rubric <- selected.items() %>%
                pivot_wider(id_cols=c(), names_from = Item, values_from = Weight) %>%
                add_column(`Last Name` = "WEIGHT", `First Name` = "", Username = "", Taken = "") %>%
                relocate(`Last Name`, `First Name`, Username, Taken)
            encoding <- encoding.items()
            if (encoding == "ISO-8859-1")
                write_csv2(rubric, file)
            else
                write_csv(rubric, file)
        }
    )
    
    # Load items data set
    data.students <- reactive({
        inFile <- input$students.file
        if (is.null(inFile))
            return(NULL)
        data <- read_csv(inFile$datapath)
        return(data %>% select(c(`Last Name`, `First Name`, Username)))
    })
    
    # Show students table
    output$studentsTable <-  DT::renderDataTable(data.students(),
                                                 options = list(
                                                     language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json'),
                                                     pageLength = 15, 
                                                     autoWidth = FALSE))
    
    # Show download button
    observeEvent(input$items.file, {
        output$downloadRubricTemplateButton <- renderUI(
            downloadButton("downloadRubricTemplate", "Download rubric template", class = "btn-primary")
        )
    })
    
    # Download rubric
    output$downloadRubricTemplate <- downloadHandler(
        filename = paste0(input$examName, "-template.csv"),
        content = function(file) {
            rubric <- selected.items() %>% 
              mutate(Weight = as.character(Weight)) %>%
              pivot_wider(id_cols=c(), names_from = Item, values_from = Weight) %>%
              add_column(Comments = "", `Last Name` = "WEIGHT", `First Name` = "", Username = "", Taken = "") %>%
              relocate(`Last Name`, `First Name`, Username, Taken) %>%
              # Add students
              add_row( data.students()) %>%
              mutate(across(everything(), ~replace_na(., "")))
            encoding <- encoding.items()
            if (encoding == "ISO-8859-1")
                write_csv2(rubric, file)
            else
                write_csv(rubric, file)
        }
    )
    
    # Encoding assessment
    encoding.assessment <- reactive({
        encoding <- unlist(guess_encoding(input$assessment.file$datapath))[1]
        return(encoding)
    })
    
    # Load correction data set
    data.assessment <- reactive({
        inFile <- input$assessment.file
        if (is.null(inFile))
            return(NULL)
        encoding <- encoding.assessment()
        if (encoding == "ISO-8859-1")
            data <- read_csv2(inFile$datapath, locale = locale(encoding = encoding))
        else 
            data <- read_csv(inFile$datapath, locale = locale(encoding = encoding))
        # Convert the comments column to character (if not when there are no comments the column is loaded as logical)
        data <- data %>% mutate(Comments = as.character(Comments))
        # Extract the weights of the questions
        weights <- data %>%
            filter(`Last Name` == "WEIGHT") %>%
            select(-c(`First Name`, `Last Name`, Username, Taken, Comments)) %>%
            pivot_longer(cols = everything(), names_to = "Item", values_to = "Weight")
        # Pivot data table
        data <- data %>%
            filter(`Last Name` != "WEIGHT") %>%
            pivot_longer(cols = -c(`First Name`, `Last Name`, Username, Taken, Comments), names_to = "Item", values_to = "Assessment") %>%
            # Combine with weights
            left_join(weights, by = "Item") 
        return(data)
    })
    
    
    # Show loaded data
    output$dataTable <- DT::renderDataTable(data.assessment(), 
                                            options = list(language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json'),
                                                           pageLength = 15,
                                                           autoWidth = FALSE))
    
    # Compute grades
    grades <- reactive({
        req(input$assessment.file)
        data <- data.assessment()
        # Compute the grades
        grades <- data %>%
            # Replace NAs by 0
            replace_na(list(Assessment = 0)) %>%
            # Add a new columns with the Score
            mutate(Score = if_else(Taken == "Y", Weight * Assessment, NaN)) %>%
            # Group by student
            group_by(`Last Name`, `First Name`, Username) %>%
            # Compute the grade
            summarise(Grade = round(sum(Score) / sum(Weight) * 10, 1))
        return(grades)
    })
    
    # Show grades
    output$gradesTable <- DT::renderDataTable(grades(), 
                                              options = list(language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json'),
                                                             pageLength = 15, 
                                                             autoWidth = FALSE))
    
    # Downdoad grades
    output$downloadGrades <- downloadHandler(
        filename = paste0(input$examName, "-grades.csv"),
        content = function(file) {
            grade <- grades() %>% replace_na(list(Grade = ''))
            write_csv(grade, file)
        }
    )
    
    # Show grades histogram
    output$histogram <- renderPlot({
        ggplot(grades(), aes(x=Grade)) + 
            geom_histogram(breaks=seq(0, 10, by = 1), fill=rgb(5, 161, 230, maxColorValue = 255), color='white') +
            labs(title=paste( input$examName, "grades distribution")) +
            labs(y="Students") + 
            scale_x_continuous(breaks=seq(0, 10, 1)) +
            theme(title = element_text(size=18),
                  axis.title=element_text(size=16),
                  axis.text = element_text(size=14))
    })
    
    # Show statistics
    output$stats <- renderTable(
        as_tibble(grades()) %>% 
            filter(!is.na(Grade)) %>% 
            summarise(n = n(), 
                      Min = min(Grade), 
                      Max=max(Grade), 
                      Mean = mean(Grade), 
                      Std.Dev=sd(Grade), 
                      Q1 = quantile(Grade, 0.25), 
                      Q2 = quantile(Grade, 0.5), 
                      Q3 = quantile(Grade, 0.75), 
                      Skewness = skewness(Grade), 
                      Kurtosis = kurtosis(Grade)) %>%
            pivot_longer(cols = everything(), names_to = "Statistics", values_to = "Value")
    )
    
    # Show select input for students
    output$selectStudent <- renderUI(
        selectInput("student","Select student", choices=
                        as.character(unique(unlist(data.assessment() %>% filter(Taken == "Y") %>% pull(`Last Name`)))))
    )
    
    # Get selected student data
    data.student <- eventReactive(input$student, {
        data <- data.assessment()
        data.student <- data %>% filter(`Last Name` == input$student) %>%
            # Convert Achieved to numeric
            mutate(Assessment = as.numeric(Assessment)) %>%
            # Replace NAs by 0
            replace_na(list(Assessment = 0)) %>%
            # Add a new columns with the Score
            mutate(Score = Weight * Assessment) %>%
            # Add a new column with Achivement
            mutate(Achieved = recode(Assessment, "0" = "No", "0.5" = "Partially", "1.0" = "Yes"))
        return(data.student)
    })
    
    # Show grade
    output$grade <- renderUI({
        req(input$student)
        grades <- grades()
        grade <- grades %>% filter(`Last Name` == input$student) %>% pull(Grade)
        h3(paste("Grade: ", round(grade, 1)))
    })
    
    # Show student Score
    output$studentReport <- function() {
        req(input$student)
        data.student <- data.student()
        data.student %>%
            mutate(Achieved = cell_spec(Achieved, "html", color = if_else(Achieved == "Yes", "green", if_else(Achieved == "Partially", "orange", "red")))) %>%
            select(Item, Weight, Achieved, Score) %>%
            kable(format = "html", escape = F, align = c("l", "c", "c", "c")) %>%
            kable_styling(bootstrap_options = c("striped"), full_width = F)
    }
    
    # Box plot
    output$boxplot <- renderPlot({
        req(input$student)
        grades <- grades()
        grade <- grades %>% filter(`Last Name` == input$student) %>% pull(Grade)
        boxplot(grades$Grade, horizontal = T, col = rgb(5, 161, 230, maxColorValue = 255), main = "Grades distribution", yaxt="n", ylim = c(0,10))
        axis(1, at = 0:10)
        text(x = grade, y = 1.1, labels = "You")
        points(grade, 1, col = "red", pch = 19)
    })
    
    # Show comments
    output$comments <- renderUI({
        req(input$student)
        data.student <- data.student()
        if (!is.na(data.student$Comments[1])) tagList(h3("Comments"), p(data.student$Comments[1]))
    })
    
    # Generate reports
    output$downloadReports <- downloadHandler(
        # For PDF output, change this to "report.pdf"
        filename = "assessments.zip",
        content = function(file) {
            # Copy the report file to a temporary directory before processing it, in
            # case we don't have write permissions to the current working dir (which
            # can happen when deployed).
            tmpdir <- tempdir()
            tempReport <- file.path(tmpdir, "assessment.Rmd")
            file.copy("assessment.Rmd", tempReport, overwrite = TRUE)
            data <- data.assessment()
            files <- c()
            setwd(tmpdir)
            withProgress(message = 'Generating assessment reports', value = 0, {
                n <- length(unique(data$Username))
                for (student in unique(data$Username)){
                    data.student <- data %>% 
                        filter(Username == student)  %>%
                        # Convert Achieved to numeric
                        mutate(Assessment = as.numeric(Assessment)) %>%
                        # Replace NAs by 0
                        replace_na(list(Assessment = 0)) %>%
                        # Add a new columns with the Score
                        mutate(Score = Weight * Assessment) %>%
                        # Add a new column with Achivement
                        mutate(Achieved = recode(Assessment, "0" = "No", "0.5" = "Partially", "1.0" = "Yes"))
                    if (data.student$Taken[1] == "Y"){
                        student.name <- paste0(data.student$`Last Name`[1], ', ', data.student$`First Name`[1])
                        output.file <-  paste0("assessment ", student.name, ".html")
                        files <- c(files, paste0(output.file))
                        params <- list(title = input$examName, data.student = data.student, grades = grades())
                        rmarkdown::render(tempReport,
                                          output_file = output.file,
                                          output_dir = tmpdir,
                                          params = params, 
                                          envir = new.env(parent = globalenv()))
                    }
                    # send.assessment(i,emails[emails$Name == i,]$Username)
                    # Sys.sleep(10)
                    # Increment the progress bar, and update the detail text.
                    incProgress(1/n, detail = student.name)
                }
            })
            zip(file, files)
        },
        contentType = "application/zip"
    )
}

# Run the application 
shinyApp(ui = ui, server = server)



