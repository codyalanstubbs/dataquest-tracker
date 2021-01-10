library(shiny)
library(dplyr)
library(lubridate)
# Define basic save and load data functions
library(googlesheets4)
library(googledrive)

table <- "https://docs.google.com/spreadsheets/d/1B-VWdlKnxVkN4I53JuO9uHJvOxyGckTz-iGMvhjzFNI/edit?usp=sharing"

#drive_auth(cache = ".secrets") #for the first time running the app in R to get the OAuth token
gs4_auth(cache = ".secrets", email = TRUE, use_oob = TRUE)

saveData <- function(data) {
    # Add the data as a new row
    sheet_append(table, data)
}

loadData <- function() {
    # Read the table
    read_sheet(table)
}

# Shiny app with 3 fields that the user can submit data for
shinyApp(
    ui = fluidPage(
        textInput("date", "Date (e.g.: 2020-12-09)", ""),
        textInput("path", "Career Path", ""),
        textInput("step", "Step Number", ""),
        textInput("course", "Course Number", ""),
        textInput("mission", "Mission Name", ""),
        textInput("duration", "Duration (Minutes)", ""),
        textInput("notes", "Notes:", ""),
        actionButton("submit", "Submit"),
        DT::dataTableOutput("responses", width = 300), tags$hr()
        # ,
        # plotOutput("plot", width = "400px")
    ),
    server = function(input, output, session) {
        
        # Whenever a field is filled, aggregate all form data
        formData <- reactive({
            data <- tibble(data.entry.date = as_date(Sys.Date()),
                           date = input$date %>% as_date(),
                           path = input$path,
                           step = input$step,
                           course = input$course,
                           mission = input$mission,
                           duration = input$duration,
                           notes = input$notes)
            
        })
        
        # When the Submit button is clicked, save the form data
        observeEvent(input$submit, {
            saveData(formData())
        })
        
        # Show the previous responses
        # (update with current response when Submit is clicked)
        output$responses <- DT::renderDataTable({
            input$submit
            loadData()
        })     
        
        # output$plot <- renderPlot(plot(x = formData$item, y = formData$weight), res = 100)
    }
)