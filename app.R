# app.R
# A Shiny application for interactive exploration of the Questionnaire Data

## Load dependencies
library(shiny)
library(ggplot2)
source("plotting.R")

## Interactively choose the data file
file <- choose.files(caption = "Select data", multi = FALSE)
if (!endsWith(file, ".csv"))
  stop("Only files with '.csv' extension are supported by this application.")
ques <- read.csv(file, stringsAsFactors = FALSE)
ques <- discard_comments(ques)

# Define UI for application that draws the charts
ui <-
  fluidPage(
    
    titlePanel("Responses to Questionnaire"),
    
    sidebarLayout(
      
      sidebarPanel(
        selectInput("chart",
                    label = "Question",
                    choices = colnames(ques))
        ),
      
      mainPanel(
        plotOutput("barChart")
      )
    )
  )

## Define the server that returns data to the user
server <- function(input, output) {
   
   output$barChart <- renderPlot({
     gg <- ggplot(ques, aes_string(input$chart)) +
       geom_bar()
     gg
   })
}

## Run the application in the browser
shinyApp(ui = ui, server = server)

