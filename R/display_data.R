#' Display Charts Interactively
#' 
#' A Shiny application for interactive exploration of the Questionnaire Data
#' 
#' @note This function is a wrapper for \code{\link[shiny]{runApp}} and also
#' provides for interactive selection of the CSV file.
#' 
#' @param filename A CSV file containing the data
#' 
#' @importFrom shiny runApp
#' 
#' @export
display_data <- function(filename)
{
  if (!endsWith(tolower(filename), ".csv"))
      stop("Expected a '.csv' file.")
  runApp(chartApp(file = filename))
}







#' @import ggplot2
#' @import shiny
#' @importFrom utils read.csv
chartApp <- function(file)
{
  varNames <- c(
    "facility",
    "date",
    "lga",
    "state",
    "zone",
    "staff.no",
    "respondent",
    "respondent.role",
    "waste.type",
    "cleaning.time",
    "know.health.impact",
    "toilet",
    "power.source",
    "gen.emission",
    "gen.noise",
    "other.noise",
    "waste.sorting"
  )
  data <- read.csv(file, stringsAsFactors = FALSE)
  if (!identical(colnames(data), varNames))
    stop(paste("The variables in",
               sQuote(file),
               "do not match the expected structure."))
  shinyApp(
    ui =
      fluidPage(
        titlePanel("Responses to Questionnaire"),
        
        sidebarLayout(
          sidebarPanel(
            
            radioButtons(
              "displayType",
              label = "Type of Display",
              choiceValues = list("dataTable", "barChart"),
              choiceNames = list("Data Table", 'Bar Chart'),
              selected = NULL
            ),
            
            conditionalPanel(
              "input.displayType === 'barChart'",
              selectInput("chart",
                          label = "Question",
                          choices = colnames(data))
            )
          ),
          mainPanel(
            conditionalPanel(
              "input.displayType === 'barChart'",
              plotOutput("barChart")),
            
            conditionalPanel(
              "input.displayType === 'dataTable'",
              tableOutput("dataTable"))
            )
        )
      ),
    
    server = function(input, output) {
      
      dataInput <- reactive({
        
        if (input$displayType == "barChart") {
          data <- discard_comments(data)
        }
        
        data
      })
      
      output$barChart <- renderPlot({
        if (input$displayType == "barChart") {
          df <- dataInput()
          gg <- ggplot(df, aes_string(input$chart)) +
            geom_bar(aes_string(fill = input$chart))
          print(gg)
        }
      })
      
      output$dataTable <- renderTable({
        if (input$displayType == "dataTable") {
          df <- dataInput()
          colnames(df) <- varNames
          df
        }
      })
    }
  )
}