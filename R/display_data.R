#' Display Data Interactively
#' 
#' A \emph{Shiny} application for the interactive display and exploration of
#' the questionnaire data.
#' 
#' @note This function is a wrapper for \code{\link[shiny]{runApp}}. Also, the
#' function works only with a dataset that has a specific set of variables.
#' 
#' @param filename A comma-separated values (CSV) file containing the data.
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
  data <- read.csv(file, stringsAsFactors = FALSE)
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
          gg <- ggplot(dataInput()) +
            aes_string(input$chart) +
            geom_bar(aes_string(fill = input$chart)) +
            theme(axis.text.x = element_blank())
          print(gg)
        }
      })
      
      output$dataTable <- renderTable({
        if (input$displayType == "dataTable") {
          df <- dataInput()
          colnames(df) <-
            c(
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
          df
        }
      })
    }
  )
}