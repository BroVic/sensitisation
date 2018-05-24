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
  dfImport <- read.csv(file, stringsAsFactors = TRUE)
  
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
                          choices = colnames(dfImport))
            )
          ),
          
          mainPanel(
            conditionalPanel("input.displayType === 'barChart'",
                             plotOutput("barChart")),
            
            
            conditionalPanel(
              "input.displayType === 'dataTable'",
              tableOutput("dataTable")
            )
          )
        )
      ),
    
    server = function(input, output, session) {
      dataInput <- reactive({
        
        ## Reorder response categories in descending order
        ## TODO: Add a control
        dfImport[[input$chart]] <-
          with(dfImport,
               dfImport[[input$chart]] <-
                 factor(dfImport[[input$chart]],
                        levels = names(sort(
                          table(dfImport[[input$chart]]), decreasing = TRUE
                        ))))
        dfImport
      })
      
      ## This code block is for displaying the bar chart
      output$barChart <- renderPlot({
        if (input$displayType == "barChart") {
          
          ## Remove responses that are only comments
          ## since they are not categorical variables
          plotDf <- .discard_comments(dataInput())
          
          ## Update the select input widget
          # observe(updateSelectInput(
          #   session,
          #   "chart",
          #   label = "Question",
          #   choices = colnames(plotDf)
          # ))
          
          ## Draw the chart
          gg <- ggplot(plotDf, aes_string(input$chart)) +
            geom_bar(aes_string(fill = input$chart), show.legend = FALSE) +
            ggtitle(.createTitle(input$chart)) +
            theme(plot.title = element_text(size = 20, face = "bold"),
                  axis.title.x = element_blank(),
                  axis.text.x = element_text(face = "bold"))
          print(gg)
        }
      })
      
      ## This code block is for displaying the data table
      output$dataTable <- renderTable({
        if (input$displayType == "dataTable") {
          tableDf <- dataInput()
          colnames(tableDf) <-
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
          tableDf
        }
      })
      
    }
  )
}