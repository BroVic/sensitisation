# shiny.R

# Code for the Shiny app proper

#' @import shiny
chartApp <- function(dfImport)
{
  stopifnot(inherits(dfImport, "data.frame"))
  
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
            
            
            conditionalPanel("input.displayType === 'dataTable'",
                             dataTableOutput("dataTable"))
          )
        )
      ),
    
    server = function(input, output, session) {
      dataInput <- reactive({
        ## Response categories in descending order
        ## TODO: Add a control
        if (!is.ordered(dfImport[[input$chart]])) {
          dfImport[[input$chart]] <-
            with(dfImport,
                 dfImport[[input$chart]] <-
                   factor(dfImport[[input$chart]],
                          levels = names(sort(
                            table(dfImport[[input$chart]]), decreasing = TRUE
                          ))))
        }
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
          
          drawBarChart(plotDf, input$chart)
        }
      })
      
      output$dataTable <- renderDataTable({
        if (input$displayType == "dataTable") {
          dTable <- dataInput()
          structure(dTable, names = colnames(dTable))
        }
      })
      
    }
  )
}