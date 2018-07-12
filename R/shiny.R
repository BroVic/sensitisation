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
                          choices = colnames(dfImport)),
              
              checkboxInput('sortOpt',
                            label = 'Sort in descending order',
                            value = FALSE)
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
      dataInput <- reactive(dfImport)
      
      ## Update the select input widget
      observe({
        if (input$displayType == "barChart") {
          
          ## Remove responses that are only comments
          ## since they are not categorical variables
          sansOpenEnded <- keepOnlyFactors(dataInput())
          updateSelectInput(session,
                            "chart",
                            label = "Question",
                            choices = sansOpenEnded)
        }
      })
      
      ## The bar chart
      output$barChart <-
        renderPlot(drawBarChart(dataInput(), input$chart, sorted = input$sortOpt))
      
      ## The data table
      output$dataTable <- renderDataTable({
        dTable <- dataInput()
        structure(dTable, names = colnames(dTable))
      })
      
    }
  )
}