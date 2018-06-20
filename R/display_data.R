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
#' @importFrom utils read.csv
#' 
#' @export
display_data <- function(filename)
{
  if (!endsWith(tolower(filename), ".csv"))
      stop("Expected a '.csv' file.")
  dat <- read.csv(filename, stringsAsFactors = TRUE)
  dat <- .prepareDataframe(dat)
  
  ## Decide on the browser 
  if (interactive()) {
    brows.typ <- getOption("shiny.launch.browser", interactive())
  }
  else {
    brows.typ <- !interactive()
  }
  
  runApp(chartApp(dat), launch.browser = brows.typ)
}








#' @import shiny
chartApp <- function(dfImport)
{
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
              dataTableOutput("dataTable")
            )
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
      
      ## This code block is for displaying the data table
      output$dataTable <- renderDataTable({
        if (input$displayType == "dataTable") {
          structure(
            dataInput(),
            names =
              c(
                "who.cleans",
                "when.cleans",
                "freq.clean",
                "how.dispose",
                "who.evacuates",
                "freq.evacuate",
                "know.effects.dirt",
                "what.effect.dirt",
                "have.toilet",
                "use.toilet",
                "why.no.use.toilet",
                "toilet.owner",
                "toilet.manager",
                "toilet.payment",
                "where.eat",
                "know.effects.work",
                "what.effect.work",
                "first.visit",
                "changes.postvisit",
                "visit.useful",
                "visit.comments",
                "waste.receptable",
                "receptacle.source",
                "waste.bin",
                "officer.comments"
              )
          )
        }
      })
      
    }
  )
}