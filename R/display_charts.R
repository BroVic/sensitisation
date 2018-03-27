#' Display Charts Interactively
#' 
#' A Shiny application for interactive exploration of the Questionnaire Data
#' 
#' @importFrom shiny runApp
#' @importFrom utils read.csv
display_charts <- function()
{
  ## Interactively choose the data file
  fileTypes <- matrix(c('Comma Separated Values (*.csv)', '*.csv'),
                      ncol = 2L, dimnames = list("csv"))
  file <- choose.files(caption = "Select data", multi = FALSE,
                       filters = fileTypes)
  ques <- read.csv(file, stringsAsFactors = FALSE)
  ques <- discard_comments(ques)
  runApp(chartApp(ques))
}







#' @import ggplot2
#' @import shiny
chartApp <- function(data)
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
              choices = c("dataTable", "barChart"),
              selected = NULL
            ),
            
            conditionalPanel(
              "input.displayType === 'barChart'",
              selectInput("chart",
                          label = "Question",
                          choices = colnames(data))
            )
          ),
          mainPanel(plotOutput("barChart"))
        )
      ),
    
    server = function(input, output) {
      
      if (identical(input$displayType, "barChart")) {
        output$barChart <- renderPlot({
          gg <- ggplot(data, aes_string(input$chart)) +
            geom_bar()
          gg
        })
      }
      else if (identical(input$displayType, "dataTable")) {
        colnames(data) <- c("facility", "date", "lga", "state", "zone",
                            "staff.no", "respondent", "respondent.role",
                            "waste.type", "cleaning.time",
                            "know.health.impact", "toilet", "power.source",
                            "gen.emission", "gen.noise", "other.noise",
                            "waste.sorting")
        output$table <- renderDataTable(data)
      }
    }
  )
}