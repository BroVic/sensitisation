#' Tabulate Questionnaire Data
#' 
#' Interactive table used for the initial display of questionnaire data.
#' 
#' @importFrom shiny runApp
#' 
#' @export
display_table <- function()
{
  runApp(tablApp)
}








#' @import shiny
#' @import RSQLite
tablApp <- shinyApp(
  ui = 
    fluidPage(
      fluidRow(
        column(12, dataTableOutput("table"))
      )
    ),
  
  server = function(input, output) {
    con <- dbConnect(SQLite(), "questionnaire.db")
    data <- dbReadTable(con, "eea")
    dbDisconnect(con)
    colnames(data) <- c("facility", "date", "lga", "state", "zone",
                        "staff.no", "respondent", "respondent.role",
                        "waste.type", "cleaning.time", "know.health.impact",
                        "toilet", "power.source", "gen.emission",
                        "gen.noise", "other.noise", "waste.sorting")
    output$table <- renderDataTable(data)
  }
)

