# display-data.R

# Table used for the initial display of questionnaire data
library(shiny)
library(RSQLite)

# Load the data from the database table "eea"
con <- dbConnect(SQLite(), "questionnaire.db")
data <- dbReadTable(con, "eea")
dbDisconnect(con)

vars <- c("facility",
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
          "waste.sorting")
colnames(data) <- vars

saveRDS(data, "dataframe1.rds")

## Shiny application for rendering
## interactive data table
if (interactive()) {
  shinyApp(
    ui = fluidPage(fluidRow(column(
      12, dataTableOutput("table")
    ))),
    
    server = function(input, output) {
      output$table <- renderDataTable(data)
    }
  )
}

