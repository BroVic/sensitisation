# read.R

## Functions for reading data

#' @importFrom RSQLite dbConnect
#' @importFrom RSQLite dbDisconnect
#' @importFrom RSQLite dbReadTable
#' @importFrom RSQLite SQLite
#' @importFrom utils read.csv 
readData <- function(file)
{
  if (!grepl("\\.(csv$|db$|sqlite$)", file, ignore.case = TRUE))
    stop("Expected a CSV or SQLite file.")
  if (endsWith(tolower(file), '.csv')) {
    dat <- read.csv(file, stringsAsFactors = TRUE)
    dat <- .prepareDataframe(dat)
  }
  else {
    dbCon <- dbConnect(SQLite(), file)
    dat <- dbReadTable(dbCon, 'ques_input')
    on.exit(dbDisconnect(dbCon))
  }
  invisible(dat)
}