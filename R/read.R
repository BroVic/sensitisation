# read.R

globalVariables('VarAttr')

## Functions for reading data
#' @import hellno
#' @importFrom utils read.csv 
readData <- function(file)
{
  if (length(file) > 1L) {
    file <- file[1]
    warning('Expect character vector of length == 1L; other elements ignored')
  }
  ext <-
    substr(file, regexpr('\\.[[:alpha:]]+$', file) + 1, nchar(file))
  invisible(switch(
    ext,
    csv = read.csv(file),
    rds = readRDS(file),
    db = readSQLite(file),
    stop(sQuote(ext), 'is an unsupported file format')
  ))
}









## Reads from the SQLite file
#' @importFrom RSQLite dbConnect
#' @importFrom RSQLite dbDisconnect
#' @importFrom RSQLite dbReadTable
#' @importFrom RSQLite SQLite
readSQLite <- function(f, tbl = 'ques_input')
{
  if (!grepl('\\.db$|.\\.sqlite$', f))
    stop('Expected an SQLite database file as input')
  dbCon <- dbConnect(SQLite(), f)
  on.exit(dbDisconnect(dbCon))
  try(dbReadTable(dbCon, tbl))
}














## Make sure that factors are set where appropriate
## @param df A data frame
## @attributeList A list of variable attributes encapsulated in the VarAttr
## object created with the 'DataEntry' package
enforceFactors <- function(df, attributeList)
{
  if (!inherits(df, 'data.frame'))
    stop(sQuote(df), 'is not a data frame')
  stopifnot(is.character(attributeList))
  if (file.exists(attributeList) & endsWith(attributeList, '.dte')) {
    if (!exists("VarAttr"))
      load(attributeList)
  }
  else
    stop('The file', sQuote(basename(attributeList)), 'was not found')
  
  if (!identical(colnames(df), names(VarAttr)))
    stop('There is a mismatch between the 2 data frames')
  
  for (i in seq_along(VarAttr)) {
    if (identical(VarAttr[[i]]$class, 'factor'))
      df[[i]] <- as.factor(df[[i]])
  }
  invisible(df)
}