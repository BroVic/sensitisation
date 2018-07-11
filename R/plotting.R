# plotting.R

#' Display the categorical data using bar charts
#' 
#' @param file A path to text file containing data in CSV or SQLite format
#' @param data A R object of class \code{data.frame} containing questionnaire 
#' data
#' 
#' @details Either one of \code{file} or \code{data} should be used. if both are
#' provided, then \code{file} is used.
#' 
#' @return The function does not return a value but displays bar charts
#' 
#' @note This function is designed for a specific data set, not for general use.
#' Responses to open-ended questions are filtered out from the analysis.
#' 
#' @import ggplot2
#' 
#' @export
show_all_barcharts <- function(file = NULL, data = NULL)
{
  if (!is.null(file)) {
    stopifnot(is.character(file))
    data <- readData(file)
  }
  else if (!is.null(data)) {
    if (inherits(data, "data.frame")) {
      remColnames <- discardComments(data)
      data <- data[, remColnames]
    }
    else
      stop("'data' is not an object of class 'data.frame'")
  }
  else
    stop("Both 'file' and 'data' were NULL")
  
  lapply(colnames(data), function(x) drawBarChart(data, x))
}










## Some intermediate processing of the data frame
.prepareDataframe <- function(dat)
{
  stopifnot(inherits(dat, "data.frame"))
  
  ## Some categories should follow a certain order
  dat$when.cleans <-
    factor(
      dat$when,
      levels = c("Morning Only", "Afternoon", "Morning and Evening"),
      ordered = TRUE
    )
  
  dat$freq.evacuates <-
    factor(
      dat$freq.evacuates,
      levels = c(
        "Twice a day",
        "Daily",
        "At least twice a week",
        "Not sure"
      ),
      ordered = TRUE
    )
  dat
}










## Drafts the title from variable name
#' @importFrom tools toTitleCase
.createTitle <- function(name)
{
  paste0(toTitleCase(paste(
    strsplit(name, split = "\\.")[[1]], collapse = " "
  )), "?")
}












## Removes open-ended questions from column names
## This will be used to update the selectInput widget
## so that these questions are not presented as input
#' @importFrom dplyr %>%
#' @importFrom dplyr contains
#' @importFrom dplyr select
discardComments <- function(df) 
{
  if (!inherits(df, "data.frame"))
    stop('\'df\' must be an object of class \'data.frame\'')
  openEndedCols <- sapply(df, FUN = inherits, 'character')
  colnames(df)[!openEndedCols]    # Note negation
}








## Creates the Shiny App's bar chart
## @param df A data frame containing the questionnaire data
## @param var A column from \code{df} for which a plot is generated 
#' @import ggplot2
#' @importFrom tidyr drop_na
drawBarChart <- function(df, var, sorted = FALSE)
{
  stopifnot(is.character(var))
    
  ## Change the order of categories
  if (sorted) 
    df[[var]] <- setBarCategoryOrder(df, var)
  
  tryCatch({
    gg <- df %>% 
      drop_na(var) %>% 
      ggplot(aes_string(var)) +
      geom_bar(aes_string(fill = var), show.legend = FALSE) +
      ggtitle(.createTitle(var)) +
      theme(plot.title = element_text(size = 20, face = "bold"),
            axis.title.x = element_blank(),
            axis.text.x = element_text(face = "bold")
      )
    print(gg)
  },
  error = function(c) {
    c$message <- "Open-ended questions are not plotted"
    stop(c)
  },
  finally = print("Plot was not generated for this question"))
}





## Sets the order of the categories for display
## @param x A data frame
## @param var A character column in 'x'
#' @importFrom dplyr %>%
#' @importFrom dplyr select
#' @importFrom forcats as_factor
#' @importFrom forcats fct_infreq
setBarCategoryOrder <- function(x, col)
{
  stopifnot(is.data.frame(x))
  stopifnot(is.character(col))
  
  x %>% 
    select(col) %>% 
    unlist(use.names = FALSE) %>% 
    as_factor() %>% 
    fct_infreq()
}
