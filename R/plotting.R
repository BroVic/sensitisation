# plotting.R

#' Display the categorical data using bar charts
#' 
#' @param file A path to text file containing data in CSV format
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
#' @importFrom tools toTitleCase
#' 
#' @export
show_all_barcharts <- function(file = NULL, data = NULL)
{
  if (!is.null(file)) {
    data <- read.csv(file, stringsAsFactors = FALSE)
  } else if (!is.null(data)) {
    data <- discard_comments(data)
  } else stop("Both 'file' and 'data' were NULL.")
  gglist <- lapply(colnames(data), function(var) {
    tit <- paste(strsplit(var, split = "\\.")[[1]], collapse = " ")
    ggplot(data, aes_string(var)) +
      geom_bar() +
      ggtitle(toTitleCase(tit)) +
      xlab("Response")
  })
  sapply(gglist, print)
}


## Removes open-ended questions from the data frame
#' @importFrom dplyr %>%
#' @importFrom dplyr contains
#' @importFrom dplyr select
discard_comments <- function(df) 
{
  stopifnot(inherits(df, "data.frame"))
  df %>%
    select(-contains("comments"))
}