# plotting.R

#' Display the categorical data using bar charts
#' data on open-ended questions are removed from the data
#' 
#' @param file A path to text file containing data in CSV format
#' @param data A R object of class \code{data.frame} containing questionnaire 
#' data
#' 
#' @return The function does not return a value but displays bar charts
#' @note This function is designed for a specific data set, not for general use.
#' 
#' @import ggplot2
#' @importFrom tools toTitleCase
#' 
#' @export
show_barchart_all <- function(file = NULL, data = NULL)
{
  if (!is.null(file)) {
    data <- read.csv(file, stringsAsFactors = FALSE)
  } else data
  data <- discard_comments(data)
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