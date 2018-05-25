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
#' 
#' @export
show_all_barcharts <- function(file = NULL, data = NULL)
{
  if (!is.null(file)) {
    data <- read.csv(file, stringsAsFactors = FALSE)
  } else if (!is.null(data)) {
    data <- .discard_comments(data)
  } else stop("Both 'file' and 'data' were NULL.")
  gglist <- lapply(colnames(data), function(var) {
    ggplot(data, aes_string(var)) +
      geom_bar() +
      ggtitle(.createTitle(var)) +
      xlab("Response")
  })
  sapply(gglist, print)
}










## Some intermediate processing of the data frame
.prepareDataframe <- function(dat)
{
  stopifnot(inherits(dat, "data.frame"))
  
  ## Some categories should follow a certain order
  dat$How.frequently.is.cleaning.done. <-
    factor(
      dat$How.frequently.is.cleaning.done.,
      levels = c("Daily", "Twice daily"),
      ordered = TRUE
    )
  
  dat$How.often.is.waste.evacuated. <-
    factor(
      dat$How.often.is.waste.evacuated.,
      levels = c(
        "Twice a day",
        "Daily",
        "Twice a week",
        "At least twice a week",
        "not sure"
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












## Removes open-ended questions from the data frame
#' @importFrom dplyr %>%
#' @importFrom dplyr contains
#' @importFrom dplyr select
.discard_comments <- function(df) 
{
  stopifnot(inherits(df, "data.frame"))
  df %>%
    select(-contains("comments"))
}








## Creates the Shiny App's bar chart
#' @import ggplot2
drawBarChart <- function(df, var)
{
  stopifnot(is.character(var))
  tryCatch({
    gg <- ggplot(df, aes_string(var)) +
      geom_bar(aes_string(fill = var), show.legend = FALSE) +
      ggtitle(.createTitle(var)) +
      theme(
        plot.title = element_text(size = 20, face = "bold"),
        axis.title.x = element_blank(),
        axis.text.x = element_text(face = "bold")
      )
    # print(gg)
    if (interactive()) {
      print(gg)
    }
    else
      message("Object of class '%s' successfully created", class(gg))
  },
  error = "Plotting error")
}