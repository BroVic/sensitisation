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
#' 
#' @export
display_data <- function(filename)
{
  stopifnot(is.character(filename))
  dat <- readData(filename)
  
  ## Decide on the browser for displaying the app
  if (interactive()) {
    brows.typ <- getOption("shiny.launch.browser", interactive())
  }
  else {
    brows.typ <- !interactive()
  }
  
  runApp(chartApp(dat), launch.browser = brows.typ)
}
