
#' Logs an Error and Ends Program
#'
#' Extension of logging::logerror to end the program
#'
#' @param msg error message
#'
#' @export
#'

logerr <- function(msg = ""){
  "Logs an error and raises and error"
  
  logging::logerror(msg)
  stop(msg)
}
