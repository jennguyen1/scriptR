#' Logs an Error and Ends Program
#'
#' Extension of logging::logerror to end the program
#'
#' @param msg error message
#'
#' @export
#'

logerr <- function(msg){
  logging::logerror(msg)
  stop(msg)
}
