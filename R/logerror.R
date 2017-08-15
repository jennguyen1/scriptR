#' Logs an Error and Ends Program
#'
#' Extension of logging::logerror to end the program
#'
#' @param msg error message
#'
#' @export
#'

logerror <- function(msg) stop(logging::logerror(msg))
