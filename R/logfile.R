#' Logfile Information
#'
#' Obtain information regarding the logfile
#'
#' @details
#'
#' exists_logfile() checks whether the logfile exists
#' get_logfile() obtains the name of the logfile
#'
#' @name logfile
NULL

#' @rdname logfile
#' @export
exists_logfile <- function() !is.null(logging::getLogger()$handlers$writeToFile)

#' @rdname logfile
#' @export
get_logfile <- function() logging::getLogger()$handlers$writeToFile$file
