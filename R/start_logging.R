#' Initiates a Logging Session
#'
#' Logs messages to command line, with option to create a log file.
#'
#' @param file_name name of the log file, if desired
#' @param log_level logging level, see logging::loglevels for available options
#'
#' @export
#'
#' @examples
#' start_logging("log_test")
#'

start_logging <- function(file_name = NULL, log_level = 'DEBUG'){

  # initialize
  logging::logReset()

  # write to console
  logging::basicConfig(level = log_level)

  # write to file
  if( !is.null(file_name) ) logging::getLogger()$addHandler(logging::writeToFile, file = file_name, level = log_level)

}
