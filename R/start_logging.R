#' Initiates a Logging Session
#'
#' Logs messages to command line, with option to create a log file.
#'
#' @param file_name basename of the log file, if desired
#'
#' @export
#'
#' @examples
#' start_logging("log_test")
#'

start_logging <- function(file_name = NULL){

  # initialize
  logging::logReset()

  # write to console
  logging::getLogger()$addHandler(logging::writeToConsole, level = 'DEBUG')

  # write to file
  if( !is.null(file_name) ){
    logfile <- paste0(file_name, ".log")
    logging::getLogger()$addHandler(logging::writeToFile, file = logfile, level = 'DEBUG')
  }

}
