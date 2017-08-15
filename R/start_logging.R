#' Initiates a Logging Session
#'
#' Logs messages to command line, with option to create a log file.
#'
#' @param make_file should a log file be created
#' @param file_name name of the log file, if none given will save to default (R_[timestamp])
#'
#' @export
#'
#' @examples
#' start_logging(FALSE)
#' start_logging(TRUE, "test.log")
#'

start_logging <- function(make_file = FALSE, file_name){

  # initialize
  logging::logReset()

  # write to console
  logging::getLogger()$addHandler(logging::writeToConsole, level = 'DEBUG')

  # write to file
  if( make_file ){
    if(missing(file_name)) file_name <- paste0('R_', stringr::str_replace_all(Sys.time(), " ", "_"))
    logfile <- file.path(getwd(), paste0(file_name, ".log"))
    logging::getLogger()$addHandler(logging::writeToFile, file = logfile, level = 'DEBUG')
  }

}
