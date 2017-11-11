#' Initiates a Logging Session
#'
#' Logs messages to command line, with option to create a log file or load configurations from a json file. 
#'
#' @param file_name name of the log file, if desired
#' @param config name of the config file, json format
#'
#' @export
#'
#' @examples
#' start_logging("log_test")
#'

start_logging <- function(file_name = NULL, config = NULL){
  logging::logReset()

  # generic result without a config file
  if( is.null(config) ){
    logging::basicConfig(level = "NOTSET")
    if( !is.null(file_name) ) logging::getLogger()$addHandler(logging::writeToFile, file = file_name, level = "NOTSET")
    
  # config file
  } else{
    d <- rjson::fromJSON(file = config)
    scriptR::dictConfig(d)
  }
}
