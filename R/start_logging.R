#' Initiates a Logging Session
#'
#' Logs messages to command line, load configurations from a json file. 
#'
#' If config file not specified, will initiate a basic session from the default config file.
#'
#' @param config name of the config file, json format
#'
#' @export
#'
#' @examples
#' start_logging("log_config.json")
#'

start_logging <- function(config = NULL){
  logging::logReset()

  if( is.null(config) ){
    d <- scriptR::log_config 
  } else{
    assertthat::assert_that(stringr::str_detect(config, "json$"), msg = "Please use json file format for configurations")
    d <- rjson::fromJSON(file = config)
  }
  scriptR::dictConfig(d)
}
