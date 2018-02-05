
#' Initiates a Logging Session
#'
#' Logs messages to command line, load configurations from a json file. 
#'
#' If config file not specified, will initiate a basic session from the default 
#' config file (saved to the list log_config).
#'
#' @param config name of the config file, json format
#'
#' @export
#'
#' @examples
#' start_logging("log_config.json")
#'

start_logging <- function(config = NULL){
  "Initiates a logging session"
  
  reset_log()

  if( is.null(config) ){
    d <- scriptR::log_config 
  } else{
    assertthat::assert_that(stringr::str_detect(config, "json$"), msg = "Please use json file format for configurations")
    d <- tryCatch({
      rjson::fromJSON(file = config)
    }, error = function(err){
      message('hi')
      stop(stringr::str_interp("${config} could not be loaded, please use json file format for configurations"))
    })
  }
  dictConfig(d)
}
