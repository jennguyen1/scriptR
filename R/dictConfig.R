
#' Takes the Logging Configuration From a List
#'
#' R adaptation of the Python logging.config.dictConfig function. 
#' See \url{https://docs.python.org/3.6/library/logging.config.html#dictionary-schema-details} 
#' for schema details.
#'
#' @param config configurations as a list
#'
#' @export
#'
#' @examples
#' # from an external json file
#' config <- rjson::fromJSON(file = "log_config.json")
#' dictConfig(config)
#'
#' # use the internal log_config list 
#' dictConfig(log_config)
#'

dictConfig <- function(config){
  "Initiates logging from a config dictionary/list"
  
  assertthat::assert_that(
    !missing(config), 
    msg = "Input config is missing"
  )
  assertthat::assert_that(
    is.list(config) & !is.data.frame(config),
    msg = "config must be a list"
  )
  
  assertthat::assert_that(
    exists("root", config), 
    msg = "root not in config"
  )
  assertthat::assert_that(
    exists("handlers", config), 
    msg = "handlers not in config"
  )
  assertthat::assert_that(
    exists("formatters", config), 
    msg = "formatters not in config"
  )
  
  assertthat::assert_that(
    is.list(config$handlers) & !is.data.frame(config$handlers),
    length(config$handlers) > 0,
    msg = "handlers in config must be an non-empty list"
  )
  assertthat::assert_that(
    is.list(config$formatters) & !is.data.frame(config$formatters),
    length(config$formatters) > 0,
    msg = "formatters in config must be a non-empty list"
  )
  
  root_options <- config$root
  assertthat::assert_that(
    is.list(root_options) & !is.data.frame(root_options),
    msg = "root must be a list"
  )
  assertthat::assert_that(
    exists("level", root_options), 
    msg = "level not found in root options"
  )
  assertthat::assert_that(
    exists("handlers", root_options), 
    msg = "handlers not found in root options"
  )
  
  assertthat::assert_that(
    root_options$level %in% c("NOTSET", "DEBUG", "INFO", "WARNING", "ERROR", "CRITICAL"), 
    msg = stringr::str_interp("Specified root level ${{root_options$level}} not available")
  )
  futile.logger::flog.threshold(futile.logger::FATAL)
  
  logging.loggers <<- root_options$handlers
  for(h in root_options$handlers){
    assertthat::assert_that(
      exists(h, where = config$handlers), 
      msg = stringr::str_interp("${h} not found in handlers")
    )
    make_handler(handler_options = config$handlers[[h]], handler_name = h, formatters = config$formatters)
  }
}
