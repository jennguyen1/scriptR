#' Takes the logging configuration from a list
#'
#' R adaptation of the Python logging.config.dictConfig function. See \url{https://docs.python.org/3.6/library/logging.config.html#dictionary-schema-details} for schema details
#'
#' @param config configurations as a list
#'
#' @export
#'
#' @examples
#' dictConfig(config)
#'

dictConfig <- function(config){
  assertthat::assert_that(exists("root", config), msg = "root not in config file")
  assertthat::assert_that(exists("handlers", config), msg = "handlers not in config file")
  assertthat::assert_that(exists("formatters", config), msg = "formatters not in config file")
  
  root_options <- config$root
  assertthat::assert_that(exists("level", root_options), msg = "level not found in root options")
  assertthat::assert_that(exists("handlers", root_options), msg = "handlers not found in root options")
  
  logging::setLevel(root_options$level)
  for(h in root_options$handlers){
    make_handler(handler_options = config$handlers[[h]], handler_name = h, formatters = config$formatters)
  }
}
