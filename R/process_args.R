
#' Process Command Line Args and Initiate Logging
#'
#' Processes command line args, logs script description & arguments passed in at the INFO level. 
#' Returns list of arg values.
#'
#' @param parser an argparse object
#' @param description description of script
#' @param ... additional options passed to parse_args() call
#' @return a list parsed args
#'
#' @export
#'
#' @examples
#'
#' description <- "TEST"
#' parser <- ArgumentParser(description = description)
#' parser$add_argument("--test_opt", help = "This is a test")
#' parser$add_argument("--log", default = NULL, help = "Name of log config file [$(default)]")
#'
#' args <- process_args(parser = parser, description = description, ...)
#'

process_args <- function(parser, description = "", ...){
  "Processes command line args, logs script description and arguments"
  
  assertthat::assert_that(
    !missing(parser),
    msg = "Input parser is missing"
  )
  assertthat::assert_that(
    all.equal(class(parser), c("proto", "environment")), 
    msg = "Input parser must be an ArgumentParser object"
  )
  
  args <- parser$parse_args(...)

  start_logging(config = args$log)
  logging.info(description)
  print_cmd_args(args)

  args
}
