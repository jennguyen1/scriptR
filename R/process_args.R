#' Process Command Line Args and Initiate Logging
#'
#' Processes command line args, logs script description & arguments passed in. Returns list of option values.
#'
#' @param parser an argparse object
#' @param description description of script
#' @param ... additional options passed to parse_args() call
#' @return a list parsed options
#'
#' @export
#'
#' @examples
#'
#' description <- "TEST"
#' parser <- ArgumentParser(description = description)
#' parser$add_argument("--test_opt", help = "This is a test")
#' parser$add_argument("--log", default = NULL, help = "Prefix of the log file; call be NULL or a default name [default]")
#'
#' args <- process_options(parser = parser, description = description, log_level = "DEBUG", ...)
#'

process_args <- function(parser, description, ...){

  args <- parser$parse_args(...)

  scriptR::start_logging(file_name = args$log, config = args$log_config)
  logging::loginfo(description)
  scriptR::print_cmd_args(args)

  return(args)
}
