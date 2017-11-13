#' Prints Command Line Args
#'
#' Prints out all command line arguments
#'
#' @param args list of command line args, generated from argparse::parse_args
#'
#' @export
#'
#' @examples
#' scriptR::start_logging()
#'
#' description <- "TEST"
#' parser <- ArgumentParser(description = description)
#' parser$add_argument("--test_opt", help = "This is a test")
#' parser$add_argument("--log", default = NULL, help = "Name of logfile [$(default)]")
#' parser$add_argument("--log_config", default = NULL, help = "Name of log config file [%(default)]")
#' args <- parser$parse_args()
#'
#' print_cmd_args(args)
#'

print_cmd_args <- function(args){

  # handle missing arg
  if(missing(args)) scriptR::logerr("Missing list of command line arguments")

  # remove log and help options in args list
  cmdargs <- purrr::discard(args, names(args) %in% c('log', 'log_config', 'help'))

  if(length(cmdargs) > 0){
    # format message
    opt <- paste(paste0(names(cmdargs), ": ", cmdargs), collapse = '\n')
    msg <- paste0("Command line args:\n", opt, "\n")

    # prints out command line args
    logging::loginfo(msg)
  }
}
