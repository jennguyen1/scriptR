#' Prints Command Line Args
#'
#' Prints out all command line arguments
#'
#' @param args list of command line args, generated from argsparse::parse_args
#'
#' @export
#'
#' @examples
#' scriptR::start_logging()
#'
#' description <- "TEST"
#' parser <- ArgumentParser(description = description)
#' parser$add_argument("--test_opt", help = "This is a test")
#' parser$add_argument("--log", default = NULL, help = "Prefix of the log file; call be NULL or a default name [default]")
#' args <- parser$parse_args()
#'
#' print_cmd_args(args)
#'

print_cmd_args <- function(args){

  # handle missing arg
  if(missing(args)) scriptR::logerr("Missing list of command line arguments")

  # remove log and help options in args list
  cmdargs <- purrr::discard(args, names(args) %in% c('log', 'help'))

  # format message
  opt <- paste(paste0(names(cmdargs), ": ", cmdargs), collapse = '\n')
  msg <- paste0("Command line args:\n", opt, "\n\n")

  # prints out command line args
  logging::loginfo(msg)

}
