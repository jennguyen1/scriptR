
#' Prints Command Line Args
#'
#' Logs out all command line arguments at the INFO level.
#'
#' @param args list of command line args, generated from argparse::parse_args
#'
#' @export
#'
#' @examples
#' library(argparse)
#'
#' start_logging()
#'
#' description <- "TEST"
#' parser <- ArgumentParser(description = description)
#' parser$add_argument("--test_opt", help = "This is a test")
#' parser$add_argument("--log", default = NULL, help = "Name of log config file [$(default)]")
#' args <- parser$parse_args()
#'
#' print_cmd_args(args)
#'

print_cmd_args <- function(args){
  "Logs command line arguments"

  assertthat::assert_that(!missing(args), msg = "Input args is missing")
  assertthat::assert_that(is.list(args), !is.data.frame(args), msg = "Input args should be a list (not a data.frame)")

  cmdargs <- purrr::discard(args, names(args) %in% c('log', 'help'))
  if(length(cmdargs) > 0){
    opt <- paste(paste0(names(cmdargs), ": ", cmdargs), collapse = '\n')
    msg <- paste0("Command line args:\n", opt, "\n")
    logging.info(msg)
  }
}
