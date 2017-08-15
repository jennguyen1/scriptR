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
#' option_list <- list(make_option("--test_opt", help = "this is a parameter"))
#' opt <- parse_args(OptionParser(description = "This is a test", option_list = option_list))
#'
#' print_cmd_args(opt)
#'

print_cmd_args <- function(args){

  # remove log and help options in args list
  cmdargs <- purrr::discard(args, names(args) %in% c('log', 'help'))

  # format message
  opt <- paste0(names(cmdargs), ": ", cmdargs) %>% paste(collapse = '\n')
  msg <- paste0("Command line args:\n", opt, "\n\n")

  # prints out command line args
  logging::loginfo(msg)

}
