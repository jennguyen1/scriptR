#' Get Logging Level
#'
#' Get the logging level across all handlers
#'
#' @export
#'
#' @examples
#' get_log_level()
#'

get_log_level <- function(){

  return( names(logging::getLogger()$level) )

}
