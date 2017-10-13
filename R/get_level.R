#' Get Logging Level
#'
#' Get the logging level across all handlers
#'
#' @export
#'
#' @examples
#' get_level()
#'

get_level <- function(){
  
  return( names(logging::getLogger()$level) )
  
}
