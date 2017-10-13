#' Globally Set Logging Level
#'
#' Set the logging level across all handlers
#'
#' @param level logging level, see logging::loglevels for available options
#'
#' @export
#'
#' @examples
#' set_level_global("CRITICAL")
#'

set_level_global <- function(level){

  if(missing(level)) scriptR::logerr("Missing level")

  logging::setLevel(level, logging::getHandler("basic.stdout"))
  if(scriptR::exists_logfile()) logging::setLevel(level, logging::getHandler("logging::writeToFile"))
  logging::setLevel(level)

}
