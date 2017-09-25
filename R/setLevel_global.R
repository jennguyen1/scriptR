#' Globally Set Logging Level
#'
#' Set the logging level across all handlers
#'
#' @param level logging level, see logging::loglevels for available options
#'
#' @export
#'
#' @examples
#' setLevel_global("CRITICAL")
#'

setLevel_global <- function(level){

  if(missing(level)) scriptR::logerr("Missing level")

  setLevel(level, getHandler("basic.stdout"))
  if(scriptR::exists_logfile()) setLevel(level, getHandler("logging::writeToFile"))
  setLevel(level)

}
