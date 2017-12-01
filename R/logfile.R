
#' Logfile Information
#'
#' Obtain information regarding the logfile
#'
#' @details
#'
#' exists_logfile() checks whether the logfile exists
#' get_logfile() obtains the name(s) of the logfile
#'
#' @name logfile
NULL

#' @rdname logfile
#' @export
exists_logfile <- function() {
  "Checks whether log files exist"
  
  handlers <- names(logging::getLogger()$handlers) 
  any(stringr::str_detect(handlers, "file"))
}

#' @rdname logfile
#' @export
get_logfile <- function(){
  "Obtains names of log files"
  
  handlers <- names(logging::getLogger()$handlers) 
  files <- stringr::str_subset(handlers, "file")
  unlist(lapply(files, function(f) logging::getLogger()$handlers[[f]]$file))
}
