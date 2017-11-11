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
  handlers <- names(logging::getLogger()$handlers) 
  any(stringr::str_detect(handlers, "file"))
}

#' @rdname logfile
#' @export
get_logfile <- function(){
  handlers <- names(logging::getLogger()$handlers) 
  stringr::str_subset(handlers, "file")
}
