
#' Python Style Logging Functions
#'
#' * logging.log() \cr
#' * logging.debug() \cr
#' * logging.info() \cr
#' * logging.warning() \cr
#' * logging.error() \cr
#' * logging.critical() \cr
#' * logging.exception()
#'
#' @param msg object to log
#' @param level logging level c("DEBUG", INFO", "WARNING", "ERROR", "CRITICAL")
#' @param e exception
#'
#' @name logging
NULL

#' @rdname logging
#' @export
logging.log <- function(msg, level = "INFO"){
  log_function <- switch(
    level,
    "DEBUG" = futile.logger::flog.debug,
    "INFO" = futile.logger::flog.info,
    "WARNING" = futile.logger::flog.warn,
    "ERROR" = futile.logger::flog.error,
    "CRITICAL" = futile.logger::flog.fatal,
    function(...) invisible()
  )
  tryCatch({
    for(n in logging.loggers){ # global variable that tracks the loggers
      if(length(msg) == 1){
        log_function(msg, name = n)
      } else{
        log_function(deparse(substitute(msg)), msg, capture = TRUE, name = n)
      }
    }
  }, error = function(e) invisible()
  )
}

#' @rdname logging
#' @export
logging.debug <- function(msg) logging.log(msg, level = "DEBUG")

#' @rdname logging
#' @export
logging.info <- function(msg) logging.log(msg, level = "INFO")

#' @rdname logging
#' @export
logging.warning <- function(msg) logging.log(msg, level = "WARNING")

#' @rdname logging
#' @export
logging.error <- function(msg) logging.log(msg, level = "ERROR")

#' @rdname logging
#' @export
logging.critical <- function(msg) logging.log(msg, level = "CRITICAL")

#' @rdname logging
#' @export
logging.exception <- function(msg, e){
  logging.error(msg)
  logging.error(e)
}
