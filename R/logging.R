
#' Python Style Logging Functions
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
logging.debug <- function(x) logging.log(x, level = "DEBUG")

#' @rdname logging
#' @export
logging.info <- function(x) logging.log(x, level = "INFO")

#' @rdname logging
#' @export
logging.warning <- function(x) logging.log(x, level = "WARNING")

#' @rdname logging
#' @export
logging.error <- function(x) logging.log(x, level = "ERROR")

#' @rdname logging
#' @export
logging.critical <- function(x) logging.log(x, level = "CRITICAL")

#' @rdname logging
#' @export
logging.exception <- function(msg){
  logging.error(msg)
  traceback()
}
