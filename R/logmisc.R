
#' Logs Misc Data Structures
#'
#' Allows for logging of data structures such as data.frames, etc.
#'
#' @param x any object to be logged
#' @param log_level level to log the message
#'
#' @export
#'
#' @examples
#' logmisc(iris)
#' logmisc(summary(iris))
#'

logmisc <- function(x, log_level = 'INFO'){

  handlers <- logging::getLogger()$handlers
  for(h in handlers){
    current_level <- h$level
    logmisc_level <- logging::loglevels[log_level]

    if(logmisc_level >= current_level){
      to_file <- "file" %in% names(h)
      logfile <- if(to_file) h$file else ""
      if(is.vector(x)){
        if(!to_file) cat(x, "\n") else cat(x, "\n", file = logfile, append = TRUE)
      } else if(is.data.frame(x)){
        if(!to_file) print(x) else suppressWarnings(write.table(x, file = logfile, append = TRUE, quote = FALSE, row.names = FALSE, sep = "\t"))
      } else{
        if(!to_file) print(x) else capture.output(x, file = logfile, append = TRUE)
      }
    }
  }
}
