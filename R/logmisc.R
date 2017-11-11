#' Logs Misc Data Structures
#'
#' Allows for logging of data structures such as dataframes, etc.
#'
#' @param x any object to be logged
#' @param log_evel level to log the message
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
    if(logmisc_level < current_level) next
    
    if("file" %in% names(h)){
      logfile <- h$file
      if(is.data.frame(x)){
        suppressWarnings(write.table(x, file = logfile, append = TRUE, quote = FALSE, row.names = FALSE, sep = "\t"))
      } else{
        capture.output(x, file = logfile, append = TRUE)
      }
    } else{
      print(x)
    }
  }
}
