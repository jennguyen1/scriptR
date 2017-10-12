#' Logs Misc Data Structures
#'
#' Allows for logging of data structures such as dataframes, etc.
#'
#' @param x any object to be logged
#' @param loglevel level to log the message
#'
#' @export
#'
#' @examples
#' logmisc(iris)
#' logmisc(summary(iris))
#'

logmisc <- function(x, loglevel = 'INFO'){

  # restrict logging to INFO level
  current_level <- logging::getLogger()$level
  logmisc_level <- logging::loglevels[loglevel]

  if(current_level <= logmisc_level){

    # print summary to console
    print(x)

    # write to logfile depending on object type
    if( scriptR::exists_logfile() ){

      logfile <- scriptR::get_logfile()
      if(is.data.frame(x)){
        suppressWarnings(write.table(x, file = logfile, append = TRUE, quote = FALSE, row.names = FALSE, sep = "\t"))
      } else{
        capture.output(x, file = logfile, append = TRUE)
      }

    }

  }

}
