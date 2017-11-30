
make_formatter <- function(format_options){
  assertthat::assert_that(
    !missing(format_options), 
    msg = "Input format_options is missing"
  )
  assertthat::assert_that(
    is.list(format_options) & !is.data.frame(format_options), 
    msg = "Input format_options need to be a list"
  )
  
  assertthat::assert_that(
    exists("format", where = format_options), 
    msg = "format not found in formatter options"
  )
  assertthat::assert_that(
    exists("style", where = format_options), 
    msg = "style not found in formatter options"
  )
  assertthat::assert_that(
    all(unlist(lapply(format_options, is.character))), 
    msg = "All format options need to be strings"
  )
  
  assertthat::assert_that(
    format_options$style == "{", 
    msg = "The only supported formatter style is {"
  )
  time_format <- ifelse(!is.null(format_options$datefmt), format_options$datefmt, "")
  use_string <- stringr::str_replace_all(format_options$format, "[{]", "${")
  
  use_formatter <- function(record){
    
    # formats of Python logging not available
    pathname <- ""
    filename <- ""
    module <- ""
    lineno <- ""
    created <- ""
    msecs <- ""
    thread <- ""
    threadName <- ""
    process <- ""
    
    # formats available in R logging package
    name <- record$logger  
    asctime <- format(as.POSIXct(record$timestamp), time_format)
    message <- record$msg
    levelname <- record$levelname
    levelno <- record$level
    
    stringr::str_interp(use_string)
  }
  use_formatter
}
