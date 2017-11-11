make_formatter <- function(format_options){
  
  assertthat::assert_that(exists("format", where = format_options), msg = "format not found in formatter options")
  assertthat::assert_that(exists("style", where = format_options), msg = "style not found in formatter options")
  
  if(format_options$style != "{") scriptR::logerr("The only supported formatter style is {")
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
  
  return(use_formatter)
}
