
make_handler <- function(handler_options, handler_name, formatters){
  assertthat::assert_that(
    !missing(handler_options), 
    msg = "Input handler_options is missing"
  )
  assertthat::assert_that(
    is.list(handler_options) & !is.data.frame(handler_options), 
    msg = "Input handler_options need to be a list"
  )
  assertthat::assert_that(
    !missing(handler_name),
    msg = "Input handler_name is missing"
  )
  assertthat::assert_that(
    is.character(handler_name), 
    length(handler_name) == 1, 
    handler_name != "",
    msg = "handler_name must be a singular non-empty string"
  )
  assertthat::assert_that(
    !missing(formatters),
    msg = "Input formatters is missing"
  )
  assertthat::assert_that(is.list(formatters))
  
  assertthat::assert_that(
    exists("class", where = handler_options), 
    msg = "class not found in handler options"
  )
  assertthat::assert_that(
    exists('level', where = handler_options), 
    msg = "level not found in handler options"
  )
  assertthat::assert_that(
    exists('formatter', where = handler_options), 
    msg = "formatter not found in handler options"
  )
  assertthat::assert_that(
    all(unlist(lapply(handler_options, is.character))), 
    msg = "All handler options need to be strings"
  )
  
  assertthat::assert_that(
    handler_options$formatter %in% names(formatters), 
    msg = stringr::str_interp("Specified handler formatter ${{handler_options$formatter}} not available in formatters")
  )
  formatter <- make_formatter(formatters[[handler_options$formatter]])
  
  assertthat::assert_that(
    handler_options$level %in% names(logging::loglevels), 
    msg = stringr::str_interp("Specified handler level ${{handler_options$level}} not available in logging::loglevels")
  )
  levelname <- handler_options$level
  
  if(handler_options$class == "logging.StreamHandler"){
    action <- logging::writeToConsole
    
    logging::getLogger()$addHandler(handler_name, action = action, formatter = formatter, level = levelname)
    
  } else if (handler_options$class == "logging.FileHandler"){
    assertthat::assert_that(
      exists("filename", where = handler_options), 
      msg = "filename not found in handler options"
    )
    assertthat::assert_that(
      exists("mode", where = handler_options), 
      msg = "mode not found in handler options"
    )
    
    assertthat::assert_that(
      stringr::str_detect(handler_name, "file"),
      msg = "handler name of logging.FileHandler must have 'file' somewhere in the name"
    )
    assertthat::assert_that(
      is.character(handler_options$mode),
      length(handler_options$mode) == 1,
      handler_options$mode %in% c("w", "a"),
      msg = "mode should be either 'w' or 'a'"
    )
    
    action <- logging::writeToFile
    filename <- handler_options$filename
    
    # remove old file if mode is write (not append)
    if(handler_options$mode == "w" & file.exists(filename)){
      file.remove(filename)
    }
    
    logging::getLogger()$addHandler(handler_name, action = action, file = filename, formatter = formatter, level = levelname)
    
  } else{
    stop("The only supported handler classes are logging.StreamHandler and logging.FileHandler")
  }
}
