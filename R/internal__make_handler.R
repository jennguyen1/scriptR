make_handler <- function(handler_options, handler_name, formatters){
  assertthat::assert_that(exists("class", where = handler_options), msg = "class not found in handler options")
  assertthat::assert_that(exists('level', where = handler_options), msg = "level not found in handler options")
  assertthat::assert_that(exists('formatter', where = handler_options), msg = "formatter not found in handler options")
  
  levelname <- handler_options$level
  formatter <- make_formatter(formatters[[handler_options$formatter]])
  
  if(handler_options$class == "logging.StreamHandler"){
    action <- logging::writeToConsole
    
    logging::getLogger()$addHandler(handler_name, action = action, formatter = formatter, level = levelname)
    
  } else if (handler_options$class == "logging.FileHandler"){
    assertthat::assert_that(exists("filename", where = handler_options), msg = "filename not found in handler options")
    assertthat::assert_that(exists("mode", where = handler_options), msg = "mode not found in handler options")
    
    action <- logging::writeToFile
    filename <- handler_options$filename
    
    # remove old file if mode is write (not append)
    if(handler_options$mode == "w" & file.exists(filename)){
      file.remove(filename)
    }
    
    logging::getLogger()$addHandler(handler_name, action = action, file = filename, formatter = formatter, level = levelname)
    
  } else{
    scriptR::logerr("The only supported formatter styles are logging.StreamHandler and logging.FileHandler")
  }
}
