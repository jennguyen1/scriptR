
#' Wrapper: Logs the Function Name
#' 
#' Logs the function name at the INFO level before running the function.
#'
#' @param func function to wrap
#'
#' @export
#'

log_function_name <- function(func){
  assertthat::assert_that(!missing(func), msg = "Input func is missing")
  assertthat::assert_that(is.function(func))
  
  wrapper <- function(...){
    args <- as.list(match.call())
    func_name <- as.character(args[[1]])
    func_msg <- stringr::str_to_title(stringr::str_replace_all(func_name, "_", " "))
    logging::loginfo(func_msg)
    
    return( func(...) )
  }
  return(wrapper)
}
