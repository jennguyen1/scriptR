
#' Reporter Functions
#'
#' Wrappers to log various things\cr
#' * report_function_name() logs function name at the INFO level\cr
#' * report_args() logs function arguments at the DEBUG level\cr
#' * report_dim() logs data frame dimensions before and after function at the INFO level \cr
#' * report_na() logs number of na per column of the data frame at the DEBUG level
#'
#' @details 
#' report_dim()'s function must take a data.frame as the first argument and return a data.frame
#'
#' @param func function
#' 
#' @return function
#'
#' @examples
#' start_logging()
#' 
#' this_is_a_function <- report_function_name(function(x) "Hi, I'm a function")
#' this_is_a_function()
#' 
#' my_function <- report_args(function(x, y, z) "Hi, I'm a function")
#' my_function(x = 1:5, y = list(a = letters[1:3], b = 'b'), z = head(iris))
#' 
#' head2 <- report_dim(head)
#' head2(mtcars)
#'
#' @name report
NULL

#' @rdname report
#' @export
report_function_name <- function(func){
  "Wrapper function, logs function name"
  
  assertthat::assert_that(!missing(func), msg = "Input func is missing")
  assertthat::assert_that(is.function(func))
  
  wrapper <- function(...){
    args <- as.list(match.call())
    func_name <- as.character(args[[1]])
    func_msg <- stringr::str_to_title(stringr::str_replace_all(func_name, "_", " "))
    func_msg <- stringr::str_interp("### ${func_msg} ###")
    logging.info(func_msg)
    return( func(...) )
  }
  return(wrapper)
}

#' @rdname report
#' @export
report_args <- function(func){
  "Wrapper function, logs arguments passed to function"
  
  assertthat::assert_that(!missing(func), msg = "Input func is missing")
  assertthat::assert_that(is.function(func))
  
  wrapper <- function(...){
    args <- as.list(match.call())
    func_name <- as.character(args[[1]])
    print_name <- stringr::str_interp("Calling ${func_name} with the follow arguments")
    logging.debug(print_name)
    
    no_names <- is.null(names(args))
    for(i in 2:length(args)){
      arg_name <- ifelse(no_names, "", names(args)[i])
      i_arg <- eval(args[[i]])
      if(nchar(arg_name) > 0) logging.debug(stringr::str_interp("${arg_name}:"))
      logging.debug(i_arg)
      logging.debug("")
    }
    return( func(...) )
  }
  return(wrapper)
}

#' @rdname report
#' @export
report_dim <- function(func){
  "Wrapper function, logs dimensions before/after"
  
  assertthat::assert_that(!missing(func), msg = "Input func is missing")
  assertthat::assert_that(is.function(func))
  
  wrapper <- function(...){
    data <- list(...)[[1]]
    result <- func(...)
    
    assertthat::assert_that(is.data.frame(data))
    assertthat::assert_that(is.data.frame(result))
    cols_before <- ncol(data)
    cols_after <- ncol(result)
    rows_before <- nrow(data)
    rows_after <- nrow(result)
    
    logging.info(stringr::str_interp("Change in rows: ${{rows_after - rows_before}} (${rows_before} -> ${rows_after})"))
    logging.info(stringr::str_interp("Change in cols: ${{cols_after - cols_before}} (${cols_before} -> ${cols_after})"))
    
    return(result)
  }
  return(wrapper)
}

#' @rdname report
#' @export
report_na <- function(func){
  "Wrapper function, logs number of NA per column"
  
  assertthat::assert_that(!missing(func), msg = "Input func is missing")
  assertthat::assert_that(is.function(func))
  
  wrapper <- function(...){
    result <- func(...)
    
    assertthat::assert_that(is.data.frame(result))
    report_na <- tidyr::gather(dplyr::summarise_all(result, ~ sum(is.na(.x))), key = "variable", value = "n_NA")
    logging.debug("Counting NAs by column")
    logging.debug(report_na)

    return(result)
  }
  return(wrapper)
}
