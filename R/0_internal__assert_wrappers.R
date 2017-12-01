# Generic Assertions

# checks whether or not to throw assert error
# x is a named list of booleans
# msg_suffix is the error message for failed columns
throw_assert <- function(x, msg_suffix){
  assertthat::assert_that(is.list(x), is.character(msg_suffix))
  assertthat::assert_that(is.logical(unlist(x)), msg = "x is not a list of booleans")
  assertthat::assert_that(is_named_list(x), msg = "x is not a named list")
  assertthat::assert_that(all(!is.na(x)), msg = "x has NA values")
  
  check_cols_fail <- names(purrr::discard(unlist(x), ~ .x))
  if(length(check_cols_fail) > 0){
    msg = stringr::str_interp("Column(s) ${{paste(check_cols_fail, collapse = ', ')}} ${msg_suffix}")
    stop(msg)
  }
}

# generic assert wrapper function when given a vector of columns
assert_generic_cols <- function(assert_func){
  assertthat::assert_that(!missing(assert_func), msg = "Input assert_func is missing")
  assertthat::assert_that(is.function(assert_func))
  
  assert <- function(func, cols = NULL){
    "Wrapper function, asserts specified columns of DF satisfy certain conditions"
    
    assertthat::assert_that(!missing(func), msg = "Input func is missing")
    assertthat::assert_that(is.function(func))
    
    wrapper <- function(...){
      result <- func(...)
      assertthat::assert_that(is.data.frame(result))
      
      if(!is.null(cols)) assert_cols_in(result, cols)
      assert_func(result, cols)
      
      return(result)
    }
    return(wrapper)
  }
  return(assert)
}

# generic assert wrapper function when given a list (dictionary)
assert_generic_dict <- function(assert_func){
  assertthat::assert_that(!missing(assert_func), msg = "Input assert_func is missing")
  assertthat::assert_that(is.function(assert_func))
  
  assert <- function(func, dict){
    "Wrapper function, asserts specified columns of DF satisfy certain conditions"
    
    assertthat::assert_that(!missing(func), msg = "Input func is missing")
    assertthat::assert_that(is.function(func))
    assertthat::assert_that(!missing(dict), msg = "Input dict is missing")
    assertthat::assert_that(is.list(dict))
    assertthat::assert_that(is_named_list(dict), msg = "dict is not a uniquely named list")
    
    wrapper <- function(...){
      result <- func(...)
      assertthat::assert_that(is.data.frame(result))
      
      assert_cols_in(result, names(dict))
      assert_func(result, dict)
      
      return(result)
    }
    return(wrapper)
  }
  return(assert)
}
