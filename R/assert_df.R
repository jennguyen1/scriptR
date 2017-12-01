
#' Assert Functions on Data Frames
#'
#' Wrappers to check that the data frame a function returns satisfies current conditions \cr
#' assert_between_boundaries() columns are between certain boundaries \cr
#' assert_between_n_std() columns are between n standard deviations \cr
#' assert_col_types() columns are of specified types \cr
#' assert_none_missing() columns are not NA \cr
#' assert_unique() columns are unique \cr
#' assert_dim() df has dimensions, can leave \cr
#' assert_rows_after() df rows satisfies certain conditions over original
#'
#' @param func function that returns a data.frame
#' @param cols character vector of columns to check for
#' @param dict list where the names are columns of output, values are arguments for function
#' @param dim numeric vector, index 1 for rows, index 2 for columns, leave NA if don't care about a dimension
#' @param condition character, options c('e', 'g', 'ge', 'l', 'le')
#' 
#' @return data.frame
#'
#' @name assert_df
NULL

#' @rdname assert_df
#' @export
assert_between_boundaries <- assert_generic_dict(check_between_boundaries)

#' @rdname assert_df
#' @export
assert_between_n_std <- assert_generic_dict(check_between_n_std)

#' @rdname assert_df
#' @export
assert_col_types <- assert_generic_dict(check_col_types)

#' @rdname assert_df
#' @export
assert_none_missing <- assert_generic_cols(check_none_missing)

#' @rdname assert_df
#' @export
assert_unique <- assert_generic_cols(check_unique)

#' @rdname assert_df
#' @export
assert_dim <- function(func, dim){
  "Wrapper function, asserts resulting DF has certain dimensions"
  
  assertthat::assert_that(!missing(func), msg = "Input func is missing")
  assertthat::assert_that(is.function(func))
  assertthat::assert_that(!missing(dim), msg = "Input dim is missing")
  
  wrapper <- function(...){
    result <- func(...)
    assertthat::assert_that(is.data.frame(result))
    
    check_dimensions(result, dim)
    return(result)
  }
  return(wrapper)
}

#' @rdname assert_df
#' @export
assert_rows_after <- function(func, condition = 'e'){
  "Wrapper function, asserts resulting DF rows compare to incoming DF"
  
  assertthat::assert_that(!missing(func), msg = "Input func is missing")
  assertthat::assert_that(is.function(func))
  assertthat::assert_that(condition %in% c('e', 'g', 'ge', 'l', 'le'), msg = "Invalid condition type")
  
  wrapper <- function(...){
    data <- list(...)[[1]]
    result <- func(...)
    
    assertthat::assert_that(is.data.frame(data))
    assertthat::assert_that(is.data.frame(result))
    rows_before <- nrow(data)
    rows_after <- nrow(result)
    
    switch(
      condition,
      "e" = assertthat::assert_that(rows_after == rows_before),
      "g" = assertthat::assert_that(rows_after > rows_before),
      "ge" = assertthat::assert_that(rows_after >= rows_before),
      "l" = assertthat::assert_that(rows_after < rows_before),
      "le" = assertthat::assert_that(rows_after <= rows_before)
    )
    return(result)
  }
  return(wrapper)
}
