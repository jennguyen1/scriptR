
#' Assert Functions on Data Frames
#'
#' Wrappers to check that the data frame a function returns satisfies current conditions\cr
#' * assert_between_boundaries() columns are between certain boundaries\cr
#' * assert_between_n_std() columns are between n standard deviations\cr
#' * assert_in() columns have values that are in specified lists\cr
#' * assert_col_types() columns are of specified types\cr
#' * assert_none_missing() columns are not NA\cr
#' * assert_unique() columns are unique\cr
#' * assert_dim() df has dimensions, can leave\cr
#' * assert_margins_after() df margins satisfies certain conditions over original
#'
#' @param func function that returns a data.frame
#' @param cols character vector of columns to check for
#' @param dict list where the names are columns of output, values are arguments for function
#' @param dim numeric vector, index 1 for rows, index 2 for columns, leave NA if don't care about a dimension
#' @param margin character, margin to query, options c('r', 'c')
#' @param condition character, how result DF compares to beginning Df, options c('e', 'g', 'ge', 'l', 'le')
#' 
#' @return function
#'
#' @examples 
#' f <- function(x) x
#' 
#' x <- data.frame(a = runif(3), b = 1:3)
#' dict <- list(a = 0:1, b = 0:1) 
#' a_bound <- assert_between_boundaries(f, dict = dict)
#' a_bound(x) # bounds on b are incorrect; will assert error
#' 
#' dict <- list(Species = c("versicolor", "virginica"))
#' a_in <- assert_in(f, dict = dict)
#' a_in(iris)
#' 
#' x <- data.frame(x = 1:26, y = letters, z = c(TRUE, FALSE), stringsAsFactors = FALSE)
#' dict <- list(x = "integer", y = "logical") 
#' a_col <- assert_col_types(f, dict = dict)
#' a_col(x) # type of y incorrect; will assert error
#' 
#' x <- data.frame(x = 1:10, y = 1:2, z = 101:110)
#' a_unique <- assert_unique(f, cols = c("x", "y")) 
#' a_unique(x) # y is not unique; will assert error
#' 
#' a_dim <- assert_dim(f, dim = c(NA, 4)) 
#' a_dim(iris) # ncol = 5 not 4, will assert error
#' 
#' a_rows <- assert_margins_after(f, margin = 'r', condition = 'l')
#' a_rows(iris) # rows equal not less than, will assert error
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
assert_in <- assert_generic_dict(check_in)

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
assert_margins_after <- function(func, margin = 'r', condition = 'e'){
  "Wrapper function, asserts resulting DF margins compare to incoming DF"
  
  assertthat::assert_that(!missing(func), msg = "Input func is missing")
  assertthat::assert_that(is.function(func))
  assertthat::assert_that(margin %in% c('r', 'c'), msg = "Invalid margin type")
  assertthat::assert_that(condition %in% c('e', 'g', 'ge', 'l', 'le'), msg = "Invalid condition type")
  n_margin <- if(margin == 'r') nrow else ncol
  
  wrapper <- function(...){
    data <- list(...)[[1]]
    result <- func(...)
    
    assertthat::assert_that(is.data.frame(data))
    assertthat::assert_that(is.data.frame(result))
    rows_before <- n_margin(data)
    rows_after <- n_margin(result)
    
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
