
#' Asserts That Columns are in Data Frame
#'
#' @param d data.frame
#' @param cols character vector of columns to check for
#'
#' @export
#' 
#' @examples
#' assert_cols_in(iris, c("Petal.Width", "Petal.Length"))
#' 

assert_cols_in <- function(d, cols){
  assertthat::assert_that(!missing(d), msg = "Input d is missing")
  assertthat::assert_that(!missing(cols), msg = "Input cols is missing")
  assertthat::assert_that(is.data.frame(d), is.character(cols))
  
  col_check <- purrr::discard(cols, ~ .x %in% colnames(d))
  if(length(col_check) > 0){
    msg <- stringr::str_interp("${{paste(col_check, collapse = ', ')}} column(s) not available in the data frame")
    stop(msg)
  }
}
