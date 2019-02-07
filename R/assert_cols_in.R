
#' Asserts That Columns are in Data Frame
#'
#' @param d data.frame
#' @param cols character vector of columns to check for
#' @param ... name of columns to check for
#'
#' @details
#' Column names can be provided either as a character vector (in parameter cols) or
#' as the names of columns to check for (without quotations). Do not mix the types.
#'
#' @export
#'
#' @examples
#' assert_cols_in(iris, c("Petal.Width", "Petal.Length")) # cols in, will do nothing
#' assert_cols_in(iris, Petal.Width, Petal.Length)
#'
#' \dontrun{
#' assert_cols_in(iris, c("x", "y")) # cols not in, will assert error
#' assert_cols_in(iris, x, y)
#' }
#'

assert_cols_in <- function(d, cols, ...){
  "Asserts that columns are in a data frame"

  assertthat::assert_that(!missing(d), msg = "Input d is missing")
  assertthat::assert_that(!missing(cols), msg = "Input cols is missing")

  tryCatch({
    invisible(list(cols, ...))

  }, error = function(e){
    c <- as.character(rlang::enexprs(cols, ...))
    cols <<- purrr::map_chr(c, stringr::str_replace, "[~]", "")

  }, finally = {
    assertthat::assert_that(is.data.frame(d), is.character(cols))

    col_check <- purrr::discard(cols, ~ .x %in% colnames(d))
    if(length(col_check) > 0){
      msg <- stringr::str_interp("${{paste(col_check, collapse = ', ')}} column(s) not available in the data frame")
      stop(msg)
    }
  })

  invisible(NULL)
}
