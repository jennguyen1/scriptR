
#' Asserts That a File Exists
#'
#' Throws an error if a file does not exist
#'
#' @param filename character, file path
#'
#' @export
#' 

assert_file_exists <- function(filename){
  assertthat::assert_that(is.character(filename))
  assertthat::assert_that(file.exists(filename), msg = stringr::str_interp("${filename} does not exist"))
  invisible()
}
