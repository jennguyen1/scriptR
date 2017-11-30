
#' Opens a File
#' 
#' Checks whether a file exists and opens it using the function you request
#'
#' @param filename character, file path
#' @param f function, reads in the file
#' @param ... additional arguments passed to f
#'
#' @export
#' 
#' @examples 
#' read.in("test.csv")
#' read.in("dat.csv", read.table, header = TRUE)
#' 

read.in <- function(filename, f = data.table::fread, ...){
  assertthat::assert_that(is.character(filename), is.function(f))
  assert_file_exists(filename)
  
  raw <- tryCatch(
    as.data.frame(f(filename, ...)), 
    error = function(e){
      message(e)
      stop("File could not be opened")
    }
  )
  raw
}
