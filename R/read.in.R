
#' Opens a File
#' 
#' Wrapper around a read in data frame in a file (like data.table::fread or 
#' read.table). Checks whether a file exists first and opens it with specified function
#'
#' @param filename character, file path
#' @param f function, reads in the file, defaults to data.table::fread
#' @param ... additional arguments passed to f
#'
#' @export
#' 
#' @examples 
#' read.in("test.csv")
#' read.in("dat.csv", read.table, header = TRUE)
#' 

read.in <- function(filename, f = data.table::fread, ...){
  assertthat::assert_that(!missing(filename), msg = "Input filename is missing")
  assertthat::assert_that(is.character(filename), is.function(f))
  assert_file_exists(filename)
  
  raw <- tryCatch(
    as.data.frame(f(filename, ...)), 
    error = function(e){
      message(e)
      stop("File could not be opened and converted to a data.frame")
    }
  )
  raw
}
