
#' Opens a File
#'
#' Wrapper around a read in data frame in a file (like data.table::fread or
#' read.table). Checks whether a file exists first and opens it with specified
#' function. Logs action at the INFO level.
#'
#' @param file character, file path
#' @param f function, reads in the file, defaults to data.table::fread
#' @param ... additional arguments passed to f
#'
#' @export
#'
#' @examples
#' \dontrun{
#' read.in("test.csv")
#' read.in("dat.csv", read.table, header = TRUE)
#' }
#'

read.in <- function(file, f = data.table::fread, ...){
  "Reads in a data.frame from a file"

  assertthat::assert_that(!missing(file), msg = "Input file is missing")
  assertthat::assert_that(is.character(file), is.function(f))
  assertthat::is.readable(file)

  logging.info(stringr::str_interp("Reading in ${file}"))
  raw <- tryCatch(
    as.data.frame(f(file, ...)),
    error = function(e){
      message(e)
      stop("File could not be opened and converted to a data.frame")
    }
  )
  logging.info(stringr::str_interp("Dimensions: (${{nrow(raw)}}, ${{ncol(raw)}})"))
  raw
}
