
#' Writes Out Data Frames
#'
#' Wrapper around write.table and write.csv depending on file extension.
#' Logs action at the INFO level.
#'
#' @param dat data.frame
#' @param file name of file; if file extension is csv or tsv saves file as such
#' @param ... other parameters to pass to writing functions
#'
#' @export
#'

write.out <- function(dat, file, ...){
  "Writes out data.frames to a file"
  
  assertthat::assert_that(!missing(dat), msg = "Input dat is missing")
  assertthat::assert_that(!missing(file), msg = "Input file is missing")
  assertthat::assert_that(
    is.data.frame(dat), 
    is.character(file)
  )
  
  csv <- stringr::str_detect(file, "csv$")
  tsv <- stringr::str_detect(file, "tsv$")

  logging::loginfo(stringr::str_interp("Writing out ${file}"))
  if(csv){
    write.csv(dat, file, row.names = FALSE, quote = FALSE, ...)
  } else if(tsv){
    write.table(dat, file, row.names = FALSE, quote = FALSE, sep = "\t", ...)
  } else{
    write.table(dat, file, row.names = FALSE, quote = FALSE, ...)
  }
}
