#' Writes Out Data Frames
#'
#' Wrapper around write.table and write.csv depending on file extension
#'
#' @param dat dataframe
#' @param file name of file
#' @param ... other parameters to pass to writing functions
#'
#' @export
#'

write.out <- function(dat, file, ...){
  csv <- stringr::str_detect(file, "csv$")
  if(csv){
    write.csv(dat, file, row.names = FALSE, quote = FALSE, ...)
  } else{
    write.table(dat, file, row.names = FALSE, quote = FALSE, ...)
  }
}
