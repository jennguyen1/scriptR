#' Writes Out Data Frames
#'
#' Wrapper around write.table and write.csv depending on file extension
#'
#' @param dat dataframe
#' @param file name of file; if file extension is csv or tsv saves file as such
#' @param ... other parameters to pass to writing functions
#'
#' @export
#'

write.out <- function(dat, file, ...){
  csv <- stringr::str_detect(file, "csv$")
  tsv <- stringr::str_detect(file, "tsv$")

  if(csv){
    write.csv(dat, file, row.names = FALSE, quote = FALSE, ...)
  } else if(tsv){
    write.table(dat, file, row.names = FALSE, quote = FALSE, sep = "\t", ...)
  } else{
    write.table(dat, file, row.names = FALSE, quote = FALSE, ...)
  }
}
