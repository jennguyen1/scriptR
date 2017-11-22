
#' Creates Folder if it Doesn't Exist
#'
#' @param path filepath to required folder
#'
#' @export
#'

ensure_requisite_folders <- function(path){
  assertthat::assert_that(!missing(path), msg = "Input path is missing")
  assertthat::assert_that(is.character(path))
  if( !dir.exists(path) ) dir.create(path, recursive = TRUE)
} 
