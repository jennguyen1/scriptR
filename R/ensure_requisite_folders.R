#' Prints Command Line Args
#'
#' @param dir filepath to required folder
#'
#' @export
#'

ensure_requisite_folders <- function(dir) if( !dir.exists(dir) ) dir.create(dir, recursive = TRUE)
