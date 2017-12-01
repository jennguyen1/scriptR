
#' Checks Whether a List is a Uniquely Named List
#'
#' @param l a list
#' @return boolean
#'
#' @export
#' 

is_named_list <- function(l){
  "Checks whether a list is a uniquely named list"
  
  assertthat::assert_that(is.list(l))
  names <- names(l)
  length(unique(names)) == length(l)
}
