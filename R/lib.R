#' Load Library Functions
#'
#' Functions to load libraries
#'
#' @details
#' install_scriptR() installs the scriptR package \cr
#'
#' lib() imports libraries depending on args: \cr
#'  data: (data wrangling) tidyverse, data.table, etc \cr
#'  develop: (development) optparse, logging \cr
#'  viz: (data visualization) ggplot2 and extensions \cr
#'
#' @name lib

#' @rdname lib
#' @export
install_scriptR <- function() devtools::install_github("jnguyen92/scriptR")

#' @rdname lib
#' @export
lib <- function(...){

  args <- as.character(substitute(list(...))[-1])
  lib_opts <- dplyr::mutate_at(data.frame(data = TRUE, develop = TRUE, viz = FALSE), dplyr::vars(dplyr::one_of(args)), function(x) TRUE)
  suppressWarnings( suppressPackageStartupMessages( import_lib(lib_opts) ) )

}

import_lib <- function(lib_opts){

  if(lib_opts$data){
    # data wrangling
    library(magrittr)
    library(stringr)
    library(tidyverse)
    library(purrrlyr)
    library(data.table)
    library(dtplyr)
  }

  if(lib_opts$viz){
    # data visualization
    library(knitr)
    library(ggplot2)
    library(grid)
    library(gridExtra)
    library(GGally)
    library(gtable)

    # set ggplot2 theme
    theme_set(theme_bw())
  }

  if(lib_opts$develop){
    # development
    library(argparse)
    library(optparse)
    library(logging)
    library(parallel)
  }

}
