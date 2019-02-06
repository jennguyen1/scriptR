
#' Load Library Functions
#'
#' Functions to load libraries
#'
#' @param ... category of libraries to load c(data, develop, viz)
#'
#' @details
#' install_scriptR() installs the scriptR package \cr
#'
#' lib() imports libraries depending on args: \cr
#'  data: (data wrangling) tidyverse, data.table, etc \cr
#'  develop: (development) argparse, logging, parallel \cr
#'  viz: (data visualization) ggplot2 and extensions \cr
#'
#' @name lib
NULL

#' @rdname lib
#' @export
install_scriptR <- function() devtools::install_github("jennguyen1/scriptR")

#' @rdname lib
#' @export
lib <- function(...){
  "Imports libraries"

  args <- as.character(substitute(list(...))[-1])
  lib_opts <- dplyr::mutate_at(data.frame(data = TRUE, develop = TRUE, viz = TRUE), dplyr::vars(dplyr::one_of(args)), function(x) TRUE)
  suppressWarnings( suppressPackageStartupMessages( import_lib(lib_opts) ) )
}

import_lib <- function(lib_opts){
  if(lib_opts$data){
    # data wrangling
    library(tidyverse)
    library(data.table)
    library(glue)

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
    ggplot2::theme_set(ggplot2::theme_bw())
  }

  if(lib_opts$develop){
    # development
    library(argparse)
    library(futile.logger)
    library(assertthat)
    library(parallel)
  }
}
