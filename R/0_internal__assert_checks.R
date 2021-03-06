# Checks for Assertions

# check boundaries
check_between_boundaries <- function(d, dict){
  checks <- purrr::map2(names(dict), dict, function(c, boundaries){
    check_col <- purrr::discard(dplyr::pull(d, c), ~ is.na(.x))
    assertthat::assert_that(is.numeric(check_col), msg = stringr::str_interp("${c} is not a numeric column"))
    assertthat::assert_that(length(boundaries) == 2)
    assertthat::assert_that(boundaries[1] < boundaries[2], msg = stringr::str_interp("${c} boundaries are not sorted correctly"))
    all(dplyr::between(check_col, boundaries[1], boundaries[2]))
  })
  names(checks) <- names(dict)
  throw_assert(checks, "are not between specified boundaries")
}

check_between_n_std <- function(d, dict){
  dict_boundaries <- purrr::map2(names(dict), dict, function(c, n){
    check_col <- purrr::discard(dplyr::pull(d, c), ~ is.na(.x))
    assertthat::assert_that(is.numeric(check_col), msg = stringr::str_interp("${c} is not a numeric column"))
    assertthat::assert_that(length(n) == 1)
    boundaries <- mean(check_col) + c(-1, 1) * n * sd(check_col)
    boundaries
  })
  names(dict_boundaries) <- names(dict)
  check_between_boundaries(d, dict_boundaries)  
}

check_in <- function(d, dict){
  checks <- purrr::map2(names(dict), dict, function(c, boundaries){
    check_col <- purrr::discard(dplyr::pull(d, c), ~ is.na(.x))
    assertthat::assert_that(is.vector(boundaries), msg = stringr::str_interp("${c} is not an atomic vector without attributes"))
    all(check_col %in% boundaries)
  })
  names(checks) <- names(dict)
  throw_assert(checks, "have values that are not in specified lists")
}

# check column types
check_col_types <- function(d, dict){
  checks <- purrr::map2(names(dict), dict, function(c, type){
    assertthat::assert_that(is.character(type), length(type) == 1)
    check_col <- dplyr::pull(d, c)
    class(check_col) == type
  })
  names(checks) <- names(dict)
  throw_assert(checks, "have the wrong types")
}


# check columns are not na
check_none_missing <- function(d, cols){
  if(is.null(cols)) cols <- colnames(d)
  check_df <- dplyr::select(d, dplyr::one_of(cols))
  
  checks <- dplyr::summarise_all(check_df, ~ all(!is.na(.x)))
  throw_assert(checks, "contain missing values")
}


# check columns are unique
check_unique <- function(d, cols){
  if(is.null(cols)) cols <- colnames(d)
  check_df <- dplyr::select(d, dplyr::one_of(cols))
  
  checks <- dplyr::summarise_all(check_df, ~ any(duplicated(.x)) == FALSE)
  throw_assert(checks, "contain duplicates")
}


# check dimensions are the same as requested
check_dimensions <- function(d, dim){
  assertthat::assert_that(length(dim) == 2)
  assertthat::assert_that(is.numeric(dim)) # checks for numeric type and both are not NA
  
  expect_rows <- dim[1]
  expect_cols <- dim[2]
  if(all(!is.na(dim))){
    assertthat::assert_that(all(dim(d) == dim), msg = stringr::str_interp("Dimensions do not match expected (${expect_rows}, ${expect_cols})"))
  } else if( !is.na(dim[1]) ){
    assertthat::assert_that(dim(d)[1] == expect_rows, msg = stringr::str_interp("Number of rows do not match expected (${expect_rows})"))
  } else{
    assertthat::assert_that(dim(d)[2] == expect_cols, msg = stringr::str_interp("Number of columns do not match expected (${expect_cols})"))
  }
}
