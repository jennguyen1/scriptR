context("Assertions")

library(dplyr)

# assert functions
test_that("assert_cols_in returns correct values", {
  expect_error(assert_cols_in(iris, letters))
  expect_null(assert_cols_in(iris, c("Petal.Length", "Species")))
})

test_that("assert_cols_in handles invalid types", {
  expect_error(assert_cols_in())
  expect_error(assert_cols_in(iris))
  expect_error(assert_cols_in(1:10, letters))
  expect_error(assert_cols_in(list(a = 1, b = 2, c = 3), c("a", "b")))
  expect_error(assert_cols_in(iris, 1:10))
})

# assert internal functions
test_that("internal_wrappers handles invalid data types", {
  f <- function(x) head(iris)
  
  # missing
  expect_error(assert_unique())
  expect_error(assert_col_types())
  
  # invalid function
  expect_error(assert_unique(1))
  expect_error(assert_col_types(1))
  
  # missing additional args
  expect_error(assert_col_types(f))
  
  # invalid additional args
  expect_error(assert_col_types(f, 1))
  expect_error(assert_col_types(f, list(1,2,3)))
  expect_error(assert_col_types(f, list(a = 1, b = 2, a = 3)))
  
  # columns not in df
  f_err <- assert_col_types(f, list(Species = "character", x = "logical"))
  expect_error(f_err())
  f_err <- assert_unique(f, c("x", "Species", "y", "Petal.Length"))
  expect_error(f_err())
  
  # does not return a df
  f <- function(x) 1
  f_err <- assert_col_types(f, list(Species = "character"))
  expect_error(f_err())
  f_err <- assert_unique(f, c("Species", "Petal.Length"))
  expect_error(f_err())  
})

test_that("throw_assert handles invalid data types", {
  expect_error(throw_assert(1:10, 1))
  expect_error(throw_assert(list(a = TRUE), 1))
  expect_error(throw_assert(list(a = 1), 'message'))
  expect_error(throw_assert(list(TRUE, FALSE), 'message'))
  expect_error(throw_assert(list(a = NA, b = NA), 'message'))
})

# assert df functions
test_that("assert_between_boundaries returns the correct values", {
  set.seed(1)
  x <- data.frame(x = runif(100), y = runif(100), z = runif(100))
  check_right <- list(x = 0:1, y = 0:1, z = 0:1)
  check_wrong <- list(x = c(0.25, .75), y = 0:1, z = 0:1)
  f <- function(x) x
  
  f_right <- assert_between_boundaries(f, check_right)
  f_wrong <- assert_between_boundaries(f, check_wrong)
  expect_equal(f_right(x), x)
  expect_error(f_wrong(x))
})

test_that("assert_between_n_std returns the correct values", {
  set.seed(1)
  x <- data.frame(x = rnorm(100), y = rnorm(100))
  check_right <- list(x = 10, y = 10)
  check_wrong <- list(x = 2, y = 2)
  f <- function(x) x
  
  f_right <- assert_between_n_std(f, check_right)
  f_wrong <- assert_between_n_std(f, check_wrong)
  expect_equal(f_right(x), x)
  expect_error(f_wrong(x))
})

test_that("assert_between_boundaries and assert_between_n_std handles invalid types", {
  set.seed(1)
  x <- data.frame(x = runif(100), y = runif(100), z = runif(100), a = letters[1:25])
  f <- function(x) x
  
  check_err <- list(a = c(1, 25))
  f_err <- assert_between_boundaries(f, check_err)
  expect_error(f_err(x))
  f_err <- assert_between_n_std(f, check_err)
  expect_error(f_err(x))
  
  check_err <- list(x = 0:1, y = c(2, 0))
  f_err <- assert_between_boundaries(f, check_err)
  expect_error(f_err(x))
  
  check_err <- list(x = c(1, 25, 100))
  f_err <- assert_between_boundaries(f, check_err)
  expect_error(f_err(x))
  f_err <- assert_between_n_std(f, check_err)
  expect_error(f_err(x))
})

test_that("assert_col_types returns the correct values", {
  x <- data.frame(x = 1:26, y = letters, z = c(TRUE, FALSE), stringsAsFactors = FALSE)
  check_right <- list(x = "integer", y = "character")
  check_wrong <- list(x = "numeric", y = "numeric")
  f <- function(x) x
  
  f_right <- assert_col_types(f, check_right)
  f_wrong <- assert_col_types(f, check_wrong)
  expect_equal(f_right(x), x)
  expect_error(f_wrong(x))
  
  x <- data_frame(x = 1:10, y = letters[1:10], z = purrr::map(1:10, ~ c(.x, .x + 1)))
  check_right <- list(y = "character", z = "list")
  f_right <- assert_col_types(f, check_right)
  expect_is(f_right(x), "data.frame") # expect equal fails because of list column
})

test_that("assert_col_types handles invalid types", {
  x <- data.frame(x = 1:26, y = letters, z = c(TRUE, FALSE), stringsAsFactors = FALSE)
  f <- function(x) x
  
  check_err <- list(x = 1)
  f_err <- assert_col_types(f, check_err)
  expect_error(f_err(x))
  
  check_err <- list(x = c("logical", "character"))
  f_err <- assert_col_types(f, check_err)
  expect_error(f_err(x))
})

test_that("assert_none_missing returns the correct values", {
  set.seed(1)
  x <- data.frame(x = rnorm(10), y = sample(c(NA, 1), 10, replace = TRUE))
  check_right <- c("x")
  check_wrong <- c("x", "y")
  f <- function(x) x
  
  f_right <- assert_none_missing(f, check_right)
  f_wrong <- assert_none_missing(f, check_wrong)
  expect_equal(f_right(x), x)
  expect_error(f_wrong(x))
})

test_that("assert_unique returns the correct values", {
  x <- data.frame(x = 1:10, y = sample(0:1, 10, replace = TRUE))
  check_right <- c("x")
  check_wrong <- c("x", "y")
  f <- function(x) x
  
  f_right <- assert_unique(f, check_right)
  f_wrong <- assert_unique(f, check_wrong)
  expect_equal(f_right(x), x)
  expect_error(f_wrong(x))
})

test_that("assert_dimensions returns the correct values", {
  x <- data.frame(x = 1:10, y = sample(0:1, 10, replace = TRUE))
  check_right <- c(10, 2)
  check_wrong <- c(10, 4)
  f <- function(x) x
  
  f_right <- assert_dim(f, check_right)
  f_wrong <- assert_dim(f, check_wrong)
  expect_equal(f_right(x), x)
  expect_error(f_wrong(x))
  
  check_right <- c(10, NA)
  check_wrong <- c(NA, 4)
  f_right <- assert_dim(f, check_right)
  f_wrong <- assert_dim(f, check_wrong)
  expect_equal(f_right(x), x)
  expect_error(f_wrong(x))
  
  check_wrong <- c(12, NA)
  f_wrong <- assert_dim(f, check_wrong)
  expect_error(f_wrong(x))
})

test_that("assert_dim handles invalid types", {
  x <- data.frame(x = 1:10, y = sample(0:1, 10, replace = TRUE))
  f <- function(x) x
  
  check_err <- c(10, 2, 5)
  f_err <- assert_dim(f, check_err)
  expect_error(f_err(x))
  
  check_err <- c(NA, NA)
  f_err <- assert_dim(f, check_err)
  expect_error(f_err(x))
  
  expect_error(assert_dim())
  expect_error(assert_dim(1))
  expect_error(assert_dim(f))
  f2 <- function() 1
  f_err <- assert_dim(f2, 1:2)
  expect_error(f_err())
})

test_that("assert_margins_after returns the correct values", {
  x <- data.frame(x = 1:10, y = sample(0:1, 10, replace = TRUE))

  f_right <- assert_margins_after(head, margin = 'r', condition = 'l')
  expect_equal(f_right(x), head(x))
  f_right <- assert_margins_after(head, margin = 'r', condition = 'le')
  expect_equal(f_right(x), head(x))
  
  f_wrong <- assert_margins_after(head, margin = 'r', condition = 'e')
  expect_error(f_wrong(x))
  f_wrong <- assert_margins_after(head, margin = 'r', condition = 'g')
  expect_error(f_wrong(x))
  f_wrong <- assert_margins_after(head, margin = 'r', condition = 'ge')
  expect_error(f_wrong(x))
})

test_that("assert_rows_after handles invalid data types", {
  expect_error(assert_margins_after())
  expect_error(assert_margins_after(1))
  expect_error(assert_margins_after(head, margin = 'x', condition = 'e'))
  expect_error(assert_margins_after(head, margin = 'r', condition = 'x'))

  f <- function(x) head(mtcars)
  f_err <- assert_margins_after(f, margin = 'r', condition = 'e')
  expect_error(f_err(1))
  f <- function(x) 1
  f_err <- assert_margins_after(f, margin = 'r', condition = 'e')
  expect_error(f_err(iris))
})
