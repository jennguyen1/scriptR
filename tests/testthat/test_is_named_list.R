context("Columns are Inside Data Frame")

test_that("is_named_list returns correct values", {
  expect_true(is_named_list(iris))
  
  l <- list(a = 1, b = 2, c = 3)
  expect_true(is_named_list(l))
  
  l <- list(1,2,3)
  expect_false(is_named_list(l))
  
  l <- list(a = 1, b = 2, a = 3)
  expect_false(is_named_list(l))
})

test_that("is_named_list handles invalid types", {
  expect_error(is_named_list(1:10))
  expect_error(is_named_list(letters))
  expect_error(is_named_list(mean))
})
