context("Logging")

library(logging)
library(readr)
library(stringr)

setup <- function(){
  blank <- log_config
  names(blank$handlers) <- c("console", "file1", "file2", "file3")
  blank$handlers$file1$filename <- "file1.log"
  blank$handlers$file2$filename <- "file2.log"
  blank$handlers$file1$mode <- "w"
  blank$handlers$file2$mode <- "a"
  blank$handlers$file2$level <- "INFO"
  blank$handlers$file2$level <- "DEBUG"
  blank$root$handlers <- c("console", "file1", "file2")
  write(rjson::toJSON(blank), file = "file.json")
  if(file.exists("file1.log")) file.remove("file1.log")
  if(file.exists("file2.log")) file.remove("file2.log")
}
shutdown <- function(){
  logReset()
  file.remove("file.json")
  file.remove("file1.log")
  file.remove("file2.log")
}

setup()

test_that("logging accurately logs levels", {
  logReset()
  start_logging("file.json")
  loginfo("hi")
  logdebug("hi")
  f1 <- read_lines("file1.log")
  f2 <- read_lines("file2.log")
  expect_equal(length(f1), 1)
  expect_equal(length(f2), 2)
})

test_that("logging accurately resets log files", {
  logReset()
  start_logging("file.json")
  logwarn("hi")
  f1 <- read_lines("file1.log")
  f2 <- read_lines("file2.log")
  expect_equal(length(f1), 1)
  expect_equal(length(f2), 3)
})

test_that("exists_logfile and get_logfile returns corrrect values", {
  expect_true(exists_logfile())
  expect_equal(get_logfile(), c("file1.log", "file2.log"))
})

test_that("logerr throws an error", {
  expect_error(expect_output(logerr("hello world")))
})

shutdown()

setup()

test_that("log_misc logs objects", {
  logReset()
  start_logging("file.json")
  logmisc(1:10)
  logmisc(11:20, "DEBUG")
  logmisc(head(iris))
  f1 <- read_lines("file1.log")
  f2 <- read_lines("file2.log")
  
  detect123 <- function(x) any(str_detect(x, "1\\s+2\\s+3"))
  expect_true(detect123(f1))
  expect_true(detect123(f2))
  
  detect10s <- function(x) any(str_detect(x, "14\\s+15\\s+16"))
  expect_false(detect10s(f1))
  expect_true(detect10s(f2))
  
  detectdf <- function(x) any(str_detect(x, "Species"))
  expect_true(detectdf(f1))
  expect_true(detectdf(f2))
})

shutdown()
