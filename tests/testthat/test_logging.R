context("Logging")

library(futile.logger)
library(readr)
library(stringr)

reset_log <- function() logging.loggers <- NULL
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
  reset_log()
  file.remove("file.json")
  file.remove("file1.log")
  file.remove("file2.log")
}

setup()

test_that("logging accurately logs levels", {
  reset_log()
  start_logging("file.json")
  logging.info("hi from info")
  logging.debug("hi from debug")
  f1 <- read_lines("file1.log")
  f2 <- read_lines("file2.log")
  expect_equal(length(f1), 1)
  expect_equal(length(f2), 2)
})

test_that("logging accurately resets log files", {
  reset_log()
  start_logging("file.json")
  logging.warning("hi")
  f1 <- read_lines("file1.log")
  f2 <- read_lines("file2.log")
  expect_equal(length(f1), 1)
  expect_equal(length(f2), 3)
})

shutdown()

setup()

test_that("logging objects", {
  reset_log()
  start_logging("file.json")
  logging.info(1:10)
  logging.debug(11:20)
  logging.info(head(iris))
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
