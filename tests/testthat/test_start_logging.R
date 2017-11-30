context("Configure Logging")

library(logging)
library(readr)

test_that("make_formatter handles missing and invalid data", {
  expect_error(make_formatter())
  expect_error(make_formatter("hi"))
  
  check_error <- function(func, ...){
    blank <- list(...)
    expect_error(make_formatter(blank))
  }
  
  check_error(f = "{message}", s = "{")
  check_error(format = "{message}")
  check_error(format = 1, style = "{")
  check_error(format = "{message}", style = "%")
})

test_that("make_handler handles missing and invalid data", {
  expect_error(make_handler())
  expect_error(make_handler("hi"))
  expect_error(make_handler(log_config$handlers$console))
  expect_error(make_handler(log_config$handlers$console, "", log_config$formatters))
  expect_error(make_handler(log_config$handlers$console, "a"))
  expect_error(make_handler(log_config$handlers$console, "a", NULL))
  
  check_error <- function(file_in_name = FALSE, ...){
    blank <- list(...)
    name <- if(file_in_name) "a_file" else "a"
    expect_error(make_handler(blank, name, log_config$formatters))
  }
  
  check_error(file_in_name = FALSE, c = "", f = "", l = "")
  check_error(file_in_name = FALSE, class = "", f = "", l = "")
  check_error(file_in_name = FALSE, class = "", formatter = "", l = "")
  check_error(file_in_name = FALSE, class = "", formatter = 1, level = "")

  check_error(file_in_name = FALSE, class = "logging.StreamHandler", formatter = "error", stream = "ext://sys.stdout", level = "NOTSET")
  check_error(file_in_name = FALSE, class = "logging.StreamHandler", formatter = "brief", stream = "ext://sys.stdout", level = "not a thing")

  check_error(file_in_name = TRUE, class = "logging.FileHandler", formatter = "brief", level = "INFO", mode = "w")
  check_error(file_in_name = TRUE, class = "logging.FileHandler", formatter = "brief", level = "INFO", filename = "~/file.log")
  check_error(file_in_name = TRUE, class = "logging.FileHandler", formatter = "brief", level = "INFO", filename = "~/file.log", mode = "i")
  check_error(file_in_name = FALSE, class = "logging.FileHandler", formatter = "brief", level = "INFO", filename = "~/file.log", mode = "w")

  check_error(file_in_name = FALSE, class = "not a thing", formatter = "brief", level = "NOTSET")
})

test_that("dictConfig handles missing and invalid data", {
  logReset()
  expect_error(dictConfig())
  expect_error(dictConfig(list(r = "", h = "", f = "")))
  expect_error(dictConfig(list(root = "", h = "", f = "")))
  expect_error(dictConfig(list(root = "", handlers = "", f = "")))
  
  logReset()
  expect_error(dictConfig(list(root = "", handlers = "", formatters = "")))
  expect_error(dictConfig(list(root = "", handlers = list(a = 1), formatters = "")))
  expect_error(dictConfig(list(root = "", handlers = list(a = 1), formatters = list(a = 1))))
  
  logReset()
  check_error <- function(...){
    blank <- log_config
    blank$root <- list(...)
    expect_error(dictConfig(blank))
  }
  check_error(l = "", h = "")
  check_error(level = "", h = "")
  check_error(level = "not a thing", handlers = "")
  check_error(level = "NOTSET", handlers = c("console", "not a thing"))
  logReset()
})

test_that("start_logging correctly intiates default logging", {
  logReset()
  expect_null(loginfo("hi"))
  start_logging()
  expect_output(loginfo("hi"))
  log <- getLogger()
  base_handlers <- names(log$handlers)
  expect_equal(base_handlers, "console")
  expect_equal(log$level, loglevels[1])
  expect_equal(log$handlers$console$level, loglevels[1])
  logReset()
})

test_that("start_logging correctly intiates customized logging", {
  # setup
  blank <- log_config
  names(blank$handlers) <- c("console", "file1", "file2", "file3")
  blank$handlers$file1$filename <- "file1.log"
  blank$handlers$file2$filename <- "file2.log"
  blank$handlers$file1$mode <- "w"
  blank$handlers$file2$mode <- "a"
  blank$handlers$file2$level <- "DEBUG"
  blank$root$handlers <- c("console", "file1", "file2")
  write(rjson::toJSON(blank), file = "file.json")
  write("hello world", file = "test.json")

  # logging file specs
  logReset()
  expect_error(start_logging("not a thing"))
  logReset()
  expect_error(start_logging("test.json"))
  
  # logging init
  logReset()
  start_logging("file.json")
  log <- getLogger()
  base_handlers <- names(log$handlers)
  expect_equal(base_handlers, c("console", "file1", "file2"))
  expect_equal(log$level, loglevels["NOTSET"])
  expect_equal(log$handlers$console$level, loglevels["NOTSET"])
  expect_equal(log$handlers$file1$level, loglevels["INFO"])
  expect_equal(log$handlers$file2$level, loglevels["DEBUG"])
  expect_equal(log$handlers$file1$file, "file1.log")
  expect_equal(log$handlers$file2$file, "file2.log")
  
  # check file existence
  logwarn("hi")
  expect_true(file.exists("file1.log"))
  expect_true(file.exists("file2.log"))
  
  # shutdown
  file.remove("file.json")
  file.remove("test.json")
  file.remove("file1.log")
  file.remove("file2.log")
})

