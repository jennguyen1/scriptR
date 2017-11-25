# scriptR

This package contains a variety of functions that simplify scripting in R. 

**Load Libraries**

* `install_scriptR()` installs scriptR from github
* `lib()` imports commonly used libraries

**File Manipulation and Creation**

* `ensure_requisite_folders()` ensures that a file path is available, otherwise creates it
* `write.out()` writes out dataframes to folder, depending on file extension

**Logging**

* `dictConfig()` intiates a logging session based off a list with configurations
* `start_logging()` initiates a logging session, using the logging package
* `exists_logfile()` and `get_logfile()` obtain information regarding the logfile(s)
* `logmisc()` extends the logging package by allowing logging of misc objects at the any specified level, such as dataframes
* `logerr()` logs an error with logging and then stops the program
* `print_cmd_args()` prints out command line arguments to console and logfile if it exists
* `process_args()` intiates a script by (1) start log, (2) process command line args, (3) log command line args

## Installation
`devtools::install_github("jennguyen1/scriptR")`

## Prerequisites
These functions depend on the following packages: logging, argparse, optparse, tidyverse, data.table. Other data wrangling packages are optional (see lib function).
