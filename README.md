# scriptR

This package contains a variety of functions for reproducible research in R. Provided are a variety of functions that simplify logging, asserting, 
scripting in R. 

**Load Libraries**

* `install_scriptR()` installs scriptR from github
* `lib()` imports commonly used libraries

**File Manipulation and Creation**

* `ensure_requisite_folders()` ensures that a file path is available, otherwise creates it
* `read.in()` reads in DFs with a specified function and options
* `write.out()` writes out DFs to folder, depending on file extension

**Logging**

* `dictConfig()` intiates a logging session based off a list with configurations
* `start_logging()` initiates a logging session, using the logging package
* `logging.*()` logging functions at variety of levels
* `print_cmd_args()` prints out command line arguments to console and logfile if it exists
* `process_args()` intiates a script by (1) start log, (2) process command line args, (3) log command line args

**Reporters**
* `report_function_name()` wrapper function, logs the function's name
* `report_args()` wrapper function, logs arguments passed to function
* `report_dim()` wrapper function, logs the dimensions before and after
* `report_na()` wrapper function, logs number of NA per column of the data frame

**Assertions**
* `assert_cols_in()` asserts that columns are inside a DF
* `assert_between_boundaries()` wrapper function, asserts specified columns of DF are between specified boundaries
* `assert_between_n_std()` wrapper function, asserts specified columns of DF are between specified standard deviations from the mean
* `assert_in()` wrapper function, asserts specified columns of DF have values that are in specified lists
* `assert_col_types()` wrapper function, asserts specified columns of DF are specified types
* `assert_dim()` wrapper function, asserts resulting DF has certain dimensions
* `assert_margins_after()` wrapper function, asserts resulting DF margins compare to incoming DF
* `assert_none_missing()` wrapper function, asserts specified columns of DF are not NA
* `assert_unique()` wrapper function, asserts specified columns of DF are unique

**Miscellaneous**
* `is_named_list()` checks whether a list is a uniquely named list

## Installation
`devtools::install_github("jennguyen1/scriptR")`

## Prerequisites
These functions depend on the following packages: logging, argparse, optparse, tidyverse, data.table. Other data wrangling packages are optional (see lib function).
