
# Pre-processing ==============================================================

# Suppress warnings
options(warn = -1)

# Load relevant packages
suppressPackageStartupMessages({
  library(tidyverse)
  library(tsibble)
  library(fable)
  library(feasts)
  library(distributional)
  library(future)
  library(progressr)
  library(pryr)
  library(tictoc)
  library(furrr)
  library(tscv)
  library(echos)
  library(fasster)
  library(fs)
  library(log4r)
  library(devtools)
  library(glue)
  library(progressr)
  library(rstudioapi)
})

# Create run name with date and time
run_name <- create_name(primary = "output")

# Create subdirectory for output
folder <- dir_create(path = paste0("output", "/",  run_name))

# Create log file
log_file <- file_create(path = paste0(folder, "/", run_name, ".txt"))

# Start logging
logger <- logger(
  threshold = "INFO",
  appenders = list(
    file_appender(log_file),
    console_appender()
    )
)

info(
  logger = logger,
  message = paste0(
    "\n",
    log_header(),
    "\n"
    )
)

info(
  logger = logger,
  message = "START  script/100_pre_processing.R"
)

.start <- Sys.time()

# Change default location and time
Sys.setlocale("LC_TIME", "C")

# Parallel computing
plan(multisession)

info(
  logger = logger,
  message = paste0(
    "FINISH script/100_pre_processing.R",
    "\n",
    log_time(start = .start),
    "\n"
  )
)
