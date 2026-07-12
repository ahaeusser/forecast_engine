
.start <- Sys.time()
.step <- "100_pre_processing.R"

# Pre-processing ==============================================================

# Suppress warnings
options(warn = set_warn)

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
  library(furrr)
  library(tscv)
  library(echos)
  library(fable.prophet)
  library(fs)
  library(devtools)
  library(glue)
  library(crayon)
  library(progressr)
  library(rstudioapi)
})

source("functions/logging.R")
source("functions/utils.R")

# Create run name with date and time
run_name <- file_name(primary = run_name)

# Create sub directory for output
folder <- dir_create(path = glue("forecast/{run_name}"))

# Parallel computing
plan(multisession)

write_lines(
  x = log_platform(),
  file = glue("{folder}/{run_name}.txt"),
  append = TRUE
)

write_lines(
  x = log_time(text = .step, start = .start),
  file = glue("{folder}/{run_name}.txt"),
  append = TRUE
)

print(log_platform(ft_bold = TRUE))

print(
  log_time(
    text = .step, 
    start = .start,
    ft_bold = TRUE
  )
)
