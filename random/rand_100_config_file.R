
# Random Initialization #######################################################

# Configuration ---------------------------------------------------------------

# Load relevant packages
library(echos)
library(tidyverse)
library(future)
library(progressr)
library(furrr)
library(tictoc)
library(tscv)
library(gt)

# Suppress warnings
set_warn <- -1
options(warn = set_warn)

# Functions -------------------------------------------------------------------

source("R/mase_vec.R")
source("R/paste_names.R")

# Seeds for random values -----------------------------------------------------
# (seed 42 is default and must be included)
n_seeds <- 30 # number of seeds
set.seed(42)

seeds <- c(
  42,
  sample(
    x = setdiff(1:999, 42),
    size = n_seeds - 1,
    replace = FALSE
  )
)

# Add unique model identifier and filter
seeds <- seeds |>
  as_tibble_col(column_name = "seed") |>
  mutate(
    model = paste_names(x = "ESN", n = n_seeds),
    .before = seed)
