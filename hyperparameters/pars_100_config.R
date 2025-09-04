
# Hyperparameter Sweep ########################################################

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

# Symmetric Mean Absolute Percentage Error
smape_vec <- function(truth,
                      estimate,
                      na_rm = TRUE) {
  percent_scale <- 100
  numer <- abs(estimate - truth)
  denom <- (abs(truth) + abs(estimate)) / 2
  mean(numer / denom, na.rm = na_rm) * percent_scale
}

mase_vec <- function(test,
                     forecast,
                     train,
                     periods = 1,
                     na_rm = TRUE) {
  # Numerator (out-of-sample MAE)
  numerator <- mean(abs(test - forecast), na.rm = na_rm)
  # Denominator (in-sample m-step naive MAE)
  naive <- lag(x = train, n = periods)
  denominator <- mean(abs(train - naive), na.rm = na_rm)
  # MASE
  numerator / denominator
}

paste_names <- function(x, n) {
  x <- paste0(
    x,"-",
    formatC(
      x = 1:n,
      width = nchar(n),
      flag = "0"))
  
  return(x)
}

# Hyperparameters -------------------------------------------------------------

# Full grid with all combinations
pars <- expand_grid(
  inf_crit = c("aic", "aicc", "bic", "hqc"),
  alpha = seq(0.1, 1.0, 0.1),
  rho = seq(0.4, 1.2, 0.1),
  tau = c(0.2, 0.4, 0.6)
)

# Number of combinations
n_pars <- nrow(pars)
# Add unique model identifier and filter
pars <- pars %>%
  mutate(model = paste_names(x = "ESN", n_pars), .before = inf_crit)

