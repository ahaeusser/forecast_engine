
# User settings ===============================================================

# Data set --------------------------------------------------------------------

input_file <- "data/elec_price.rds"

series_id <- "bidding_zone"      # unique identifier for time series
value_id <- "value"              # identifier for measurement column
index_id <- "time"               # identifier for time index column

context <- list(
  series_id = series_id,
  value_id = value_id,
  index_id = index_id
)

# Setup for run ---------------------------------------------------------------

set_warn <- -1                  # warnings are suppressed
run_name <- "test_price"        # set a name for the run
Sys.setlocale("LC_TIME", "C")   # change default location and time

test_run <- TRUE                # test run or full run?
test_seed <- 678                # for reproducibility
n_test_splits <- 10             # number of random splits for testing
n_test_series <- 4              # number of random series for testing

# Setup for time series cross validation --------------------------------------

type <- "first"                 # type of initial training window
value <- 2400                   # size for initial training window
n_ahead <- 24                   # size for testing window (forecast horizon)
n_skip <- 23                    # skip 23 observations
n_lag <- 0                      # no lag
mode <- "slide"                 # fixed window approach
exceed <- FALSE                 # out-of-sample forecasts?

# Evaluation of forecast accuracy ---------------------------------------------

set_metric <- "rMAE"            # Accuracy metric for overall summary
benchmark <- "SNAIVE"           # Benchmark method for rMAE

# Modeling --------------------------------------------------------------------

periods <- c(24, 168)           # seasonal periods
outlier <- TRUE                 # outlier adjustment
shift <- 200                    # constant shift to avoid negative values (DSHW)
n_models <- 500                 # number of models for feature selection (ARX)

models <- c(
  # "NAIVE",
  # "DRIFT",
  "SNAIVE",
  "SNAIVE2",
  # "MEAN",
  # "MEDIAN",
  # "TSLM",
  # "ARIMA",
  # "ETS",
  # "THETA", 
  # "DSHW",
  # "TBATS",
  # "DHR-ARIMA",
  "STL-NAIVE",
  "STL-ARIMA",
  "STL-ETS",
  # "ELM",
  # "FASSTER",
  "ARX",
  # "EXPERT",
  "ESN"
  )

# Echo State Networks ---------------------------------------------------------

lags <- list(c(1))
fourier <- NULL
xreg <- NULL
dy <- NULL
dx <- 1
inf_crit <- "aic"
n_seed <- 42
alpha <- 1
rho <- 1
n_states <- 500
n_models <- 500
n_vars <- 50
density <- 0.1
scale_win <- 0.5
scale_wres <- 0.5
scale_inputs <- c(-1, 1)

# Save user_settings as list --------------------------------------------------

user_settings <- mget(ls())
