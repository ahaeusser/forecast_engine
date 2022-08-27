
# User settings ===============================================================

# Data set --------------------------------------------------------------------

input_file <- "data/M4_Monthly_42_1000_main.rds"

series_id <- "series"            # unique identifier for time series
value_id <- "value"              # identifier for measurement column
index_id <- "index"              # identifier for time index column

context <- list(
  series_id = series_id,
  value_id = value_id,
  index_id = index_id
)

# Setup for run ---------------------------------------------------------------

set_warn <- -1                  # warnings are suppressed
run_name <- "M4_Monthly_42_1000" # set a name for the run
Sys.setlocale("LC_TIME", "C")   # change default location and time

test_run <- TRUE                # test run or full run?
test_seed <- 234                # for reproducibility
n_test_splits <- 1              # number of random splits for testing
n_test_series <- 5              # number of random series for testing

# Setup for time series cross validation --------------------------------------

type <- "last"                  # type of initial training window
value <- 17                     # size for initial training window
n_ahead <- 18                   # size for testing window (forecast horizon)
n_skip <- 0                     # skip no observations
n_lag <- 0                      # no lag
mode <- "slide"                 # fixed window approach
exceed <- FALSE                 # out-of-sample forecasts?

# Evaluation of forecast accuracy ---------------------------------------------

set_metric <- "MAPE"            # Accuracy metric for overall summary
benchmark <- "NAIVE"           # Benchmark method for rMAE

# Modeling --------------------------------------------------------------------

periods <- c(12)                # seasonal periods
outlier <- FALSE                 # outlier adjustment
shift <- 200                    # constant shift to avoid negative values (DSHW)

models <- c(
  "NAIVE",
  "DRIFT",
  "SNAIVE",
  # "SNAIVE2",
  # "MEAN",
  # "MEDIAN",
  "TSLM",
  "ARIMA",
  "ETS",
  "THETA", 
  # "DSHW",
  # "TBATS",
  # "DHR-ARIMA",
  # "STL-NAIVE",
  # "STL-ARIMA",
  # "STL-ETS",
  # "ELM",
  "NNETAR",
  "PROPHET",
  # "FASSTER",
  # "EXPERT".
  "ESN"
  )

# Echo State Networks ---------------------------------------------------------

lags <- list(c(1))
fourier <- NULL
xreg <- NULL
dy <- NULL
dx <- 0
inf_crit <- "bic"
operator <- "mean"
n_seed <- 42
alpha <- 1
rho <- 1
n_states <- 100 # 120
n_models <- 200 # 240
density <- 0.1 # 0.05
scale_win <- 0.5 # 0.5
scale_wres <- 0.5 # 0.5
scale_inputs <- c(-0.5, 0.5)

# Save user_settings as list --------------------------------------------------

user_settings <- mget(ls())
