
# User settings ===============================================================

# Data set --------------------------------------------------------------------

input_file <- "data/main_qtr_sample.rds"

series_id <- "series"             # unique identifier for time series
value_id <- "value"               # identifier for measurement column
index_id <- "index"               # identifier for time index column

context <- list(
  series_id = series_id,
  value_id = value_id,
  index_id = index_id
)

# Setup for run ---------------------------------------------------------------

set_warn <- -1                    # warnings are suppressed
run_name <- "M4_Quarterly_Sample" # set a name for the run
Sys.setlocale("LC_TIME", "C")     # change default location and time

test_run <- FALSE                 # test run or full run?
test_seed <- 234                  # for reproducibility
n_test_splits <- 1                # number of random splits for testing
n_test_series <- 5                # number of random series for testing

# Setup for time series cross validation --------------------------------------

type <- "last"                    # type of initial training window
value <- 7                       # size for initial training window
n_ahead <- 8                     # size for testing window (forecast horizon)
n_skip <- 0                       # skip no observations
n_lag <- 0                        # no lag
mode <- "slide"                   # fixed window approach
exceed <- FALSE                   # out-of-sample forecasts?

# Evaluation of forecast accuracy ---------------------------------------------

set_metric <- "sMAPE"              # Accuracy metric for overall summary
benchmark <- "NAIVE"              # Benchmark method for rMAE

# Modeling --------------------------------------------------------------------

periods <- c(4)                  # seasonal periods
outlier <- TRUE                  # outlier adjustment
shift <- 200                      # constant shift to avoid negative values (DSHW)

models <- c(
  "NAIVE",
  "DRIFT",
  "SNAIVE",
  # "MEAN",
  # "MEDIAN",
  "TSLM",
  "ARIMA",
  "ETS",
  "THETA", 
  "TBATS",
  # "STL-NAIVE",
  # "STL-ARIMA",
  # "STL-ETS",
  # "ELM",
  # "MLP",
  # "NNETAR",
  # "PROPHET",
  "ESN"
)

# Echo State Networks ---------------------------------------------------------

lags <- list(c(1))
fourier <- NULL
xreg <- NULL
dy <- NULL
dx <- 0
inf_crit <- "bic"
n_seed <- 42
alpha <- 1
rho <- 1
n_states <- NULL
n_models <- NULL
density <- 0.5
lambda <- c(1e-4, 2)
scale_win <- 0.5
scale_wres <- 0.5
scale_inputs <- c(-0.5, 0.5)

# Save user_settings as list --------------------------------------------------

user_settings <- mget(ls())
