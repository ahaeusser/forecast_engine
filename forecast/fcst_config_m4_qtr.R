
# User settings ===============================================================

# Data set --------------------------------------------------------------------

input_file <- "data/main_qtr_forecast.rds"

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
run_name <- "quarterly_fcst"      # set a name for the run
Sys.setlocale("LC_TIME", "C")     # change default location and time

test_run <- FALSE                 # test run or full run?

# Setup for time series cross validation --------------------------------------

type <- "last"                    # type of initial training window
value <- 7                        # size for initial training window
n_ahead <- 8                      # size for testing window (forecast horizon)
n_skip <- 0                       # skip no observations
n_lag <- 0                        # no lag
mode <- "slide"                   # fixed window approach
exceed <- FALSE                   # out-of-sample forecasts?

# Evaluation of forecast accuracy ---------------------------------------------

set_metric <- "sMAPE"             # Accuracy metric for overall summary
benchmark <- NULL                 # Benchmark method for rMAE

# Modeling --------------------------------------------------------------------

periods <- c(4)                   # seasonal periods
outlier <- TRUE                   # outlier adjustment
shift <- 200                      # constant shift to avoid negative values (DSHW)

models <- c(
  "NAIVE",
  "DRIFT",
  "SNAIVE",
  "MEAN",
  "ARIMA",
  "ETS",
  "THETA", 
  "TBATS",
  "ESN"
)

# Save user_settings as list --------------------------------------------------

user_settings <- mget(ls())
