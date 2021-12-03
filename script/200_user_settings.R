
info(
  logger = logger,
  message = "START  script/200_user_settings.R"
)

.start <- Sys.time()

# User settings ===============================================================

# Identifier of data set ------------------------------------------------------

input_file <- "data/elec_price.rds"

series_id <- "series"            # unique identifier for time series
series_id <- "bidding_zone"
value_id <- "value"              # identifier for measurement column
index_id <- "time"               # identifier for time index column

context <- list(
  series_id = series_id,
  value_id = value_id,
  index_id = index_id
)

# Setup for test run ----------------------------------------------------------

test_run <- TRUE                # test run or full run?
test_seed <- 123                # for reproducibility
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
  # "DSHW",
  # "TBATS",
  # "DHR-ARIMA",
  # "STL-NAIVE",
  "STL-ARIMA",
  # "STL-ETS",
  # "ELM",
  # "FASSTER",
  "EXPERT"
  # "ESN"
  )

# Echo State Networks ---------------------------------------------------------

# lags = list(c(1:6, 12))
# fourier = NULL
# xreg = NULL
# dy = NULL
# dx = 0
# inf_crit = "aic"
# n_seed = 42
# alpha <- c(0.5, 0.7, 0.9, 1)
# rho <- c(0.5, 1)
# n_states <- 50
# density = 0.1
# scale_win = 0.5
# scale_wres = 0.5
# scale_inputs = c(-1, 1)

# Save user_settings as list --------------------------------------------------

user_settings <- mget(ls())

save(
  object = user_settings,
  file = paste0(folder, "/", "user_settings.rda")
  )

info(
  logger = logger,
  message = paste0(
    "FINISH script/200_user_settings.R",
    "\n",
    log_time(start = .start),
    "\n"
  )
)
