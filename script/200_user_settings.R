
info(
  logger = logger,
  message = "START  script/200_user_settings.R"
)

.start <- Sys.time()

# User settings ===============================================================

# set_series <- "M23824"
# set_series <- "M23898"
# set_series <- "M30884"
# set_series <- "M35105"
# set_series <- "M35161"
# set_series <- "M14395"
# set_series <- "M13450"
# set_series <- "M18491"
# set_series <- "M30851"
# set_series <- "M45533"
# set_series <- "M15320"
# set_series <- "M5187"
set_series <- "M5535"
# set_series <- "M5937"
# set_series <- "M34487"
# set_series <- "M10960"
# set_series <- "M12123"
# set_series <- "M12717"
# set_series <- "M13027"
# set_series <- "M23117"
# set_series <- "M23679"
# set_series <- "M3697"


test_run <- FALSE

# Setup for time series cross validation --------------------------------------


data <- M4_monthly_data %>%
  filter(series_id == set_series)

n_obs <- nrow(data)
n_init <- floor(n_obs * 0.8)
n_ahead <- 18
mode <- "slide"
n_skip <- 0
n_lag <- 0
period <- c(12)

# n_init <- 2400               # size for training window
# n_ahead <- 24                # size for testing window (forecast horizon)
# mode <- "slide"              # fixed window approach
# n_skip <- 23                 # skip 23 observations
# n_lag <- 0                   # no lag

# Modeling --------------------------------------------------------------------

# period <- c(24, 168)         # seasonal periods
outlier <- FALSE              # outlier adjustment
# shift <- 200                 # constant shift to avoid negative values (DSHW)

# models <- c(
#   "ESN",
#   "SNAIVE",
#   "SNAIVE2",
#   "STL-NAIVE",
#   "STL-ARIMA",
#   "STL-ETS",
#   "EXPERT",
#   "DHR-ARIMA",
#   "DSHW",
#   "TBATS",
#   "ELM",
#   "FASSTER"
# )

models <- c(
  "ESN",
  "ARIMA",
  "STL-ARIMA",
  "STL-NAIVE"
  )

# Echo State Networks ---------------------------------------------------------

lags = list(c(1:6, 12))
fourier = list(c(12), c(6))
xreg = NULL
dy = 1
dx = 0
inf_crit = "aic"
n_seed = 42
alpha = 0.7
rho = 1
density = 0.1
scale_win = 0.5
scale_wres = 0.5
scale_inputs = c(-1, 1)

# Create meta_object ----------------------------------------------------------

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
