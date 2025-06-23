
# Test hyperparameters ========================================================

# Configuration ---------------------------------------------------------------

# Load relevant packages
library(echos)
library(tidyverse)
library(future)
library(progressr)
library(furrr)
library(tictoc)
library(tscv)

# Suppress warnings
set_warn <- -1
options(warn = set_warn)

# Parallel computing
plan(multisession)

# Frequency of dataset
# set_freq <- "monthly"
set_freq <- "quarterly"

outlier <- TRUE

# Directory and file names, forecast horizon and period
if (set_freq == "monthly") {
  file_main <- "data/main_mth_sample.rds"
  n_ahead <- 18
  periods <- 12
}

if (set_freq == "quarterly") {
  file_main <- "data/main_qtr_sample.rds"
  n_ahead <- 8
  periods <- 4
}

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


# Pre-process input -----------------------------------------------------------

# Hyperparameters
pars <- read_delim(
  file = "analysis/hyperparameters.csv",
  delim = ";",
  escape_double = FALSE,
  locale = locale(decimal_mark = ",", grouping_mark = "."),
  trim_ws = TRUE
)

# Raw dataset
main_frame <- readRDS(file = file_main)
series_name <- unique(main_frame[["series"]])
# series_name <- series_name[1:10]

# Prepare data as list
main_frame <- main_frame %>%
  filter(series %in% series_name)

# Adjust outliers

if (outlier == TRUE) {
  main_frame <- main_frame %>%
    group_by(series) %>%
    mutate(
      value = smooth_outlier(
        x = value,
        periods = periods)) %>%
    ungroup()
}

pars <- pars %>%
  mutate(model = paste0("ESN-", model))

# Combinations of pars and series
full_grid <- expand_grid(
  pars,
  series_name
)

# Modify column Model and add identifier
full_grid <- full_grid %>%
  mutate(ID = row_number(), .before = model)

#Number of parameters
n_pars <- nrow(pars)
# Number of iterations
n_steps <- nrow(full_grid)


# Run models with different hyperparameters -----------------------------------
tic()
pars_frame <- with_progress({
  future_map_dfr(
    .x = seq_len(n_steps),
    .f = ~{
      
      model_id <- full_grid[["model"]][.x]
      xseries <- full_grid[["series_name"]][.x]
      alpha <- full_grid[["alpha"]][.x]
      rho <- full_grid[["rho"]][.x]
      inf_crit <- full_grid[["inf_crit"]][.x]
      lambda_lower <- full_grid[["lambda_lower"]][.x]
      lambda_upper <- full_grid[["lambda_upper"]][.x]
      n_states <- full_grid[["n_states"]][.x]
      
      # Extract series
      x <- main_frame %>%
        filter(series == xseries) %>%
        pull(value)
      
      # Number of observations
      n_obs <- length(x)
      # Number of observations for training
      n_train <- n_obs - n_ahead
      # Prepare train and test data
      xtrain <- x[(1:n_train)]
      xtest <- x[((n_train+1):n_obs)]
      
      # Train ESN model
      xmodel <- train_esn(
        y = xtrain,
        inf_crit = inf_crit,
        alpha = alpha,
        rho = rho,
        lambda = c(lambda_lower, lambda_upper),
        n_states = min(floor(n_states*n_train), 200)
      )
      
      # Forecast ESN model
      xfcst <- forecast_esn(
        object = xmodel,
        n_ahead = n_ahead,
        n_sim = NULL
      )
      
      smape <- smape_vec(
        truth = xtest,
        estimate = xfcst[["point"]]
      )
      
      mase <- mase_vec(
        test = xtest,
        forecast = xfcst[["point"]],
        train = xtrain,
        periods = periods
      )
      
      tibble(
        model = model_id,
        series = xseries,
        alpha = alpha,
        rho = rho,
        inf_crit = inf_crit,
        lambda_lower = lambda_lower,
        lambda_upper = lambda_upper,
        n_states = n_states,
        smape = smape,
        mase = mase
      )
    },
    .progress = TRUE
  )
})
toc()

# Post-process output ---------------------------------------------------------

# Average sMAPE and MASE and combine with hyperparameters
pars_summary <- pars_frame %>%
  group_by(model) %>%
  summarise(
    avg_smape = mean(smape, na.rm = TRUE),
    avg_mase = mean(mase, na.rm = TRUE)) %>%
  ungroup()

pars_summary <- left_join(
  x = pars,
  y = pars_summary,
  by = "model"
  )

pars_summary <- pars_summary %>%
  arrange(avg_mase)

# Saving workspace to the path
save.image(paste0("analysis/pars_",set_freq, ".RData"))



# # Case (1) WITHOUT library(future) ============================================
# 
# library(echos)
# library(fabletools)
# library(tidyverse)
# 
# # Case (1.a) (base function) --------------------------------------------------
# xa <- as.numeric(AirPassengers)
# modela <- train_esn(xa)
# summary(modela)
# fcsta <- forecast_esn(modela, n_ahead = 18)
# fcsta <- fcsta$point
# 
# # Case (1.b) (tidy function) --------------------------------------------------
# xb <- as_tsibble(AirPassengers)
# modelb <- xb %>%
#   model("ESN" = ESN(value))
# report(modelb)
# fcstb <- modelb %>% forecast(h = 18)
# fcstb <- fcstb$.mean
# 
# # Comparison ------------------------------------------------------------------
# all.equal(fcsta, fcstb)
# 
# 
# # Case (2) WITH library(future) ===============================================
# 
# library(echos)
# library(fabletools)
# library(tidyverse)
# library(future)
# 
# # Case (2.a) (base function) --------------------------------------------------
# xa <- as.numeric(AirPassengers)
# modela <- train_esn(xa)
# summary(modela)
# fcsta <- forecast_esn(modela, n_ahead = 18)
# fcsta <- fcsta$point
# 
# # Case (2.b) (tidy function) --------------------------------------------------
# xb <- as_tsibble(AirPassengers)
# modelb <- xb %>%
#   model("ESN" = ESN(value))
# report(modelb)
# fcstb <- modelb %>% forecast(h = 18)
# fcstb <- fcstb$.mean
# 
# # Comparison ------------------------------------------------------------------
# all.equal(fcsta, fcstb)



# smape_vec <- function(truth,
#                       estimate,
#                       na_rm = TRUE) {
#   percent_scale <- 100
#   numer <- abs(estimate - truth)
#   denom <- (abs(truth) + abs(estimate)) / 2
#   mean(numer / denom, na.rm = na_rm) * percent_scale
# }
# 
# library(echos)
# library(tidyverse)
# 
# n_ahead <- 18
# main_frame <- readRDS("C:/Users/Alexander Haeusser/Projects/forecast_engine/data/main_mth_sample.rds")
# 
# # Extract series
# x <- main_frame %>%
#   filter(series == "M11505") %>%
#   pull(value)
# 
# # Number of observations
# n_obs <- length(x)
# # Number of observations for training
# n_train <- n_obs - n_ahead
# # Prepare train and test data
# xtrain <- x[(1:n_train)]
# xtest <- x[((n_train+1):n_obs)]
# 
# model <- train_esn(x)
# summary(model)
# fcst <- forecast_esn(model, n_ahead = 18)
# fcst <- fcst$point
# fcst
# 
# 
# fcst2 <- main_frame %>%
#   filter(series == "M11505") %>%
#   as_tsibble() %>%
#   model("ESN" = ESN(value)) %>%
#   forecast(h = n_ahead)
# 
# 
# fcst
# fcst2$.mean




