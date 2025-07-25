
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

# Number of random hyperparameters
n_pars <- 200

# Frequency of dataset
set_freq <- "monthly"
# set_freq <- "quarterly"

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

paste_names <- function(x, n) {
  x <- paste0(
    x,"-",
    formatC(
      x = 1:n,
      width = nchar(n),
      flag = "0"))
  
  return(x)
}


# Pre-process input -----------------------------------------------------------

# Hyperparameters
# pars <- read_delim(
#   file = "analysis/hyperparameters.csv",
#   delim = ";",
#   escape_double = FALSE,
#   locale = locale(decimal_mark = ",", grouping_mark = "."),
#   trim_ws = TRUE
# )


set.seed(123)
model <- paste_names(x = "ESN", n_pars)
inf_crit <- sample(x = c("bic", "aic", "aicc", "hqc"), size = n_pars, replace = TRUE)
alpha <- sample(x = seq(0.1, 1.0, 0.05), size = n_pars, replace = TRUE)
rho <- sample(x = seq(0.5, 1.5, 0.1), size = n_pars, replace = TRUE)
tau <- sample(x = c(0.2, 0.4, 0.6, 0.8, 1.0), size = n_pars, replace = TRUE)

pars <- tibble(
  model = model,
  inf_crit = inf_crit,
  alpha = alpha,
  rho = rho,
  tau = tau
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


# Combinations of pars and series
full_grid <- expand_grid(
  pars,
  series_name
)

# Modify column Model and add identifier
full_grid <- full_grid %>%
  mutate(ID = row_number(), .before = model)

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
      inf_crit <- full_grid[["inf_crit"]][.x]
      alpha <- full_grid[["alpha"]][.x]
      rho <- full_grid[["rho"]][.x]
      tau <- full_grid[["tau"]][.x]
      
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
        n_states = min(floor(tau*n_train), 200)
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
        inf_crit = inf_crit,
        alpha = alpha,
        rho = rho,
        tau = tau,
        smape = smape,
        mase = mase
      )
    },
    .progress = TRUE
  )
})
toc()

# Saving workspace to the path
save.image(paste0("analysis/pars_",set_freq, ".RData"))


# File documentation ==========================================================

# 20250621_pars_monthly: Monthly dataset WITHOUT outlier adjustment 
# 20250621_pars_quarterly: Quarterly dataset WITHOUT outlier adjustment

# 20250624_pars_monthly: Monthly dataset WITH outlier adjustment 
# 20250623_pars_quarterly: Quarterly dataset WITH outlier adjustment 

# =============================================================================


# Post-process output ---------------------------------------------------------

library(gt)
library(tidyverse)

# Tables main text ------------------------------------------------------------

# Average sMAPE and MASE and combine with hyperparameters
pars_summary <- pars_frame %>%
  group_by(model) %>%
  summarise(
    mase_mean    = round(mean(mase, na.rm = TRUE), 3),
    mase_median  = round(median(mase, na.rm = TRUE), 3),
    smape_mean   = round(mean(smape, na.rm = TRUE), 3),
    smape_median = round(median(smape, na.rm = TRUE), 3)
    ) %>%
  ungroup()

pars_summary <- left_join(
  x = pars,
  y = pars_summary,
  by = "model"
)

pars_summary <- pars_summary %>%
  arrange(mase_mean) %>%
  mutate(
    inf_crit = recode(
      inf_crit,
      "aic" = "AIC",
      "bic" = "BIC",
      "aicc" = "AICc",
      "hqc" = "HQC"
    )
  )

# Create table as LaTeX code
pars_summary %>%
  slice_head(n = 30) %>%
  mutate(rank = row_number()) %>%
  select(rank, model, inf_crit, alpha, rho, tau, mase_mean, mase_median, smape_mean, smape_median) %>%
  gt() %>%
  as_latex() %>%
  as.character() %>%
  cat()




# Tables appendix -------------------------------------------------------------

# Descriptive statistics
pars_summary <- pars_frame %>%
  group_by(model) %>%
  summarise(
    "mase_min"     = round(min(mase, na.rm = TRUE), 3),
    "mase_q1"      = round(quantile(x = mase, probs = 0.25), 3),
    "mase_mean"    = round(mean(mase, na.rm = TRUE), 3),
    "mase_median"  = round(median(mase, na.rm = TRUE), 3),
    "mase_q3"      = round(quantile(x = mase, probs = 0.75), 3),
    "mase_max"     = round(max(mase, na.rm = TRUE), 3),
    "mase_std"     = round(sd(mase, na.rm = TRUE), 3),
    "smape_min"    = round(min(smape, na.rm = TRUE), 3),
    "smape_q1"     = round(quantile(x = smape, probs = 0.25), 3),
    "smape_mean"   = round(mean(smape, na.rm = TRUE), 3),
    "smape_median" = round(median(smape, na.rm = TRUE), 3),
    "smape_q3"     = round(quantile(x = smape, probs = 0.75), 3),
    "smape_max"    = round(max(smape, na.rm = TRUE), 3),
    "smape_std"    = round(sd(smape, na.rm = TRUE), 3)
  ) %>%
  ungroup()

# Create table as LaTeX code
pars_summary %>%
  gt() %>%
  as_latex() %>%
  as.character() %>%
  cat()



# To Do: Rework
# Create model reference table as LaTeX code
pars %>%
  mutate(rank = row_number()) %>%
  select(model, inf_crit, alpha, rho, tau) %>%
  gt() %>%
  as_latex() %>%
  as.character() %>%
  cat()













# # Tables appendix -------------------------------------------------------------
# 
# metric <- "mase"
# criterion <- "bic"
# 
# # Descriptive statistics
# pars_summary <- pars_frame %>%
#   select(model, series, alpha, rho, inf_crit, lambda_lower, lambda_upper, n_states, {{ metric }}) %>%
#   rename(value := !!sym(metric)) %>%
#   group_by(model) %>%
#   summarise(
#     "min"    = round(min(value, na.rm = TRUE), 3),
#     "q25"    = round(quantile(x = value, probs = 0.25), 3),
#     "mean"   = round(mean(value, na.rm = TRUE), 3),
#     "median" = round(median(value, na.rm = TRUE), 3),
#     "q75"    = round(quantile(x = value, probs = 0.75), 3),
#     "max"    = round(max(value, na.rm = TRUE), 3),
#     "std"    = round(sd(value, na.rm = TRUE), 3)
#   ) %>%
#   ungroup()
# 
# pars_summary <- left_join(
#   x = pars,
#   y = pars_summary,
#   by = "model"
# )
# 
# pars_summary <- pars_summary %>%
#   filter(inf_crit == criterion) %>%
#   arrange(model) %>%
#   mutate(
#     inf_crit = recode(
#       inf_crit,
#       "aic" = "AIC",
#       "bic" = "BIC",
#       "aicc" = "AICc",
#       "hqc" = "HQC"
#     )
#   )
# 
# # Create table as LaTeX code
# pars_summary %>%
#   # mutate(rank = row_number()) %>%
#   mutate(penalty = paste0("$[", lambda_lower, ", ", lambda_upper, "]$")) %>%
#   select(model, inf_crit, alpha, rho, penalty, n_states, min, q25, mean, median, q75, max, std) %>%
#   mutate(min = round(x = min, digits = 3)) %>%
#   mutate(q25 = round(x = q25, digits = 3)) %>%
#   mutate(mean = round(x = mean, digits = 3)) %>%
#   mutate(median = round(x = median, digits = 3)) %>%
#   mutate(q75 = round(x = q75, digits = 3)) %>%
#   mutate(max = round(x = max, digits = 3)) %>%
#   mutate(std = round(x = std, digits = 3)) %>%
#   gt() %>%
#   as_latex() %>%
#   as.character() %>%
#   cat()











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

