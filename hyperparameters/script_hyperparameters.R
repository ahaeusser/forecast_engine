
# Hyperparameter Sweep ########################################################

# Pre-process input ===========================================================

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
# Information criterion
set_inf_crit <- "hqc"
# Outlier adjustment
outlier <- TRUE
# Test run
test_run <- FALSE

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
  mutate(model = paste_names(x = "ESN", n_pars), .before = inf_crit) %>%
  filter(inf_crit == set_inf_crit)

# Raw dataset
main_frame <- readRDS(file = file_main)
series_name <- unique(main_frame[["series"]])

# Test run
if (test_run == TRUE) {
  series_name <- series_name[1:10]
  main_frame <- main_frame %>%
    filter(series %in% series_name)
}

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


# Saving object to the path
file_name <- paste0(
  "analysis/", 
  format(Sys.time(), "%Y%m%d"), "_pars_", set_freq, "_", set_inf_crit,
  ".rds")

saveRDS(
  object = pars_frame,
  file = file_name
  )

# save.image(paste0("analysis/pars_",set_freq, ".RData"))



# Post-process output =========================================================

# Configuration ---------------------------------------------------------------

# Load relevant packages
library(gt)
library(tidyverse)
library(tscv)

# Frequency of dataset
set_freq <- "monthly"
set_freq <- "quarterly"

# Functions -------------------------------------------------------------------

paste_names <- function(x, n) {
  x <- paste0(
    x,"-",
    formatC(
      x = 1:n,
      width = nchar(n),
      flag = "0"))
  
  return(x)
}


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

# Directory and file names, forecast horizon and period
if (set_freq == "monthly") {
  files <- list(
    "analysis/20250815_pars_monthly_aic.rds",
    "analysis/20250818_pars_monthly_aicc.rds",
    "analysis/20250822_pars_monthly_bic.rds",
    "analysis/20250827_pars_monthly_hqc.rds"
  )
}

if (set_freq == "quarterly") {
  files <- list(
    "analysis/20250827_pars_quarterly_aic.rds",
    "analysis/20250827_pars_quarterly_aicc.rds",
    "analysis/20250828_pars_quarterly_bic.rds",
    "analysis/20250828_pars_quarterly_hqc.rds"
  )
}

# Read rds-files from list
pars_list <- lapply(files, readRDS)

# Combine data frames row-wise
pars_frame <- bind_rows(lapply(files, readRDS))


# Tables main text ------------------------------------------------------------

# Average sMAPE and MASE and combine with hyperparameters
pars_model <- pars_frame %>%
  group_by(model) %>%
  summarise(
    mase_mean    = round(mean(mase, na.rm = TRUE), 3),
    mase_median  = round(median(mase, na.rm = TRUE), 3),
    smape_mean   = round(mean(smape, na.rm = TRUE), 3),
    smape_median = round(median(smape, na.rm = TRUE), 3)
    ) %>%
  ungroup()

pars_model <- left_join(
  x = pars,
  y = pars_model,
  by = "model"
)

pars_model <- pars_model %>%
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
pars_model %>%
  slice_head(n = 30) %>%
  mutate(rank = row_number()) %>%
  select(rank, model, inf_crit, alpha, rho, tau, mase_mean, mase_median, smape_mean, smape_median) %>%
  gt() %>%
  as_latex() %>%
  as.character() %>%
  cat()



# Tables appendix -------------------------------------------------------------

# Leakage rate alpha
pars_alpha <- pars_frame %>%
  group_by(alpha) %>%
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
  ungroup() %>%
  rename(value = alpha) %>%
  mutate(value = as_factor(value)) %>%
  mutate(par = "alpha", .before = value)


# Spectral radius rho
pars_rho <- pars_frame %>%
  group_by(rho) %>%
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
  ungroup() %>%
  rename(value = rho) %>%
  mutate(value = as_factor(value)) %>%
  mutate(par = "rho", .before = value)


# Reservoir scaling tau
pars_tau <- pars_frame %>%
  group_by(tau) %>%
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
  ungroup() %>%
  rename(value = tau) %>%
  mutate(value = as_factor(value)) %>%
  mutate(par = "tau", .before = value)


# Information criterion
pars_inf_crit <- pars_frame %>%
  group_by(inf_crit) %>%
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
  ungroup() %>%
  mutate(
    inf_crit = recode(
      inf_crit,
      "aic" = "AIC",
      "bic" = "BIC",
      "aicc" = "AICc",
      "hqc" = "HQC")) %>%
  rename(value = inf_crit) %>%
  mutate(value = as_factor(value)) %>%
  mutate(par = "inf_crit", .before = value)


pars_dist <- bind_rows(
  pars_alpha,
  pars_rho,
  pars_tau,
  pars_inf_crit)

# Create table as LaTeX code
pars_dist %>%
  gt() %>%
  as_latex() %>%
  as.character() %>%
  cat()




# Figure ----------------------------------------------------------------------

# Reorder facets manually
pars_dist$par <- factor(
  pars_dist$par,
  levels = c(
    "alpha", 
    "rho", 
    "tau", 
    "inf_crit"
    )
  )

pars_dist <- pars_dist %>%
  mutate(
    par = recode(
      par,
      "alpha" = "Leakage Rate",
      "rho" = "Spectral Radius",
      "tau" = "Reservoir Scaling",
      "inf_crit" = "Information Criterion"))

# find row(s) with minimum mase_mean per facet
min_points <- pars_dist %>%
  group_by(par) %>%
  slice_min(mase_mean, n = 1, with_ties = FALSE)


p <- ggplot(
  data = pars_dist,
  aes(
    x = factor(value),
    y = mase_mean,
    group = 1)
  )

p <- p + geom_point(color = "grey35", size = 4)
p <- p + geom_line(color = "grey35", size = 1)

p <- p + geom_point(
  data = min_points,
  aes(
    x = factor(value), 
    y = mase_mean),
  color = "#F8766D", 
  size = 5)

p <- p + facet_wrap(~ par, scales = "free_y", ncol = 1)
p <- p + coord_flip()
p <- p + labs(x = "Hyperparameter", y = "Mean MASE")
p <- p + theme_tscv()
p









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

