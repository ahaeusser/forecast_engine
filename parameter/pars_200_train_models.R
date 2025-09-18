
# Hyperparameter Sweep ########################################################

# Pre-processing --------------------------------------------------------------

source("parameter/pars_100_config_file.R")

# Parallel computing
plan(multisession)

# Frequency of dataset
set_freq <- "monthly"
# set_freq <- "quarterly"
# Information criterion
set_inf_crit <- "bic"
# Outlier adjustment
outlier <- TRUE
# Test run
test_run <- FALSE

# Directory and file names, forecast horizon and period
if (set_freq == "monthly") {
  file_main <- "data/main_mth_parameter.rds"
  n_ahead <- 18
  periods <- 12
}

if (set_freq == "quarterly") {
  file_main <- "data/main_qtr_parameter.rds"
  n_ahead <- 8
  periods <- 4
}

# Filter information criterion
pars <- pars %>%
  filter(inf_crit == set_inf_crit)

# Raw dataset
main_frame <- readRDS(file = file_main)
series_name <- unique(main_frame[["series"]])

# Test run
if (test_run == TRUE) {
  series_name <- series_name[1:5]
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

pars_frame <- pars_frame %>%
  mutate(freq = set_freq, .before = model)

# Saving object to the path
file_name <- paste0(
  "parameter/", 
  format(Sys.time(), "%Y%m%d"), "_pars_", set_freq, "_", set_inf_crit,
  ".rds")

saveRDS(
  object = pars_frame,
  file = file_name
)
