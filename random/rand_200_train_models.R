
# Random Initialization #######################################################

# Pre-processing --------------------------------------------------------------

source("random/rand_100_config_file.R")

# Parallel computing
plan(multisession)

# Frequency of dataset
set_freq <- "monthly"
# set_freq <- "quarterly"

# Outlier adjustment
outlier <- TRUE
# Test run
test_run <- TRUE

# Directory and file names, forecast horizon and period
if (set_freq == "monthly") {
  # Forecast setup
  file_main <- "data/main_mth_parameter.rds"
  n_ahead <- 18
  periods <- 12
  
  # ESN setup
  pars_esn <- list(
    inf_crit = "aicc", 
    alpha = 1.0, 
    rho = 0.9, 
    tau = 0.4
  )
}

if (set_freq == "quarterly") {
  # Forecast setup
  file_main <- "data/main_qtr_parameter.rds"
  n_ahead <- 8
  periods <- 4
  
  # ESN setup
  pars_esn <- list(
    inf_crit = "aic", 
    alpha = 1.0, 
    rho = 0.4, 
    tau = 0.6
  )
}

# Raw dataset
main_frame <- readRDS(file = file_main)
series_name <- unique(main_frame[["series"]])

# Test run
if (test_run == TRUE) {
  series_name <- series_name[1:20]
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
  seeds,
  series_name
)

# Modify column Model and add identifier
full_grid <- full_grid %>%
  mutate(ID = row_number(), .before = model)

# Number of iterations
n_steps <- nrow(full_grid)


# Run models with different hyperparameters -----------------------------------
tic()
seed_frame <- with_progress({
  future_map_dfr(
    .x = seq_len(n_steps),
    .f = ~{
      
      model_id <- full_grid[["model"]][.x]
      xseries <- full_grid[["series_name"]][.x]
      seed <- full_grid[["seed"]][.x]
      
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
        inf_crit = pars_esn[["inf_crit"]],
        alpha = pars_esn[["alpha"]],
        rho = pars_esn[["rho"]],
        tau = pars_esn[["tau"]],
        n_seed = seed
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
        seed = seed,
        smape = smape,
        mase = mase
      )
    },
    .progress = TRUE
  )
})
toc()

seed_frame <- seed_frame %>%
  mutate(freq = set_freq, .before = model)

# Saving object to the path
file_name <- paste0(
  "random/", 
  format(Sys.time(), "%Y%m%d"), "_rand_", set_freq,
  ".rds")

saveRDS(
  object = seed_frame,
  file = file_name
)
