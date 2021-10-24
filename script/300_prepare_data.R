
info(
  logger = logger,
  message = "START  script/300_prepare_data.R"
)

.start <- Sys.time()

# Data preparation ============================================================

# Load and check data ---------------------------------------------------------

# data <- readRDS(input_file)
# 
# # Special treatment
# data <- data %>%
#   filter(bidding_zone %in% set_zones) %>%
#   check_data() %>%
#   mutate(series = paste0(series, " ", unit)) %>%
#   select(-unit) %>%
#   rename(BZN = bidding_zone) %>%
#   rename(Value = value)

# Interpolate missing values --------------------------------------------------

data <- data %>%
  group_by(series_id) %>%
  mutate(
    Value = interpolate_missing(
      x = Value,
      period = period)) %>%
  ungroup()

# Adjust outliers -------------------------------------------------------------

if (outlier == TRUE) {
  data <- data %>%
    group_by(series_id) %>%
    mutate(
      Value = smooth_outlier(
        x = Value,
        period = period)) %>%
    ungroup()
}

save(
  object = data,
  file = paste0(folder, "/", "data.rda")
  )

# Split data ------------------------------------------------------------------
data <- split_data(
  data = data,
  n_init = n_init,
  n_ahead = n_ahead,
  mode = mode,
  n_skip = n_skip,
  n_lag = n_lag
  )

train <- data$train
test <- data$test

# Create model grid
grid <- create_grid(data = train)

# Reduce number of splits in grid for fast testing
if (test_run == TRUE) {
  
  set.seed(123)
  
  random_splits <- sample(
    x = unique(grid$split),
    size = 20
  )
  
  grid <- grid %>%
    filter(split %in% random_splits)
}

# Save objects ----------------------------------------------------------------

save(
  object = train,
  file = paste0(folder, "/", "train.rda")
  )

save(
  object = test,
  file = paste0(folder, "/", "test.rda")
  )

save(
  object = grid,
  file = paste0(folder, "/", "grid.rda")
  )

info(
  logger = logger,
  message = paste0(
    "FINISH script/300_prepare_data.R",
    "\n",
    log_time(start = .start),
    "\n"
  )
)
