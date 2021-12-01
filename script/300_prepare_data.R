
info(
  logger = logger,
  message = "START  script/300_prepare_data.R"
)

.start <- Sys.time()

# Data preparation ============================================================

# Load and check data ---------------------------------------------------------

input_frame <- readRDS(input_file)

meta_frame <- input_frame %>%
  select(-c(!!sym(index_id), !!sym(value_id))) %>%
  distinct() %>%
  mutate(series = make_names(x = "series", n = n())) %>%
  select(series, everything())

cols <- setdiff(
  x = names(meta_frame),
  y = "series"
)

main_frame <- left_join(
  x = input_frame,
  y = meta_frame,
  by = cols) %>%
  select("time", "series", everything())

# Interpolate missing values --------------------------------------------------

main_frame <- main_frame %>%
  group_by(!!sym(series_id)) %>%
  mutate(
    !!sym(value_id) := interpolate_missing(
      x = !!sym(value_id),
      period = period)) %>%
  ungroup()

# Adjust outliers -------------------------------------------------------------

if (outlier == TRUE) {
  main_frame <- main_frame %>%
    group_by(!!sym(series_id)) %>%
    mutate(
      !!sym(value_id) := smooth_outlier(
        x = !!sym(value_id),
        period = period)) %>%
    ungroup()
}

# Create split into training and testing --------------------------------------

split_frame <- make_split(
  main = main_frame,
  context = context,
  type = type,
  value = value,
  n_ahead = n_ahead,
  n_skip = n_skip,
  n_lag = n_lag,
  mode = mode,
  exceed = exceed
)

# Test run --------------------------------------------------------------------

# Reduce number of series and splits in split_frame for fast testing
if (test_run == TRUE) {
  
  set.seed(test_seed)
  
  random_splits <- sample(
    x = unique(split_frame[["split"]]),
    size = n_test_splits,
    replace = FALSE
  )
  
  random_series <- sample(
    x = unique(split_frame[[series_id]]),
    size = n_test_series,
    replace = FALSE
  )
  
  split_frame <- split_frame %>%
    filter(split %in% random_splits) %>%
    filter(!!sym(series_id) %in% random_series)
}

# Initialize future_frame (empty object to store forecasts) --------------------

future_frame <- vector(
  mode = "list",
  length = length(models)
)

names(future_frame) <- models

# Save objects ----------------------------------------------------------------

save(
  object = main_frame,
  file = paste0(folder, "/", "main_frame.rda")
)

save(
  object = split_frame,
  file = paste0(folder, "/", "split_frame.rda")
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
