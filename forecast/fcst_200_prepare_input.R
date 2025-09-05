
.start <- Sys.time()
.step <- "200_prepare_input.R"

# Data preparation ============================================================

# Load and check data ---------------------------------------------------------

input_frame <- readRDS(input_file)
main_frame <- input_frame

# Specific adjustments ........................................................

main_summary <- main_frame %>%
  summarise_data(context = context)

# Interpolate missing values --------------------------------------------------

main_frame <- main_frame %>%
  group_by(!!sym(series_id)) %>%
  mutate(
    !!sym(value_id) := interpolate_missing(
      x = !!sym(value_id),
      periods = periods)) %>%
  ungroup()

# Adjust outliers -------------------------------------------------------------

if (outlier == TRUE) {
  main_frame <- main_frame %>%
    group_by(!!sym(series_id)) %>%
    mutate(
      !!sym(value_id) := smooth_outlier(
        x = !!sym(value_id),
        periods = periods)) %>%
    ungroup()
}

# Create split into training and testing --------------------------------------

split_frame <- make_split(
  main_frame = main_frame,
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
  
  random_series <- unique(main_frame$series)[1:5]
  
  split_frame <- split_frame %>%
    filter(!!sym(series_id) %in% random_series)
  
}

# Initialize future_frame (empty object to store forecasts) --------------------

future_frame <- vector(
  mode = "list",
  length = length(models)
)

names(future_frame) <- models

# Initialize time_frame (empty object to store run time) ----------------------

time_frame <- vector(
  mode = "list",
  length = length(models)
)

names(time_frame) <- models

# Save objects ----------------------------------------------------------------

save(
  object = main_frame,
  file = paste0(folder, "/", "main_frame.rda")
)

save(
  object = split_frame,
  file = paste0(folder, "/", "split_frame.rda")
  )

save(
  object = main_summary,
  file = paste0(folder, "/", "main_summary.rda")
)

write_lines(
  x = log_time(text = .step, start = .start),
  file = glue("{folder}/{run_name}.txt"),
  append = TRUE
)

print(
  log_time(
    text = .step, 
    start = .start,
    ft_bold = TRUE
  )
)
