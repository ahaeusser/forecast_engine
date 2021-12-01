
info(
  logger = logger,
  message = "START  script/500_evaluate_accuracy.R"
)

.start <- Sys.time()

# Evaluate forecast accuracy ==================================================

# Concatenate forecasts row-wise (flatten list) -------------------------------

future_frame <- bind_rows(future_frame)


# Check for failed models or forecasts and exclude them -----------------------

mdls_failed <- future_frame %>%
  filter(is.na(point)) %>%
  select(!!sym(series_id), split, model) %>%
  distinct() %>%
  mutate(id = paste(!!sym(series_id), split, model, sep = "_"))

future_frame <- future_frame %>%
  mutate(id = paste(!!sym(series_id), split, model, sep = "_")) %>%
  filter(id %out% mdls_failed$id) %>%
  select(-id)

# Calculate forecast errors and percentage forecast errors --------------------

error_frame <- make_errors(
  future = future_frame,
  main = main_frame,
  context = context
)

# Estimate accuracy metrics ------------------------------------------------------

# Accuracy by split
accuracy_split <- make_accuracy(
  future = future_frame,
  main = main_frame,
  dimension = "split",
  benchmark = benchmark
)

# Accuracy by horizon
accuracy_horizon <- make_accuracy(
  future = future_frame,
  main = main_frame,
  dimension = "horizon",
  benchmark = benchmark
)

accuracy_frame <- bind_rows(
  accuracy_split,
  accuracy_horizon
)

# Estimate overall mean of error metrics --------------------------------------

accuracy_mean <- accuracy_horizon %>%
  filter(metric == set_metric) %>%
  group_by(!!sym(series_id), model) %>%
  summarise(
    mean_group = round(mean(value, na.rm = TRUE), 3),
    .groups = "drop") %>%
  pivot_wider(
    names_from = !!sym(series_id),
    # names_from = model,
    values_from = mean_group) %>%
  rowwise() %>%
  mutate(total = round(mean(c_across(-model)), 3)) %>%
  ungroup() %>%
  arrange(total)

# Save objects ----------------------------------------------------------------

save(
  object = future_frame,
  file = paste0(folder, "/", "future_frame.rda")
)

save(
  object = error_frame,
  file = paste0(folder, "/", "error_frame.rda")
  )

save(
  object = accuracy_frame,
  file = paste0(folder, "/", "accuracy_frame.rda")
  )

save(
  object = accuracy_mean,
  file = paste0(folder, "/", "accuracy_mean.rda")
)

info(
  logger = logger,
  message = paste0(
    "FINISH script/500_evaluate_accuracy.R",
    "\n",
    log_time(start = .start),
    "\n"
  )
)

