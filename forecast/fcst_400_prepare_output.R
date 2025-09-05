
.start <- Sys.time()
.step <- "400_prepare_output.R"

# Evaluate forecast accuracy ==================================================

# Concatenate forecasts row-wise (flatten list) -------------------------------

future_frame <- bind_rows(future_frame)

# Check for failed models or forecasts and exclude them -----------------------

models_failed <- future_frame %>%
  filter(is.na(point)) %>%
  select(!!sym(series_id), split, model) %>%
  distinct() %>%
  mutate(id = paste(!!sym(series_id), split, model, sep = "_"))

future_frame <- future_frame %>%
  mutate(id = paste(!!sym(series_id), split, model, sep = "_")) %>%
  filter(id %out% models_failed$id) %>%
  select(-id)

# Calculate forecast errors and percentage forecast errors --------------------

error_frame <- make_errors(
  future_frame = future_frame,
  main_frame = main_frame,
  context = context
)

# Estimate accuracy metrics ------------------------------------------------------

# Accuracy by split
accuracy_split <- make_accuracy(
  future_frame = future_frame,
  main_frame = main_frame,
  context = context,
  dimension = "split",
  benchmark = benchmark
)

# Accuracy by horizon
accuracy_horizon <- make_accuracy(
  future_frame = future_frame,
  main_frame = main_frame,
  context = context,
  dimension = "horizon",
  benchmark = benchmark
)

accuracy_frame <- bind_rows(
  accuracy_split,
  accuracy_horizon
)

# Estimate overall mean of error metrics --------------------------------------

mean_model <- accuracy_horizon %>%
  filter(metric == set_metric) %>%
  group_by(!!sym(series_id), model) %>%
  summarise(
    value = round(mean(value, na.rm = TRUE), 3),
    .groups = "drop")

mean_total <- mean_model %>%
  group_by(model) %>%
  summarise(
    value = round(mean(value, na.rm = TRUE), 3),
    .groups = "drop") %>%
  mutate(!!sym(series_id) := "TOTAL")

accuracy_mean <- bind_rows(
  mean_model,
  mean_total) %>%
  pivot_wider(
    names_from = model,
    values_from = value
  )

# Evaluate run-time ===========================================================

# Concatenate run-times row-wise (flatten list) -------------------------------

time_frame <- bind_rows(time_frame)
time_frame <- time_frame %>%
  pivot_longer(
    cols = everything(),
    names_to = "model",
    values_to = "value") %>%
  mutate(unit = "[secs]") %>%
  mutate(value = round(value, digits = 3)) %>%
  arrange(value)

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

save(
  object = time_frame,
  file = paste0(folder, "/", "time_frame.rda")
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
