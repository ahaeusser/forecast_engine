
info(
  logger = logger,
  message = "START  script/500_evaluate_accuracy.R"
)

.start <- Sys.time()

# Evaluate forecast accuracy ==================================================

# Concatenate forecasts row-wise ----------------------------------------------
fcst <- bind_rows(
  fcst_esn,
  fcst_snaive,
  fcst_snaive2,
  fcst_stl_naive,
  fcst_stl_arima,
  fcst_stl_ets,
  fcst_expert,
  fcst_dhr_arima,
  fcst_dshw,
  fcst_tbats,
  fcst_elm,
  fcst_fasster,
  fcst_arima
  )


# Check for failed models or forecasts and exclude them -----------------------
mdls_failed <- fcst %>%
  filter(is.na(.mean)) %>%
  as_tibble() %>%
  select(series_id, split, .model) %>%
  distinct() %>%
  mutate(id = paste(series_id, split, .model, sep = "_"))

fcst <- fcst %>%
  mutate(id = paste(series_id, split, .model, sep = "_")) %>%
  filter(id %out% mdls_failed$id) %>%
  select(-id)


# Calculate forecast errors and percentage forecast errors --------------------
error <- errors(
  fcst = fcst,
  test = test
  )

error_pct <- pct_errors(
  fcst = fcst,
  test = test
  )

# Estimate error metrics ------------------------------------------------------

# Forecast horizon
metrics_horizon <- error_metrics(
  fcst = fcst,
  test = test,
  train = train,
  period = max(period),
  by = "horizon"
  )

# Split
metrics_split <- error_metrics(
  fcst = fcst,
  test = test,
  train = train,
  period = max(period),
  by = "split"
  )

# Estimate overall mean of error metrics --------------------------------------

set_metric <- "MAPE"
set_zones <- unique(metrics_horizon$series_id)

metrics_mean <- metrics_horizon %>%
  filter(metric == set_metric) %>%
  group_by(series_id, .model) %>%
  summarise(mean_group = round(mean(value, na.rm = TRUE), 3), .groups = "drop") %>%
  pivot_wider(
    names_from = series_id,
    values_from = mean_group) %>%
  rowwise() %>%
  mutate(Total = round(mean(c_across(-.model)), 3)) %>%
  rename(Model = .model) %>%
  ungroup() %>%
  arrange(Total)

# Save objects ----------------------------------------------------------------

save(
  object = error,
  file = paste0(folder, "/", "error.rda")
  )

save(
  object = error_pct,
  file = paste0(folder, "/", "error_pct.rda")
  )

save(
  object = metrics_horizon,
  file = paste0(folder, "/", "metrics_horizon.rda")
  )

save(
  object = metrics_split,
  file = paste0(folder, "/", "metrics_split.rda")
  )

save(
  object = metrics_mean,
  file = paste0(folder, "/", "metrics_mean.rda")
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

