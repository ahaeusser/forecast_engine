
# Create tables of forecast results

# Pre-processing --------------------------------------------------------------

# Load packages
library(tidyverse)
library(tsibble)
library(tscv)
library(echos)
library(gt)

# Change default location and time
Sys.setlocale("LC_TIME", "C")

# Frequency of dataset
# set_freq <- "monthly"
set_freq <- "quarterly"

# Directory and file names
if (set_freq == "monthly") {
  file_workspace <- "forecast/20260716_114915_monthly_fcst/workspace.Rdata" # outlier = FALSE
  file_workspace <- "forecast/20260716_144616_monthly_fcst/workspace.Rdata" # outlier = TRUE
  file_workspace <- "forecast/20260717_112247_monthly_fcst/workspace.Rdata" # outlier = TRUE, avg pars
}

if (set_freq == "quarterly") {
  file_workspace <- "forecast/20260716_141805_quarterly_fcst/workspace.Rdata" # outlier = FALSE
  file_workspace <- "forecast/20260716_164855_quarterly_fcst/workspace.Rdata" # outlier = TRUE
  file_workspace <- "forecast/20260717_134841_quarterly_fcst/workspace.Rdata" # outlier = TRUE, avg pars
}

# Load workspace and meta data
load(file = file_workspace, envir = .GlobalEnv)


# Evaluation of forecast accuracy =============================================

# MASE ========================================================================

train_frame <- slice_train(
  main_frame = main_frame,
  split_frame = split_frame,
  context = context
)

numerator <- accuracy_split %>%
  filter(metric == "MAE") %>%
  select(series, model, value) %>%
  rename(MAE = value)

# Estimate in-sample MAE
denominator <- train_frame %>%
  group_by(series, split) %>%
  mutate(lagged = lag(value, n = periods)) %>%
  summarise(
    mae_train = mae_vec(
      truth = value,
      estimate = lagged)) %>%
  ungroup() %>%
  select(series, mae_train)

mase <- left_join(
  x = numerator,
  y = denominator,
  by = c("series")
)

mase <- mase %>%
  mutate(value = MAE / mae_train) %>%
  mutate(metric = "MASE")

# =============================================================================

mase <- mase %>%
  mutate(dimension = "split") %>%
  mutate(n = 1) %>%
  select(series, model, dimension, n, metric, value)

mape <- accuracy_split %>%
  filter(metric == "MAPE")

smape <- accuracy_split %>%
  filter(metric == "sMAPE")

mpe <- accuracy_split %>%
  filter(metric == "MPE")


# Tables within text ----------------------------------------------------------

# MASE and sMAPE
table_mase <- mase %>%
  group_by(model) %>%
  summarise(
    MASE_Mean   = mean(value, na.rm = TRUE),
    MASE_Median = median(value, na.rm = TRUE),
    .groups = "drop") %>%
  rename(Model = model)

table_smape <- smape %>%
  group_by(model) %>%
  summarise(
    sMAPE_Mean   = mean(value, na.rm = TRUE),
    sMAPE_Median = median(value, na.rm = TRUE),
    .groups = "drop") %>%
  rename(Model = model)

# Combine and calculate OWA
table_metrics <- left_join(
  x = table_mase,
  y = table_smape,
  by = "Model") %>%
  mutate(
    OWA = 0.5 * (
      MASE_Mean / MASE_Mean[Model == "NAIVE2"] +
        sMAPE_Mean / sMAPE_Mean[Model == "NAIVE2"])) %>%
  relocate(OWA, .after = Model) %>%
  mutate(across(where(is.numeric), ~ round(.x, 3))) %>%
  arrange(OWA)

# Computational run-time and complexity
n_series <- length(unique(main_frame[["series"]]))

table_time <- time_frame %>%
  rename(Model = model) %>%
  rename(Total = value) %>%
  mutate(Mean = round(Total / n_series, 3)) %>%
  select(Model, Mean, Total)

table_metrics <- left_join(
  x = table_metrics,
  y = table_time,
  by = "Model"
)

table_metrics




table_metrics %>%
  gt() %>%
  as_latex() %>%
  as.character() %>%
  cat()


# Detailed tables for appendix ------------------------------------------------

mean_mase <- mase %>%
  filter(model == "ESN") %>%
  pull(value) %>%
  mean(na.rm = TRUE) %>%
  round(digits = 3)

table_mase <- mase %>%
  group_by(model) %>%
  summarise(
    "Min"    = round(min(value, na.rm = TRUE), 3),
    "Q25"    = round(quantile(x = value, probs = 0.25), 3),
    "Mean"   = round(mean(value, na.rm = TRUE), 3),
    "Median" = round(median(value, na.rm = TRUE), 3),
    "Q75"    = round(quantile(x = value, probs = 0.75), 3),
    "Max"    = round(max(value, na.rm = TRUE), 3),
    "Std"    = round(sd(value, na.rm = TRUE), 3)
  ) %>%
  ungroup() %>%
  arrange(Mean) %>%
  rename(Model = model) %>%
  mutate(Rank = row_number(), .before = Model)


mean_smape <- smape %>%
  filter(model == "ESN") %>%
  pull(value) %>%
  mean(na.rm = TRUE) %>%
  round(digits = 3)

table_smape <- smape %>%
  group_by(model) %>%
  summarise(
    "Min"    = round(min(value, na.rm = TRUE), 3),
    "Q25"    = round(quantile(x = value, probs = 0.25), 3),
    "Mean"   = round(mean(value, na.rm = TRUE), 3),
    "Median" = round(median(value, na.rm = TRUE), 3),
    "Q75"    = round(quantile(x = value, probs = 0.75), 3),
    "Max"    = round(max(value, na.rm = TRUE), 3),
    "Std"    = round(sd(value, na.rm = TRUE), 3)
  ) %>%
  ungroup() %>%
  arrange(Mean) %>%
  rename(Model = model) %>%
  mutate(Rank = row_number(), .before = Model)

# Code for LaTeX tables
table_mase %>%
  gt() %>%
  as_latex() %>%
  as.character() %>%
  cat()

table_smape %>%
  gt() %>%
  as_latex() %>%
  as.character() %>%
  cat()
