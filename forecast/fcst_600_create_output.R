
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
  file_workspace <- "forecast/20251116_150022_monthly_fcst/workspace.Rdata"
}

if (set_freq == "quarterly") {
  file_workspace <- "forecast/20251116_163611_quarterly_fcst/workspace.Rdata"
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

mae_vec <- function(truth,
                    estimate,
                    na_rm = TRUE) {
  
  mean(abs(truth - estimate), na.rm = na_rm)
}



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

table_mase <- mase %>%
  group_by(model) %>%
  summarise(
    "MASE_Mean"     = round(mean(value, na.rm = TRUE), 3),
    "MASE_Median"   = round(median(value, na.rm = TRUE), 3)
  ) %>%
  ungroup() %>%
  arrange(MASE_Mean) %>%
  rename(Model = model)

table_smape <- smape %>%
  group_by(model) %>%
  summarise(
    "sMAPE_Mean"     = round(mean(value, na.rm = TRUE), 3),
    "sMAPE_Median"   = round(median(value, na.rm = TRUE), 3)
  ) %>%
  ungroup() %>%
  arrange(sMAPE_Mean) %>%
  rename(Model = model)


table_metrics <- left_join(
  x = table_mase,
  y = table_smape,
  by = "Model"
)


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




