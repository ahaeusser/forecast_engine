
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
set_freq <- "monthly"
set_freq <- "quarterly"

# Directory and file names
if (set_freq == "monthly") {
  file_workspace <- "output/20260727_102141_monthly_fcst/workspace.Rdata"
}

if (set_freq == "quarterly") {
  file_workspace <- "output/20260727_120756_quarterly_fcst/workspace.Rdata"
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




# Pairwise Wilcoxon Tests for MASE ============================================

# Model order -----------------------------------------------------------------

models <- c(
  "ESN",
  "NAIVE",
  "NAIVE2",
  "SNAIVE",
  "MEAN",
  "DRIFT",
  "ARIMA",
  "ETS",
  "TBATS",
  "THETA",
  "RNN",
  "MLP",
  "ES-RNN"
)


# Create model combinations ---------------------------------------------------

model_pairs <- combn(models,m = 2) |>
  t() |>
  as_tibble(.name_repair = ~ c("model_1", "model_2")) |>
  mutate(id = row_number(), .before = 1)


# Pairwise Wilcoxon tests -----------------------------------------------------

mase_tests <- map_dfr(
  .x = 1:nrow(model_pairs),
  .f = ~{
    
    xmodel_1 <- model_pairs$model_1[.x]
    xmodel_2 <- model_pairs$model_2[.x]
    
    # Extract MASE values
    xvalue_1 <- mase |>
      filter(
        metric == "MASE",
        model == xmodel_1) |>
      arrange(series) |>
      pull(value)
    
    xvalue_2 <- mase |>
      filter(
        metric == "MASE",
        model == xmodel_2) |>
      arrange(series) |>
      pull(value)
    
    # Keep complete pairs only
    xcomplete <- complete.cases(
      xvalue_1,
      xvalue_2
    )
    
    xvalue_1 <- xvalue_1[xcomplete]
    xvalue_2 <- xvalue_2[xcomplete]
    
    # Calculate paired differences
    xdifference <- xvalue_1 - xvalue_2
    
    # Paired Wilcoxon signed-rank test
    if (all(xdifference == 0)) {
      xp_value <- 1
    } else {
      xtest <- wilcox.test(
        x = xvalue_1,
        y = xvalue_2,
        paired = TRUE,
        exact = FALSE
      )
      
      xp_value <- xtest$p.value
    }
    
    # Store results
    xoutput <- tibble(
      id = model_pairs$id[.x],
      model_1 = xmodel_1,
      model_2 = xmodel_2,
      n_series = length(xvalue_1),
      median_mase_difference = median(xdifference),
      p_value = xp_value
    )
  }
)


# Adjust p-values -------------------------------------------------------------

mase_tests <- mase_tests |>
  mutate(
    p_adjusted = p.adjust(
      p_value,
      method = "holm"
    )
  )


# Heatmap data ================================================================

heatmap_mase <- bind_rows(
  
  # Pairwise comparisons
  mase_tests |>
    transmute(
      row_model = model_1,
      col_model = model_2,
      p_adjusted,
      median_difference = median_mase_difference
    ),
  
  # Main diagonal
  tibble(
    row_model = models,
    col_model = models,
    p_adjusted = NA_real_,
    median_difference = NA_real_)) |>
  mutate(
    result = case_when(
      row_model == col_model ~ "Diagonal",
      p_adjusted >= 0.05 ~ "Not significant",
      median_difference < 0 ~ "Row better",
      median_difference > 0 ~ "Column better",
      TRUE ~ "Not significant"),
    label = case_when(
      row_model == col_model ~ "",
      p_adjusted < 0.001 ~ "<0.001",
      TRUE ~ format(
        round(p_adjusted, 3),
        nsmall = 3)),
    row_model = factor(
      row_model,
      levels = rev(models)),
    col_model = factor(
      col_model,
      levels = models
    )
  )


# Visualization ===============================================================

p <- ggplot(
  data = heatmap_mase,
  mapping = aes(
    x = col_model,
    y = row_model,
    fill = result
  )
)

p <- p + geom_tile(
  colour = "white",
  linewidth = 0.5
)

p <- p + geom_text(
  aes(label = label),
  size = 3
)

p <- p + scale_fill_manual(
  values = c(
    "Row better" = "#A2C0D9",
    "Column better" = "#FFD27F",
    "Not significant" = "grey85",
    "Diagonal" = "white"),
  breaks = c(
    "Row better",
    "Column better",
    "Not significant"
  )
)

p <- p + coord_equal()

p <- p + labs(
  x = NULL,
  y = NULL,
  fill = NULL
)

p <- p + theme_minimal()

p <- p + theme(
  panel.grid = element_blank(),
  axis.text.x = element_text(
    angle = 45,
    hjust = 1),
  legend.position = "bottom"
)

p



figure_name <- paste0("output/figure_07_test_wilcoxon_", set_freq, ".pdf")

fig_width <- 17
fig_height <- 17

ggsave(
  filename = figure_name,
  width = fig_width,
  height = fig_height,
  units = "cm"
)




# Table of ESN comparisons ====================================================

table_esn_tests <- heatmap_mase |>
  filter(
    row_model == "ESN",
    col_model != "ESN"
  ) |>
  transmute(
    Model = col_model,
    `Median difference` = median_difference,
    `Adjusted p-value` = label,
    Result = recode(
      result,
      "Row better" = "ESN better",
      "Column better" = "Comparison model better",
      "Not significant" = "Not significant"
    )
  )


# LaTeX table -----------------------------------------------------------------

table_esn_tests |>
  gt() |>
  fmt_number(
    columns = `Median difference`,
    decimals = 3
  ) |>
  as_latex() |>
  as.character() |>
  cat()


