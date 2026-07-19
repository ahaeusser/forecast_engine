
# Random Initialization #######################################################

# Pre-processing --------------------------------------------------------------

source("random/rand_100_config_file.R")

# Directory and file names, forecast horizon and period
files <- list(
  "output/20260718_rand_monthly.rds",
  "output/20260719_rand_quarterly.rds"
)

# Read rds-files and combine data frames row-wise
seed_frame <- bind_rows(lapply(files, readRDS))

# Table main text (top n models, one table per frequency) ---------------------

set_freq <- "monthly"
# set_freq <- "quarterly"
n_rows <- 30

# Average sMAPE and MASE and combine with hyperparameters
seed_model <- seed_frame %>%
  filter(freq == set_freq) %>%
  group_by(model) %>%
  summarise(
    mase_mean    = round(mean(mase, na.rm = TRUE), 3),
    mase_median  = round(median(mase, na.rm = TRUE), 3),
    smape_mean   = round(mean(smape, na.rm = TRUE), 3),
    smape_median = round(median(smape, na.rm = TRUE), 3)
  ) %>%
  ungroup()

seed_model <- left_join(
  x = seeds,
  y = seed_model,
  by = "model"
)

seed_model <- seed_model %>%
  arrange(mase_mean)

# Create table as LaTeX code
seed_model %>%
  slice_head(n = n_rows) %>%
  mutate(rank = row_number()) %>%
  select(rank, model, seed, mase_mean, mase_median, smape_mean, smape_median) %>%
  gt() %>%
  as_latex() %>%
  as.character() %>%
  cat()


# Prepare data per frequency and hyperparameter -------------------------------

# Leakage rate alpha
seed_dist <- seed_frame %>%
  group_by(freq, seed) %>%
  summarise(
    "mase_min"     = round(min(mase, na.rm = TRUE), 3),
    "mase_q1"      = round(quantile(x = mase, probs = 0.25), 3),
    "mase_mean"    = round(mean(mase, na.rm = TRUE), 3),
    "mase_median"  = round(median(mase, na.rm = TRUE), 3),
    "mase_q3"      = round(quantile(x = mase, probs = 0.75), 3),
    "mase_max"     = round(max(mase, na.rm = TRUE), 3),
    "mase_std"     = round(sd(mase, na.rm = TRUE), 3),
    "smape_min"    = round(min(smape, na.rm = TRUE), 3),
    "smape_q1"     = round(quantile(x = smape, probs = 0.25), 3),
    "smape_mean"   = round(mean(smape, na.rm = TRUE), 3),
    "smape_median" = round(median(smape, na.rm = TRUE), 3),
    "smape_q3"     = round(quantile(x = smape, probs = 0.75), 3),
    "smape_max"    = round(max(smape, na.rm = TRUE), 3),
    "smape_std"    = round(sd(smape, na.rm = TRUE), 3),
    .groups = "drop")


# Tables appendix (one table per frequency) -----------------------------------

set_freq <- "monthly"
# set_freq <- "quarterly"

# Create table as LaTeX code
seed_dist %>%
  filter(freq == set_freq) %>%
  select(-freq) %>%
  gt() %>%
  as_latex() %>%
  as.character() %>%
  cat()


# Figure (one figure for both frequencies) ====================================

# Prepare data ----------------------------------------------------------------

plot_frame <- seed_frame |>
  pivot_longer(
    cols = c(smape, mase),
    names_to = "metric",
    values_to = "value"
  ) |>
  group_by(freq, series, metric) |>
  mutate(
    median_value = median(value, na.rm = TRUE),
    deviation = 100 * (value / median_value - 1)
  ) |>
  ungroup() |>
  mutate(
    metric = factor(
      metric,
      levels = c("smape", "mase"),
      labels = c("sMAPE", "MASE")
    )
  )

# Plot deviations by seed ----------------------------------------------------

p <- ggplot(
  data = plot_frame,
  mapping = aes(
    x = model,
    y = deviation
  )
)

p <- p + geom_boxplot(
  fill = "lightsteelblue",
  colour = "grey40",
  outlier.shape = NA
)

p <- p + geom_point(
  position = position_jitter(width = 0.15),
  alpha = 0.4,
  size = 1
)

p <- p + geom_hline(
  yintercept = 0,
  colour = "grey50",
  linetype = "dashed",
  linewidth = 0.5
)

p <- p + facet_grid(
  rows = vars(metric),
  cols = vars(freq),
  scales = "free_y"
)

p <- p + labs(
  title = "Sensitivity to random initialization",
  subtitle = "Deviation from the median accuracy for each time series",
  x = "Random initialization",
  y = "Deviation from series median (%)"
)

p <- p + theme_minimal()

p <- p + theme(
  axis.text.x = element_text(
    angle = 45,
    hjust = 1
  )
)

p


figure_name <- "output/figure_05_rand_summary.pdf"
fig_width <- 17
fig_hight <- 15

ggsave(
  filename = figure_name,
  width = fig_width,
  height = fig_hight,
  units = "cm"
)

