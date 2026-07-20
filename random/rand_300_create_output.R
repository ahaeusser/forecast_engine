
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

# Summarize deviations by seed ------------------------------------------------

summary_frame <- plot_frame |>
  group_by(freq, model) |>
  summarise(
    q1 = quantile(deviation, probs = 0.25, na.rm = TRUE),
    median = median(deviation, na.rm = TRUE),
    q3 = quantile(deviation, probs = 0.75, na.rm = TRUE),
    .groups = "drop") |>
  mutate(
    freq = recode(
      freq,
      monthly = "Monthly",
      quarterly = "Quarterly"
    )
  )

# Plot median deviations and IQR ----------------------------------------------

p <- ggplot(
  data = summary_frame,
  mapping = aes(
    x = model,
    y = median,
    colour = freq,
    group = 1
  )
)

# p <- p + geom_errorbar(
#   aes(
#     ymin = q1,
#     ymax = q3),
#   width = 0.2,
#   linewidth = 0.6
# )

p <- p + geom_line(
  linewidth = 0.6
)

# p <- p + geom_point(
#   size = 2.5
# )

p <- p + geom_point(shape = 21, fill = "white", size = 3, stroke = 0.8)

p <- p + geom_hline(
  yintercept = 0,
  colour = "grey50",
  linetype = "dashed",
  linewidth = 0.5
)

p <- p + facet_wrap(
  facets = vars(freq),
  scales = "free_y"
)

p <- p + coord_flip()

p <- p + scale_colour_manual(
  values = c(
    "Monthly" = "darkorange",
    "Quarterly" = "steelblue"
  ),
  guide = "none"
)

p <- p + labs(
  x = "Random initialization",
  y = "Deviation from series median MASE (%)"
)

p <- p + theme_tscv()
p




figure_name <- "output/figure_05_rand_summary.pdf"
fig_width <- 17
fig_hight <- 18

ggsave(
  filename = figure_name,
  width = fig_width,
  height = fig_hight,
  units = "cm"
)

