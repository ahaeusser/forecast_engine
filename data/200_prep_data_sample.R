
# Prepare sample of M4 data per frequency #####################################

# Configuration ---------------------------------------------------------------

# Load relevant packages
library(tidyverse)

# Change default location and time
Sys.setlocale("LC_TIME", "C")

save_data <- TRUE

# Sample 1, used for hyperparameters sweep
n_seed <- 42
sample <- "Parameter"

# # Sample 2, used for forecast benchmark
# n_seed <- 123
# sample <- "Forecast"

n_obs_mth <- 240
n_series_mth <- 2400

n_obs_qtr <- 80
n_series_qtr <- 1200

###############################################################################

main_mth_full <- read_rds(file = "data/main_mth_full.rds")
meta_mth_full <- read_rds(file = "data/meta_mth_full.rds")

main_qtr_full <- read_rds(file = "data/main_qtr_full.rds")
meta_qtr_full <- read_rds(file = "data/meta_qtr_full.rds")

###############################################################################


# M4 dataset (full) -----------------------------------------------------------
main_mth_full <- main_mth_full %>%
  mutate(dataset = "Full")

meta_mth_full <- meta_mth_full %>%
  mutate(dataset = "Full")

main_qtr_full <- main_qtr_full %>%
  mutate(dataset = "Full")

meta_qtr_full <- meta_qtr_full %>%
  mutate(dataset = "Full")


# Random sample from M4 dataset
# Monthly ---------------------------------------------------------------------

set.seed(n_seed)

meta_mth_sample <- meta_mth_full %>%
  filter(n_obs <= n_obs_mth)

set_series <- sample(
  x = meta_mth_sample[["series"]],
  size = n_series_mth
)

meta_mth_sample <- meta_mth_sample %>%
  filter(series %in% set_series) %>%
  mutate(dataset = sample)

main_mth_sample <- main_mth_full %>%
  filter(series %in% set_series) %>%
  mutate(dataset = sample)


# Quarterly -------------------------------------------------------------------

set.seed(n_seed)

meta_qtr_sample <- meta_qtr_full %>%
  filter(n_obs <= n_obs_qtr)

set_series <- sample(
  x = meta_qtr_sample[["series"]],
  size = n_series_qtr
)

meta_qtr_sample <- meta_qtr_sample %>%
  filter(series %in% set_series) %>%
  mutate(dataset = sample)

main_qtr_sample <- main_qtr_full %>%
  filter(series %in% set_series) %>%
  mutate(dataset = sample)


# Save data -------------------------------------------------------------------

if (save_data == TRUE) {
  # Main data
  saveRDS(
    object = main_mth_sample,
    file = paste0("data/main_mth_", tolower(sample), ".rds"),
    compress = "xz"
  )
  
  saveRDS(
    object = main_qtr_sample,
    file = paste0("data/main_qtr_", tolower(sample), ".rds"),
    compress = "xz"
  )
  
  # Meta data
  saveRDS(
    object = meta_mth_sample,
    file = paste0("data/meta_mth_", tolower(sample), ".rds"),
    compress = "xz"
  )
  
  saveRDS(
    object = meta_qtr_sample,
    file = paste0("data/meta_qtr_", tolower(sample), ".rds"),
    compress = "xz"
  )
}
