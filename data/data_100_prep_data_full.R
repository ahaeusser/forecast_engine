
# Prepare sample of M4 data per frequency #####################################

# Pre-processing ==============================================================

# install.packages("https://github.com/carlanetto/M4comp2018/releases/download/0.2.0/M4comp2018_0.2.0.tar.gz", repos=NULL)
# remotes::install_github("carlanetto/M4comp2018", ref = "6e75d59eb30e47cbb6bd5093d5bf2515493a6050")

# Load packages
library(M4comp2018)
library(tidyverse)
library(lubridate)
library(tsibble)

# Ensure English abbreviations
Sys.setlocale("LC_TIME", "C")


# Settings ====================================================================

save_data <- TRUE         # save main and meta data as rds file
set_freq <- "Quarterly"   # set frequency
n_seed <- 42              # set seed for reproducibility
n_series <- 10            # number of random time series
n_series <- 24000

# Prepare main and meta data ==================================================

# Extract time series by frequency
data(M4)
data <- Filter(function(l) l$period == set_freq, M4)

file_name <- paste0("M4_", set_freq, "_", n_seed, "_", n_series)
n_total <- length(data)
set.seed(n_seed)

# series_id <- sample(
#   x = seq_len(n_total),
#   size = n_series
# )

series_id <- seq_len(n_series)

data <- map(
  .x = series_id,
  .f = ~{
    
    # Extract data
    series <- data[[.x]][["st"]]
    category <- data[[.x]][["type"]]
    train <- as_tsibble(data[[.x]][["x"]])
    test <- as_tsibble(data[[.x]][["xx"]])
    n_ahead <- data[[.x]][["h"]]
    n_obs <- data[[.x]][["n"]]
    start <- min(train[["index"]])
    end <- max(train[["index"]])
    
    # Time series data
    main <- bind_rows(train, test) %>%
      as_tibble() %>%
      mutate(series = series) %>%
      mutate(category = category) %>%
      select(series, category, index, value)
    
    # Meta data
    meta <- tibble(
      series = series,
      category = category,
      freq = set_freq,
      n_ahead = n_ahead,
      n_obs = n_obs,
      start = start,
      end = end
    )
    
    list(
      main = main,
      meta = meta
    )
  }
)

# Extract main data from list and bind row-wise
main <- map_dfr(
  .x = seq_len(n_series),
  .f = ~{
    data[[.x]][["main"]]
  }
)

# Extract meta data from list and bind row-wise
meta <- map_dfr(
  .x = seq_len(n_series),
  .f = ~{
    data[[.x]][["meta"]]
  }
)

# Assign data and save as rds-file ============================================

if (save_data == TRUE) {
  # Main
  assign(paste0(file_name, "_main"), main)
  
  saveRDS(
    object = main,
    file = paste0(file_name, "_main.rds"),
    compress = "xz"
  )
  
  # Meta
  assign(paste0(file_name, "_meta"), meta)
  
  saveRDS(
    object = meta,
    file = paste0(file_name, "_meta.rds"),
    compress = "xz"
  )
  
}

# M4_Monthly_42_48000_main %>%
#   filter(series == "M39578") %>%
#   plot_line(x = index, y =  value)
