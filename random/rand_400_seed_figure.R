
# Visualize forecasts for different random initializations ====================

# Load libraries --------------------------------------------------------------

library(tidyverse)
library(echos)
library(tsibble)
library(tscv)

# Configuration ---------------------------------------------------------------

source("random/rand_100_config_file.R")

# Change default location and time
Sys.setlocale("LC_TIME", "C")

# Frequency-specific setup
setup <- tibble(
  freq = c("Monthly", "Quarterly"),
  series = c("M21655", "Q13921"),
  n_ahead = c(18, 8),
  inf_crit = c("aicc", "aic"),
  alpha = c(1.0, 1.0),
  rho = c(0.9, 0.4),
  tau = c(0.4, 0.6),
  data = list(
    m4_monthly_subset,
    M4_quarterly_data
  )
)

# Train and forecast ESN models -----------------------------------------------

result <- map(
  .x = seq_len(nrow(setup)),
  .f = ~{
    setup_id <- .x
    
    freq <- setup[["freq"]][setup_id]
    series_id <- setup[["series"]][setup_id]
    n_ahead <- setup[["n_ahead"]][setup_id]
    
    # Extract series
    main_frame <- setup[["data"]][[setup_id]] |>
      filter(series == series_id) |>
      arrange(index)
    
    # Prepare training and testing data
    train_frame <- main_frame |>
      slice_head(
        n = nrow(main_frame) - n_ahead
      )
    
    test_frame <- main_frame |>
      slice_tail(
        n = n_ahead
      )
    
    xtrain <- train_frame |>
      pull(value)
    
    # Train and forecast models
    fcst <- map_dfr(
      .x = seq_len(nrow(seeds)),
      .f = ~{
        model_id <- seeds[["model"]][.x]
        seed <- seeds[["seed"]][.x]
        
        # Train ESN model
        xmodel <- train_esn(
          y = xtrain,
          inf_crit = setup[["inf_crit"]][setup_id],
          alpha = setup[["alpha"]][setup_id],
          rho = setup[["rho"]][setup_id],
          tau = setup[["tau"]][setup_id],
          n_seed = seed
        )
        
        # Forecast ESN model
        xfcst <- forecast_esn(
          object = xmodel,
          n_ahead = n_ahead,
          n_sim = NULL
        )
        
        # Prepare forecasts
        tibble(
          freq = freq,
          series = series_id,
          model = model_id,
          seed = seed,
          index = as.Date(test_frame[["index"]]),
          value = xfcst[["point"]]
        )
      }
    )
    
    # Prepare actual values
    actual <- test_frame |>
      transmute(
        freq = freq,
        series = series_id,
        index = as.Date(index),
        value = value
      )
    
    list(
      fcst = fcst,
      actual = actual
    )
  }
)

# Combine results -------------------------------------------------------------

fcst <- result |>
  map_dfr("fcst") |>
  mutate(
    freq = factor(
      freq,
      levels = c("Monthly", "Quarterly")
    )
  )

actual <- result |>
  map_dfr("actual") |>
  mutate(
    freq = factor(
      freq,
      levels = c("Monthly", "Quarterly")
    )
  )

# Plot forecasts --------------------------------------------------------------

p <- ggplot()

# Forecasts plotted first
p <- p + geom_line(
  data = fcst,
  mapping = aes(
    x = index,
    y = value,
    group = model,
    colour = freq),
  linewidth = 0.4,
  alpha = 0.25
)

# Actual values plotted on top
p <- p + geom_line(
  data = actual,
  mapping = aes(
    x = index,
    y = value),
  colour = "black",
  linewidth = 0.9
)

p <- p + facet_wrap(
  facets = vars(freq),
  ncol = 2,
  scales = "free"
)

p <- p + scale_colour_manual(
  values = c(
    "Monthly" = "orange",
    "Quarterly" = "steelblue"),
  guide = "none"
)

p <- p + scale_x_date(labels = scales::label_date_short())

p <- p + labs(
  x = "Time",
  y = "Value"
)

p <- p + theme_tscv()
p


figure_name <- "output/figure_06_fcst_rand.pdf"
fig_width <- 17
fig_hight <- 8

ggsave(
  filename = figure_name,
  width = fig_width,
  height = fig_hight,
  units = "cm"
)
