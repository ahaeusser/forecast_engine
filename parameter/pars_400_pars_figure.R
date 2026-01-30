
# Visualize forecasts for different hyperparameters

# Load libraries --------------------------------------------------------------
library(tidyverse)
library(echos)
library(tsibble)
library(fabletools)
library(patchwork)
library(tscv)

# Configuration ---------------------------------------------------------------

# Change default location and time
Sys.setlocale("LC_TIME", "C")

# Forecast horizon
n_ahead <- 18
# Number of columns for faceting
fig_ncol <- 2

alpha <- tibble(
  par = "Leakage Rate",
  alpha = seq(0.1, 1.0, 0.1),
  rho = 1) %>%
    mutate(
    .model = paste0("alpha = ", alpha), 
    .before = par
)

rho <- tibble(
  par = "Spectral Radius",
  alpha = 1,
  rho = seq(0.2, 1.2, 0.1)) %>%
    mutate(
    .model = paste0("rho = ", rho), 
    .before = par
)

pars <- bind_rows(alpha, rho)


# Prepare data and train model ------------------------------------------------

# Prepare train data
main_frame <- m4_data %>%
  filter(series %in% c("M21655"))

train_frame <- main_frame %>%
  slice_head(n = nrow(main_frame) - n_ahead)

test_frame <- main_frame %>%
  slice_tail(n = n_ahead) %>%
  mutate(index = as.Date(index))

# Train ESN models
mable_frame <- map(
  .x = 1:nrow(pars),
  .f = ~{
    train_frame %>%
      model(
        !!(pars[[".model"]][.x]) := ESN(
          value, 
          alpha = pars[["alpha"]][.x], 
          rho = pars[["rho"]][.x]
        )
      )
  }
)

# Forecast ESN models
fable_frame <- map(
  .x = 1:nrow(pars),
  .f = ~{
    mable_frame[[.x]] %>%
      forecast(h = n_ahead)
  }
)


# Extract forecasts and bind row-wise
fcst <- map_dfr(
  .x = 1:nrow(pars),
  .f = ~{
    fable_frame[[.x]] %>%
      as_tibble() %>%
      mutate(type = "FORECAST") %>%
      select(-value) %>%
      rename(value = .mean) %>%
      select(series, .model, type, index, value)
  }
)

fcst <- left_join(
  x = fcst,
  y = pars,
  by = ".model") %>%
  mutate(index = as.Date(index)
)


# Plot leakage rate -----------------------------------------------------------

fcst_alpha <- fcst %>%
  filter(par == "Leakage Rate")

p1 <- ggplot()

p1 <- p1 + geom_line(
  data = fcst_alpha,
  linewidth = 0.8,
  aes(
    x = index,
    y = value,
    group = .model,
    color = alpha,
  )
)

p1 <- p1 + geom_line(
  data = test_frame,
  linewidth = 0.8,
  color = "grey35",
  aes(
    x = index,
    y = value
  )
)

p1 <- p1 + scale_color_gradient(low = "steelblue", high = "orange")
p1 <- p1 + ylim(4700, 6800)

p1 <- p1 + facet_wrap(
  vars(par),
  ncol = fig_ncol,
  scales = "free")

p1 <- p1 + labs(x = "Time")
p1 <- p1 + labs(y = "Value")
p1 <- p1 + scale_x_date(labels = scales::label_date_short())
p1 <- p1 + theme_tscv()
p1 <- p1 + theme(legend.position = "bottom")


# Plot spectral radius --------------------------------------------------------

fcst_rho <- fcst %>%
  filter(par == "Spectral Radius")

p2 <- ggplot()

p2 <- p2 + geom_line(
  data = fcst_rho,
  linewidth = 0.8,
  aes(
    x = index,
    y = value,
    group = .model,
    color = rho,
  )
)

p2 <- p2 + geom_line(
  data = test_frame,
  linewidth = 0.8,
  color = "grey35",
  aes(
    x = index,
    y = value
  )
)


p2 <- p2 + scale_color_gradient(low = "steelblue", high = "orange")
p2 <- p2 + ylim(4700, 6800)

p2 <- p2 + facet_wrap(
  vars(par),
  ncol = fig_ncol,
  scales = "free")

p2 <- p2 + labs(x = "Time")
p2 <- p2 + labs(y = "Value")
p2 <- p2 + scale_x_date(labels = scales::label_date_short())
p2 <- p2 + theme_tscv()
p2 <- p2 + theme(legend.position = "bottom")


# Combine subplots and save as pdf --------------------------------------------
p <- p1 + p2
p

figure_name <- "output/figure_03_fcst_pars.pdf"
fig_width <- 17
fig_hight <- 12

ggsave(
  filename = figure_name,
  width = fig_width,
  height = fig_hight,
  units = "cm"
)

