
# Visualize forecasts for different hyperparameters

# Load libraries --------------------------------------------------------------
library(tidyverse)
library(echos)
library(tsibble)
library(fabletools)

# Configuration ---------------------------------------------------------------

# Forecast horizon
n_ahead <- 18
# Number of columns for faceting
fig_ncol <- 2

alpha <- tibble(
  par = "Leakage Rate",
  alpha = seq(0.1, 1.0, 0.1),
  rho = 1
)

rho <- tibble(
  par = "Spectral Radius",
  alpha = 1,
  rho = seq(0.2, 1.2, 0.1)
)

pars <- bind_rows(alpha, rho) %>%
  mutate(id = paste0("alpha = ", alpha, " / rho = ", rho), .before = par)


# Prepare data and train model ------------------------------------------------

# Prepare train data
main_frame <- m4_data %>%
  filter(series %in% c("M21655"))

train_frame <- main_frame %>%
  group_by_key() %>%
  slice_head(n = nrow(main_frame) - n_ahead)

test_frame <- main_frame %>%
  group_by_key() %>%
  slice_tail(n = n_ahead)

# Train ESN models
mable_frame <- map(
  .x = 1:nrow(pars),
  .f = ~{
    train_frame %>%
      model(
        !!(pars[["id"]][.x]) := ESN(
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

# # Create test data and bind row-wise
# test <- map_dfr(
#   .x = 1:nrow(pars),
#   .f = ~{
#     test_frame %>%
#       as_tibble() %>%
#       mutate(type = "ACTUAL") %>%
#       mutate(.model = pars[["id"]][.x]) %>%
#       select(series, .model, type, index, value)
#   }
# )
# 
# # Bind all data row-wise for plotting
# model_frame <- bind_rows(test, fcst) %>%
#   mutate(index = as.Date(index))


pars <- pars %>% rename(.model = id)

fcst <- left_join(
  x = fcst,
  y = pars,
  by = ".model") %>%
  mutate(index = as.Date(index))


fcst <- fcst %>%
  pivot_longer(
    cols = c("alpha", "rho"),
    names_to = "par2",
    values_to = "value2"
  )



# Plot data -------------------------------------------------------------------
p <- ggplot()

p <- p + geom_line(
  data = fcst,
  aes(
    x = index,
    y = value,
    group = interaction(series, .model, drop = TRUE),
    color = value2,
  )
)

# p <- p + scale_color_manual(values = c("grey35", "#F8766D", "#00BFC4"))
# p <- p + scale_size_manual(values = c(0.5, 0.5, 1.0))

# p <- p + scale_color_continuous()
p <- p + scale_color_viridis_c(name = "value2") 

p <- p + facet_wrap(
  vars(par),
  ncol = fig_ncol,
  scales = "free")

p <- p + labs(x = "Time")
p <- p + labs(y = "Value")
p <- p + scale_x_date(labels = scales::label_date_short())
p <- p + theme_tscv()
# p <- p + theme(legend.position="none")
p







fcst <- fcst %>%
  mutate(
    value2 = as.numeric(value2),     # just in case it's character
    index  = as.Date(index)          # just in case it's character
  )

p <- ggplot(fcst, aes(index, value)) +
  geom_line(
    aes(
      color = value2,
      group = interaction(series, .model, drop = TRUE)  # one line per model run
    ),
    linewidth = 0.3,
    alpha = 0.85
  ) +
  facet_wrap(vars(par), ncol = fig_ncol, scales = "free_y") +  # free_y usually what you want
  scale_color_viridis_c(name = "value2") +                     # continuous color
  scale_x_date(labels = scales::label_date_short()) +
  labs(x = "Time", y = "Value") +
  theme_tscv()

p






