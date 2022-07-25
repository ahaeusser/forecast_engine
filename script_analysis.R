
library(plotly)

# Visualize forecast and actual values ========================================

# series_ids <- main_frame %>%
#   pull(series) %>%
#   unique()
# 
# series_ids


set_series <- "M42876"
set_split <- 30
set_models <- c("ESN", "THETA")
set_error <- "MAPE"

actual <- main_frame %>%
  filter(!!sym(series_id) == set_series) %>%
  # select(-category) %>%
  mutate(model = "ACTUAL") %>%
  mutate(split = NA_integer_) %>%
  mutate(horizon = NA_integer_)

fcst <- future_frame %>%
  filter(!!sym(series_id) == set_series) %>%
  filter(model %in% set_models) %>%
  filter(split %in% set_split) %>%
  rename(value = point)


# trend <- 0.95 ^ seq(1, n_ahead, 1)
# trend <- 1.05 ^ seq(1, n_ahead, 1)

# fcst_esn <- fcst %>%
#   filter(model == "ESN") %>%
#   mutate(value = value * trend) %>%
#   mutate(model = "ESN_trend")


# last_actual <- fcst %>%
#   filter(model == "NAIVE") %>%
#   pull(value)
# 
# 
# fcst_esn <- fcst %>%
#   filter(model == "ESN") %>%
#   mutate(value = (value + last_actual) / 2) %>%
#   mutate(model = "ESN_trend")
# 
# 
# df <- bind_rows(actual, fcst, fcst_esn)

df <- bind_rows(actual, fcst)







error <- accuracy_split %>%
  filter(!!sym(series_id) %in% set_series) %>%
  filter(model %in% set_models) %>%
  filter(metric %in% set_error)

# p <- plot_line(
#   data = error,
#   x = n,
#   y = value,
#   color = model
# )
# 
# ggplotly(p)


error %>%
  filter(n %in% set_split) %>%
  arrange(value)



p <- plot_line(
  data = df,
  x = !!sym(index_id),
  y = !!sym(value_id),
  color = model
)

ggplotly(p)



# Problems
# M30851 (21)
# M38262 (27)
# M47802 (30) --> peak-to-peak 7 month, then 4 month



# x <- actual %>%
#  pull(value)
# 
# 
# acf(x[51:80])
# pacf(x[51:80])






# Deep Dive ESN ###############################################################

lags <- list(c(1))
fourier <- NULL
xreg <- NULL
dy <- NULL
dx <- 0
inf_crit <- "aic"
alpha <- 1
rho <- 1
n_states <- 100 # 100
n_models <- 100 # 100
n_vars <- 12    #  10
n_seed <- 42
density <- 0.05
scale_win <- 0.5
scale_wres <- 0.5
scale_inputs <- c(-1, 1)



.x <- 1

# Slice training data according to split
train_frame <- slice_train(
  main_frame = main_frame,
  split_frame = split_frame[.x, ],
  context = context)


# Convert to tsibble, model and forecast
mable_frame <- train_frame %>%
  as_tsibble(
    index = !!sym(index_id),
    key = c(!!sym(series_id), split)) %>%
  model(
    "ESN" = ESN(
      !!sym(value_id),
      lags = lags,
      fourier = fourier,
      xreg = xreg,
      dy = dy,
      dx = dx,
      inf_crit = inf_crit,
      n_states = n_states,
      n_models = n_models,
      n_seed = n_seed,
      alpha = alpha,
      rho = rho,
      density = density,
      scale_win = scale_win,
      scale_wres = scale_wres,
      scale_inputs = scale_inputs))

fable_frame <- mable_frame %>%
  forecast(h = n_ahead)



# Convert fable to future_frame
future_frame <- make_future(
  fable = fable_frame,
  context = context
)


# Actual and fitted values ====================================================

train_frame <- train_frame %>%
  select(-category) %>%
  mutate(type = "ACTUAL") %>%
  mutate(.model = NA_character_)

resid_frame <- mable_frame %>%
  residuals() %>%
  as_tibble() %>%
  mutate(type = "RESID") %>%
  rename(value = .resid)

fitted_frame <- mable_frame %>%
  fitted() %>%
  as_tibble() %>%
  mutate(type = "FITTED") %>%
  rename(value = .fitted)




model_frame <- bind_rows(
  train_frame,
  fitted_frame
)

plot_line(
  data = model_frame,
  x = index,
  y = value,
  color = type
)




# Residuals ===================================================================

# plot_line(
#   data = resid_frame,
#   x = index,
#   y = value
# )



# Estimate sample partial autocorrelation function
corr_acf <- estimate_acf(
  .data = drop_na(resid_frame),
  context = context,
  lag_max = 24
)

corr_pacf <- estimate_pacf(
  .data = drop_na(resid_frame),
  context = context,
  lag_max = 24
)

corr_frame <- bind_rows(
  corr_acf,
  corr_pacf
)

# # Visualize PACF as correlogram
# corr_frame %>%
#   plot_bar(
#     x = lag,
#     y = value,
#     color = sign,
#     facet_var = type,
#     position = "dodge",
#     title = "Sample autocorrelation function",
#     xlab = "Lag",
#     ylab = "Correlation"
#   )





# Convert to tsibble, model and forecast
mable_frame <- resid_frame %>%
  drop_na() %>%
  as_tsibble(
    index = !!sym(index_id),
    key = c(!!sym(series_id), split)) %>%
  model(
    "ESN" = ESN(
      !!sym(value_id),
      lags = lags,
      fourier = fourier,
      xreg = xreg,
      dy = dy,
      dx = dx,
      inf_crit = inf_crit,
      n_states = n_states,
      n_models = n_models,
      n_vars = n_vars,
      n_seed = n_seed,
      alpha = alpha,
      rho = rho,
      density = density,
      scale_win = scale_win,
      scale_wres = scale_wres,
      scale_inputs = scale_inputs))

fable_frame2 <- mable_frame %>%
  forecast(h = n_ahead)




test_frame <- slice_test(
  main_frame = main_frame,
  split_frame = split_frame[.x, ],
  context = context)



df <- tibble(
  ac = test_frame$value,
  fc1 = fable_frame$.mean,
  fc2 = (fable_frame$.mean + fable_frame2$.mean)
)

plot(df$ac, type = "l", ylim = c(min(df), max(df)))
lines(df$fc1, col = "blue")
lines(df$fc2, col = "red")









model <- mable_frame$ESN[[1]]$fit$model

states <- model$states_train$`reservoir(1)`
states2 <- states^2
states3 <- states^3

rows <- c(100:200)
cols <- c(1:5)

matplot(states[rows, cols], type = "l")

matplot(states2[rows, cols], type = "l")

matplot(states3[rows, cols], type = "l")




main_summary <- main_frame %>%
  summarise_data(context = context) %>%
  mutate(n_vars = floor(n_obs * 0.1)) %>%
  mutate(n_vars2 = floor(n_obs * 0.7 * 0.1))

accuracy_mean2 <- accuracy_mean %>%
  select(-total) %>%
  filter(model %in% c("ETS", "THETA", "ESN")) %>%
  pivot_longer(
    cols = -model,
    names_to = "series",
    values_to = "value") %>%
  pivot_wider(
    names_from = "model",
    values_from = "value"
  )

df <- left_join(
  x = main_summary,
  y = accuracy_mean2,
  by = "series"
)






# Write data as Excel file
library(writexl)

data <- list(
  "main_frame" = main_frame,
  "future_frame" = future_frame,
  "accuracy_mean" = accuracy_mean
)

write_xlsx(x = data, path = paste0(folder, "/", "data.xlsx"))






library(lubridate)


train <- main_frame %>%
  filter(bidding_zone == "ES") %>%
  mutate(id = row_number()) %>%
  mutate(hour = hour(time)) %>%
  mutate(day = day(time)) %>%
  filter(id %in% c(1001:5800))

plot_line(
  data = train,
  x = time,
  y = value,
  facet_var = hour
)













mean_model <- accuracy_horizon %>%
  # filter(series %out% "M43782") %>% # 123_100
  # filter(series %out% "M31758") %>% # 42_100
  filter(series %out% "M42876") %>% # 666_100
  filter(metric == set_metric) %>%
  group_by(!!sym(series_id), model) %>%
  summarise(
    value = round(mean(value, na.rm = TRUE), 3),
    .groups = "drop")

mean_total <- mean_model %>%
  group_by(model) %>%
  summarise(
    value = round(mean(value, na.rm = TRUE), 3),
    .groups = "drop") %>%
  mutate(!!sym(series_id) := "TOTAL")

accuracy_mean <- bind_rows(
  mean_model,
  mean_total) %>%
  pivot_wider(
    names_from = model,
    values_from = value
  )








x <- accuracy_mean %>%
  pivot_longer(
    cols = -series,
    names_to = "model",
    values_to = "value") %>%
  arrange(series, model) %>%
  filter(model %in% c("ESN", "THETA"))
  # filter(series %out% "TOTAL")


y <- main_frame %>%
  summarise_data(context = context) %>%
  select(series, start, end, n_obs)

xx <- left_join(
  x = x,
  y = y,
  by = "series"
)


p <- plot_point(
  data = xx,
  x = n_obs,
  y = value,
  color = model,
  point_size = 4
)

ggplotly(p)



main_frame %>%
  filter(series == "M31758") %>%
  plot_line(
    x = index,
    y = value
    )



library(forecast)


x <- main_frame %>%
  filter(series == "M31758") %>%
  pull(value)

x <- ts(data = x, frequency = 12)

ndiffs(x)
nsdiffs(x)




median_model <- accuracy_horizon %>%
  filter(metric == set_metric) %>%
  group_by(!!sym(series_id), model) %>%
  summarise(
    value = round(median(value, na.rm = TRUE), 3),
    .groups = "drop")

median_total <- median_model %>%
  group_by(model) %>%
  summarise(
    value = round(median(value, na.rm = TRUE), 3),
    .groups = "drop") %>%
  mutate(!!sym(series_id) := "TOTAL")

accuracy_median <- bind_rows(
  median_model,
  median_total) %>%
  pivot_wider(
    names_from = model,
    values_from = value
  )






