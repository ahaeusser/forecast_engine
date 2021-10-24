
### Main script for calculations ##############################################

source("logging.R")

source("script/100_pre_processing.R")
source("script/200_user_settings.R")
source("script/300_prepare_data.R")
source("script/401_model_esn.R")
source("script/402_model_snaive.R")
source("script/403_model_snaive2.R")
source("script/404_model_stl-naive.R")
source("script/405_model_stl-arima.R")
source("script/406_model_stl-ets.R")
source("script/407_model_expert.R")
source("script/408_model_dhr-arima.R")
source("script/409_model_dshw.R")
source("script/410_model_tbats.R")
source("script/411_model_elm.R")
source("script/412_model_fasster.R")
source("script/413_model_arima.R")
source("script/500_evaluate_accuracy.R")
source("script/600_post_processing.R")

metrics_mean

# M4_monthly_data %>%
#   plot_line(
#     x = date_time,
#     y = Value,
#     facet_var = series_id
#   )


metrics_split %>%
  filter(metric == "MAPE") %>%
  plot_error_metrics(
    title = "Evaluation of forecast accuracy",
    subtitle = "Mean absolute percentage error (MAPE)",
    xlab = "Forecast horizon (n-step-ahead)"
  )

# Visualize forecasts
plot_forecast(
  fcst = fcst,
  data = bind_rows(train, test),
  include = 24,
  split = 15
)


