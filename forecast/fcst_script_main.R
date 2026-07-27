
### Main script for calculations ##############################################

# source("forecast/fcst_config_m4_mth.R")
source("forecast/fcst_config_m4_qtr.R")

source("forecast/fcst_100_pre_processing.R")
source("forecast/fcst_200_prepare_input.R")
  source("forecast/fcst_301_model_naive.R")
  source("forecast/fcst_302_model_naive2.R")
  source("forecast/fcst_303_model_drift.R")
  source("forecast/fcst_304_model_snaive.R")
  source("forecast/fcst_305_model_mean.R")
  source("forecast/fcst_306_model_arima.R")
  source("forecast/fcst_307_model_ets.R")
  source("forecast/fcst_308_model_theta.R")
  source("forecast/fcst_309_model_tbats.R")
  source("forecast/fcst_310_model_esn.R")
  source("forecast/fcst_399_model_import.R")
source("forecast/fcst_400_prepare_output.R")
source("forecast/fcst_500_post_processing.R")
