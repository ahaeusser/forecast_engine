
if ("DHR-ARIMA" %in% models) {
  
  info(
    logger = logger,
    message = "START  script/408_model_dhr-arima.R"
  )
  
  .start <- Sys.time()
  
  # Dynamic harmonic regression with ARMA errors ==============================
  
  # Train and forecast models -------------------------------------------------
  
  with_progress({
    p <- progressor(steps = nrow(grid))
    fcst_dhr_arima <- future_map_dfr(
      .x = seq_len(nrow(grid)),
      .f = ~{
        p()
        train %>%
          filter(BZN == grid$BZN[.x]) %>%
          filter(split == grid$split[.x]) %>%
          model("DHR-ARIMA" = ARIMA(Value ~ fourier("day", 10) + fourier("week", 5) + pdq(d = 0) + PDQ(0, 0, 0))) %>%
          forecast(h = n_ahead)
      })
  })
  
  # Save objects --------------------------------------------------------------
  
  save(
    object = fcst_dhr_arima,
    file = paste0(folder, "/", "fcst_dhr_arima.rda")
  )
  
  info(
    logger = logger,
    message = paste0(
      "FINISH script/408_model_dhr-arima.R",
      "\n",
      log_time(start = .start),
      "\n"
    )
  )
  
  } else {
  fcst_dhr_arima <- NULL
}
