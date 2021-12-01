
if ("DHR-ARIMA" %in% models) {
  
  info(
    logger = logger,
    message = "START  script/412_model_dhr-arima.R"
  )
  
  .start <- Sys.time()
  
  # Dynamic harmonic regression with ARMA errors ==============================
  
  # Train and forecast models -------------------------------------------------
  
  with_progress({
    p <- progressor(steps = nrow(split_frame))
    future_dhr_arima <- future_map_dfr(
      .x = seq_len(nrow(split_frame)),
      .f = ~{
        p()
        # Slice training data according to split
        train_frame <- slice_train(
          main = main_frame,
          split = split_frame[.x, ],
          context = context)
        # Convert to tsibble, model and forecast
        fable_frame <- train_frame %>%
          as_tsibble(
            index = !!sym(index_id),
            key = c(!!sym(series_id), split)) %>%
          model("DHR-ARIMA" = ARIMA(!!sym(value_id) ~ fourier(period[1], period[1]/2) + fourier(period[2], period[1]/4) + pdq(d = 0) + PDQ(0, 0, 0))) %>%
          forecast(h = n_ahead)
        # Convert fable to future_frame
        future_frame <- make_future(
          fable = fable_frame,
          context = context)
      })
  })
  
  # Store forecasts in future_frame -------------------------------------------
  
  future_frame[["DHR-ARIMA"]] <- future_dhr_arima
  rm(future_dhr_arima)
  
  info(
    logger = logger,
    message = paste0(
      "FINISH script/412_model_dhr-arima.R",
      "\n",
      log_time(start = .start),
      "\n"
    )
  )
}
