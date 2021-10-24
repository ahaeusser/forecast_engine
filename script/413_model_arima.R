
if ("ARIMA" %in% models) {
  
  info(
    logger = logger,
    message = "START  script/413_model_arima.R"
  )
  
  .start <- Sys.time()
  
  # ARIMA =====================================================================
  
  # Train and forecast models -------------------------------------------------
  
  with_progress({
    p <- progressor(steps = nrow(grid))
    fcst_arima <- future_map_dfr(
      .x = seq_len(nrow(grid)),
      .f = ~{
        p()
        train %>%
          filter(series_id == grid$series_id[.x]) %>%
          filter(split == grid$split[.x]) %>%
          model("ARIMA" = ARIMA(Value)) %>%
          forecast(h = n_ahead)
      })
  })
  
  # Save objects --------------------------------------------------------------
  
  save(
    object = fcst_arima,
    file = paste0(folder, "/", "fcst_arima.rda")
  )
  
  info(
    logger = logger,
    message = paste0(
      "FINISH script/413_model_arima.R",
      "\n",
      log_time(start = .start),
      "\n"
    )
  )

  } else {
  fcst_arima <- NULL
}
