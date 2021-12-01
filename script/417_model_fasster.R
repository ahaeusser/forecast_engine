
if ("FASSTER" %in% models) {
  
  info(
    logger = logger,
    message = "START  script/417_model_fasster.R"
  )
  
  .start <- Sys.time()
  
  # FASSTER ===================================================================
  
  # Train and forecast models -------------------------------------------------
  
  with_progress({
    p <- progressor(steps = nrow(split_frame))
    future_fasster <- future_map_dfr(
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
          model("FASSTER" = FASSTER(!!sym(value_id) ~ trend(1) + fourier(period[1], period[1]/2) + fourier(period[2], period[1]/4))) %>%
          forecast(h = n_ahead)
        # Convert fable to future_frame
        future_frame <- make_future(
          fable = fable_frame,
          context = context)
      })
  })
  
  # Store forecasts in future_frame -------------------------------------------
  
  future_frame[["FASSTER"]] <- future_fasster
  rm(future_fasster)
  
  info(
    logger = logger,
    message = paste0(
      "FINISH script/417_model_fasster.R",
      "\n",
      log_time(start = .start),
      "\n"
    )
  )
}
