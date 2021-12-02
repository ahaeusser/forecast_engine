
if ("DRIFT" %in% models) {
  
  info(
    logger = logger,
    message = "START  script/402_model_drift.R"
  )
  
  .start <- Sys.time()
  
  # DRIFT (random walk plus drift forecast) ===================================
  
  # Train and forecast models -------------------------------------------------
  
  with_progress({
    p <- progressor(steps = nrow(split_frame))
    future_drift <- future_map_dfr(
      .x = seq_len(nrow(split_frame)),
      .f = ~{
        p()
        # Slice training data according to split
        train_frame <- slice_train(
          main_frame = main_frame,
          split_frame = split_frame[.x, ],
          context = context)
        # Convert to tsibble, model and forecast
        fable_frame <- train_frame %>%
          as_tsibble(
            index = !!sym(index_id),
            key = c(!!sym(series_id), split)) %>%
          model("DRIFT" = RW(!!sym(value_id) ~ drift())) %>%
          forecast(h = n_ahead)
        # Convert fable to future_frame
        future_frame <- make_future(
          fable = fable_frame,
          context = context
        )
      })
  })
  
  # Store forecasts in future_frame -------------------------------------------
  
  future_frame[["DRIFT"]] <- future_drift
  rm(future_drift)
  
  info(
    logger = logger,
    message = paste0(
      "FINISH script/402_model_drift.R",
      "\n",
      log_time(start = .start),
      "\n"
    )
  )
}
