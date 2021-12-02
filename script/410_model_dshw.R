
if ("DSHW" %in% models) {
  
  info(
    logger = logger,
    message = "START  script/410_model_dshw.R"
  )
  
  .start <- Sys.time()
  
  # DSHW (Double Seasonal Holt-Winters) =======================================
  
  # Train and forecast models -------------------------------------------------
  
  with_progress({
    p <- progressor(steps = nrow(split_frame))
    future_dshw <- future_map_dfr(
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
          mutate(!!sym(value_id) := !!sym(value_id) + shift) %>%
          model("DSHW" = DSHW(!!sym(value_id), periods = period)) %>%
          forecast(h = n_ahead)
        # Convert fable to future_frame
        future_frame <- make_future(
          fable = fable_frame,
          context = context) %>%
          mutate(point = point - shift)
      })
  })
  
  # Store forecasts in future_frame -------------------------------------------
  
  future_frame[["DSHW"]] <- future_dshw
  rm(future_dshw)
  
  info(
    logger = logger,
    message = paste0(
      "FINISH script/410_model_dshw.R",
      "\n",
      log_time(start = .start),
      "\n"
    )
  )
}
