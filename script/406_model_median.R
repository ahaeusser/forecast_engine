
if ("MEDIAN" %in% models) {
  
  info(
    logger = logger,
    message = "START  script/406_model_median.R"
  )
  
  .start <- Sys.time()
  
  # MEDIAN (median forecast) ======================================================
  
  # Train and forecast models -------------------------------------------------
  
  with_progress({
    p <- progressor(steps = nrow(split_frame))
    future_median <- future_map_dfr(
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
          model("MEDIAN" = MEDIAN(!!sym(value_id))) %>%
          forecast(h = n_ahead)
        # Convert fable to future_frame
        future_frame <- make_future(
          fable = fable_frame,
          context = context
        )
      })
  })
  
  # Store forecasts in future_frame -------------------------------------------
  
  future_frame[["MEDIAN"]] <- future_median
  rm(future_median)
  
  info(
    logger = logger,
    message = paste0(
      "FINISH script/406_model_median.R",
      "\n",
      log_time(start = .start),
      "\n"
    )
  )
}
