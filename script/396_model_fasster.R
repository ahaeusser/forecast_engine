
if ("FASSTER" %in% models) {
  
  .start <- Sys.time()
  .step <- "396_model_fasster.R"
  
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
          main_frame = main_frame,
          split_frame = split_frame[.x, ],
          context = context)
        # Convert to tsibble, model and forecast
        fable_frame <- train_frame %>%
          as_tsibble(
            index = !!sym(index_id),
            key = c(!!sym(series_id), split)) %>%
          model("FASSTER" = FASSTER(!!sym(value_id) ~ trend(1) + fourier(periods[1], periods[1]/2) + fourier(periods[2], periods[1]/4))) %>%
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
  
  # Store run time in time_frame ----------------------------------------------
  time_frame[["FASSTER"]] <- as.numeric(
    difftime(
      time1 = Sys.time(),
      time2 = .start, 
      units = "secs"
    )
  )
  
  write_lines(
    x = log_time(text = .step, start = .start),
    file = glue("{folder}/{run_name}.txt"),
    append = TRUE
  )
  
  print(
    log_time(
      text = .step, 
      start = .start,
      ft_bold = TRUE
    )
  )
}
