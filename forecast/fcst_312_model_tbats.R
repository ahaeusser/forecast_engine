
if ("TBATS" %in% models) {
  
  .start <- Sys.time()
  .step <- "312_model_tbats.R"
  
  # TBATS (Trigonometric Box-Cox ARMA Trend and Season) =======================
  
  # Train and forecast models -------------------------------------------------
  
  with_progress({
    p <- progressor(steps = nrow(split_frame))
    future_tbats <- future_map_dfr(
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
          model("TBATS" = TBATS(!!sym(value_id), periods = periods)) %>%
          forecast(h = n_ahead)
        # Convert fable to future_frame
        future_frame <- make_future(
          fable = fable_frame,
          context = context)
      })
  })
  
  # Store forecasts in future_frame -------------------------------------------
  
  future_frame[["TBATS"]] <- future_tbats
  rm(future_tbats)
  
  # Store run time in time_frame ----------------------------------------------
  time_frame[["TBATS"]] <- as.numeric(
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
