
if ("ESN" %in% models) {
  
  .start <- Sys.time()
  .step <- "399_model_esn.R"
  
  # ESN (Echo State Network) ==================================================
  
  # Train and forecast models -------------------------------------------------
  
  future_esn <- with_progress({
    future_map_dfr(
      .x = seq_len(nrow(split_frame)),
      .f = ~{
        
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
          model("ESN" = ESN(!!sym(value_id))) %>%
          forecast(h = n_ahead)
        
        # Convert fable to future_frame
        future_frame <- make_future(
          fable = fable_frame,
          context = context
        )
        
      },
      .progress = TRUE
    )
  })
  
  # Store forecasts in future_frame -------------------------------------------
  
  future_frame[["ESN"]] <- future_esn
  rm(future_esn)
  
  # Store run time in time_frame ----------------------------------------------
  time_frame[["ESN"]] <- as.numeric(
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
