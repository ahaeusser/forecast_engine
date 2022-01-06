
if ("ARX" %in% models) {
  
  .start <- Sys.time()
  .step <- "397_model_arx.R"
  
  # ARX (Autoregressive model) ================================================
  
  # Train and forecast models -------------------------------------------------
  
  with_progress({
    p <- progressor(steps = nrow(split_frame))
    future_arx <- future_map_dfr(
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
          model(
            "ARX" = ARX(
              !!sym(value_id),
              lags = lags,
              fourier = fourier,
              xreg = xreg,
              dy = dy,
              dx = dx,
              inf_crit = inf_crit,
              n_models = n_models,
              n_seed = n_seed,
              scale_inputs = scale_inputs)) %>%
          forecast(h = n_ahead)
        # Convert fable to future_frame
        future_frame <- make_future(
          fable = fable_frame,
          context = context
        )
      })
  })
  
  # Store forecasts in future_frame -------------------------------------------
  
  future_frame[["ARX"]] <- future_arx
  rm(future_arx)
  
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
