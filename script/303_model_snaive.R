
if ("SNAIVE" %in% models) {
  
  .start <- Sys.time()
  .step <- "303_model_snaive.R"
  
  # SNAIVE (seasonal naive forecast) ==========================================
  
  # Train and forecast models -------------------------------------------------
  
  with_progress({
    p <- progressor(steps = nrow(split_frame))
    future_snaive <- future_map_dfr(
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
          model("SNAIVE" = SNAIVE(!!sym(value_id) ~ lag(max(periods)))) %>%
          forecast(h = n_ahead)
        # Convert fable to future_frame
        future_frame <- make_future(
          fable = fable_frame,
          context = context
        )
      })
  })
  
  # Store forecasts in future_frame -------------------------------------------
  
  future_frame[["SNAIVE"]] <- future_snaive
  rm(future_snaive)
  
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
