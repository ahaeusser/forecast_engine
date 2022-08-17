
if ("MLP" %in% models) {
  
  .start <- Sys.time()
  .step <- "318_model_mlp.R"
  
  # MLP (Multilayer Perceptron) ===============================================
  
  # Train and forecast models -------------------------------------------------
  
  set.seed(42)
  
  with_progress({
    p <- progressor(steps = nrow(split_frame))
    future_mlp <- future_map_dfr(
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
          model("MLP" = MLP(!!sym(value_id))) %>%
          forecast(h = n_ahead)
        # Convert fable to future_frame
        future_frame <- make_future(
          fable = fable_frame,
          context = context
        )
      })
  })
  
  # Store forecasts in future_frame -------------------------------------------
  
  future_frame[["MLP"]] <- future_mlp
  rm(future_mlp)
  
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
