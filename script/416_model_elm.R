
if ("ELM" %in% models) {
  
  info(
    logger = logger,
    message = "START  script/416_model_elm.R"
  )
  
  .start <- Sys.time()
  
  # ELM (Extreme Learning Machine) ============================================
  
  # Train and forecast models -------------------------------------------------
  
  set.seed(42)
  
  with_progress({
    p <- progressor(steps = nrow(split_frame))
    future_elm <- future_map_dfr(
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
          model("ELM" = ELM(!!sym(value_id))) %>%
          forecast(h = n_ahead)
        # Convert fable to future_frame
        future_frame <- make_future(
          fable = fable_frame,
          context = context
        )
      })
  })
  
  # Store forecasts in future_frame -------------------------------------------
  
  future_frame[["ELM"]] <- future_elm
  rm(future_elm)
  
  info(
    logger = logger,
    message = paste0(
      "FINISH script/416_model_elm.R",
      "\n",
      log_time(start = .start),
      "\n"
    )
  )
}
