
if ("TBATS" %in% models) {
  
  info(
    logger = logger,
    message = "START  script/411_model_tbats.R"
  )
  
  .start <- Sys.time()
  
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
          model("TBATS" = TBATS(!!sym(value_id), periods = period)) %>%
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
  
  info(
    logger = logger,
    message = paste0(
      "FINISH script/411_model_tbats.R",
      "\n",
      log_time(start = .start),
      "\n"
    )
  )
}
