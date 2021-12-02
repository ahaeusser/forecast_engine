
if ("STL-ETS" %in% models) {
  
  info(
    logger = logger,
    message = "START  script/415_model_stl-ets.R"
  )
  
  .start <- Sys.time()
  
  # STL-ARIMA (STL decomposition plus ETS forecast) ===========================
  
  # Train and forecast models -------------------------------------------------
  
  with_progress({
    p <- progressor(steps = nrow(split_frame))
    future_stl_ets <- future_map_dfr(
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
          model("STL-ETS" = decomposition_model(STL(!!sym(value_id)), ETS(season_adjust ~ season("N")))) %>%
          forecast(h = n_ahead)
        # Convert fable to future_frame
        future_frame <- make_future(
          fable = fable_frame,
          context = context)
      })
  })
  
  # Store forecasts in future_frame -------------------------------------------
  
  future_frame[["STL-ETS"]] <- future_stl_ets
  rm(future_stl_ets)
  
  info(
    logger = logger,
    message = paste0(
      "FINISH script/415_model_stl-ets.R",
      "\n",
      log_time(start = .start),
      "\n"
    )
  )
}
