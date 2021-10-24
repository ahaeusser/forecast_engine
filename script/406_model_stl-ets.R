
if ("STL-ETS" %in% models) {
  
  info(
    logger = logger,
    message = "START  script/406_model_stl-ets.R"
  )
  
  .start <- Sys.time()
  
  # STL-ARIMA (STL decomposition plus ETS forecast) ===========================
  
  # Train and forecast models -------------------------------------------------
  
  with_progress({
    p <- progressor(steps = nrow(grid))
    fcst_stl_ets <- future_map_dfr(
      .x = seq_len(nrow(grid)),
      .f = ~{
        p()
        train %>%
          filter(BZN == grid$BZN[.x]) %>%
          filter(split == grid$split[.x]) %>%
          model("STL-ETS" = decomposition_model(STL(Value), ETS(season_adjust ~ season("N")))) %>%
          forecast(h = n_ahead)
      })
  })
  
  # Save objects --------------------------------------------------------------
  
  save(
    object = fcst_stl_ets,
    file = paste0(folder, "/", "fcst_stl_ets.rda")
  )
  
  info(
    logger = logger,
    message = paste0(
      "FINISH script/406_model_stl-ets.R",
      "\n",
      log_time(start = .start),
      "\n"
    )
  )
  
  } else {
  fcst_stl_ets <- NULL
}
