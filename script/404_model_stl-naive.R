
if ("STL-NAIVE" %in% models) {
  
  info(
    logger = logger,
    message = "START  script/404_model_stl-naive.R"
  )
  
  .start <- Sys.time()
  
  # STL-NAIVE (STL decomposition plus naive forecast) =========================
  
  # Train and forecast models -------------------------------------------------
  
  with_progress({
    p <- progressor(steps = nrow(grid))
    fcst_stl_naive <- future_map_dfr(
      .x = seq_len(nrow(grid)),
      .f = ~{
        p()
        train %>%
          filter(series_id == grid$series_id[.x]) %>%
          filter(split == grid$split[.x]) %>%
          model("STL-NAIVE" = decomposition_model(STL(Value), NAIVE(season_adjust))) %>%
          forecast(h = n_ahead)
      })
  })
  
  # Save objects --------------------------------------------------------------
  
  save(
    object = fcst_stl_naive,
    file = paste0(folder, "/", "fcst_stl_naive.rda")
  )
  
  info(
    logger = logger,
    message = paste0(
      "FINISH script/404_model_stl-naive.R",
      "\n",
      log_time(start = .start),
      "\n"
    )
  )
  
  } else {
  fcst_stl_naive <- NULL
}
