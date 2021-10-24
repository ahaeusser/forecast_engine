
if ("DSHW" %in% models) {
  
  info(
    logger = logger,
    message = "START  script/409_model_dshw.R"
  )
  
  .start <- Sys.time()
  
  # DSHW (Double Seasonal Holt-Winters) =======================================
  
  # Train and forecast models -------------------------------------------------
  
  with_progress({
    p <- progressor(steps = nrow(grid))
    fcst_dshw <- future_map_dfr(
      .x = seq_len(nrow(grid)),
      .f = ~{
        p()
        train %>%
          filter(BZN == grid$BZN[.x]) %>%
          filter(split == grid$split[.x]) %>%
          mutate(Value = Value + shift) %>%
          model("DSHW" = DSHW(Value, periods = period)) %>%
          forecast(h = n_ahead) %>%
          mutate(.mean = .mean - shift) %>%
          mutate(Value = dist_normal(mu = .mean, sigma = NA_real_))
      })
  })
  
  # Save objects --------------------------------------------------------------
  
  save(
    object = fcst_dshw,
    file = paste0(folder, "/", "fcst_dshw.rda")
  )
  
  info(
    logger = logger,
    message = paste0(
      "FINISH script/409_model_dshw.R",
      "\n",
      log_time(start = .start),
      "\n"
    )
  )
  
  } else {
  fcst_dshw <- NULL
}
