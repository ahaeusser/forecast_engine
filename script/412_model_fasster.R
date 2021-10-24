
if ("FASSTER" %in% models) {
  
  info(
    logger = logger,
    message = "START  script/412_model_fasster.R"
  )
  
  .start <- Sys.time()
  
  # FASSTER ===================================================================
  
  # Train and forecast models -------------------------------------------------
  
  with_progress({
    p <- progressor(steps = nrow(grid))
    fcst_fasster <- future_map_dfr(
      .x = seq_len(nrow(grid)),
      .f = ~{
        p()
        train %>%
          filter(BZN == grid$BZN[.x]) %>%
          filter(split == grid$split[.x]) %>%
          model("FASSTER" = FASSTER(Value ~ trend(1) + fourier("day", 3) + fourier("week", 3))) %>%
          forecast(h = n_ahead)
      })
  })
  
  # Save objects --------------------------------------------------------------
  
  save(
    object = fcst_fasster,
    file = paste0(folder, "/", "fcst_fasster.rda")
  )
  
  info(
    logger = logger,
    message = paste0(
      "FINISH script/412_model_fasster.R",
      "\n",
      log_time(start = .start),
      "\n"
    )
  )
  
  } else {
  fcst_fasster <- NULL
}
