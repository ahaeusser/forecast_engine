
if ("SNAIVE" %in% models) {
  
  info(
    logger = logger,
    message = "START  script/402_model_snaive.R"
  )
  
  .start <- Sys.time()
  
  # SNAIVE (seasonal naive forecast) ==========================================
  
  # Train and forecast models -------------------------------------------------
  
  with_progress({
    p <- progressor(steps = nrow(grid))
    fcst_snaive <- future_map_dfr(
      .x = seq_len(nrow(grid)),
      .f = ~{
        p()
        train %>%
          filter(BZN == grid$BZN[.x]) %>%
          filter(split == grid$split[.x]) %>%
          model("SNAIVE" = SNAIVE(Value ~ lag("week"))) %>%
          forecast(h = n_ahead)
      })
  })
  
  # Save objects --------------------------------------------------------------
  
  save(
    object = fcst_snaive,
    file = paste0(folder, "/", "fcst_snaive.rda")
  )
  
  info(
    logger = logger,
    message = paste0(
      "FINISH script/402_model_snaive.R",
      "\n",
      log_time(start = .start),
      "\n"
    )
  )

  } else {
  fcst_snaive <- NULL
}
