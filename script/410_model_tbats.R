
if ("TBATS" %in% models) {
  
  info(
    logger = logger,
    message = "START  script/410_model_tbats.R"
  )
  
  .start <- Sys.time()
  
  # TBATS (Trigonometric Box-Cox ARMA Trend and Season) =======================
  
  # Train and forecast models -------------------------------------------------
  
  with_progress({
    p <- progressor(steps = nrow(grid))
    fcst_tbats <- future_map_dfr(
      .x = seq_len(nrow(grid)),
      .f = ~{
        p()
        train %>%
          filter(BZN == grid$BZN[.x]) %>%
          filter(split == grid$split[.x]) %>%
          model("TBATS" = TBATS(Value, periods = period)) %>%
          forecast(h = n_ahead)
      })
  })
  
  # Save objects --------------------------------------------------------------
  
  save(
    object = fcst_tbats,
    file = paste0(folder, "/", "fcst_tbats.rda")
  )
  
  info(
    logger = logger,
    message = paste0(
      "FINISH script/410_model_tbats.R",
      "\n",
      log_time(start = .start),
      "\n"
    )
  )
  
  } else {
  fcst_tbats <- NULL
}
