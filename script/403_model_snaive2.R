
if ("SNAIVE2" %in% models) {
  
  info(
    logger = logger,
    message = "START  script/403_model_snaive2.R"
  )
  
  .start <- Sys.time()
  
  # SNAIVE2 (seasonal naive forecast) =========================================
  
  # Train and forecast models -------------------------------------------------
  
  with_progress({
    p <- progressor(steps = nrow(grid))
    fcst_snaive2 <- future_map_dfr(
      .x = seq_len(nrow(grid)),
      .f = ~{
        p()
        train %>%
          filter(BZN == grid$BZN[.x]) %>%
          filter(split == grid$split[.x]) %>%
          model("SNAIVE2" = SNAIVE2(Value)) %>%
          forecast(h = n_ahead)
      })
  })
  
  # Save objects --------------------------------------------------------------
  
  save(
    object = fcst_snaive2,
    file = paste0(folder, "/", "fcst_snaive2.rda")
  )
  
  info(
    logger = logger,
    message = paste0(
      "FINISH script/403_model_snaive2.R",
      "\n",
      log_time(start = .start),
      "\n"
    )
  )

  } else {
    fcst_snaive2 <- NULL
}
