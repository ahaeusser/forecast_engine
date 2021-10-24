
if ("EXPERT" %in% models) {
  
  info(
    logger = logger,
    message = "START  script/407_model_expert.R"
  )
  
  .start <- Sys.time()
  
  # EXPERT model ==============================================================
  
  # Train and forecast models -------------------------------------------------
  
  with_progress({
    p <- progressor(steps = nrow(grid))
    fcst_expert <- future_map_dfr(
      .x = seq_len(nrow(grid)),
      .f = ~{
        p()
        train %>%
          filter(BZN == grid$BZN[.x]) %>%
          filter(split == grid$split[.x]) %>%
          model("EXPERT" = EXPERT(Value, periods = period)) %>%
          forecast(h = n_ahead)
      })
  })
  
  # Save objects --------------------------------------------------------------
  
  save(
    object = fcst_expert,
    file = paste0(folder, "/", "fcst_expert.rda")
  )
  
  info(
    logger = logger,
    message = paste0(
      "FINISH script/407_model_expert.R",
      "\n",
      log_time(start = .start),
      "\n"
    )
  )
  
  } else {
  fcst_expert <- NULL
}
