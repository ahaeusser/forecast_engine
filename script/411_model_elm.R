
if ("ELM" %in% models) {
  
  info(
    logger = logger,
    message = "START  script/411_model_elm.R"
  )
  
  .start <- Sys.time()
  
  # ELM (Extreme Learning Machine) ============================================
  
  # Train and forecast models -------------------------------------------------
  
  set.seed(42)
  
  with_progress({
    p <- progressor(steps = nrow(grid))
    fcst_elm <- future_map_dfr(
      .x = seq_len(nrow(grid)),
      .f = ~{
        p()
        train %>%
          filter(BZN == grid$BZN[.x]) %>%
          filter(split == grid$split[.x]) %>%
          model("ELM" = ELM(Value)) %>%
          forecast(h = n_ahead)
      })
  })
  
  # Save objects --------------------------------------------------------------
  
  save(
    object = fcst_elm,
    file = paste0(folder, "/", "fcst_elm.rda")
  )
  
  info(
    logger = logger,
    message = paste0(
      "FINISH script/411_model_elm.R",
      "\n",
      log_time(start = .start),
      "\n"
    )
  )
  
  } else {
  fcst_elm <- NULL
}
