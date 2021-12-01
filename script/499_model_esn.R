
if ("ESN" %in% models) {
  
  info(
    logger = logger,
    message = "START  script/499_model_esn.R"
  )
  
  .start <- Sys.time()
  
  # ESN (Echo State Network) ==================================================
  
  # Train and forecast models -------------------------------------------------
  
  with_progress({
    p <- progressor(steps = nrow(grid))
    lst_esn <- future_map(
      .x = seq_len(nrow(grid)),
      .f = ~{
        p()
        # Train models
        mdls_esn <- train %>%
          filter(series_id == grid$series_id[.x]) %>%
          filter(split == grid$split[.x]) %>%
          model(
            "ESN" = ESN(
              Value,
              lags = lags,
              fourier = fourier,
              xreg = xreg,
              dy = dy,
              dx = dx,
              inf_crit = inf_crit,
              n_states = n_states,
              n_seed = n_seed,
              alpha = alpha,
              rho = rho,
              density = density,
              scale_win = scale_win,
              scale_wres = scale_wres,
              scale_inputs = scale_inputs
            )
          )
        
        # # Extract model details
        # mdls_esn_tbl <- mdls_esn %>%
        #   extract_esn()
        
        # Forecast models
        fcst_esn <- mdls_esn %>%
          forecast(
            h = n_ahead,
            n_sim = NULL
          )
        
        # Store results
        list(
          # mdls_esn_tbl = mdls_esn_tbl,
          fcst_esn = fcst_esn
        )
      })
  })
  
  # Extract objects
  # mdls_esn_tbl <- future_map_dfr(
  #   .x = seq_len(nrow(grid)),
  #   .f = ~{lst_esn[[.x]][["mdls_esn_tbl"]]
  #     })
  
  fcst_esn <- future_map_dfr(
    .x = seq_len(nrow(grid)),
    .f = ~{lst_esn[[.x]][["fcst_esn"]]
      })
  
  # Store forecasts in future_frame -------------------------------------------
  
  future_frame[["ESN"]] <- future_esn
  rm(future_esn)
  
  # save(
  #   object = mdls_esn_tbl,
  #   file = paste0(folder, "/", "mdls_esn_tbl.rda")
  # )
  
  info(
    logger = logger,
    message = paste0(
      "FINISH script/499_model_esn.R",
      "\n",
      log_time(start = .start),
      "\n"
    )
  )
}
