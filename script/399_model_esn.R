
if ("ESN" %in% models) {
  
  .start <- Sys.time()
  .step <- "399_model_esn.R"
  
  # ESN (Echo State Network) ==================================================
  
  # Train and forecast models -------------------------------------------------
  
  with_progress({
    p <- progressor(steps = nrow(split_frame))
    future_esn <- future_map_dfr(
      .x = seq_len(nrow(split_frame)),
      .f = ~{
        p()
        # Slice training data according to split
        train_frame <- slice_train(
          main_frame = main_frame,
          split_frame = split_frame[.x, ],
          context = context)
        # Convert to tsibble, model and forecast
        fable_frame <- train_frame %>%
          as_tsibble(
            index = !!sym(index_id),
            key = c(!!sym(series_id), split)) %>%
          model(
            "ESN" = ESN(
              !!sym(value_id),
              lags = lags,
              fourier = fourier,
              xreg = xreg,
              dy = dy,
              dx = dx,
              inf_crit = inf_crit,
              n_states = n_states,
              n_models = n_models,
              n_vars = n_vars,
              n_seed = n_seed,
              alpha = alpha,
              rho = rho,
              density = density,
              scale_win = scale_win,
              scale_wres = scale_wres,
              scale_inputs = scale_inputs)) %>%
          forecast(h = n_ahead)
        # Convert fable to future_frame
        future_frame <- make_future(
          fable = fable_frame,
          context = context
        )
      })
  })
  
  # with_progress({
  #   p <- progressor(steps = nrow(grid))
  #   lst_esn <- future_map(
  #     .x = seq_len(nrow(grid)),
  #     .f = ~{
  #       p()
  #       # Train models
  #       mdls_esn <- train %>%
  #         filter(series_id == grid$series_id[.x]) %>%
  #         filter(split == grid$split[.x]) %>%
  #         model(
  #           "ESN" = ESN(
  #             Value,
  #             lags = lags,
  #             fourier = fourier,
  #             xreg = xreg,
  #             dy = dy,
  #             dx = dx,
  #             inf_crit = inf_crit,
  #             n_states = n_states,
  #             n_seed = n_seed,
  #             alpha = alpha,
  #             rho = rho,
  #             density = density,
  #             scale_win = scale_win,
  #             scale_wres = scale_wres,
  #             scale_inputs = scale_inputs
  #           )
  #         )
  #       
  #       # # Extract model details
  #       # mdls_esn_tbl <- mdls_esn %>%
  #       #   extract_esn()
  #       
  #       # Forecast models
  #       fcst_esn <- mdls_esn %>%
  #         forecast(
  #           h = n_ahead,
  #           n_sim = NULL
  #         )
  #       
  #       # Store results
  #       list(
  #         # mdls_esn_tbl = mdls_esn_tbl,
  #         fcst_esn = fcst_esn
  #       )
  #     })
  # })
  
  # Extract objects
  # mdls_esn_tbl <- future_map_dfr(
  #   .x = seq_len(nrow(grid)),
  #   .f = ~{lst_esn[[.x]][["mdls_esn_tbl"]]
  #     })
  
  # fcst_esn <- future_map_dfr(
  #   .x = seq_len(nrow(grid)),
  #   .f = ~{lst_esn[[.x]][["fcst_esn"]]
  #     })
  
  # Store forecasts in future_frame -------------------------------------------
  
  future_frame[["ESN"]] <- future_esn
  rm(future_esn)
  
  # save(
  #   object = mdls_esn_tbl,
  #   file = paste0(folder, "/", "mdls_esn_tbl.rda")
  # )
  
  write_lines(
    x = log_time(text = .step, start = .start),
    file = glue("{folder}/{run_name}.txt"),
    append = TRUE
  )
  
  print(
    log_time(
      text = .step, 
      start = .start,
      ft_bold = TRUE
    )
  )
}
