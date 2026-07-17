
.start <- Sys.time()
.step <- "399_model_import.R"

if (c("MLP") %in% models) {
  
  # Multilayer Perceptron (MLP) ===============================================
  
  future_mlp <- import_model(
    file = "data/submission-MLP.csv",
    model = "MLP",
    main_frame = main_frame,
    n_ahead = n_ahead
  )
  
  future_frame[["MLP"]] <- future_mlp
  rm(future_mlp)
}

if (c("RNN") %in% models) {
  
  # Recurrent Neural Networks (RNN) ===========================================
  future_rnn <- import_model(
    file = "data/submission-RNN.csv",
    model = "RNN",
    main_frame = main_frame,
    n_ahead = n_ahead
  )
  
  future_frame[["RNN"]] <- future_rnn
  rm(future_rnn)
}

if (c("ES-RNN") %in% models) {
  
  # Exponential Smoothing + Recurrent Neural Network (ES-RNN) =================
  # (winning solution; submission no. 118)
  
  future_118 <- import_model(
    file = "data/submission-118.csv",
    model = "ES-RNN",
    main_frame = main_frame,
    n_ahead = n_ahead
  )
  
  future_frame[["ES-RNN"]] <- future_118
  rm(future_118)
}

# Write log file ============================================================

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
