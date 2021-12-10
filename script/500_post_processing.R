
.start <- Sys.time()
.step <- "500_post_processing.R"

# Post-processing =============================================================

save.image(file = paste0(folder, "/", "workspace.RData"))

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
