
info(
  logger = logger,
  message = "START  script/600_post_processing.R"
)

.start <- Sys.time()

# Post-processing =============================================================

save.image(file = paste0(folder, "/", "workspace.RData"))

info(
  logger = logger,
  message = paste0(
    "FINISH script/600_post_processing.R",
    "\n",
    log_time(start = .start),
    "\n"
  )
)
