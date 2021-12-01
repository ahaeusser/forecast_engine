

# Empty list

models <- c(
  "NAIVE",
  "DRIFT",
  "SNAIVE",
  "SNAIVE2",
  "MEAN"
  # "TSLM",
  # "ARIMA",
  # "ETS",
  # "DSHW",
  # "TBATS",
  # "DHR-ARIMA",
  # "STL-NAIVE",
  # "STL-ARIMA",
  # "STL-ETS",
  # "ELM",
  # "FASSTER"
  # "EXPERT",
  # "ESN"
)

future_naive <- tibble(
  x = 1:10,
  y = 11:20,
  z = "NAIVE"
)

future_drift <- tibble(
  x = 1:10,
  y = 11:20,
  z = "DRIFT"
)



future_frame <- vector(
  mode = "list",
  length = length(models)
  )

names(future_frame) <- models

future_frame$NAIVE <- future_naive
rm(future_naive)

future_frame$DRIFT <- future_drift
rm(future_drift)

future_frame

future_frame <- bind_rows(future_frame)

future_frame




# Read and write

test_frame <- tibble(
  x = 1:10,
  y = 11:20
)




folder <- "C:/Users/Alexander Haeusser/Projects/forecast_engine/my_name.rds"


write_rds(
  x = test_frame,
  file = folder
)



saveRDS(
  object = test_frame,
  file = folder
)


write_object <- function(object,
                         folder) {
  
  object_name <- deparse(substitute(object))
  path <- paste0(folder, "/", object_name, ".rds")
  
  write_rds(
    object = object,
    file = path
  )
  
}

future_frame2 <- future_frame

write_object(
  object = future_frame2,
  folder = folder
)







read_object <- function(object,
                        folder) {
  
}


file_name


object <- "future_frame2"
folder <- folder


assign(
  x = object,
  value = readRDS(file = paste0(folder, "/", object, ".rds"))
)
