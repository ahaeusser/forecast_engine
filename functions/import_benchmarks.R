
#' @title Import M4 Competition Forecasts
#'
#' @description
#' Imports forecasts from an M4 Competition submission file and converts them
#' from wide to long format. Only forecasts for the time series contained in
#' `main_frame` are retained.
#'
#' The function supports forecast columns named either `X1`, `X2`, and so on,
#' or `F1`, `F2`, and so on. Future indices are constructed from the final
#' observed index of each series in `main_frame`.
#'
#' @param file Character value. Path to the CSV file containing the M4
#'   forecasts.
#' @param model Character value. Name assigned to the forecasting model.
#' @param main_frame A data frame containing the historical time series. It
#'   must contain the columns `series` and `index`. The `index` column must
#'   support addition of integer forecast horizons, such as a monthly or
#'   quarterly year-month index.
#' @param n_ahead Integer value. The forecast horizon.
#'
#' @return
#' A tibble with one row per series and forecast horizon and the following
#' columns:
#'
#' \describe{
#'   \item{index}{Future monthly or quarterly index.}
#'   \item{series}{Time series identifier.}
#'   \item{model}{Forecasting model name.}
#'   \item{split}{Forecast split identifier, fixed to `1`.}
#'   \item{horizon}{Forecast horizon.}
#'   \item{point}{Point forecast.}
#' }
#'
#' @details
#' The input CSV file must contain an `id` column identifying the time series
#' and forecast columns following one of these naming conventions:
#'
#' \itemize{
#'   \item `X1`, `X2`, ..., `Xh`
#'   \item `F1`, `F2`, ..., `Fh`
#' }
#'
#' Missing forecast values are removed. The function stops with an error if
#' the submission file does not contain forecasts for all series in
#' `main_frame`.
#'
#' Monthly and quarterly data should be processed separately so that the
#' `index` column has a consistent class.
#'
#' @examples
#' \dontrun{
#' future_mlp <- import_m4_forecasts(
#'   file = "submission-MLP.csv",
#'   model = "MLP",
#'   main_frame = main_frame)
#'
#' future_esrnn <- import_m4_forecasts(
#'   file = "submission-118.csv",
#'   model = "ES-RNN",
#'   main_frame = main_frame)
#' }
#'
#' @export

import_benchmarks <- function(file, 
                              model, 
                              main_frame,
                              n_ahead) {
  
  # Forecast origin for each selected series
  forecast_origin <- main_frame |>
    group_by(series) |>
    summarise(
      index = max(index) - n_ahead,
      .groups = "drop"
    )
  
  # Import and reshape forecasts
  forecast_values <- read_csv(
    file,
    show_col_types = FALSE,
    progress = FALSE) |>
    rename(series = id) |>
    semi_join(
      forecast_origin,
      by = "series") |>
    pivot_longer(
      cols = matches("^[XF][0-9]+$"),
      names_to = "horizon",
      names_pattern = "[XF]([0-9]+)",
      names_transform = list(
        horizon = as.integer),
      values_to = "point",
      values_drop_na = TRUE
    )
  
  # Check whether all required series are available
  missing_series <- setdiff(
    forecast_origin$series,
    forecast_values$series
  )
  
  if (length(missing_series) > 0) {
    stop(
      paste(
        "Missing forecasts for:",
        paste(missing_series, collapse = ", ")
      ),
      call. = FALSE
    )
  }
  
  # Create future frame
  forecast_values |>
    left_join(
      forecast_origin,
      by = "series") |>
    mutate(
      index = index + horizon,
      model = model,
      split = 1L) |>
    select(
      index,
      series,
      model,
      split,
      horizon,
      point) |>
    arrange(
      series,
      horizon
    )
}
