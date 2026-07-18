
#' @title Calculate Mean Absolute Scaled Error
#'
#' @description Calculates the mean absolute scaled error (MASE) using the 
#'   in-sample naive forecast error as the scaling factor.
#'
#' @param test A numeric vector of observed out-of-sample values.
#' @param forecast A numeric vector of forecast values corresponding to
#'   `test`.
#' @param train A numeric vector of in-sample training values.
#' @param periods A positive integer specifying the lag used for the naive
#'   benchmark. The default is `1`.
#' @param na_rm A logical value indicating whether missing values should be
#'   removed when calculating the means. The default is `TRUE`.
#'
#' @return A numeric value containing the mean absolute scaled error.
#'
#' @details
#' The numerator is the out-of-sample mean absolute error. The denominator is
#' the in-sample mean absolute error of a naive forecast with lag `periods`.
#'
#' @examples
#' train <- 1:10
#' test <- c(11, 12, 13)
#' forecast <- c(10.5, 12.5, 12.8)
#'
#' mase_vec(
#'   test = test,
#'   forecast = forecast,
#'   train = train
#' )
#'
#' @export

mase_vec <- function(test,
                     forecast,
                     train,
                     periods = 1,
                     na_rm = TRUE) {
  # Numerator (out-of-sample MAE)
  numerator <- mean(abs(test - forecast), na.rm = na_rm)
  # Denominator (in-sample m-step naive MAE)
  naive <- lag(x = train, n = periods)
  denominator <- mean(abs(train - naive), na.rm = na_rm)
  # MASE
  numerator / denominator
}
