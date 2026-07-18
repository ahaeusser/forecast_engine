
#' @title Paste sequential numbers to names
#'
#' @description Appends zero-padded sequential numbers to a character vector 
#'   using a hyphen as the separator.
#'
#' @param x A character vector containing the names.
#' @param n A positive integer specifying the length of the sequence.
#'
#' @return A character vector containing the names with appended sequential
#' numbers.
#'
#' @examples
#' paste_names(
#'   x = "model",
#'   n = 10
#' )
#'
#' @export

paste_names <- function(x, n) {
  x <- paste0(
    x,"-",
    formatC(
      x = 1:n,
      width = nchar(n),
      flag = "0"))
  
  return(x)
}
