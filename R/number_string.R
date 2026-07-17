
#' @title Helper function to create numbered strings.
#'
#' @description Creates a character vector in the form
#'   \code{c("x(1)", x(2), ..., x(n))}.
#'
#' @param x Character value.
#' @param n Integer value.
#'
#' @return x Character vector.
#' @export

number_string <- function(x, n) {
  x <- paste0(
    x,"(",
    formatC(
      x = 1:n,
      width = nchar(n),
      flag = "0"),
    ")")
  
  return(x)
}
