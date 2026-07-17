
#' @title Negated value matching
#'
#' @description The function '%out%' is the negation of '%in%'.
#'
#' @param x Values to be matched.
#' @param table Values to be matched against.
#'
#' @return Logical vector, indicating if a non-match was located for each element of x
#' @export

'%out%' <- function(x,
                    table) {
  match(x, table, nomatch = 0L) == 0L
}
