
#' @title Assign objects within a list to an environment
#'
#' @description \code{lst_to_env} is a helper function that assigns
#'   the objects within a list to an environment.
#'
#' @param x A list containing the objects to assign.
#' @param envir The environment to use (default is \code{.GlobalEnv}).
#' @param ... Further arguments passed to \code{assign()}.

#' @export

lst_to_env <- function(x,
                       envir = .GlobalEnv,
                       ...) {
  
  lst_names <- names(x)
  
  for (i in lst_names) {
    assign(
      x = i,
      value = x[[i]],
      envir = envir,
      ...
    )
  }
}
