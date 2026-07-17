
#' @title Create a name for a folder or file
#'
#' @description The function creates a name for a folder or file.
#'
#' @param primary Character value. Primary file name to be used.
#' @param extension Character value. File name extension to be used
#'   (e.g. \code{".csv"}, \code{".rda"}).
#' @param add_time Logical value. If \code{TRUE}, a time stamp is added in
#'   front of the file name.
#' @param time_format Character value. The time format of the time stamp.
#'
#' @return Character value.
#' @export

file_name <- function(primary,
                      extension = NULL,
                      add_time = TRUE,
                      time_format = "%Y%m%d_%H%M%S") {
  
  if (add_time == TRUE) {
    time_stamp <- format(Sys.time(), format = time_format)
    time_stamp <- paste0(time_stamp, "_")
  } else {
    time_stamp <- NULL
  }
  
  paste0(time_stamp, primary, extension)
}
