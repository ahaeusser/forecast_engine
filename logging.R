
#' @title Create string with elapsed time.
#' 
#' @description Create a string with the elapsed time between start and end for
#'   the log file.
#'
#' @param start Start date and time.
#' @param end End date and time.
#' @param digits Integer value. The number of digits for rounding.
#'
#' @return Character value.

log_time <- function(start = Sys.time(),
                     end = Sys.time(),
                     digits = 1) {
  diff <- end - start
  time <- round(diff[[1]], digits = digits)
  unit <- units(diff)
  x <- glue("TIME  [{time} {unit}]")
  return(x)
}


#' @title Create string with header for log file.
#' 
#' @description Create a string with information about the current R session
#'   for the log file.
#'
#' @return Character value.

log_header <- function() {
  
  session <- session_info()$platform
  user <- Sys.info()[["user"]]
  rstudio <- versionInfo()
  
  x <- glue(
    "-------------------------------------- \n",
    "version: {session$version} \n",
    "os:      {session$os} \n",
    "system:  {session$system} \n",
    "ui:      {session$ui} {rstudio$version} \n",
    "user:    {user} \n",
    "date:    {session$date} \n",
    "tz:      {session$tz} \n",
    "-------------------------------------- \n"
  )
  return(x)
}
