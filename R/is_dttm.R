#' Test for dttm
#'
#' Return true if input is in datetime fornmat. Intended behavior similar to is.numeric.
#' @param x filepath
#' @export

is_dttm <- function (x)
{
  inherits(x, "POSIXct")
}

#taken from the function vld_date_time from the chk package
#trace(vld_date_time, edit = TRUE)
