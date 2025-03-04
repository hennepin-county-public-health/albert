#' Impute missing date information.
#'
#' Impute missing information for dates in character, YYYYMMDD, format. Used in HIV and HIV D2C scripts. Not useful for dates in other formats.
#' @param x a dataframe
#' @param month the value imputed for a missing month
#' @param year the value imputed for a missing day
#' @export

impute_date <- function(x, month = "06", day = "15"){
  x2 <- str_remove_all(x, "[.]")

  case_when(
    is.na(x2) ~ as.character(NA),
    nchar(x2)==8 ~ x2,
    nchar(x2)==6 ~ paste0(x2, month),
    nchar(x2)==4 ~ paste0(x2, day),
    TRUE ~ "ERROR"
  )
}
