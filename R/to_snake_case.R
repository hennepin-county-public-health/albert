#' Data cleaning
#'
#' Convert column names to snake_case. Similar functionality to janitor::clean_names()
#' @param data a dataframe
#' @export
#'
to_snake_case <- function(data){

  cln <- function(nms){
    stringr::str_replace_all(nms, "\\s|\\.|\\-", "_") |>
      stringr::str_replace_all("([:lower:])([:upper:])", "\\1_\\2") |>
      stringr::str_replace_all("([:upper:][:upper:])([:upper:]*[:lower:])", "\\1_\\2") |>
      stringr::str_replace_all("%", "_percent") |>
      stringr::str_replace_all("&", "and") |>
      stringr::str_replace_all("#", "number") |>
      stringr::str_remove_all("\\(|\\)") |>
      stringr::str_replace_all("_+", "_") |>
      stringr::str_replace_all("([_])|[[:punct:]]", "\\1") |>
      stringr::str_to_lower()
  }
  names(data) <- cln(names(data))
  return(data)
}
