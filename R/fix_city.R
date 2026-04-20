#' Clean city names.
#'
#' Cleans city names of common errors using the CityNameFix.txt file.
#' @param data a dataframe
#' @param variable name of city variable
#' @param path path to CityNameFix.txt
#' @export
#'
fix_city <- function(data, variable, path){

  city_fix <- readr::read_csv(paste0(path, "CityNameFix.txt")) |>
    janitor::clean_names() |>
    dplyr::distinct()

  temp <- data |>
    dplyr::mutate({{variable}} := toupper(!!sym(variable))) |>
    dplyr::left_join(city_fix, by = setNames("incorrect_city", variable)) %>%
    pipe_print(paste("----", nrow(filter(., !is.na(corrected_city))), "CITY NAMES FIXED ----")) |>
    dplyr::mutate({{variable}} := ifelse(!is.na(corrected_city), corrected_city, !!sym(variable))) |>
    dplyr::select(-c(objectid, corrected_city))

  return(temp)
}
