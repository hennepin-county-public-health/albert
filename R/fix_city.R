#' Clean city names.
#'
#' Cleans all city names of common errors using the CityNameFix.txt file.
#' @param data a dataframe
#' @param x name of city variable
#' @export
#'
fix_city <- function(data, x){

  city_fix <- readr::read_csv("/dbfs/mnt/phmdw/Trusted/PublicHealth/Reference/MN geography/CityNameFix.txt") %>%
    janitor::clean_names() |>
    dplyr::distinct()
    

  temp <- data %>%
    dplyr::mutate({{x}} := toupper(!!sym(x))) %>%
    dplyr::left_join(city_fix, by= setNames("incorrect_city", x)) %>%
    pipe_print(paste("####", nrow(filter(., !is.na(corrected_city))), "CITY NAMES FIXED ####")) %>%
    dplyr::mutate({{x}} := ifelse(!is.na(corrected_city), corrected_city, !!sym(x))) %>%
    dplyr::select(-c(objectid, corrected_city))

  return(temp)
}
