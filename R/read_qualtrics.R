#' Read Qualtrics data.
#'
#' Read and clean .csv files in the standard Qualtrics export format.
#' @param path file path
#' @export

read_qualtrics <- function(path){
  readr::read_csv(path) %>%
    to_snake_case() %>%
    dplyr::slice(3:nrow(.)) %>%
    dplyr::select(-c(x1, start_date:ip_address, duration_in_seconds, recipient_email, location_latitude:distribution_channel)) %>%
    dplyr::mutate(dplyr::across(where(is.character), ~stringr::str_replace_all(., "â€™", "'"))) #this is a common error in text imported from Qualtrics
}
