#' Adjust PMI values
#'
#' Add leadings zeroes that are missing from PMI values.
#' @param df A dataframe
#' @param x PMI column/value
#' @export

pmi_leading_zero <- function(df, x) {
  df |>
    mutate({{ x }} := dplyr::if_else(nchar({{ x }}) == 1, stringr::str_c("0000000", {{ x }}), {{ x }}),
           {{ x }} := dplyr::if_else(nchar({{ x }}) == 2, stringr::str_c("000000", {{ x }}), {{ x }}),
           {{ x }} := dplyr::if_else(nchar({{ x }}) == 3, stringr::str_c("00000", {{ x }}), {{ x }}),
           {{ x }} := dplyr::if_else(nchar({{ x }}) == 4, stringr::str_c("0000", {{ x }}), {{ x }}),
           {{ x }} := dplyr::if_else(nchar({{ x }}) == 5, stringr::str_c("000", {{ x }}), {{ x }}),
           {{ x }} := dplyr::if_else(nchar({{ x }}) == 6, stringr::str_c("00", {{ x }}), {{ x }}),
           {{ x }} := dplyr::if_else(nchar({{ x }}) == 7, stringr::str_c("0", {{ x }}), {{ x }})
    )
}
