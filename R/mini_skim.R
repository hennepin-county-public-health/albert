#' Skim dataframe
#'
#' Simple version of skimr::skim so I don't need to load the package.
#' @param .data a dataframe
#' @param status the text to print
#' @export
#'
mini_skim <- function(x, display = TRUE){
  types <- x %>%
    purrr::map_df(class) %>%
    tidyr::pivot_longer(cols = tidyselect::everything(), names_to = "Variable", values_to = "Type")

  type_count <- types %>% dplyr::count(Type)

  n_missing <- tibble::tibble(Variable = names(x), n_missing = colSums(is.na(x)), n = nrow(x)) %>%
    dplyr::mutate(complete_rate = round(1- (n_missing/n), digits = 5)) %>%
    dplyr::select(-n)

  output <- types %>%
    dplyr::left_join(n_missing, by = "Variable") %>%
    dplyr::mutate(across(c(n_missing, complete_rate), as.character)) %>%
    tibble::add_row(Variable = "-", Type= "-", n_missing = "-", complete_rate = "-", .before = 1) %>%
    tibble::add_row(Variable = "Numeric", Type= as.character(dplyr::filter(type_count, Type == "numeric")$n), n_missing = "-", complete_rate = "-", .before = 1) %>%
    tibble::add_row(Variable = "Logical", Type= as.character(dplyr::filter(type_count, Type == "logical")$n), n_missing = "-", complete_rate = "-", .before = 1) %>%
    tibble::add_row(Variable = "Date", Type= as.character(dplyr::filter(type_count, Type == "Date")$n), n_missing = "-", complete_rate = "-", .before = 1) %>%
    tibble::add_row(Variable = "Character", Type= as.character(dplyr::filter(type_count, Type == "character")$n), n_missing = "-", complete_rate = "-", .before = 1) %>%
    tibble::add_row(Variable = "Variable Type", Type= "Count", n_missing = "-", complete_rate = "-", .before = 1) %>%
    tibble::add_row(Variable = "-", Type= "-", n_missing = "-", complete_rate = "-", .before = 1)

  #may want to turn this off if I want to do further transformation of the data, such as sorting etc.
  if (display == TRUE){
    display(output)
  }
}
