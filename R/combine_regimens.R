#' Combine STI antibiotic treatment regimens
#'
#' Combine drugs included in separate treatment regimens after using clean_antibiotics. Will deduplicate and order all drugs alphabetically in a single column.
#' @param data a dataframe with multiple treatment regimens
#' @param input1 a column with data for the first regimen
#' @param input2 a column with data for the second regimen
#' @param output the name of the new, combined variable
#' @param drop drop the input columns? Default is TRUE.
#' @export

combine_regimens <- function(data, input1, input2, output = "antibiotic_treatment", drop = TRUE){

  #Break each drug into its own column
  data2 <- data |>
    mutate(
      antibiotic_treatment = dplyr::case_when(
        !!sym(input1) != "Unknown" & !!sym(input2) != "Unknown" ~ stringr::str_c(!!sym(input1), !!sym(input2), sep = ", "),
        !!sym(input2) == "Unknown" ~ !!sym(input1),
        !!sym(input1) == "Unknown" ~ !!sym(input2)),
      #if_else(!!sym(input2) != "Unknown", str_c(!!sym(input1), !!sym(input2), sep = ", "), !!sym(input1)),
      max_drugs = stringr::str_count(antibiotic_treatment, ",") + 1) %>%
    tidyr::separate_wider_delim(antibiotic_treatment, names = str_c('ab', 1:max(.$max_drugs)), delim = ",", too_few = "align_start", too_many = "error")

  ab_ordered <- purrr::map(1:nrow(data2), function(x){
    temp <- data2 |>
      dplyr::filter(row_number() == x) |>
      dplyr::mutate(dplyr::across(tidyselect::matches("ab\\d"), ~dplyr::if_else(is.na(.), "", .)))

    #This will deduplicate and order all antibiotics alphabetically
    antibiotic_treatment <- tibble::tibble(!!sym(output) := stringr::str_c(stringr::str_sort(unique(c(stringr::str_trim(temp$ab1), stringr::str_trim(temp$ab2), stringr::str_trim(temp$ab3), {if("ab4" %in% names(temp)) stringr::str_trim(temp$ab4) else NULL}))), collapse = ",")) |>
      mutate(!!sym(output) := stringr::str_remove(!!sym(output), "^\\,"),
             !!sym(output) := stringr::str_replace_all(!!sym(output), ",", ", "))

    return(antibiotic_treatment)
  }) |>
    purrr::list_rbind()

  return(data %>%
           {if (drop == TRUE){select(., -c(!!sym(input1), !!sym(input2)))} else .} %>%
           dplyr::bind_cols(ab_ordered))
}
