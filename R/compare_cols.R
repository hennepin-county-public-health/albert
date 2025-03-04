#' Compare column types between DF.
#'
#' ID differences in column type before combining dataframes. It will also identify columns not present in both dataframes.
#' @param df1 A dataframe
#' @param df2 A dataframe
#' @export

compare_cols <- function(df1, df2){ #if ever necessary, could adjust to compare 2+

  mismatches <- df1 %>%
    purrr::map_df(class) %>%
    tidyr::pivot_longer(cols = tidyselect::everything(), values_to = "type1", names_to = "names") %>%
    full_join(df2 %>%
                purrr::map_df(class) %>%
                tidyr::pivot_longer(cols = tidyselect::everything(), values_to = "type2", names_to = "names"),
              by = "names") #%>%
    #dplyr::filter(type1 != type2) #remove this, can add this behavior afterward

  return(mismatches)
}
