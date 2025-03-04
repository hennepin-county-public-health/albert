#' Adjust probabilistic matching.
#'
#' Flag matches where first names do not match, but are a valid given name - nickname pair. This flag could then be used to adjust weighting or to allow variable weighting. This is useful because when given names and nicknames are quite different (i.e. Theodore, Ted) matches may not have a very high weight.
#' @param data a dataframe
#' @param name1 name of first matched first name field
#' @param name2 name of second matched first name field
#' @export

#Originally found this data source here: https://github.com/carltonnorthern/nicknames
#But decided to do this ourselves so we don't need to rely on their update schedule/infrastructure.

flag_nicknames <- function(data, name1 = "first_name_1", name2 = "first_name_2"){
  
  if (!exists('names_to_nicknames')){
    
    names_to_nicknames <- rvest::read_html("https://www.caagri.org/nicknames.html") |>
      rvest::html_table() |>
      pluck(1) |>
      rename(given_name = 1, nickname = 2) |>
      slice(-1) |>
      filter(!str_detect(given_name, "^\\("),
             given_name != "",
             !is.na(given_name)) |>
      #Some given names have multiple combined names. Separate.
      separate_longer_delim(given_name, delim = ",") |>
      #For ease of matching, move each nickname to it's own row.
      separate_longer_delim(nickname, delim = ",") |>
      mutate(across(everything(), str_to_upper),
             across(everything(), str_trim),
             across(everything(), ~str_remove_all(., " ")),
             across(everything(), ~str_remove(., "\\(.*\\)"))) |>
      filter(nickname != "" & nickname != " ") |>
      distinct()
    
    assign("names_to_nicknames", names_to_nicknames, envir = globalenv())
    message("names_to_nicknames dataframe written to globalenv")
  }
  
  temp <- map(1:nrow(data), function(x){
    
    name_pair <- data |>
      slice(x) |>
      select(!!sym(name1), !!sym(name2)) |>
      rename(name1 = 1, name2 = 2)
    
    nickname <- names_to_nicknames |>
      filter((given_name %in% name_pair$name1 & nickname %in% name_pair$name2) | (given_name %in% name_pair$name2 & nickname %in% name_pair$name1))
    
    nickname_flag = if_else(nrow(nickname) != 0, 1, 0) |>
      tibble(nickname_flag = _)
    
    return(nickname_flag)
  })
  
  nns <- sum(bind_rows(temp))
  message(paste0(nns, " given name - nickname match", if_else(nns == 1, " IDed.", "es IDed.")))
  return(cbind(data, bind_rows(temp)))
}
