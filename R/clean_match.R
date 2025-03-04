#' Clean data before probabilistic matching.
#'
#' Clean first and last name fields before matching with RecordLinkage or other tools.
#' @param x a dataframe
#' @param first name of variable with first name
#' @param last name of variable with first name
#' @param multi_flag will flag last names that appear to be multi-part for use later in the matching process
#' @export

#One common issue is that people who have a multi-part/hypenated last name in one data source receive a lower match weight than is reasonable
#using common sense because the algo needs to make quite a few changes to add an entire new surname. Especially the case when the new part is
#quite long. Can use the multi_flag option to ID when this may be the case, so weights can be manually adjusted if necessary.
#Results will be flagged if any of these conditions is true:
#1) There is a space somewhere in the name, 2) there is a hyphen in the name, 3) There are multiple uppercase letters separated by at least 3 lowercase letters
#See adj_name_wt for how this is used, but essentially if the flag exists in one dataset but not the other it will check to see if the complete single name
#makes up either the beginning or end of the multipart name. If so, adjust up to a minimum value such as .9. Could condition this on first name being
#a perfect match.

clean_match <- function(x, first = "first_name", last = "last_name", multi_flag = FALSE){
  temp <- x |>
    dplyr::mutate({{last}} := stringr::str_to_title(!!sym(last)), #next line will have issues if its already all upper
                  last_flag = ifelse(stringr::str_detect(!!sym(last), "\\-|^[:upper:]\\w\\w\\w+[:upper:]\\w\\w\\w+"), 1, 0),
                  dplyr::across(c(tidyselect::all_of(first), tidyselect::all_of(last)), ~stringr::str_remove_all(.x, "[[:punct:]]")),
                  dplyr::across(c(tidyselect::all_of(first), tidyselect::all_of(last)), toupper),
                  dplyr::across(c(tidyselect::all_of(first), tidyselect::all_of(last)), ~stringr::str_remove_all(.x, " JR| IV$| III| II| SR$")),
                  {{last}} := stringr::str_trim(!!sym(last)),
                  last_flag = ifelse(stringr::str_detect(!!sym(last), "\\s"), 1, last_flag),
                  dplyr::across(c(tidyselect::all_of(first), tidyselect::all_of(last)), ~stringr::str_remove_all(.x, " ")))

  if (multi_flag == FALSE){

    return(temp |> dplyr::select(-last_flag))

  } else {

    message(paste(sum(temp$last_flag), "potential multi-part last names IDed."))
    return(temp)

  }
}
