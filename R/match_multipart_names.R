#' Adjust probabilistic matching.
#'
#' Using fields created in clean_match, enhances matching accuracy by better handling potential matches where at least one record has a multi-part last name. If only one last name is multi-part, it tests whether the single name is part of the other. If both are, it tests whether reversing one of the names results in a direct match. It produces the variables `last_partial` and `last_switched` which can be used when filtering matches to those we accept.
#' @param x a dataframe
#' @export

match_multipart_names <- function(x){

  #Fail if required fields are missing
  if (!all(c("last_flag_1", "last_flag_2", "last_name_raw_1", "last_name_raw_1") %in% names(data))){
    stop("Required fields `last_flag` and/or `last_name_raw` are missing. Possible causes could include:\n  - These columns were not created by clean_match(multi_flag = TRUE).\n  - These columns were not added as excluded columns in the match.\n  - These column names were not cleaned to tidy format after matching.")
  }

  #I haven't added any particular error handling for three-part+ names yet.. something to think about
  temp <- x |>
    dplyr::mutate(
      #First, flag those with one multi-part name but not two.
      last_flag = last_flag_1 + last_flag_2,
      #Then, test whether the single name is part of the multi-part one.
      last_partial = case_when(
        last_flag != 1 ~ 0,
        last_flag_1 == 1 & stringr::str_detect(last_name_1, paste0("^", last_name_2, ".*$")) ~ 1,
        last_flag_1 == 1 & stringr::str_detect(last_name_1, paste0("^.*", last_name_2, "$")) ~ 1,
        last_flag_2 == 1 & stringr::str_detect(last_name_2, paste0("^", last_name_1, ".*$")) ~ 1,
        last_flag_2 == 1 & stringr::str_detect(last_name_2, paste0("^.*", last_name_1, "$")) ~ 1,
        .default = 0),
      #Now, if both are multi-part names, test what happens if one is reversed
      last_name_switched = ifelse(last_flag == 2, last_name_raw_1, NA),
      last_name_switched = ifelse(!is.na(last_name_switched), stringr::str_replace_all(last_name_switched, "([A-Z]*) (.*$)", "\\2 \\1"), NA),
      last_switched = dplyr::case_when(
        is.na(last_name_switched) ~ 0,
        last_name_switched == last_name_raw_2 ~ 1,
        .default = 0)) |>
    select(-last_flag, -last_name_switched, -last_name_raw_1, -last_name_raw_2) #remove these fields, unless we want them for other things

  return(temp)
}
