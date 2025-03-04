#' Match date values
#'
#' This function is used to match dates when doing probabalistic matched. Rather than doing simple string or numeric comparison, it includes a few options through which potential matches can be evaluated.
#' @param data A linked dataframe based on probablistic matching of non-date fields.
#' @param date1 The date from the first matched dataset
#' @param date2 The date from the second matched dataset
#' @param exact Should only exact matches be retained? Default value is FALSE.
#' @param switch Should pairs that match once day and month are reversed in one date be kept? Default is TRUE.
#' @param threshold How many days is an acceptable difference between two dates? Default is 5, set to 0 to ignore.
#' @export

#This function is designed to be used during probalistic matching after two datasets have already been joined based on names etc.
#Typically use this before filtering out potential matches based on the match weight from the prior join.
#It's designed to allow matching of dates, taking into account some potential issues that may commonly occur.
#1) You have the option of only taking exact date matches. We often do this.
#2) You have the option of also keeping pairs that match once date and month have been switched. This can keep real matches affected by input errors.
#3) You can keep dates within a specified day range. Small differences may also be the result on input errors.
#After this function drops some potential matches, the remainder can be evaluated (automatically or manually) based on match weight and other info

match_dates <- function(data, date1, date2, exact = FALSE, switch = TRUE, threshold = 5){

  dt_trans <- data |>
    dplyr::mutate(d1 = !!sym(date1),
                  d2 = !!sym(date2),
                  dplyr::across(d1:d2, as.Date),
                  dplyr::across(d1:d2, as.numeric, .names = "{.col}_num"),
                  dif = abs(d1_num - d2_num),
                  dplyr::across(d1:d2, as.character, .names = "{.col}_chr"),
                  dplyr::across(d1_chr:d2_chr, ~str_replace(., "(....)\\-(..)\\-(..)", "\\1\\-\\3\\-\\2"), .names = "{.col}_switch"),
                  swap = ifelse(d1_chr == d2_chr_switch | d2_chr == d1_chr_switch, 1, 0))

  if (exact == TRUE){ #only keep exact matches

    dt_final <- dt_trans |> dplyr::filter(d1 == d2)
    print(paste(nrow(dt_final), "exact date matches,", nrow(dt_trans) - nrow(dt_final), "date mismatches dropped."))

  } else if (switch == TRUE) { #keep dates based on all 3 conditions

    dt_final <- dt_trans |>
      dplyr::mutate(dif = abs(d1_num - d2_num)) |>
      dplyr::filter(d1 == d2 | dif <= as.numeric(threshold) | swap == 1)

    print(paste(nrow(filter(dt_final, d1 == d2)), "exact date matches"))
    print(paste(nrow(filter(dt_final, dif <= as.numeric(threshold) & d1 != d2)), "retained because difference was within threshold."))
    print(paste(nrow(filter(dt_final, !(dif <= as.numeric(threshold) | d1 == d2) & swap == 1)), "retained because month and day may be switched."))
    print(paste(nrow(dt_trans) - nrow(dt_final), "date mismatches dropped."))

  } else { #keep exact matches, differences w/in threshold

    dt_final <- dt_trans |>
      dplyr::mutate(dif = abs(d1_num - d2_num)) |>
      dplyr::filter(d1 == d2 | dif <= as.numeric(threshold))

    print(paste(nrow(filter(dt_final, d1 == d2)), "exact date matches"))
    print(paste(nrow(filter(dt_final, dif <= as.numeric(threshold) & d1 != d2)), "differences within threshold"))
    print(paste(nrow(dt_trans) - nrow(dt_final), "date mismatches dropped."))

  }

  return(dt_final |> dplyr::select(-c(d1:swap)))
}
