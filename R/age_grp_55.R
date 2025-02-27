#' Generate age groups.
#'
#' Generate age groups based on 'age' variable, up to 55+. Commonly used in HC reports.
#' @param x age variable
#' @export

age_grp_55 <- function(x){
  dplyr::case_when(
    x < 10 ~ "<10",
    x >= 10 & x < 15 ~ "10-14",
    x >= 15 & x < 20 ~ "15-19",
    x >= 20 & x < 25 ~ "20-24",
    x >= 25 & x < 30 ~ "25-29",
    x >= 30 & x < 35 ~ "30-34",
    x >= 35 & x < 40 ~ "35-39",
    x >= 40 & x < 45 ~ "40-44",
    x >= 45 & x < 50 ~ "45-49",
    x >= 50 & x < 55 ~ "50-54",
    x >= 55 ~ "55+")
}
