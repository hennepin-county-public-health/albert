#' Generate age groups.
#'
#' Generate age groups based on 'age' variable, matching Census standards.
#' @param x age variable
#' @export

census_age_grp <- function(x){
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
    x >= 55 & x < 60 ~ "55-59",
    x >= 60 & x < 65 ~ "60-64",
    x >= 65 & x < 70 ~ "65-69",
    x >= 70 & x < 75 ~ "70-74",
    x >= 75 & x < 80 ~ "75-79",
    x >= 80 & x < 85 ~ "80-84",
    x >= 85 ~ "85+")
}
