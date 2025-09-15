#' Generate age groups.
#'
#' Generate age groups based on age value. Most potential age group schemes should be achievable, barring those with unequally sized buckets. Function defaults match Census standards.
#' @param age age column in a dataframe
#' @param group_size the size of the age groups (besides the minimum and maximum groups)
#' @param group_min the upper cutoff of the bottom group (i.e 10 for "<10")
#' @param group_max the bottom cutoff for the top group (i.e. 85 for "85+" )
#' @returns a character vector.
#' @examples
#' ages <- data.frame(age = 1:100)
#' create_age_group(ages[12,])
#'
#' Or use inside of mutate()
#' dplyr::mutate(ages, age_group = create_age_group(age))

create_age_group <- function(age, group_size = 5, group_min = 10, group_max = 85){

  #Make sure age isn't negative - value could be NA placeholder or error
  if (sum(age < 0, na.rm = TRUE) != 0){
    stop("Ages below 0 detected. Clean before group creation.")
  }

  if (sum(age > 130, na.rm = TRUE) != 0){
    stop("Ages above 130 detected. Clean before group creation.")
  }

  agen <- as.numeric(gsub("\\D", "", age))

  #Raise errors given unexpected input
  if (is.na(group_size) | group_size == 0){
    stop("Specify a group size of at least 1.")
  }

  #Deal with missing min/max values
  if (is.na(group_min)){
    group_min <- 0
  }

  if (is.na(group_max)){
    group_max <- max(agen) + (max(agen) %% group_size)
  }

  #could still add something for max must be bigger than min

  range = group_min:group_max

  purrr::map_chr(agen, function(y){
    if (is.na(y)){
      "Unknown"
    } else if (y < group_min){
      paste0("<", group_min)
    } else if (y >= group_max){
      paste0(group_max, "+")
    } else {
      paste0(max(range[range %% group_size == 0 & range <= y]), "-",
             max(range[range %% group_size == 0 & range <= y]) + (group_size - 1))}
  })
}
