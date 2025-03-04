#' Data validation
#'
#' Check whether columns values are not in the expected format. This function with create a boolean row that indicates unexpected values, and can be combined with check_expectations to halt a pipeline or export violations for review.
#' @param data a dataframe with data to validate
#' @param value_pairs a named list of columns and their acceptable format (using regex/string comparison)
#' @param na_accept should NA values be considered valid?
#' @param warn should the function itself warn about violations? Alternatively, use check_expectations(). 
#' @export

expect_format <- function(data, value_pairs, na_accept = FALSE, warn = FALSE){
  
  #Create a expectation column for each supplied column.
  temp <- map(seq_along(value_pairs), function(x){
    
    temp_col <- names(value_pairs)[x]
    temp_patrn <- value_pairs[[x]]
    
    meta_col <- data |>
      mutate("exp_{temp_col}_xformat" := if_else(str_detect(!!sym(temp_col), temp_patrn), TRUE, FALSE), .keep = "none") |>
      #convert NA to TRUE or FALSE depending on argument
      mutate(across(everything(), ~ if_else(is.na(.), as.logical(na_accept), .)))
    
    #If warn is true, print out number of violations
    if (warn == TRUE){
      viols <- nrow(filter(meta_col, !!sym(names(meta_col)[1]) == FALSE))
      message(str_c(viols, " ", if_else(viols == 1, "violation", "violations"), " in the expected *FORMAT* of *", temp_col, "*"))
    }
    
    return(meta_col)
    
  })
  
  #Add new columns to the data
  return(cbind(data, temp))
}