#' Data validation
#'
#' Check whether unexpected values are included in a column. This function with create a boolean row that indicates unexpected values, and can be combined with check_expectations to halt a pipeline or export violations for review.
#' @param data a dataframe with data to validate
#' @param value_pairs a named list of columns and their acceptable values
#' @param na_accept should NA values be considered valid?
#' @param warn should the function itself warn about violations? Alternatively, use check_expectations(). 
#' @export

expect_values <- function(data, value_pairs, na_accept = FALSE, warn = FALSE){
  
  #Create a expectation column for each supplied column.
  temp <- map(seq_along(value_pairs), function(x){
    
    temp_col <- names(value_pairs)[x]
    temp_vals <- value_pairs[[x]]
    
    #If TRUE, NA values are not treated as violations.
    if (na_accept == TRUE){
      temp_vals <- c(temp_vals, NA)
    }
    
    meta_col <- data |>
      mutate("exp_{temp_col}_xvalue" := if_else(!!sym(temp_col) %in% {temp_vals}, TRUE, FALSE), .keep = "none")
    
    #If warn is true, print out number of violations
    if (warn == TRUE){
      viols <- nrow(filter(meta_col, !!sym(names(meta_col)[1]) == FALSE))
      message(str_c(viols, " ", if_else(viols == 1, "violation", "violations"), " of the expected *VALUES* in *", temp_col, "*"))
    }
    
    return(meta_col)
  })
  
  #Add new columns to the data
  return(cbind(data, temp))
}
