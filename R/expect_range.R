#' Data validation
#'
#' Check whether column values are outside of the expected range. This function with create a boolean row that indicates unexpected values, and can be combined with check_expectations to halt a pipeline or export violations for review.
#' @param data a dataframe with data to validate
#' @param value_pairs a named list of columns and their acceptable range (a vector with a minimum and maximum number or date in the YMD format)
#' @param na_accept should NA values be considered valid?
#' @param warn should the function itself warn about violations? Alternatively, use check_expectations(). 
#' @export

#data: a dataframe with data to validate
#value_pairs: a named list of columns and their acceptable range (minium and maximum numbers or dates in the YMD format)
#na_accept: should NA values be considered valid or invalid?
#warn: should the function itself warn about violations -- alternatively, can use check_expections()
expect_range <- function(data, value_pairs, na_accept = FALSE, warn = FALSE){
  
  #Create a expectation column for each supplied column.
  temp <- map(seq_along(value_pairs), function(x){
    
    temp_col <- names(value_pairs)[x]
    
    temp_min <- value_pairs[[x]][1]
    temp_max <- value_pairs[[x]][2]
    
    #Handle either date or number - better way?
    if (is.na(lubridate::ymd(temp_min))){
      temp_min <- as.numeric(temp_min)
      temp_max <- as.numeric(temp_max)
    }
    
    if(!is.na(lubridate::ymd(temp_min))){
      temp_min <- lubridate::ymd(temp_min)
      temp_max <- lubridate::ymd(temp_max)
    }
    
    #Stop if max or min is missing. Cannot supply just one.
    #If one of them is not desired, can supply a very low or high artificial value.
    if (is.na(temp_max) | is.na(temp_min)){
      stop(str_c(temp_col, ": Missing minimum or maximum value. Check that supplied format was numeric or YMD."))
    }
    
    #Stop if max is not greater than min
    if (temp_max <= temp_min){
      stop(str_c(temp_col, ": Maximum value must be greater than minimum."))
    }
    
    meta_col <- data |>
      mutate("exp_{temp_col}_xrange" := if_else(!!sym(temp_col) >= temp_min & !!sym(temp_col) <= temp_max, TRUE, FALSE), .keep = "none") |>
      #convert NA to TRUE or FALSE depending on argument
      mutate(across(everything(), ~ if_else(is.na(.), as.logical(na_accept), .)))
    
    #If warn is true, print out number of violations
    if (warn == TRUE){
      viols <- nrow(filter(meta_col, !!sym(names(meta_col)[1]) == FALSE))
      message(str_c(viols, " ", if_else(viols == 1, "violation", "violations"), " of the expected *RANGE* in *", temp_col, "*"))
    }
    
    return(meta_col)
  })
  
  #Add new columns to the data
  return(cbind(data, temp)) 
}