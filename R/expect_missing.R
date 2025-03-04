#' Data validation
#'
#' Check the percentage of column values that are missing does not exceed a threshold. This function with create a boolean row that indicates unexpected values, and can be combined with check_expectations to halt a pipeline or export violations for review.
#' @param data a dataframe with data to validate
#' @param value_pairs a list where names are column names and values are the maximum expected missing percentage (between 0 and 1)
#' @param warn should the function itself warn about violations? Alternatively, use check_expectations(). 
#' @export

expect_missing <- function(data, value_pairs, warn = FALSE){
  
  #Loop through each list item (column)
  temp <- map(seq_along(value_pairs), function(c){
    
    temp_col <- names(value_pairs)[c]
    temp_perc <- as.numeric(value_pairs[[c]])
    
    #Ensure % is between 0 and 1
    if (temp_perc < 0 | temp_perc > 1){
      stop("Percent missing thresholds must be between 0 and 1.")
    }
    
    #Get # of missing rows
    n_missing <- data |>
      select(!!sym(temp_col)) |>
      filter(is.na(!!sym(temp_col))) |>
      nrow()
    
    #Calculate % missing
    p_missing <- n_missing/nrow(data)
    log_value <- if_else(p_missing > temp_perc, FALSE, TRUE)
    
    #Warn, if TRUE
    if (warn == TRUE & log_value == FALSE){
      message(paste0("Expectation violated: Percent *MISSING* (", p_missing*100, "%) in *", temp_col, "* exceeded expected (", temp_perc*100, "%)"))
    }
    
    #Create exp_ column, return
    cname <- paste0("exp_", temp_col, "_xmissing") #define exp_column name
    exp_col <- tibble(!!sym(cname) := rep(log_value, nrow(data)))
    return(exp_col)
    
  })
  
  return(bind_cols(data, bind_cols(temp)))
  
}