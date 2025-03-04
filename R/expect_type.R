#' Data validation
#'
#' Check that a column is of the expected type. This function with create a boolean row that indicates unexpected values, and can be combined with check_expectations to halt a pipeline or export violations for review.
#' @param data a dataframe with data to validate
#' @param value_pairs a list where names are column types (numeric, character, date, logical, or factor) and values are a vector of columns
#' @param warn should the function itself warn about violations? Alternatively, use check_expectations(). 
#' @export

expect_type <- function(data, value_pairs, warn = FALSE){
  
  #Loop through each type
  temp <- map(seq_along(value_pairs), function(t){
    
    temp_type <- str_to_lower(names(value_pairs)[t])
    temp_type <- if_else(temp_type == "date", "Date", temp_type)
    
    #Stop if type is not in the intended list
    if (!temp_type %in% c("numeric", "character", "Date", "logical", "factor")){
      stop("Accepted column types are: character, date, factor, logical, numeric.")
    }
    
    #Within each type, loop through and evalute each column
    meta_col <- map(seq_along(value_pairs[[t]]), function(c){
      
      temp_col <- value_pairs[[t]][[c]]
      dname <- deparse(substitute(data)) #get input df name in text
      #The code below will test whether the column is the correct type
      type_test <- eval(parse(text = paste0(if_else(temp_type == "Date", "lubridate::is.", "is."), 
                                            temp_type, "(", dname, "$", temp_col, ")"))) 
      cname <- paste0("exp_", temp_col, "_xtype") #define exp_column name
      log_value <- tibble(!!sym(cname) := rep(type_test, nrow(data))) #create named tibble (returned as part of a list)
      
      #If warn is TRUE, notify of violations
      if (warn == TRUE & log_value[1,1] == FALSE){
        message(paste0("Expectation violated: *", temp_col, "* is not *", str_to_upper(temp_type), "* type"))
      }
      
      return(log_value)
    })
  })
  #Combine all column-T/F pairs into a single dataframe, combine w/ data
  return(bind_cols(data, bind_cols(temp)))
}