#' Data validation
#'
#' Check whether there are duplicate values in a column. This function with create a boolean row that indicates unexpected values, and can be combined with check_expectations to halt a pipeline or export violations for review.
#' @param data a dataframe with data to validate
#' @param columns a vector of columns or of vectors of columns. A nested vector can have up to 4 columns.
#' @param warn should the function itself warn about violations? Alternatively, use check_expectations(). 
#' @export

expect_unique <- function(data, columns, warn = FALSE){
  
  #Loop through each item (col, vector of cols)
  temp <- map(seq_along(columns), function(x){
    
    temp_col <- columns[[x]]
    temp_data <- data |>
      select(all_of(temp_col))
    
    #Create a single combined column if multiple are being tested
    #How can I make this flexible so it works for columns of any number?
    if (length(temp_col) == 2){
      temp_data <- temp_data |>
        mutate(multi_col = paste(!!sym(temp_col[1]), !!sym(temp_col[2])), .keep = "unused")
    }
    
    if (length(temp_col) == 3){
      temp_data <- temp_data |>
        mutate(multi_col = paste(!!sym(temp_col[1]), !!sym(temp_col[2]), !!sym(temp_col[3])), .keep = "unused")
    }
    
    if (length(temp_col) == 4){
      temp_data <- temp_data |>
        mutate(multi_col = paste(!!sym(temp_col[1]), !!sym(temp_col[2]), !!sym(temp_col[3]), !!sym(temp_col[4])), .keep = "unused")
    }
    
    if (length(temp_col) > 4){
      stop("This function currently accepts a max of 4 columns for combined review.")
    }
    
    #Create meta column name
    cname <- paste0("exp_", paste(temp_col, collapse = "_"), "_xdupe") #define exp_column name
    
    #Flag duplicates
    dupe_col <- (duplicated(pull(temp_data[1]), incomparables = NA) | duplicated(pull(temp_data[1]), fromLast = TRUE, incomparables = NA)) |>
      as_tibble() |>
      mutate(value = !value) |>
      rename(!!sym(cname) := value)
    
    #If warn is true, print number of violations
    if (warn == TRUE){
      
      cnt <- dupe_col |> pull(1)
      cnt <- length(cnt[cnt == FALSE])
      message(str_c(cnt, " *DUPLICATE* ", if_else(cnt == 1, "value", "values"),  " found in *", paste(temp_col, collapse = ","), "*"))
    }
    return(dupe_col)
  })
  return(bind_cols(data, bind_cols(temp)))
}