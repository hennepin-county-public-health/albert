#' Data validation
#'
#' Evaluate the boolean columns created using an expect_ functions, optionally stopped code execution and/or exporting violations to a dataframe for review.
#' @param data a dataframe with at least one metadata column created by the "expect_" functions
#' @param fail should violations result in an error? Default is TRUE.
#' @param export should a dataframe named violations_export of up to 1,000 violations be created? Default is TRUE.
#' @param export_keep by default the violations dataframe will keep any metadata columns with violations and the corresponding data. Here, specify any additional columns that should be retained (such as a unique ID). Default is NULL.
#' @export

check_expectations <- function(data, fail = TRUE, export = TRUE, export_keep = NULL){
  
  #Fail if there are no expectation columns
  if (length(names(data)[str_detect(names(data), "^exp_")]) == 0){
    stop("No expectation columns in the dataframe. Generate these using an 'expect_' function.")
  }
  
  #Print relevant validation information.
  message(str_c("Validating data using ", length(names(data)[str_detect(names(data), "^exp_")]), " conditions."))
  
  #Identify invalid values
  violations <- data |>
    filter(if_any(starts_with("exp_"), ~ . == FALSE)) |>
    select(!starts_with("exp_") | where(~ is.logical(.) && !all(., na.rm = TRUE))) |> #remove exp_ columns with no violations
    relocate(starts_with("exp_"))
  
  #Only keep relevant non-'exp' columns as well
  ckeep <- names(violations)[str_detect(names(violations), "^exp_")] |>
    #THIS WOULD NEED TO UPDATED WHEN NEW FUNCTIONS ARE ADDED. NOT IDEAL, BUT BETTER THAN IT WAS.
    str_remove_all("exp_|_xvalue*$|_xformat*$|_xmissing*$|_xrange*$|_xtype*$|_xdupe*$")
  
  violations <- violations |>
    select(starts_with("exp_"), all_of(ckeep), all_of(export_keep)) #also add in anything specified in export_keep
  
  #If there are no violations, return a DF along with an appropriate message
  if (nrow(violations) == 0){
    message("All expectations satisfied. Validation successful.")
    return(data |> select(-starts_with("exp_"))) #remove metadata columns
  } 
  
  #The remaining code handles cases where there are violations.
  #Print violations
  vnames <- names(violations)[str_detect(names(violations), "^exp_")]
  
  walk(vnames, function(x){
    nme <- str_remove(x, "^exp_")
    coln <- str_remove(nme, "_xvalue*$|_xformat*$|_xmissing*$|_xrange*$|_xtype*$|_xdupe*$")
    vtype <- str_remove(nme, str_c(coln, "_x"))
    
    #Get number of violations for each condition
    vnum <- violations |>
      select(all_of(x)) |>
      filter(!!sym(x) == FALSE) |>
      nrow()
    
    message(str_c(vnum, " ", if_else(vnum == 1, "violation", "violations"), " of the expected *", str_to_upper(vtype), "* in *", coln, "*"))
  })
  
  #Export up to the first 1,000 violations if export == TRUE
  if (export == TRUE){
    violations_export <- violations |>
      slice(1:1000)
    
    #exp_name <- str_c(deparse(substitute(data)), "_violations")
    assign("violations_export", violations_export, envir = globalenv())
    message("Up to the first 1,000 violations written to *violations_export* dataframe.")
  }
  
  #Stop or return data based on failure preference
  if (fail == TRUE){
    stop("Violations flagged. Data validation failed.")
  } else if (fail == FALSE){
    message("Violations flagged. Review output data.")
    return(data |> select(-starts_with("exp_")))   
  }
}