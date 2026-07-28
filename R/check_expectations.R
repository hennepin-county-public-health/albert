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
  if (length(names(data)[stringr::str_detect(names(data), "^exp_")]) == 0){
    stop("No expectation columns in the dataframe. Generate these using an 'expect_' function.")
  }

  #Print relevant validation information.
  message(str_c("Validating data using ", length(names(data)[stringr::str_detect(names(data), "^exp_")]), " conditions."))

  #Identify invalid values
  violations <- data |>
    dplyr::filter(dplyr::if_any(dplyr::starts_with("exp_"), ~ . == FALSE)) |>
    dplyr::select(!dplyr::starts_with("exp_") | dplyr::where(~ is.logical(.) && !all(., na.rm = TRUE))) |> #remove exp_ columns with no violations
    dplyr::relocate(dplyr::starts_with("exp_"))

  #Only keep relevant non-'exp' columns as well
  ckeep <- names(violations)[stringr::str_detect(names(violations), "^exp_")] |>
    #THIS WOULD NEED TO UPDATED WHEN NEW FUNCTIONS ARE ADDED. NOT IDEAL, BUT BETTER THAN IT WAS.
    stringr::str_remove_all("exp_|_xvalue*$|_xformat*$|_xmissing*$|_xrange*$|_xtype*$|_xdupe*$")

  violations <- violations |>
    select(dplyr::starts_with("exp_"), dplyr::all_of(ckeep), dplyr::all_of(export_keep)) #also add in anything specified in export_keep

  #If there are no violations, return a DF along with an appropriate message
  if (nrow(violations) == 0){
    message("All expectations satisfied. Validation successful.")
    return(data |> dplyr::select(-starts_with("exp_"))) #remove metadata columns
  }

  #The remaining code handles cases where there are violations.
  #Print violations
  vnames <- names(violations)[stringr::str_detect(names(violations), "^exp_")]

  purrr::walk(vnames, function(x){
    nme <- stringr::str_remove(x, "^exp_")
    coln <- stringr::str_remove(nme, "_xvalue*$|_xformat*$|_xmissing*$|_xrange*$|_xtype*$|_xdupe*$")
    vtype <- stringr::str_remove(nme, str_c(coln, "_x"))

    #Get number of violations for each condition
    vnum <- violations |>
      dplyr::select(all_of(x)) |>
      dplyr::filter(!!sym(x) == FALSE) |>
      nrow()

    message(stringr::str_c(vnum, " ", ifelse(vnum == 1, "violation", "violations"), " of the expected *", stringr::str_to_upper(vtype), "* in *", coln, "*"))
  })

  #Export up to the first 1,000 violations if export == TRUE
  if (export == TRUE){
    violations_export <- violations |>
      dplyr::slice(1:1000)

    #exp_name <- str_c(deparse(substitute(data)), "_violations")
    assign("violations_export", violations_export, envir = globalenv())
    message("Up to the first 1,000 violations written to *violations_export* dataframe.")
  }

  #Stop or return data based on failure preference
  if (fail == TRUE){
    stop("Violations flagged. Data validation failed.")
  } else if (fail == FALSE){
    message("Violations flagged. Review output data.")
    return(data |> dplyr::select(-starts_with("exp_")))
  }
}
