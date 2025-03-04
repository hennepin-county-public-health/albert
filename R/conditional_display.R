#' Display dataset
#'
#' Display returns an error if dataframe is empty. Using this modified version to avoid that to prevent script-stopping errors.
#' @param data A dataframe
#' @export

conditional_display <- function(data){
  if (nrow(data) == 0){
    print("No rows in table")
  } else if (nrow(data) > 0) {
    display(data)
  }
}
