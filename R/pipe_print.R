#' Print text.
#'
#' Print text in the middle of a pipe.
#' @param .data a dataframe
#' @param status the text to print
#' @export

pipe_print <- function(.data, status) {
  message(status); .data}
