#' Identify addresses
#'
#' Identify whether a string is a valid address using (about) the most permissive criteria: it begins with at least two numbers that are at some point followed by a space and a text character.
#' @param x a string that may or may not be an address
#' @export

#Could also require an address-related term by including like st, apt, north. See how that affects things.
#Only requiring one # at the start would also occasionally catch additional addresses, although I generally think this is better.

is_address <- function(x){
  map_lgl(x, function(y){
    result <- str_detect(y, "^\\d\\d.*\\s\\w")
    
    if (is.na(result)){
      return(FALSE)
    } else {
      return(result)
    }
  })
}
