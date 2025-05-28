#' List to Character
#' 
#' Handles null values by replacing them with NAs
#'
#' @param x List. List of character values.
#'
#' @returns Character. Vector of character values
#' @export
#'
#' @examples
list_to_char <- function(x){
  if(is.null(x)){
    x <- NA_character_
  }
  x
}
