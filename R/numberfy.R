#' Numberfy
#' 
#' Make a number look European
#' @return a character string
#' @import scales
#' @export

numberfy <- function(x){
  gsub(',', '.', scales::comma(x), fixed = TRUE)
}
