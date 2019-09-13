#' Round to nearest multiple of base
#' 
#' Round percentages to get a whole integer
#' @param x A numeric vector
#' @param base Round to the nearest multiple of this
#' @return A numeric vector
#' @export

mround <- function(x,base){ 
  base*round(x/base) 
} 
