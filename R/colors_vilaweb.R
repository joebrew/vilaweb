#' Vilaweb colors
#'
#' Apply the vilaweb look to a ggplot2-generated visualization

#' @return A vector of colors
#' @importFrom grDevices colorRampPalette
#' @export

colors_vilaweb <- function(){
  
  rgb2hex <- function(r,g,b) rgb(r, g, b, maxColorValue = 255)
  groc <- rgb2hex(240, 214, 0)
  rosa <- rgb2hex(232,98,130)
  verd <- rgb2hex(126, 177, 23)
  turquesa <- rgb2hex(44, 175, 194)
  blau <- rgb2hex(104, 133, 189)
  lila <- rgb2hex(156, 118, 172)

  out <- c(groc, rosa, verd, turquesa, blau, lila)
  # names(out) <- c('groc', 'rosa', 'verd', 'turquesa', 'blau', 'lila') 
  return(out)
}
