#' Self cite
#' 
#' Generate a URL to the github repo for directory open
#' @return a character vector of length 1
#' @export

self_cite <- function(){
  this_dir <- getwd()
  part1 <- 'https://github.com/joebrew/vilaweb/tree/master/analyses/'
  part2 <- unlist(strsplit(this_dir, '/'))
  part2 <- part2[length(part2)]
  out <- paste0(part1, part2)
  return(out)
}