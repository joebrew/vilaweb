#' Make Catalan date
#' 
#' Create a Catalan date
#' @import tidyverse
#' @return Function
#' @export

catalan_date <- function(month = TRUE, new_line = TRUE){
  function(x){
    part2 <- format(x, '%d')
    part3 <- format(x, '%Y')
    part1 <- data.frame(x = as.numeric(format(x, '%m')))
    right <- data_frame(x = 1:12,
                        y = c('Gen', 'Feb', 'Mar', 'Abr', 'Mai', 'Jun',
                              'Jul', 'Ago', 'Set', 'Oct', 'Nov', 'Des'))
    joined <- left_join(part1, right) %>%
      .$y
    out <- paste0(part2, ' ', joined, ' ', part3)
    if(!month){
      return(out)
    } else {
      out <- substr(out, 4, nchar(out))
      if(new_line){
        out <- gsub(' ', '\n', out)
      }
      return(out)
    }
  }
}