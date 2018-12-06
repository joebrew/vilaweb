#' Truncate a date
#' 
#' Truncate a date to the level of a year, quarter or month in the style of SQL date truncation
#' @param date_object A date vector
#' @param level One of "year", "quarter" or "month", indicating the level to which the dates should be truncated
#' @return A Date vector of identical length to the input containing the truncated dates
#' @export

date_truncate <- function(date_object, level = c('month', 'quarter', 'year')){
  
  if(is.null(level)){
    stop('You must provide a level argument of either "month", "quarter" or "year".')
  }
  date_object <- as.Date(date_object)
  # If all NAs, return as is
  if(sum(!is.na(date_object)) == 0){
    return(date_object)
  }
  
  if(level == 'month'){
    return_object <- date_object
    return_object[!is.na(return_object)] <-
      as.Date(paste0(format(return_object[!is.na(return_object)], '%Y-%m'), '-01'))
    return(return_object)
  }
  if(level == 'quarter'){
    q_month <- (((((as.numeric(format(date_object, '%m')))-1) %/% 3) + 1) * 3) - 2
    return_object <- date_object
    return_object[!is.na(return_object)] <-
      
      as.Date(paste0(format(return_object[!is.na(return_object)], '%Y'), 
                     ifelse(nchar(q_month[!is.na(return_object)]) == 2, '-', '-0'),
                     q_month,
                     '-01'))
    return(return_object)
  }
  if(level == 'year'){
    return_object <- date_object
    return_object[!is.na(return_object)] <-
      as.Date(paste0(format(return_object[!is.na(return_object)], '%Y'), '-01-01'))
    
    
    return(return_object)
  }
  if(level == 'week'){
    return_object <- cut(as.Date(date_object), "week")
    return_object <- as.Date(as.character(return_object))
    return(return_object)
  }
}