#' Get twitter data from database
#' 
#' Get all tweets for a subset of people from the local twitter database
#' @param people user handles
#' @import RPostgreSQL
#' @import DBI
#' @return a tibble
#' @export

get_twitter_data_from_database <- function(people = 'joethebrew'){
  pg = DBI::dbDriver("PostgreSQL")
  con = DBI::dbConnect(pg, dbname="twitter")
  query = paste0("SELECT * FROM twitter where username = ANY ('{",
                 paste0('"', people, '"', collapse = ','),
                 "}')")
  tl <- RPostgreSQL::dbGetQuery(
    con,
    query
  )
  return(tl)
  dbDisconnect(con)
}
