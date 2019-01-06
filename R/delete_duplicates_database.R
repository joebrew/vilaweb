#' Delete duplicates
#' 
#' Delete duplicates in the database
#' @return A set up database
#' @import dplyr
#' @import RPostgreSQL
#' @import readr
#' @import DBI
#' @export

delete_duplicates_database <- function(){
  
  require(dplyr)
  require(RPostgreSQL)
  require(readr)
  require(DBI)
  # Connect to the db
  pg = DBI::dbDriver("PostgreSQL")
  con = DBI::dbConnect(pg, dbname="twitter")
  
  message('DELETING DUPLICATE ROWS IN THE DB')
  # dbSendQuery(con, 'DELETE FROM twitter
  #                     WHERE  ctid NOT IN (
  #                        SELECT min(ctid) 
  #                        FROM   twitter
  #                        GROUP  BY id);')
  tl <- dbReadTable(con, 'twitter')
  # Flag the duplicates
  flag <- duplicated(tl$id)
  tl <- tl[!flag,]
  message('Overwriting old table')
  # Re-writing whole table
  dbWriteTable(con, "twitter", tl, overwrite = TRUE, row.names = FALSE)
  
  # disconnect from the database
  dbDisconnect(con) 
} 
