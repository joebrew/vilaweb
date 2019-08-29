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
  
  query <- paste0("DELETE FROM twitter a USING (SELECT MIN(ctid) as ctid, id FROM twitter GROUP BY id HAVING COUNT(*) > 1) b WHERE a.id = b.id AND a.ctid <> b.ctid;")
  message('DELETING DUPLICATE ROWS IN THE DB')
    dbSendQuery(con, query)
  
  # tl <- dbReadTable(con, 'twitter')
  # # Flag the duplicates
  # flag <- duplicated(tl$id)
  # tl <- tl[!flag,]
  # message('Overwriting old table')
  # # Re-writing whole table
  # dbWriteTable(con, "twitter", tl, overwrite = TRUE, row.names = FALSE)
  
  # disconnect from the database
  dbDisconnect(con) 
} 
