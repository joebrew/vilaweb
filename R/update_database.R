#' Update the local twitter database
#' 
#' Update the local twitter database, which should have been set up via inst/rmd/set_up_database/set_up_database.R
#' @param people A character vector of twitter accounts. If null, will do existing people
#' @param get_new Get new data. If TRUE, will fetch all tweets again.
#' @param delete_duplicates If TRUE, will export the data to R, remove duplicates, then overwrite the db
#' @return A set up database
#' @import dplyr
#' @import RPostgreSQL
#' @import readr
#' @import DBI
#' @export

update_database <- function(people = NULL, get_new = FALSE, delete_duplicates = TRUE){
  
  require(dplyr)
  require(RPostgreSQL)
  require(readr)
  require(DBI)
  # Connect to the db
  pg = DBI::dbDriver("PostgreSQL")
  con = DBI::dbConnect(pg, dbname="twitter")
  
  # If null, do everyone
  if(is.null(people)){
    dtab = RPostgreSQL::dbGetQuery(con, "select distinct username from twitter")
    people <- sort(unique(tolower(dtab$username)))
  }
  
  # Loop through each person and get an update
  for(p in 1:length(people)){
    this_person <- people[p]
    this_data <- dbGetQuery(con,
                            paste0("select MAX(date) from twitter where username='", this_person, "'"))
    this_id <- this_data$max
    
    if(is.na(this_id)){
      # No previous data, need to fetch for first time
      get_all <- TRUE
    } else {
      # There is previous data, just get new stuff
      get_all <- FALSE
    }
    if(get_new){
      get_all <- TRUE
    }
    wd <- getwd()
    if(get_all){
      message(toupper(this_person), ': Getting all data')
      system(paste0("python3 ../foreign/twint/Twint.py -u ",
                    this_person,
                    " -o ", wd, "/temp_tweets.csv --csv"))
      
    } else {
      message(toupper(this_person), ': Getting recent data only')
      # Just getting update
      system(paste0("python3 ../foreign/twint/Twint.py -u ",
                    this_person,
                    " --since ",
                    this_id,
                    " -o ", wd, "/temp_tweets.csv --csv"))
    }
    
    # Read the temp written file
    message('Reading temp file which was written for ', this_person)
    tl <- read_csv('temp_tweets/tweets.csv')
    
    # Ensure no duplicates
    tl <- tl %>% dplyr::distinct(.keep_all = TRUE) %>%
      filter(!duplicated(id))
    
    # Ensure not already in database
    already_in <- dbGetQuery(con,
                             paste0("select distinct id from twitter where username='", this_person, "'"))
    already_in <- already_in$id
    
    tl <- tl %>% filter(!id %in% already_in)
    
    # Add rows to the database
    message('Adding new rows to the database for ', this_person)
    dbWriteTable(con, "twitter", tl, append = TRUE, row.names = FALSE)
    
    # Rm the temp_tweets file
    file.remove('temp_tweets/tweets.csv')
    file.remove('temp_tweets/users.csv')
    file.remove('temp_tweets/')
  }
  # Ensure no duplicate rows in db
  if(delete_duplicates){
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
    
  }

  
  # disconnect from the database
  dbDisconnect(con) 
} 
update_database()
