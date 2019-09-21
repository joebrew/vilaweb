#' Update the local twitter database
#' 
#' Update the local twitter database, which should have been set up via inst/rmd/set_up_database/set_up_database.R
#' @param people A character vector of twitter accounts. If null, will do existing people
#' @param get_new Get new data. If TRUE, will fetch all tweets again.
#' @param delete_duplicates If TRUE, will export the data to R, remove duplicates, then overwrite the db
#' @param only_new_people Whether to only fetch people who are NOT yet in the database (ie, automatically filters the people argument)
#' @param after The date after which tweets should be retrieved. Default to 2000-01-01
#' @param until The date until which tweets will be retrieved
#' @param force_old Set to TRUE if retrieving tweets older than what is already in the database (only for exceptional cases)
#' @param refetch Set to TRUE to delete previous entries in the database for the person in question and rescrape
#' @param refetch_since Whether to only refetch since a certain date (ie, delete all entries until that date). Only applicable if refetch is true
#' @return A set up database
#' @import dplyr
#' @import RPostgreSQL
#' @import readr
#' @import DBI
#' @export

update_database <- function(people = NULL, 
                            get_new = FALSE, 
                            delete_duplicates = TRUE,
                            only_new_people = FALSE,
                            after = '2000-01-01',
                            until = as.character(Sys.Date() +1),
                            force_old = FALSE,
                            refetch = FALSE,
                            refetch_since = NULL){
  
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
  
  # Make everything lowercase
  people <- tolower(people)
  
  # If only new people, filter
  if(only_new_people){
    dtab = RPostgreSQL::dbGetQuery(con, "select distinct username from twitter")
    already <- sort(unique(tolower(dtab$username)))
    people <- people[!people %in% already]
  }
  # If no people say so
  if(length(people) == 0){
    stop('No new people. Consider turning off only_new_people')
  }
  
  # Loop through each person and get an update
  for(p in 1:length(people)){
    this_person <- people[p]
    
    # If refetch, delete all the old rows from the db
    if(refetch){
      if(!is.null(refetch_since)){
        delete_query <- paste0("DELETE FROM twitter WHERE (username='", 
                               this_person,
                               "' AND date >= '", as.character(refetch_since),
                               "');")
        message('Deleting rows for ', this_person, ' since ', as.character(refetch_since))
      } else {
        delete_query <- paste0("DELETE FROM twitter WHERE username='", 
                               this_person,
                               "';")
        message('Deleting all old rows for ', this_person)
      }
      
      
      dbSendQuery(con, delete_query)
    }
    
    
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
      
      bash_text <- paste0(
        'twint -u ',
        this_person,
        ' -o data/',
        this_person,
        " --since ",
        after,
        " --until ", until,
        " -o ", wd, "/temp_tweets.csv --csv"
      )
      system(bash_text)
      
    } else if(force_old){
      message(toupper(this_person), ': Getting old data')
      bash_text <- paste0(
        'twint -u ',
        this_person,
        " --since ",
        after,
        " --until ", until,
        " -o ", wd, "/temp_tweets.csv --csv")
      system(bash_text)
    } else {
      message(toupper(this_person), ': Getting recent data only')
      # Just getting update
      bash_text <- paste0(
        'twint -u ',
        this_person,
        " --since ",
        this_id,
        " --until ", until,
        " -o ", wd, "/temp_tweets.csv --csv")
      system(bash_text)
    }
    
    # Read the temp written file
    message('Reading temp file which was written for ', this_person)
    if(file.exists('temp_tweets.csv')){
      tl <- read_csv('temp_tweets.csv')
      
      # Ensure no duplicates
      tl <- tl %>% dplyr::distinct(.keep_all = TRUE) %>%
        filter(!duplicated(id))
      
      # Ensure not already in database
      already_in <- dbGetQuery(con,
                               paste0("select distinct id from twitter where username='", this_person, "'"))
      already_in <- already_in$id
      
      tl <- tl %>% filter(!id %in% already_in)
      
    # Ensure the person is there
      if(length(unique(tl$username)) == 1 &
         tl$username[1] %in% people){
        # Add rows to the database
        message('Adding new rows to the database for ', this_person)
        dbWriteTable(con, "twitter", tl, append = TRUE, row.names = FALSE)
      } else {
        message('Either too many people in tl, or the person was accidentally gathered.')
      }
    }
    
    # Rm the temp_tweets file
    suppressWarnings({
      file.remove('temp_tweets.csv')
    })
    
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
