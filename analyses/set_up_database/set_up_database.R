
library(dplyr)
library(readr)
library(RPostgreSQL)
# # From within psql
# CREATE DATABASE twitter;
# # Now from command line:
# psql twitter

set_up_database <- function(people = NULL){
  
  # If null, do everyone
  if(is.null(people)){
    library(gsheet)
    if(!'goog.RData' %in% dir()){
      goog_people <- gsheet::gsheet2tbl(url = 'https://docs.google.com/spreadsheets/d/1k6_AlqojK47MMqzuFYAzBnDfYXysmUgSseaKvHTb3W4/edit#gid=1425313388')
      save(goog_people,
           file = 'goog.RData')
    } else {
      load('goog.RData')
    }
    people <- tolower(goog_people$username)
  }
  
  # Make sure everything in data is lowercase
  if(!dir.exists('data')){
    dir.create('data')
  }
  
  # Get twitter data
  pg = dbDriver("PostgreSQL")
  con = dbConnect(pg, dbname="twitter")
  for(p in 1:length(people)){
    this_person <- people[p]
    file_name <- (paste0('data/', this_person, '_tweets.csv'))
    if(!file.exists(file_name)){
      message(toupper(this_person), '----------------')
      bash_text <- paste0(
        'twint -u ',
        this_person,
        ' -o data/',
        this_person,
        '_tweets.csv --csv'
      )
      system(bash_text)
      }
    # Read in the data
    tl <- read_csv(file_name)
    dbWriteTable(con,'twitter',tl, row.names=FALSE,append = TRUE)
    
  }
  # Write the database
  # Read back
  # dtab = dbGetQuery(con, "select * from twitter")
  # disconnect from the database
  dbDisconnect(con)
  
}
