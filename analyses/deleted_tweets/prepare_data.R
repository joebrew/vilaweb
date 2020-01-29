# Libraries
library(vilaweb)
library(tidyverse)
library(lubridate)
library(RCurl)
library(jsonlite)
library(rvest)

# Define whether all the data needs to be captured from politwoops or 
# not (this takes about 12 hours, so we only want to do it once
# and not every time the Rmd is rendered!)
first_time <- FALSE

if('all_people.RData' %in% dir()){
  load('all_people.RData')
} else {
  # Get a list of all people
  x = get_google_data(name = 'politicians')
  y = get_google_data(name = 'parlament')
  z = get_google_data(name = 'congreso')
  all_people <- c(x$username, y$username, z$screen_name)
  all_people <- sort(unique(tolower(all_people)))
  save(all_people, file = 'all_people.RData')
}

# Capture the data from politwoops API
if(first_time){
  # Define function for searching
  person_exists <- function(x){
    url <- paste0('https://www.politwoops.eu/user/',
                  x)
    tried <- tryCatch({
      webpage <- read_html(url)
      if(is.list(webpage)){
        return(TRUE)
      }
    },
    error = function(a){
      message('No page for this person')
      return(FALSE)
    })
  }
  
  # Retrieve data
  # dir.create('csvs')
  for(i in 1:length(all_people)){
    this_person <- all_people[i]
    out_list <- list()
    counter <- 0
    message('Row ', i, ' of ', length(all_people), ': ', this_person)
    ok <- TRUE
    # this_urx <- paste0(df$urx[i], '.json')
    j = 0
    
    # See if the person even exists
    this_urx <- paste0('https://www.politwoops.eu/user/',
                       this_person)
    this_person_exists <- person_exists(this_person)
    if(!this_person_exists){
      ok <- FALSE
    }
    
    while(ok){
      j <- j + 1
      message('--- page ', j)
      # The person exists, move forward
      # webpage <- read_html(this_urx)
      this_urx_page <- paste0(this_urx, '.json?page=', j)
      
      
      tried <- tryCatch({
        out <- fromJSON(url(this_urx_page))
        if(is.list(out)){
          return(TRUE)
        }
      },
      error = function(a){
        message('No more pages')
        return(FALSE)
      })
      
      if(!tried){
        ok <- FALSE
        out <- NULL
      }
      
      ok <- length(out) > 0
      if(ok){
        Sys.sleep(1)
        counter <- counter + 1
        out_df <- out %>% dplyr::select(created_at, content, updated_at, user_name, id, content)
        statuses <- out$details$user$statuses_count
        total_deleted <- out$politician$twitter_total_deleted
        deleted_at <- out$details$deleted_at
        created_at_b <- out$details$created_at
        out_df <- out_df %>% mutate(statuses = statuses,
                                    deleted_at = deleted_at,
                                    created_at_b = created_at_b)
        out_list[[counter]] <- out_df
      }
    }
    if(this_person_exists){
      if(length(out_list) > 0){
        done <- bind_rows(out_list)
        message('Writing csv for ', this_person)
        write_csv(done, paste0('csvs/', this_person, '.csv'))
      }
    }
  }
  # Read in the captured data
  csvs <- dir('csvs')
  csvs <- csvs[grepl('.csv', csvs, fixed = TRUE)]
  csvs <- paste0('csvs/', csvs)
  csv_list <- list()
  for(i in 1:length(csvs)){
    csv_list[[i]] <- read_csv(csvs[i])
  }
  df <- bind_rows(csv_list)
  
  # Remove potential duplicates
  df <- df %>% dplyr::distinct(user_name, content, created_at, deleted_at, .keep_all = TRUE)
  
  # Create a few more variables
  df <- df %>%
    mutate(seconds_up = updated_at - created_at) %>%
    mutate(minutes_up = seconds_up / 60) %>%
    mutate(hours_up = minutes_up / 60) %>%
    mutate(days_up = hours_up / 24) %>%
    mutate(months_up = days_up / (365.25 / 12)) %>%
    # make variable names clearer
    dplyr::select(user_name,
                  content, 
                  created_at, 
                  updated_at,
                  contains('_up'),
                  statuses) %>%
    dplyr::rename(deleted_at = updated_at)
  
  # Save
  dir.create('data')
  write_csv(df, 'data/deleted_tweets.csv')
  write_csv(tibble(username = all_people), 'data/all_people.csv')
  write_csv(tibble(username = sort(unique(df$user_name))), 'data/politwoops_people.csv')
}
