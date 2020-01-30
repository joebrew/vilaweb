# Libraries
library(vilaweb)
library(tidyverse)
library(lubridate)
library(RCurl)
library(jsonlite)
library(rvest)
library(databrew)


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
  df$username <- tolower(df$user_name)
  # Define retweets
  df$rt <- substr(df$content, 1, 3) == 'RT '
  
  # Save
  dir.create('data')
  write_csv(df, 'data/deleted_tweets.csv')
  write_csv(tibble(username = all_people), 'data/all_people.csv')
  write_csv(tibble(username = sort(unique(df$user_name))), 'data/politwoops_people.csv')
  
  # Read in local database data with all tweets
  if('tl.RData' %in% dir()){
    load('tl.RData')
  } else {
    tl <- get_twitter_data_from_database(people = tolower(sort(unique(df$user_name))))
    save(tl, file = 'tl.RData')
  }
  
  # Use Twitter's API to get total number of statuses for a person
  if('users.RData' %in% dir()){
    load('users.RData')
  } else {
    # Read in twitter credentials
    library(yaml)
    twitter_credentials <- yaml.load_file('../../credentials/credentials.yaml')
    ## load rtweet package
    library(rtweet)
    token <- create_token(
      app = "bcndata",
      consumer_key = twitter_credentials$twitter_api_key,
      consumer_secret = twitter_credentials$twitter_api_secret_key,
      access_token = twitter_credentials$twitter_access_token,
      access_secret = twitter_credentials$twitter_access_token_secret)
    
    
    users = rtweet::lookup_users(sort(unique(df$user_name)))
    save(users, file = 'users.RData')
  }
}
load('users.RData')
df <- read_csv('data/deleted_tweets.csv')

plot_n_deleted <- function(ca = FALSE){
  pd <- df %>%
    group_by(user_name) %>%
    tally %>%
    arrange(desc(n)) %>%
    mutate(user_name = factor(user_name, levels = user_name))
  
  if(ca){
    the_labs <- labs(x = 'Usuari',
                     y = 'Piulets',
                     title = 'Piulets esborrats',
                     caption = 'Dades: Politwoops. Gràfic: @joethebrew.')
  } else {
    the_labs <- labs(x = 'Username',
                     y = 'Tweets',
                     title = 'Number of deleted tweets',
                     caption = 'Data: Politwoops. Chart: @joethebrew')
  }
  
  ggplot(data = pd,
         aes(x = user_name,
             y = n)) +
    geom_point() +
    geom_segment(aes(xend = user_name,
                     yend = 0)) +
    theme_simple() +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 7)) +
    the_labs +
    ylim(0, max(pd$n) * 1.1) +
    geom_text(aes(label = round(n, digits = 1)),
              angle = 90,
              nudge_y = 700, size = 2,
              alpha = 0.6)
}

plot_p_deleted <- function(ca = FALSE){
  left <- df %>%
    group_by(username) %>%
    summarise(n = n()) %>%
    ungroup
  
  right <- users %>%
    group_by(username = tolower(screen_name)) %>%
    summarise(denom = sum(statuses_count))
  
  pd <- left_join(left, right, by = 'username') %>%
    # Get the total number of both deleted and non-deleted tweets
    mutate(denom = denom + n) %>%
    # Get the percentage
    mutate(p = n / denom * 100) %>%
    arrange(desc(p)) %>%
    mutate(username = factor(username, levels = username))
  
  if(ca){
    the_labs <- labs(x = 'Usuari',
                     y = 'Percentatge',
                     title = 'Percentatge de piulets que s\'acaben esborrant',
                     caption = 'Dades: Politwoops. Gràfic: @joethebrew.')
  } else {
    the_labs <- labs(x = 'Username',
                     y = 'Percentage',
                     title = 'Percentage of tweets which get deleted',
                     caption = 'Data: Politwoops. Chart: @joethebrew')
  }
  
  ggplot(data = pd,
         aes(x = username,
             y = p)) +
    geom_point() +
    geom_segment(aes(xend = username,
                     yend = 0)) +
    theme_simple() +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 7)) +
    the_labs +
    ylim(0, max(pd$p) * 1.1) +
    geom_text(aes(label = round(p, digits = 1)),
              angle = 90,
              nudge_y = 7, size = 2,
              alpha = 0.6)
}


plot_when <- function(user = 'krls', ca = FALSE, input_data){
  
  if(is.null(user)){
    pd <- input_data
    user_text <- 'Tots'
  } else {
    user_text <- paste0('@', user)
    pd <- df %>%
      # mutate(rt = substr(df$content, 1, 3) == 'RT ') %>%
      filter(username == user)
  }
  
   pd <- pd %>%
    # mutate(month_created = created_at,
    #        month_deleted = deleted_at)# %>%
    mutate(month_created = as.Date(cut(created_at, 'month')),
           month_deleted = as.Date(cut(deleted_at, 'month'))) %>%
    group_by(month_created,
             month_deleted,
             rt) %>%
    tally %>%
    ungroup
  
  nd <- sum(pd$n)
  
  if(ca){
    the_labs <- labs(x = 'Mes quan es va crear el piulet',
                     y = 'Mes quan es va esborrar el piulet',
                     title = paste0(user_text, ' (', nd, ' piulets esborrats)'),
                     subtitle = 'Calendari de la creació i eliminació de piulets')
    pd$rt <- ifelse(pd$rt, 'Repiulets', 'Piulets')
  } else {
    the_labs <- labs(x = 'Month when the tweet was created',
                     y = 'Month when the tweet was deleted',
                     title = paste0(user_text, ' (', nd, ' deleted tweets)'),
                     subtitle = 'Timing of creation and deletion of tweets')
    pd$rt <- factor(ifelse(pd$rt, 'Retweets', 'Tweets'), levels = c('Tweets', 'Retweets'))
  }
  
  rangey <- range(c(pd$month_created, pd$month_deleted))
  ggplot(data = pd,
         aes(x = month_created,
             y = month_deleted,
             size = n,
             color = rt)) +
    geom_abline(slope = 1, intercept = 0, alpha = 0.5, lty = 1) +
    geom_point(alpha = 0.7) +
    theme_simple() +
    scale_size_continuous(name = '') +
    scale_color_manual(name = '',
                       values = c('darkorange', 'darkblue')) +
    the_labs +
    ylim(rangey) +
    xlim(rangey)
}
