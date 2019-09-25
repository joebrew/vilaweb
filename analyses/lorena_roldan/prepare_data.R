# Libraries
library(vilaweb)
library(tidyverse)
library(lubridate)

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

# if('rt.RData' %in% dir()){
#   load('rt.RData')
# } else {
#   rt <- search_tweets(
#     '"goma2" OR "goma 2" OR "goma-2"', 
#     n = 1000000000, 
#     include_rts = T, 
#     retryonratelimit = TRUE
#   )
#   save(rt, file = 'rt.RData')
# }


if('lr.RData' %in% dir()){
  load('lr.RData')
} else {
  lr <- search_tweets(
    '"Lorena Roldán" OR "@Lroldansu"', 
    n = 1000000000, 
    include_rts = T, 
    retryonratelimit = TRUE
  )
  save(lr, file = 'lr.RData')
}

lorena_plot <- function(timey = 'hour',
                        return_table = FALSE){
  pd <- lr
  if(timey == 'hour'){
    pd$date <- as.POSIXct(cut(pd$created_at, timey))
  } else {
    pd$date <- as.Date(pd$created_at)
  }
  pd <- pd %>%
    group_by(date) %>%
    summarise(Piulets = n(),
              # Repiulets = sum(retweet_count, na.rm = T),
              `M'agrada` = sum(favorite_count, na.rm = T)) %>%
    mutate(Interactions = Piulets + #Repiulets + 
             `M'agrada` ) %>%
    gather(key, value, Piulets:`M'agrada`)
  options(scipen = '999')
  pd$key <- factor(pd$key, levels = c('Piulets', 'M\'agrada', 'Repiulets'))
  if(return_table){
    return(pd)
  } else {
    ggplot(data = pd,
           aes(x = date,
               y = value)) +
      geom_area(fill = 'darkorange') +
      geom_line() +
      theme_vilaweb() +
      theme(axis.text.x = element_text(angle = 90, hjust = 0.5, vjust = 1)) +
      # ggthemes::theme_fivethirtyeight() +
      facet_wrap(~key, scales = 'free_y') +
      labs(title = 'Piulets que esmenten Lorena Roldán, per hora',
           subtitle = '("Lorena Roldán" o "@Lroldansu")',
           x = '',
           y = '',
           caption = paste0('Dades: API de Twitter. Gràfic: Joe Brew.\nCodi per transparència/reproducció: ', self_cite()))
  }
  
}

