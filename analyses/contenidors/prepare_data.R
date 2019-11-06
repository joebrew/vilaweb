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

if('contenidors.RData' %in% dir()){
  load('contenidors.RData')
} else {
  contenidors <- search_tweets(
    'contenidors',
    n = 1000000000,
    include_rts = F,
    retryonratelimit = TRUE
  )
  save(contenidors, file = 'contenidors.RData')
}


if('turull.RData' %in% dir()){
  load('turull.RData')
} else {
  turull <- search_tweets(
    'turull',
    n = 1000000000,
    include_rts = F,
    retryonratelimit = TRUE
  )
  save(turull, file = 'turull.RData')
}

if('bassa.RData' %in% dir()){
  load('bassa.RData')
} else {
  bassa <- search_tweets(
    'Dolors Bassa',
    n = 1000000000,
    include_rts = F,
    retryonratelimit = TRUE
  )
  save(bassa, file = 'bassa.RData')
}

if('forcadell.RData' %in% dir()){
  load('forcadell.RData')
} else {
  forcadell <- search_tweets(
    'Forcadell',
    n = 1000000000,
    include_rts = F,
    retryonratelimit = TRUE
  )
  save(forcadell, file = 'forcadell.RData')
}

if('rull.RData' %in% dir()){
  load('rull.RData')
} else {
  rull <- search_tweets(
    'rull',
    n = 1000000000,
    include_rts = F,
    retryonratelimit = TRUE
  )
  save(rull, file = 'rull.RData')
}

if('junqueras.RData' %in% dir()){
  load('junqueras.RData')
} else {
  junqueras <- search_tweets(
    'junqueras',
    n = 1000000000,
    include_rts = F,
    retryonratelimit = TRUE
  )
  save(junqueras, file = 'junqueras.RData')
}

if('romeva.RData' %in% dir()){
  load('romeva.RData')
} else {
  romeva <- search_tweets(
    'romeva',
    n = 1000000000,
    include_rts = F,
    retryonratelimit = TRUE
  )
  save(romeva, file = 'romeva.RData')
}

if('forn.RData' %in% dir()){
  load('forn.RData')
} else {
  forn <- search_tweets(
    'quim forn',
    n = 1000000000,
    include_rts = F,
    retryonratelimit = TRUE
  )
  save(forn, file = 'forn.RData')
}

if('cuixart.RData' %in% dir()){
  load('cuixart.RData')
} else {
  cuixart <- search_tweets(
    'cuixart',
    n = 1000000000,
    include_rts = F,
    retryonratelimit = TRUE
  )
  save(cuixart, file = 'cuixart.RData')
}

if('sanchez.RData' %in% dir()){
  load('sanchez.RData')
} else {
  sanchez <- search_tweets(
    'Jordi Sànchez',
    n = 1000000000,
    include_rts = F,
    retryonratelimit = TRUE
  )
  save(sanchez, file = 'sanchez.RData')
}

# Combine
combined <- 
  bind_rows(
    contenidors %>% mutate(subject = 'Contenidors'),
    turull %>% mutate(subject = 'Turull'),
    forcadell %>% mutate(subject = 'Carme Forcadell'),
    bassa %>% mutate(subject = 'Dolors Bassa'),
    sanchez %>% mutate(subject = 'Jordi Sànchez'),
    cuixart %>% mutate(subject = 'Jordi Cuixart'),
    romeva %>% mutate(subject = 'Romeva'),
    rull %>% mutate(subject = 'Rull'),
    forn %>% mutate(subject = 'Joaquim Forn'),
    junqueras %>% mutate(subject = 'Junqueras')
  )

pd <- combined %>%
  group_by(date = as.POSIXct(cut(created_at, 'hour')),
           subject) %>%
  summarise(tweets = n(),
            with_retweets = n() + sum(retweet_count, na.rm = TRUE))

ggplot(data = pd %>%
         filter(subject %in% c('Dolors Bassa',
                               'Carme Forcadell',
                               'Contenidors'),
                date >= '2019-10-12'),
       aes(x = date,
           y = tweets,
           color = subject)) +
  geom_line() +
  scale_color_manual(name = '',
                     values = c('blue', 'red', 'darkgreen')) +
  labs(x = 'Hora',
       y = 'Piulets') +
  databrew::theme_databrew() +
  theme(legend.text = element_text(size = 20),
        plot.title = element_text(size = 20),
        plot.subtitle = element_text(size = 15)) +
  labs(title = 'Piulets esmentant "Forcadell", "Bassa" o "Contenidors"',
       subtitle = 'Piulets únics, sense incloure repiulets')



if('cc.RData' %in% dir()){
  load('cc.RData')
} else {
  cc <- search_tweets(
    '"Carlos Carrizosa" OR "@carrizosacarlos"', 
    n = 1000000000, 
    include_rts = T, 
    retryonratelimit = TRUE
  )
  save(cc, file = 'cc.RData')
}


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

carlos_plot <- function(timey = 'hour',
                        return_table = FALSE){
  pd <- cc
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
      # theme_vilaweb() +
      ggthemes::theme_fivethirtyeight() +
      
      theme(axis.text.x = element_text(angle = 90, hjust = 0.5, vjust = 1)) +
      # ggthemes::theme_fivethirtyeight() +
      facet_wrap(~key, scales = 'free_y') +
      labs(title = 'Piulets que esmenten Carlos Carrisoza, per hora',
           subtitle = '("Carlos Carrizosa" o "@carrizosacarlos")',
           x = '',
           y = '',
           caption = paste0('Dades: API de Twitter. Gràfic: Joe Brew.'))
  }
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

