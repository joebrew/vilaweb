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
    '"contenidors" OR "contenedores"',
    n = 1000000000,
    include_rts = F,
    retryonratelimit = TRUE
  )
  save(contenidors, file = 'contenidors.RData')
}


if('sentencia.RData' %in% dir()){
  load('sentencia.RData')
} else {
  sentencia <- search_tweets(
    '"sentencia" OR "sentència"',
    n = 1000000000,
    include_rts = F,
    retryonratelimit = TRUE
  )
  save(sentencia, file = 'sentencia.RData')
}

if('violencia.RData' %in% dir()){
  load('violencia.RData')
} else {
  violencia <- search_tweets(
    '"violencia" OR "violència"',
    n = 1000000000,
    include_rts = F,
    retryonratelimit = TRUE
  )
  save(violencia, file = 'violencia.RData')
}

if('torraviolencia.RData' %in% dir()){
  load('torraviolencia.RData')
} else {
  torraviolencia <- search_tweets(
    '"Torra no condena la violencia" OR "Torra no condemna la violència"',
    n = 1000000000,
    include_rts = F,
    retryonratelimit = TRUE
  )
  save(torraviolencia, file = 'torraviolencia.RData')
}

if('torranoviolencia.RData' %in% dir()){
  load('torranoviolencia.RData')
} else {
  torranoviolencia <- search_tweets(
    '"Torra condena la violencia" OR "Torra condemna la violència"',
    n = 1000000000,
    include_rts = F,
    retryonratelimit = TRUE
  )
  save(torranoviolencia, file = 'torranoviolencia.RData')
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
    sanchez %>% mutate(subject = 'Jordi Sànchez'),
    cuixart %>% mutate(subject = 'Jordi Cuixart'),
    sentencia %>% mutate(subject = 'Sentència'),
    violencia %>% mutate(subject = 'Violència'),
    torraviolencia %>% mutate(subject = 'Torra condemna\nla violència'),
    torranoviolencia %>% mutate(subject = 'Torra no condemna\nla violència'))

pd <- combined %>%
  group_by(date = as.POSIXct(cut(created_at, 'day')),
           subject) %>%
  summarise(tweets = n(),
            with_retweets = n() + sum(retweet_count, na.rm = TRUE)) %>%
  filter(subject %in% c('Contenidors', 'Jordi Sànchez', 'Jordi Cuixart'))

ggplot(data = pd,
       aes(x = date,
           y = tweets,
           color = subject)) +
  geom_line() +
  labs(x = 'Hora',
       y = 'Piulets') +
  databrew::theme_databrew() +
  theme(legend.text = element_text(size = 20),
        plot.title = element_text(size = 20),
        plot.subtitle = element_text(size = 15)) +
  labs(x = 'Hora',
       y = 'Piulets per hora (sense repiulets)',
       title = 'Piulets per hora: els Jordis vs els contenidors',
       caption = 'Búsquedes: 1. "Cuixart"; 2. "Jordi Sànchez". 3. "Contenedores OR Contenidors"\nPiulets per hora, sense incloure repiulets.') +
  scale_color_manual(name = '',
                     values = c('red', 'blue', 'darkgreen'))

