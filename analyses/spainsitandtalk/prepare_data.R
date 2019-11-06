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

if('spainsitandtalk.RData' %in% dir()){
  load('spainsitandtalk.RData')
} else {
  spainsitandtalk <- search_tweets(
    '#spainsitandtalk',
    n = 1000000000,
    include_rts = F,
    retryonratelimit = TRUE
  )
  save(spainsitandtalk, file = 'spainsitandtalk.RData')
}

if('ahoraespana.RData' %in% dir()){
  load('ahoraespana.RData')
} else {
  ahoraespana <- search_tweets(
    '#ahoraespaña',
    n = 1000000000,
    include_rts = F,
    retryonratelimit = TRUE
  )
  save(ahoraespana, file = 'ahoraespana.RData')
}

if('ahoraunidad.RData' %in% dir()){
  load('ahoraunidad.RData')
} else {
  ahoraunidad <- search_tweets(
    '#ahoraunidad',
    n = 1000000000,
    include_rts = F,
    retryonratelimit = TRUE
  )
  save(ahoraunidad, file = 'ahoraunidad.RData')
}

if('cent.RData' %in% dir()){
  load('cent.RData')
} else {
  cent <- search_tweets(
    '#155YA',
    n = 1000000000,
    include_rts = F,
    retryonratelimit = TRUE
  )
  save(cent, file = 'cent.RData')
}

if('laespanaquesuma.RData' %in% dir()){
  load('laespanaquesuma.RData')
} else {
  laespanaquesuma <- search_tweets(
    '#LaEspañaQueSuma',
    n = 1000000000,
    include_rts = F,
    retryonratelimit = TRUE
  )
  save(laespanaquesuma, file = 'laespanaquesuma.RData')
}


# Combine
combined <- 
  bind_rows(
    spainsitandtalk %>% mutate(subject = '#SpainSitAndTalk'),
    ahoraespana %>% mutate(subject = '#AhoraEspana'),
    ahoraunidad %>% mutate(subject = '#AhoraUnidad'),
    cent %>% mutate(subject = '#155YA'),
    laespanaquesuma %>% mutate(subject = '#LaEspañaQueSuma'))

pd <- combined %>%
  group_by(subject,date = as.POSIXct(cut(created_at, 'hour'))) %>%
  summarise(tweets = n(),
            with_retweets = n() + sum(retweet_count, na.rm = TRUE))

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
  labs(title = 'Piulets esmentant "Forcadell", "Bassa" o "Contenidors"',
       subtitle = 'Piulets únics, sense incloure repiulets')

pd <- combined %>%
  filter(!subject %in% c('#LaEspañaQueSuma')) %>%
  filter(as.Date(created_at) == '2019-10-21') %>%
  group_by(subject) %>%
  summarise(tweets = n(),
            with_retweets = n() + sum(retweet_count, na.rm = TRUE))

ggplot(data = pd,
       aes(x = subject,
           y = tweets)) +
  geom_bar(stat = 'identity',
           fill = colors_vilaweb()[5], alpha = 0.7) +
  geom_text(aes(label = scales::comma(tweets)),
            nudge_y = 2000,
            size = 8,
            alpha = 0.6) +
  ggthemes::theme_fivethirtyeight() +
  labs(x = '',
       title = 'Ús de hashtags, 21 d\'octubre',
       subtitle = '(Fins a les 21:30, piulets únics sense incloure repiulets)') +
  theme(axis.text.x = element_text(size = 15))
