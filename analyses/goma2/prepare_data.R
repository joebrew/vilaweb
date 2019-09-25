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

if('rt.RData' %in% dir()){
  load('rt.RData')
} else {
  rt <- search_tweets(
    '"goma2" OR "goma 2" OR "goma-2"', 
    n = 1000000000, 
    include_rts = T, 
    retryonratelimit = TRUE
  )
  save(rt, file = 'rt.RData')
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

pd <- lr %>%
  group_by(date = as.POSIXct(cut(created_at, 'hour'))) %>%
  summarise(Tweets = n(),
            Retweets = sum(retweet_count, na.rm = T),
            Likes = sum(favorite_count, na.rm = T)) %>%
  mutate(Interactions = Tweets + Retweets + Likes ) %>%
  gather(key, value, Tweets:Interactions)
options(scipen = '999')
ggplot(data = pd,
       aes(x = date,
           y = value)) +
  geom_area() +
  ggthemes::theme_fivethirtyeight() +
  facet_wrap(~key, scales = 'free_y') +
  labs(title = 'Piulets esmentant Lorena Roldán',
       subtitle = '("Lorena Roldán" o "@Lroldansu")')
