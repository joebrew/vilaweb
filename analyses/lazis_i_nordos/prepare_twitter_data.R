library(tidyverse)     
library(lubridate)

if('twitter.RData' %in% dir()){
  load('twitter.RData')
} else {
#   # # Get tweets (ran separately in terminal)
# twint -s '(" lazi" OR " lazis")' --since 2017-01-01 --until 2019-08-22 -o data/lazi --csv
# twint -s '("separata" OR "separatas")' --since 2017-01-01 --until 2019-08-22 -o data/separata --csv
# twint -s '("ñordo" OR "ñordos")' --since 2017-01-01 --until 2019-08-22 -o data/nordo --csv
# twint -s '("golpista" OR "golpistas")' --since 2017-01-01 --until 2019-08-22 -o data/golpista --csv
df <- 
  bind_rows(
    read_csv('data/golpista/tweets.csv') %>%
      filter(!duplicated(id)) %>% mutate(subject = 'Golpista'),
    read_csv('data/lazi/tweets.csv') %>%
      filter(!duplicated(id)) %>% mutate(subject = 'Lazi'),
    read_csv('data/nordo/tweets.csv') %>%
      filter(!duplicated(id)) %>% mutate(subject = 'Ñordo'),
    read_csv('data/separata/tweets.csv') %>%
      filter(!duplicated(id)) %>% mutate(subject = 'Separata') 
  )

# df %>% group_by(subject) %>% summarise(min_date = min(date, na.rm = T))
#   
  # Adjust for time zone
  df$date_time <- as.POSIXct(paste0(df$date, ' ', df$time, ' ', '+0', df$timezone))
  Sys.setenv(TZ='CET')
  
  # Remove those which are likely from other languages
  df <- df %>%
    filter(!grepl('ž', tolower(tweet)),
           !grepl('não', tolower(tweet))) %>%
    # Remove potential references to other coups
    filter(!grepl('zimbabwe|yemen|gabon|sudan|etiopia|amhara', tolower(tweet))) %>%
    # Make sure that the search contains our terms only (and doesn't capture users with the username with the term, etc)
    filter((grepl('golpista', tolower(tweet)) |
              grepl('lazi', tolower(tweet)) |
              grepl('ñordo', tolower(tweet)) |
              grepl('separata', tolower(tweet)))) %>%
    filter(date >= '2017-01-01') %>%
    # Calculate number of interactions
    mutate(interactions = retweets_count + likes_count + 1)
    save(df, file = 'twitter.RData')
}


pd <- df %>%
  group_by(subject, date) %>% 
  tally %>%
  ungroup

# Time series
ggplot(data = pd,
       aes(x = date,
           y = n)) +
  geom_line(aes(color = subject))

# Retweets by subject
df %>% group_by(subject) %>%
  summarise(tweets = n(),
            interactions = sum(interactions),
            replies = sum(replies_count)) %>%
  mutate(replies_to_interactions = replies / interactions)
