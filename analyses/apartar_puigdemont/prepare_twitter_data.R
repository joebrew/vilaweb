
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


if('data.RData' %in% dir()){
  load('data.RData')
} else {
  puigdemont_bcn <-
    rt <- search_tweets(
      '"Carles Puigdemont" OR "@KRLS"', 
      n = 1000000000, 
      include_rts = F, 
      retryonratelimit = TRUE,
      geocode = "41.385,2.173,20mi"
    )
  save(puigdemont_bcn,
       file = 'puigdemont_bcn.RData')
  puigdemont_mad <-
    rt <- search_tweets(
      '"Carles Puigdemont" OR "@KRLS"', 
      n = 1000000000, 
      include_rts = F, 
      retryonratelimit = TRUE,
      geocode = "40.41678,-3.703,20mi"
    )
  save(puigdemont_mad,
       file = 'puigdemont_mad.RData')
  
  sanchez_bcn <-
    rt <- search_tweets(
      '"Pedro Sánchez" OR "@sanchezcastejon"', 
      n = 1000000000, 
      include_rts = F, 
      retryonratelimit = TRUE,
      geocode = "41.385,2.173,20mi"
    )
  save(sanchez_bcn,
       file = 'sanchez_bcn.RData')
  sanchez_mad <-
    rt <- search_tweets(
      '"Pedro Sánchez" OR "@sanchezcastejon"', 
      n = 1000000000, 
      include_rts = F, 
      retryonratelimit = TRUE,
      geocode = "40.41678,-3.703,20mi"
    )
  save(sanchez_mad,
       file = 'sanchez_mad.RData')
  
  sanchez <-
    rt <- search_tweets(
      '"Pedro Sánchez" OR "@sanchezcastejon"', 
      n = 1000000000, 
      include_rts = F, 
      retryonratelimit = TRUE
    )
  puigdemont <-
    rt <- search_tweets(
      '"Carles Puigdemont" OR "@KRLS"', 
      n = 1000000000, 
      include_rts = F, 
      retryonratelimit = TRUE
    )
  puigdemonta <- search_tweets(
    '@KRLS', 
    n = 1000000000, 
    include_rts = F, 
    retryonratelimit = TRUE
  )
  sancheza <- search_tweets(
    '@sanchezcastejon', 
    n = 1000000000, 
    include_rts = F, 
    retryonratelimit = TRUE
  )
  
  save(puigdemont, puigdemont_mad, puigdemont_bcn,
       sanchez, sanchez_mad, sanchez_bcn, puigdemonta, sancheza,
       file = 'data.RData')
  
}

if('twitter.RData' %in% dir()){
  load('twitter.RData')
} else {
  # # Get tweets
  # twint -s "('Carles Puigdemont' OR '@KRLS')" --since 2019-05-01 --until 2019-08-14 -o data/puigdemont --csv
  # twint -s "('Pedro Sánchez' OR '@sanchezcastejon')" --since 2019-05-01 --until 2019-08-14 -o data/sanchez --csv
  
  df <- read_csv('data/sanchez/tweets.csv') %>%
    mutate(who = 'Sánchez') %>%
    bind_rows(read_csv('data/puigdemont/tweets.csv') %>%
                mutate(who = 'Puigdemont')) %>%
    filter(!duplicated(id))
  
  # Adjust for time zone
  library(lubridate)
  df$date_time <- as.POSIXct(paste0(df$date, ' ', df$time, ' ', '+0', df$timezone))
  Sys.setenv(TZ='CET')
  
  
  save(df, file = 'twitter.RData')
}