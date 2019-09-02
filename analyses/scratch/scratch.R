# Libraries
library(vilaweb)
library(tidyverse)
library(DBI)
library(stringi)

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

if('investimpuigdemont.RData' %in% dir()){
  load('investimpuigdemont.RData')
} else {
  rt <- search_tweets(
      '#investimpuigdemont', 
      n = 1000000000, 
      include_rts = T, 
      retryonratelimit = TRUE
    )
  save(rt,
       file = 'investimpuigdemont.RData')
}
pd <- rt %>%
  group_by(date = as.POSIXct(cut(created_at, 'hour'))) %>%
  summarise(n = n(),
            rts = sum(retweet_count))

ggplot(data = pd,
       aes(x = date,y = rts)) +
  geom_line()

# Define the people to be examined (Spanish and Catalan socialists)
url <- 'https://docs.google.com/spreadsheets/d/1DBKQi5eN9zT_Pj4J3MRiE3qLXB2VPxvd8BVdSc012Ug/edit#gid=0'
if(!'diputats.RData' %in% dir()){
  diputats <- gsheet::gsheet2tbl(url = url)
  save(diputats,
       file = 'diputats.RData')
} else {
  load('diputats.RData')
}
people <- tolower(diputats$username)
people <- people[!is.na(people)]


if(file.exists('tl.RData')){
  load('tl.RData')
} else {
  # Connect to the db
  pg = DBI::dbDriver("PostgreSQL")
  con = DBI::dbConnect(pg, dbname="twitter")
  query = paste0("SELECT * FROM twitter where username = ANY ('{",
                 paste0('"', people, '"', collapse = ','),
                 "}')")
  tl <- RPostgreSQL::dbGetQuery(
    con,
    query
  )
  save(tl, file = 'tl.RData')  
  dbDisconnect(con)
}

# Join the party affiliation
tl <- tl %>%
  left_join(diputats %>% dplyr::select(username, partit), by = 'username')

find_term <- function(term, x){
  grepl(term, tolower(x))
}

# tl$dialeg <- find_term('diàleg|diálogo|dialogar', tl$tweet)
# tl$indepe <- find_term('independència|república|independencia', tl$tweet)
# tl$presos <- find_term('presos polítics|presos políticos|pres polític|preso político', tl$tweet)
# tl$puigdemont <- find_term('puigdemont', tl$tweet)
# tl$violencia <- find_term('violencia|violència', tl$tweet)
tl$hongkong <- find_term('hong kong', tl$tweet)

pd <- tl %>%
  filter(hongkong)
View(pd)

# 
# # Get dialogue by month / party
# pd <- tl %>%
#   group_by(date = as.Date(cut(date, 'month')),
#            who = partit,
#            partit) %>%
#   summarise(all_tweets = n(),
#             indepe_tweets = length(which(indepe)),
#             hk_tweets = length(which(hong))
#             violencia_tweets = length(which(violencia)),
#             puigdemont_tweets = length(which(puigdemont)),
#             presos_tweets = length(which(presos))) %>%
#   ungroup %>%
#   mutate(p_indepe = indepe_tweets / all_tweets * 100) %>%
#   mutate(p_violencia = violencia_tweets / all_tweets * 100) %>%
#   mutate(p_puigdemont = puigdemont_tweets / all_tweets * 100) %>%
#   mutate(p_presos = presos_tweets / all_tweets * 100) %>%
#   filter(date >= '2017-09-01',
#          date <= '2018-12-31') %>%
#   gather(key, value, indepe_tweets:p_presos)
  
ggplot(data = pd %>% 
         filter(key %in% c('p_violencia')),
       aes(x = date,
           y = value,
           color = key)) +
  geom_bar(stat = 'identity') +
  # geom_line() +
  # geom_point(aes(size = all_tweets),
  #            alpha = 0.3) +
  # geom_smooth() +
  facet_wrap(~who) +
  geom_vline(xintercept = as.Date('2017-10-01'))
  # geom_vline(xintercept = as.Date(c('2017-09-20', 
  #                      '2017-10-01')))

if('twitter.RData' %in% dir()){
  load('twitter.RData')
} else {
  # # Get tweets
  # twint -s "('Carles Puigdemont' OR '@KRLS')" --since 2017-01-01 --until 2019-08-31 -o data/puigdemont --csv

  df <- read_csv('data/puigdemont/tweets.csv') %>%
                mutate(who = 'Puigdemont') %>%
    filter(!duplicated(id))
  
  # Adjust for time zone
  library(lubridate)
  df$date_time <- as.POSIXct(paste0(df$date, ' ', df$time, ' ', '+0', df$timezone))
  Sys.setenv(TZ='CET')
  
  
  save(df, file = 'twitter.RData')
}

pd <- df %>%
  group_by(date) %>%
  tally
