library(tidyverse)
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

# ## check to see if the token is loaded
# identical(token, get_token())


if('saved.RData' %in% dir()){
  load('saved.RData')
} else {
  rt <- search_tweets(
    'independentismo dividido', 
    n = 100000, 
    include_rts = F, 
    retryonratelimit = TRUE
  )
  save(rt, file = 'saved.RData')
}
