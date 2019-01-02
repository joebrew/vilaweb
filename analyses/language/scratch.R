library(tidyverse)
# Read in twitter credentials
library(yaml)
twitter_credentials <- yaml.load_file('../../../credentials/credentials.yaml')
## load rtweet package
library(rtweet)
token <- create_token(
  app = "bcndata",
  consumer_key = twitter_credentials$twitter_api_key,
  consumer_secret = twitter_credentials$twitter_api_secret_key,
  access_token = twitter_credentials$twitter_access_token,
  access_secret = twitter_credentials$twitter_access_token_secret)



# barcelona_violencia <-
#   rt <- search_tweets(
#     'Barcelona violencia', 
#     n = 10000000000, 
#     include_rts = T, 
#     retryonratelimit = TRUE
#   )
# save(barcelona_violencia, file = 'barcelona_violencia.RData')
# los_cdrs <-
#   rt <- search_tweets(
#     '"los CDRs"', 
#     n = 10000000000, 
#     include_rts = T, 
#     retryonratelimit = TRUE
#   )
# save(los_cdrs, file = 'los_cdrs.RData')

load('barcelona_violencia.RData')
load('los_cdrs.RData')


ts_plot(barcelona_violencia, by = '60 mins', trim = 0) +
  databrew::theme_databrew() +
  labs(x = 'Temps (intervals de 60 minuts)',
       y = 'Tuits',
       title = 'Tuits sobre la vaga de fam',
       subtitle = 'Freqüència de tuits amb les paraules "vaga de fam"*',
       caption = '\nFont: Dades recollides del API REST de Twitter via rtweet a 2018-12-18 per @joethebrew.\n*Inclou les expressions "vaga de fam", "vagadefam", "#vagadefam", "huelga de hambre", "hunger strike".') +
  geom_area(fill = 'black', alpha = 0.3) +
  theme(plot.subtitle = element_text(size = 18),
        plot.title = element_text(size = 34)) 
ggsave('~/Desktop/vaga.png')

# el155 <-
#   rt <- search_tweets(
#     '"el 155"', 
#     n = 10000000000, 
#     include_rts = T, 
#     retryonratelimit = TRUE
#   )
# save(el155, file = 'el155')

