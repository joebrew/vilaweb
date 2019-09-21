# Libraries
library(vilaweb)
library(tidyverse)
library(lubridate)
library(rlang)

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


processed <- 'processed.RData'
if(processed %in% dir()){
  load(processed)
} else {

  
  people <- c('junqueras', 
              'miqueliceta', 'lozanoirene',
              'manuelvalls', 'albert_rivera', 'santi_abascal', 'inesarrimadas',
              'gabrielrufian', 'quimtorraipla', 'krls', 'josepcosta', 'elsa_artadi', 'toni_comin', 'rogertorrent', 'ernestmaragall',
              'adacolau', 'jaumecollboni', 'josepborrellf', 'sanchezcastejon', 'forcadellcarme', 'jcuixart', 'jordialapreso', 'rosaliavt')
  
  ## Ensure they are updated in the database
  # update_database(people = people)
  
  # Get their tweets
  if('tweets.RData' %in% dir()){
    load('tweets.RData')
  } else {
    tl <- get_twitter_data_from_database(people = people)
    save(tl, file = 'tweets.RData')
  }
  
  # Generate helper variables
  tl <- tl %>%
    mutate(month = as.Date(cut(date, 'month'))) %>%
    mutate(year = as.numeric(format(date, '%Y'))) 
  
  get_mentions <- function(z){
    z <- gsub('[', '', z, fixed = T)
    z <- gsub(']', '', z, fixed = T)
    z <- gsub("'", '', z, fixed = T)
    z <- gsub(' ', '', z)
    # z <- trimws(z, which = 'both')
    z <- strsplit(z, split = ',')
    return(z)
  }
  all_mentions <- get_mentions(tl$mentions)
  
  # Generate tag variables
  ll <- setNames(rep(NA, length(people)), as.list(people))
  tl <- tl %>% mutate( !!! ll )
  for(j in 1:length(people)){
    message(j, ' of ', length(people))
    person <- people[j]
    out = lapply(all_mentions, function(x){person %in% x})
    tl[,person] <- unlist(out)
  }
  
  # # Get who follows who
  # followers <- diputats$username[!is.na(diputats$username)]
  # followers_list <- list()
  # for(i in 1:length(followers)){
  #   message(i, ' of ', length(followers))
  #   this_person <- followers[i]
  #   fds <- get_friends(this_person, retryonratelimit = TRUE)
  #   while(nrow(fds) == 0){
  #     sleeper <- rnorm(mean = 300, sd = 100, n = 1)
  #     sleeper <- ifelse(sleeper < 50, 100, sleeper)
  #     message('Sleeping ', sleeper, ' seconds')
  #     Sys.sleep(time = sleeper)
  #     fds <- get_friends(this_person, retryonratelimit = TRUE)
  #   }
  #   followers_list[[i]] <- fds
  #   save(followers_list, i, followers, file = 'temp.RData')
  # }
  # followers <- bind_rows(followers_list)
  
  # Get info on the diputats
  diputats_info <- lookup_users(people, parse = TRUE, token = NULL)
  diputats_info <- diputats_info %>%
    dplyr::select(user_id,
                  screen_name,
                  name,
                  location,
                  description,
                  url,
                  protected,
                  followers_count,
                  friends_count,
                  listed_count,
                  statuses_count,
                  favourites_count,
                  account_created_at,
                  verified,
                  profile_url,
                  account_lang) %>%
    mutate(user_name = tolower(screen_name))
  
  # First define probable replies
  library(stringr)
  detect_reply <- function(ttl = tl){
    ttl <- ttl %>% dplyr::select(tweet, mentions)
    the_mentions <- get_mentions(ttl$mentions)
    the_full_mentions <- str_extract_all(ttl$tweet, "(?<=^|\\s)@[^\\s]+")
    the_full_mentions <- lapply(the_full_mentions, function(x){gsub('@', '', x)})
    # Detect reply by saying if the number of people mentioned in the_mentions
    # is greater than those in the the text of the tweet
    ir_list <- list()
    for(i in 1:length(the_full_mentions)){
      message(i)
      mentions_length <- length(unlist(the_mentions[[i]]))
      full_mentions_length <- length(unlist(the_full_mentions[[i]]))
      out <- mentions_length > full_mentions_length
      ir_list[[i]] <- out
    }
    out <- unlist(ir_list)
    return(out)
  }
  tl$is_reply <- detect_reply(tl)
  
  save(tl, all_mentions, people, diputats_info,
       file = processed)
}

get_mentions <- function(z){
  z <- gsub('[', '', z, fixed = T)
  z <- gsub(']', '', z, fixed = T)
  z <- gsub("'", '', z, fixed = T)
  z <- gsub(' ', '', z)
  # z <- trimws(z, which = 'both')
  z <- strsplit(z, split = ',')
  return(z)
}


# Get the sum of retweets, 2019 through august
pd <- tl %>%
  filter(date >= '2019-01-01',
         date <= '2019-08-31') %>%
  # mutate(ratio = replies_count / retweets_count) %>%
  group_by(username) %>%
  summarise(tweets = n(),
            tweets_no_replies = length(tweet[!is_reply]),
            retweets = sum(retweets_count),
            retweets_no_replies = sum(retweets_count[!is_reply]),
            replies_no_replies = sum(replies_count[!is_reply]),
            likes = sum(likes_count)) %>%
  ungroup %>%
  mutate(retweets_per_tweet_no_replies = retweets_no_replies / tweets_no_replies) %>%
  mutate(positive = tweets + likes) %>%
  # mutate(ratio = replies) %>%
  arrange(desc(retweets_per_tweet_no_replies)) %>%
  filter(!username %in% c('santi_abascal', 'rosaliavt'))

library(ggthemes)
pd$username <- paste0('@', pd$username)
pd$username <- factor(pd$username, levels = rev(pd$username))
ggplot(data = pd,
       aes(x = username,
           y = retweets_per_tweet_no_replies)) +
  geom_segment(aes(yend = 0, xend = username), alpha = 0.7) +
  geom_point() +
  geom_text(aes(label = numberfy(retweets_per_tweet_no_replies)), nudge_y = 100, alpha = 0.6, size = 3) +
  theme_fivethirtyeight() +
  # theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 0, size = 16)) +
  labs(y = 'Retweets',
       title = 'Nombre de retweets de mitjana per tweet',
       subtitle = '(Sense incloure respostes, 1 de gener al 31 d\'agost de 2019)') +
  coord_flip()

# Get Lozano
lozano <- get_twitter_data_from_database(people = 'lozanoirene')
