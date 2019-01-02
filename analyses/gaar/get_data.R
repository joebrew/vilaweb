library(vilaweb)
library(databrew)
library(tidyverse)
library(rtweet)
# Get 115 data
if(!file.exists('file2/tweets.csv')){
  system(paste0('python3 ../../../foreign/twint/Twint.py -s \'"el 155"\' --since 2018-12-01 -o file2.csv --csv'))
}
if(!file.exists('file3/tweets.csv')){
  system(paste0('python3 ../../../foreign/twint/Twint.py -s \'"el 21D" OR "este 21D" OR "aquest 21D" OR "#21D"\' --since 2018-12-01 -o file2.csv --csv'))
}

# # Get gaar data
# if(!file.exists('file/tweets.csv')){
#   system(paste0('python3 ../../../foreign/twint/Twint.py -s \'"LOS GAAR" OR "ELS GAAR"\' --since 2018-12-01 -o file.csv --csv'))
# }
# Read gaar data
gaar_tweets <- read_csv('file/tweets.csv')

# Get all gaar people
gaar_people <- 
  gaar_tweets$username

gaar_people_details <- lookup_users(users = unique(gaar_tweets$user_id))

plot_data <- gaar_people_details %>%
  filter(account_created_at >= '2018-07-01') %>%
  mutate(date = date_truncate(account_created_at, 'week')) %>%
  # filter(account_lang %in% c('es', 'ca')) %>%
  group_by(date,
           account_lang) %>%
  tally %>%
  ungroup %>%
  mutate(p = n / sum(n) * 100)
ggplot(plot_data,
       aes(x = date,
           y = n,
           fill = account_lang)) +
  geom_bar(stat = 'identity')

# 155
tweets_155 <- read_csv('file2/tweets.csv')
if('people155.csv' %in% dir()){
  load('people155.csv')
} else {
  people_details <- lookup_users(users = unique(tweets_155$user_id))
  save(people_details, file = 'people155.csv') 
}

plot_data <- people_details %>%
  filter(account_created_at >= '2018-06-25') %>%
  mutate(date = date_truncate(account_created_at, 'week')) %>%
  # mutate(date = as.Date(a))
  mutate(tweet_rate = statuses_count / as.numeric(Sys.Date() - as.Date(account_created_at))) %>%
  mutate(en = account_lang == 'en') %>%
  mutate(bot = en & date == '2018-12-03')

bots <- plot_data %>% filter(bot)
bot_tweets <- tweets_155 %>%
  filter(username %in% bots$screen_name)
  
plot_data <- plot_data %>% 
# filter(account_lang %in% c('es', 'ca')) %>%
  group_by(date = date) %>%
  summarise(n = n(),
            tweet_rate = mean(tweet_rate),
            en_rate = length(which(en)) / n() * 100) %>%
  ungroup %>%
  mutate(p = n / sum(n) * 100) %>%
  mutate(crazy = date == '2018-12-03')
cols <- c('darkblue', 'darkorange')
ggplot(plot_data,
       aes(x = date,
           y = n)) +
  geom_bar(stat = 'identity',
           aes(fill = crazy),
           alpha = 0.6,
           lwd = 0.3,
           color = 'black') +
  theme_vilaweb() +
  scale_x_date(breaks = sort(unique(plot_data$date))) +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(x = 'First day of week of account creation',
       y = 'Accounts',
       title = 'Creation week of twitter accounts tweeting about "155"',
       subtitle = '',
       caption = '@joethebrew') +
  scale_fill_manual(name = '',
                    values = cols) +
  theme(legend.position = 'none')


ggplot(plot_data,
       aes(x = date,
           y = en_rate)) +
  geom_bar(stat = 'identity',
           aes(fill = crazy),
           alpha = 0.6,
           lwd = 0.3,
           color = 'black') +
  theme_vilaweb() +
  scale_x_date(breaks = sort(unique(plot_data$date))) +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(x = 'First day of week of account creation',
       y = 'Accounts',
       title = 'Creation week of twitter accounts tweeting about "155"',
       subtitle = '',
       caption = '@joethebrew') +
  scale_fill_manual(name = '',
                    values = cols) +
  theme(legend.position = 'none')
