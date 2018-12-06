# devtools::install_github("meneos/elecciones")
library(elecciones)
x = mesas('generales', yr = 2015, mes = '12')

# ## install rtweet from CRAN
# install.packages("rtweet")
# 
# Read in twitter credentials
library(yaml)
twitter_credentials <- yaml.load_file('../credentials/credentials.yaml')
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

x = get_friends('joethebrew')
z = search_users(q = x[1])
y = get_mentions('joethebrew')


hunger_strike <-
  rt <- search_tweets(
    "spain OR spanish OR OR catalonia OR catalan AND political prisoners", 
    n = 10000000000, 
    include_rts = T, 
    retryonratelimit = TRUE
  )

## preview tweets data
rt

ggplot(data = rt,
       aes(x = created_at))

## preview users data
users_data(rt)
library(dplyr)
library(tidyr)

rt$date <- as.Date(rt$created_at)
rt %>% 
  group_by(date) %>%
  tally %>%
  mutate(dow = weekdays(date))
save(rt, file = 'hunger_strike.RData')
x <- rt %>%
  mutate(esp = grepl('cat|span|spain|prison|supreme|jordi', tolower(text)),
         car = grepl('carav|migran|mexic', tolower(text))) %>%
  dplyr::select(created_at, date, esp, car)
x$hour <- as.POSIXlt(x$created_at)$hour
x$date_time <- as.POSIXct(paste0(x$date, ' ', x$hour, ':00:00'))

x <- x %>%
  gather(key, value, esp:car)
x <- x %>%
  group_by(key, date_time) %>%
  summarise(n = length(which(value)))

ggplot(data = x %>%
         mutate(key = ifelse(key == 'car', 'Caravan', 'Political prisoners')) %>%
         filter(date_time <= '2018-12-03 12:00:00 CET'),
       aes(x = date_time,
           y = n,
           color = key)) +
  geom_line()


pp <-
  rt <- search_tweets(
    "political prisoners", 
    n = 10000000000, 
    include_rts = T, 
    retryonratelimit = TRUE
  )
ts_plot(pp)
x <- rt 
x$date <- as.Date(x$created_at)

x$hour <- as.POSIXlt(x$created_at)$hour
x$date_time <- as.POSIXct(paste0(x$date, ' ', x$hour, ':00:00'))

x %>%
  group_by(date) %>% tally
library(ggplot2)
library(databrew)
## plot time series (if ggplot2 is installed)
ts_plot(x, by = '30 mins', trim = 0) +
  databrew::theme_databrew() +
  labs(x = 'Time (30 minute intervals)',
       y = 'Tweets',
       title = '"Political prisoners" on twitter',
       subtitle = 'Frequency of tweets containing phrase "political prisoners"',
       caption = "\nSource: Data collected from Twitter's REST API via rtweet at 2018-12-04 01:00:00. | @joethebrew") +
  geom_area(fill = 'black', alpha = 0.3) +
  theme(plot.subtitle = element_text(size = 18),
        plot.title = element_text(size = 34))
ggsave('~/Desktop/prisoners.png')

## plot time series of tweets
ts_plot(rt, "3 hours") +
  ggplot2::theme_minimal() +
  ggplot2::theme(plot.title = ggplot2::element_text(face = "bold")) +
  ggplot2::labs(
    x = NULL, y = NULL,
    title = "Frequency of #rstats Twitter statuses from past 9 days",
    subtitle = "Twitter status (tweet) counts aggregated using three-hour intervals",
    caption = "\nSource: Data collected from Twitter's REST API via rtweet"
  )

## search for 250,000 tweets containing the word data
rt <- search_tweets(
  "data", n = 250000, retryonratelimit = TRUE
)

# Stream all tweets mentioning realDonaldTrump or Trump for a week.

## stream london tweets for a week (60 secs x 60 mins * 24 hours *  7 days)
stream_tweets(
  "realdonaldtrump,trump",
  timeout = 60 * 60 * 24 * 7,
  file_name = "tweetsabouttrump.json",
  parse = FALSE
)

## read in the data as a tidy tbl data frame
djt <- parse_stream("tweetsabouttrump.json")

Get friends
Retrieve a list of all the accounts a user follows.

## get user IDs of accounts followed by CNN
cnn_fds <- get_friends("cnn")

## lookup data on those accounts
cnn_fds_data <- lookup_users(cnn_fds$user_id)

Get followers
Retrieve a list of the accounts following a user.

## get user IDs of accounts following CNN
cnn_flw <- get_followers("cnn", n = 75000)

## lookup data on those accounts
cnn_flw_data <- lookup_users(cnn_flw$user_id)

Or if you really want ALL of their followers:
  
  ## how many total follows does cnn have?
  cnn <- lookup_users("cnn")

## get them all (this would take a little over 5 days)
cnn_flw <- get_followers(
  "cnn", n = cnn$followers_count, retryonratelimit = TRUE
)

Get the most recent 3,200 tweets from cnn, BBCWorld, and foxnews.

## get user IDs of accounts followed by CNN
tmls <- get_timelines(c("cnn", "BBCWorld", "foxnews"), n = 3200)

## plot the frequency of tweets for each user over time
tmls %>%
  dplyr::filter(created_at > "2017-10-29") %>%
  dplyr::group_by(screen_name) %>%
  ts_plot("days", trim = 1L) +
  ggplot2::geom_point() +
  ggplot2::theme_minimal() +
  ggplot2::theme(
    legend.title = ggplot2::element_blank(),
    legend.position = "bottom",
    plot.title = ggplot2::element_text(face = "bold")) +
  ggplot2::labs(
    x = NULL, y = NULL,
    title = "Frequency of Twitter statuses posted by news organization",
    subtitle = "Twitter status (tweet) counts aggregated by day from October/November 2017",
    caption = "\nSource: Data collected from Twitter's REST API via rtweet"
  )

## search for users with #rstats in their profiles
usrs <- search_users("#rstats", n = 1000)

Get trends
Discover what’s currently trending in San Francisco.

sf <- get_trends("san francisco")

Posting statuses
post_tweet("my first rtweet #rstats")
Following users
## ty for the follow ;)
post_follow("kearneymw")

system(paste0('workon twint; (echo "import twint" ; echo "twint -u joethebrew -o file.csv --csv") | python3'))
