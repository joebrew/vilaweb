
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

# ## check to see if the token is loaded
# identical(token, get_token())

x = get_friends('joethebrew')
z = search_users(q = x[1])
y = get_mentions('joethebrew')


hunger_strike <-
  rt <- search_tweets(
    '"vaga de fam" OR "vagadefam" OR "#vagadefam" OR "huelga de hambre" OR "hunger strike"', 
    n = 10000000000, 
    include_rts = T, 
    retryonratelimit = TRUE
  )

## preview tweets data
rt <- hunger_strike
x <- rt
# save(hunger_strike, file = 'hunger_strike_10_des.RData')
load('hunger_strike.RData')
x <- bind_rows(x, rt)
x <- x %>% dplyr::distinct(status_id, .keep_all = TRUE)
ts_plot(rt, by = '30 mins', trim = 0) +
  databrew::theme_databrew() +
  labs(x = 'Temps (intervals de 30 minuts)',
       y = 'Tuits',
       title = 'Tuits sobre la vaga de fam',
       subtitle = 'Freqüència de tuits amb les paraules "vaga de fam"*',
       caption = '\nFont: Dades recollides del API REST de Twitter via rtweet a 2018-12-09 23:00:00 per @joethebrew.\n*Inclou les expressions "vaga de fam", "vagadefam", "#vagadefam", "huelga de hambre", "hunger strike".') +
  geom_area(fill = 'black', alpha = 0.3) +
  theme(plot.subtitle = element_text(size = 18),
        plot.title = element_text(size = 34))
ggsave('~/Desktop/vaga.png')


## preview users data
# users_data(rt)
library(dplyr)
library(tidyr)

rt$date <- as.Date(rt$created_at)
rt %>% 
  group_by(date) %>%
  tally %>%
  mutate(dow = weekdays(date))
# save(rt, file = 'hunger_strike.RData')
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

# Get friends
# Retrieve a list of all the accounts a user follows.

## get user IDs of accounts followed by CNN
cnn_fds <- get_friends("cnn")

## lookup data on those accounts
cnn_fds_data <- lookup_users(cnn_fds$user_id)

# Get followers
# Retrieve a list of the accounts following a user.

## get user IDs of accounts following CNN
cnn_flw <- get_followers("cnn", n = 75000)

## lookup data on those accounts
cnn_flw_data <- lookup_users(cnn_flw$user_id)

# Or if you really want ALL of their followers:
  
## how many total follows does cnn have?
cnn <- lookup_users("cnn")

## get them all (this would take a little over 5 days)
cnn_flw <- get_followers(
  "cnn", n = cnn$followers_count, retryonratelimit = TRUE
)

# Get the most recent 3,200 tweets from cnn, BBCWorld, and foxnews.

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

# Get trends
# Discover what’s currently trending in San Francisco.

sf <- get_trends("san francisco")

# Posting statuses
post_tweet("my first rtweet #rstats")
# Following users
## ty for the follow ;)
post_follow("kearneymw")

```{r}
# Libraries
library(vilaweb)
library(rtweet)
library(igraph)
library(hrbrthemes)
library(ggraph)
library(tidyverse)
library(databrew)
library(translateR)
```

```{r}
rstats <- search_tweets('lazis', n=1500)

# same as previous recipe
filter(rstats, retweet_count > 0) %>% 
  select(screen_name, mentions_screen_name) %>%
  unnest(mentions_screen_name) %>% 
  filter(!is.na(mentions_screen_name)) %>% 
  graph_from_data_frame() -> rt_g
```

```{r}
#To help de-clutter the vertex labels, we’ll only add labels for nodes that have a degree of 20 or more (rough guess — you should look at the degree distribution for more formal work). We’ll also include the degree for those nodes so we can size them properly:
V(rt_g)$node_label <- unname(ifelse(degree(rt_g)[V(rt_g)] > 20, names(V(rt_g)), ""))
V(rt_g)$node_size <- unname(ifelse(degree(rt_g)[V(rt_g)] > 20, degree(rt_g), 0))

# V(rt_g)$node_label <- unname(names(V(rt_g)), "")
# V(rt_g)$node_size <- unname(degree(rt_g), 0)
```

```{r}
# Now, we’ll create the graph. Using ..index.. for the alpha channel will help show edge weight without too much extra effort. Note the heavy customization of geom_node_label(). Thomas made it way too easy to make beautiful network graphs with ggraph:
ggraph(rt_g, layout = 'linear', circular = TRUE) + 
  geom_edge_arc(edge_width=0.125, aes(alpha=..index..)) +
  geom_node_label(aes(label=node_label, size=node_size),
                  label.size=0, fill="#ffffff66", segment.colour="springgreen",
                  color="slateblue", repel=TRUE, family=font_rc, fontface="bold") +
  coord_fixed() +
  scale_size_area(trans="sqrt") +
  labs(title="Retweet Relationships", subtitle="Most retweeted screen names labeled. Darkers edges == more retweets. Node size == larger degree") +
  theme_graph(base_family=font_rc) +
  theme(legend.position="none")
```