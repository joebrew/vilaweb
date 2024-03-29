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


rt <- search_tweets(
    '"#GlobalSpain"', 
    n = 1000000000, 
    include_rts = T, 
    retryonratelimit = TRUE
  )
rt2 <- search_tweets(
  '"#ThisIsTheRealSpain"', 
  n = 1000000000, 
  include_rts = T, 
  retryonratelimit = TRUE
)
save(rt, rt2,
     file = 'global_spain.RData')
agg <-
  rt %>%
  mutate(hashtag = '#GlobalSpain') %>%
  bind_rows(rt2 %>%
              mutate(hashtag = '#ThisIsTheRealSpain')) %>%
  mutate(date = cut(created_at, 'hour')) %>%
  mutate(date = as.POSIXct(date)) %>%
  group_by(date, hashtag) %>%
  tally

ggplot(data = agg,
       aes(x = date,
           y = n,
           color = hashtag)) +
  geom_line(size = 1) +
  databrew::theme_databrew() +
  # geom_smooth() +
  labs(y = 'Piulades (per hora)',
       x = 'Hora',
       title = 'Use of hashtags "#GlobalSpain" and "#ThisIsTheRealSpain"',
       caption = 'Data obtained via Twitter\'s REST API at approx 20:00 CET on Friday, Feb 1, 2019 by Joe Brew | @joethebrew.') +
  theme(legend.text = element_text(size = 30),
        plot.title = element_text(size = 20),
        plot.subtitle =  element_text(size = 16))


x = rt2 %>%
  mutate(location = Hmisc::capitalize(location)) %>%
  group_by(location) %>% tally %>% ungroup %>% arrange(desc(n)) %>%
  filter(location != '')
x$location <- factor(x$location, levels = x$location)
ggplot(data = x[1:30,],
       aes(x = location,
           y = n)) +
  geom_bar(stat = 'identity',
           alpha = 0.8,
           fill = 'darkgreen') +
  databrew::theme_databrew() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5,
                                   size = 14)) +
  labs(x = 'Location (as written by user)',
       y = 'Number of twitter accounts',
       title = 'Location of accounts using "#ThisIsTheRealSpain" hashtag',
       subtitle = 'Locations as written by the user in his/her profile. Excludes accounts with no location.',
       caption = 'Period from Jan 24 - Feb 1 2019. Data from Twitter API. Data processing and chart by Joe Brew | @joethebrew.')
ggsave('~/Desktop/realspain.png', height = 7)
# x = get_friends('joethebrew')
# z = search_users(q = x[1])
# y = get_mentions('joethebrew')

# violencia_bcn <-
#   rt <- search_tweets(
#     'violencia', 
#     n = 1000000000, 
#     include_rts = F, 
#     retryonratelimit = TRUE,
#     geocode = "41.385,2.173,20mi"
#   )
# violencia_mad <-
#   rt <- search_tweets(
#     'violencia', 
#     n = 1000000000, 
#     include_rts = F, 
#     retryonratelimit = TRUE,
#     geocode = "40.41678,-3.703,20mi"
#   )
# save(violencia_mad, violencia_bcn,
#      file = 'violencia.RData')

# violencia_bcnrt <-
#   rt <- search_tweets(
#     'violencia', 
#     n = 1000000000, 
#     include_rts = T, 
#     retryonratelimit = TRUE,
#     geocode = "41.385,2.173,20mi"
#   )
# 
# save(violencia_madrt, violencia_bcnrt,
#      file = 'violenciart.RData')
violencia_bcnrt2 <-
  rt <- search_tweets(
    '"violencia" OR "violència"', 
    n = 1000000000, 
    include_rts = T, 
    retryonratelimit = TRUE,
    geocode = "41.385,2.173,20mi"
  )
violencia_madrt <-
  rt <- search_tweets(
    '"violencia" OR "violència"', 
    n = 1000000000, 
    include_rts = T, 
    retryonratelimit = TRUE,
    geocode = "40.41678,-3.703,20mi"
  )
save(violencia_bcnrt2,
     violencia_madrt,
     file = 'violencia_bcnrt2.RData')
df <- bind_rows(violencia_bcnrt2 %>% mutate(city = 'Barcelona'), 
                violencia_madrt %>% mutate(city = 'Madrid'))
agg <-
  df %>%
  mutate(date = cut(created_at, 'hour')) %>%
  mutate(date = as.POSIXct(date)) %>%
  group_by(date, city) %>%
  tally %>%
  ungroup

ggplot(data = agg %>% 
         filter(date >= '2018-12-01 01:00:00'),
       aes(x = date,
           y = n,
           group = city,
           color = city)) +
  geom_line(size = 1) +
  databrew::theme_databrew() +
  scale_color_manual(name = '',
                     values = c('blue', 'red')) +
  # geom_smooth() +
  labs(y = 'Piulades (per hora)',
       x = 'Hora',
       title = 'Freqüencia de la paraula "violència" en piulades',
       subtitles = 'Piulades geolocalitzades a < 32 km del centre de BCN vs. MAD',
       caption = 'Inclou "violencia" i "violència". Fins al 20 des 6:00. Dades: API REST de Twitter. Gràfic: Joe Brew.') +
  theme(legend.text = element_text(size = 30),
        plot.title = element_text(size = 20),
        plot.subtitle =  element_text(size = 16))
ggsave('~/Desktop/violence.png')


madrtcdr <-
  rt <- search_tweets(
    '"los CDR" OR "els "CDR"', 
    n = 1000000000, 
    include_rts = T, 
    retryonratelimit = TRUE,
    geocode = "40.41678,-3.703,20mi"
  )
bcnrtcdr <- search_tweets(
  '"los CDR" OR "els "CDR"', 
  n = 1000000000, 
  include_rts = T, 
  retryonratelimit = TRUE,
  geocode = "41.385,2.173,20mi"
)
save(bcnrtcdr, madrtcdr,
     file = 'cdr.RData')
df <- bind_rows(bcnrtcdr %>% mutate(city = 'Barcelona'), 
                madrtcdr %>% mutate(city = 'Madrid'))
agg <-
  df %>%
  mutate(date = cut(created_at, 'hour')) %>%
  mutate(date = as.POSIXct(date)) %>%
  group_by(date, city) %>%
  tally %>%
  ungroup

ggplot(data = agg %>% 
         filter(date >= '2018-12-01 6:00:00'),
       aes(x = date,
           y = n,
           group = city,
           color = city)) +
  geom_line(size = 1) +
  databrew::theme_databrew() +
  scale_color_manual(name = '',
                     values = c('blue', 'red')) +
  # geom_smooth() +
  labs(y = 'Piulades (per hora)',
       x = 'Hora',
       title = 'Freqüencia de la paraula "CDR" en piulades',
       subtitles = 'Piulades geolocalitzades a < 32 km del centre de BCN vs. MAD',
       caption = 'Dades: API REST de Twitter. Gràfic: Joe Brew.') +
  theme(legend.text = element_text(size = 30),
        plot.title = element_text(size = 20),
        plot.subtitle =  element_text(size = 16))

# 
# hunger_strike <-
#   rt <- search_tweets(
#     '"vaga de fam" OR "vagadefam" OR "#vagadefam" OR "huelga de hambre" OR "hunger strike"', 
#     n = 10000000000, 
#     include_rts = T, 
#     retryonratelimit = TRUE
#   )
# 
# 
# # save(hunger_strike, file = 'hunger_strike_17_des.RData')
# 
# ## preview tweets data
# rt <- hunger_strike
# x <- rt
# # save(hunger_strike, file = 'hunger_strike_10_des.RData')
# # load('hunger_strike.RData')
# 
# load('hunger_strike_10_des.RData')
# h1 <- hunger_strike
# load('hunger_strike_17_des.RData')
# h2 <- hunger_strike
# # x <- bind_rows(x, rt)
# x <- bind_rows(h1, h2)
# x <- x %>% dplyr::distinct(status_id, .keep_all = TRUE)
# 
# library(vilaweb)
# agg <- 
#   x %>%
#   mutate(date = as.Date(cut(created_at, 'day'))) %>%
#   mutate(english = grepl('hunger strike', tolower(text))) %>%
#   group_by(date, english) %>%
#   tally %>%
#   ungroup %>%
#   mutate(english = ifelse(english, 'English', 'Catalan or\nSpanish'))
# agg <- agg %>% filter(date < '2018-12-18')
# 
# cols <- databrew::make_colors(n=9)[c(5,2)]
# ggplot(data = agg,
#        aes(x = date,
#            y = n,
#            fill = english)) +
#   geom_bar(stat = 'identity') +
#   databrew::theme_databrew() +
#   labs(x = '',
#        y = 'Tuits',
#        title = 'Tuits sobre la vaga de fam',
#        subtitle = 'Freqüència de tuits amb les paraules "vaga de fam"*',
#        caption = '\nFont: Dades recollides del API REST de Twitter via rtweet a 2018-12-18 per @joethebrew.\n*Inclou les expressions "vaga de fam", "vagadefam", "#vagadefam", "huelga de hambre", "hunger strike".\nClassified as English if containing the exact term "hunger strike".') +
#   theme(plot.subtitle = element_text(size = 18),
#         plot.title = element_text(size = 34),
#         axis.text.x = element_text(angle = 90)) +
#   scale_fill_manual(name = '',
#                     values = cols) +
#   scale_x_date(breaks = sort(unique(agg$date)))
#   
# 
# ts_plot(x, by = '60 mins', trim = 0) +
#   databrew::theme_databrew() +
#   labs(x = 'Temps (intervals de 60 minuts)',
#        y = 'Tuits',
#        title = 'Tuits sobre la vaga de fam',
#        subtitle = 'Freqüència de tuits amb les paraules "vaga de fam"*',
#        caption = '\nFont: Dades recollides del API REST de Twitter via rtweet a 2018-12-18 per @joethebrew.\n*Inclou les expressions "vaga de fam", "vagadefam", "#vagadefam", "huelga de hambre", "hunger strike".') +
#   geom_area(fill = 'black', alpha = 0.3) +
#   theme(plot.subtitle = element_text(size = 18),
#         plot.title = element_text(size = 34)) 
# ggsave('~/Desktop/vaga.png')
# 
# 
# ## preview users data
# # users_data(rt)
# library(dplyr)
# library(tidyr)
# 
# rt$date <- as.Date(rt$created_at)
# rt %>% 
#   group_by(date) %>%
#   tally %>%
#   mutate(dow = weekdays(date))
# # save(rt, file = 'hunger_strike.RData')
# x <- rt %>%
#   mutate(esp = grepl('cat|span|spain|prison|supreme|jordi', tolower(text)),
#          car = grepl('carav|migran|mexic', tolower(text))) %>%
#   dplyr::select(created_at, date, esp, car)
# x$hour <- as.POSIXlt(x$created_at)$hour
# x$date_time <- as.POSIXct(paste0(x$date, ' ', x$hour, ':00:00'))
# 
# x <- x %>%
#   gather(key, value, esp:car)
# x <- x %>%
#   group_by(key, date_time) %>%
#   summarise(n = length(which(value)))
# 
# ggplot(data = x %>%
#          mutate(key = ifelse(key == 'car', 'Caravan', 'Political prisoners')) %>%
#          filter(date_time <= '2018-12-03 12:00:00 CET'),
#        aes(x = date_time,
#            y = n,
#            color = key)) +
#   geom_line()
# 
# 
# x$hour <- as.POSIXlt(x$created_at)$hour
# x$date_time <- as.POSIXct(paste0(x$date, ' ', x$hour, ':00:00'))
# 
# x %>%
#   group_by(date) %>% tally
# library(ggplot2)
# library(databrew)
# ## plot time series (if ggplot2 is installed)
# ts_plot(x, by = '30 mins', trim = 0) +
#   databrew::theme_databrew() +
#   labs(x = 'Time (30 minute intervals)',
#        y = 'Tweets',
#        title = '"Political prisoners" on twitter',
#        subtitle = 'Frequency of tweets containing phrase "political prisoners"',
#        caption = "\nSource: Data collected from Twitter's REST API via rtweet at 2018-12-04 01:00:00. | @joethebrew") +
#   geom_area(fill = 'black', alpha = 0.3) +
#   theme(plot.subtitle = element_text(size = 18),
#         plot.title = element_text(size = 34))
# ggsave('~/Desktop/prisoners.png')
# 
# ## plot time series of tweets
# ts_plot(rt, "3 hours") +
#   ggplot2::theme_minimal() +
#   ggplot2::theme(plot.title = ggplot2::element_text(face = "bold")) +
#   ggplot2::labs(
#     x = NULL, y = NULL,
#     title = "Frequency of #rstats Twitter statuses from past 9 days",
#     subtitle = "Twitter status (tweet) counts aggregated using three-hour intervals",
#     caption = "\nSource: Data collected from Twitter's REST API via rtweet"
#   )
# 
# ## search for 250,000 tweets containing the word data
# rt <- search_tweets(
#   "data", n = 250000, retryonratelimit = TRUE
# )
# 
# # Stream all tweets mentioning realDonaldTrump or Trump for a week.
# 
# ## stream london tweets for a week (60 secs x 60 mins * 24 hours *  7 days)
# stream_tweets(
#   "realdonaldtrump,trump",
#   timeout = 60 * 60 * 24 * 7,
#   file_name = "tweetsabouttrump.json",
#   parse = FALSE
# )
# 
# ## read in the data as a tidy tbl data frame
# djt <- parse_stream("tweetsabouttrump.json")
# 
# # Get friends
# # Retrieve a list of all the accounts a user follows.
# 
# ## get user IDs of accounts followed by CNN
# cnn_fds <- get_friends("cnn")
# 
# ## lookup data on those accounts
# cnn_fds_data <- lookup_users(cnn_fds$user_id)
# 
# # Get followers
# # Retrieve a list of the accounts following a user.
# 
# ## get user IDs of accounts following CNN
# cnn_flw <- get_followers("cnn", n = 75000)
# 
# ## lookup data on those accounts
# cnn_flw_data <- lookup_users(cnn_flw$user_id)
# 
# # Or if you really want ALL of their followers:
#   
# ## how many total follows does cnn have?
# cnn <- lookup_users("cnn")
# 
# ## get them all (this would take a little over 5 days)
# cnn_flw <- get_followers(
#   "cnn", n = cnn$followers_count, retryonratelimit = TRUE
# )
# 
# # Get the most recent 3,200 tweets from cnn, BBCWorld, and foxnews.
# 
# ## get user IDs of accounts followed by CNN
# tmls <- get_timelines(c("cnn", "BBCWorld", "foxnews"), n = 3200)
# 
# ## plot the frequency of tweets for each user over time
# tmls %>%
#   dplyr::filter(created_at > "2017-10-29") %>%
#   dplyr::group_by(screen_name) %>%
#   ts_plot("days", trim = 1L) +
#   ggplot2::geom_point() +
#   ggplot2::theme_minimal() +
#   ggplot2::theme(
#     legend.title = ggplot2::element_blank(),
#     legend.position = "bottom",
#     plot.title = ggplot2::element_text(face = "bold")) +
#   ggplot2::labs(
#     x = NULL, y = NULL,
#     title = "Frequency of Twitter statuses posted by news organization",
#     subtitle = "Twitter status (tweet) counts aggregated by day from October/November 2017",
#     caption = "\nSource: Data collected from Twitter's REST API via rtweet"
#   )
# 
# ## search for users with #rstats in their profiles
# usrs <- search_users("#rstats", n = 1000)
# 
# # Get trends
# # Discover what’s currently trending in San Francisco.
# 
# sf <- get_trends("san francisco")
# 
# # Posting statuses
# post_tweet("my first rtweet #rstats")
# # Following users
# ## ty for the follow ;)
# post_follow("kearneymw")
# 
# ```{r}
# # Libraries
# library(vilaweb)
# library(rtweet)
# library(igraph)
# library(hrbrthemes)
# library(ggraph)
# library(tidyverse)
# library(databrew)
# library(translateR)
# ```
# 
# ```{r}
# rstats <- search_tweets('lazis', n=1500)
# 
# # same as previous recipe
# filter(rstats, retweet_count > 0) %>% 
#   select(screen_name, mentions_screen_name) %>%
#   unnest(mentions_screen_name) %>% 
#   filter(!is.na(mentions_screen_name)) %>% 
#   graph_from_data_frame() -> rt_g
# ```
# 
# ```{r}
# #To help de-clutter the vertex labels, we’ll only add labels for nodes that have a degree of 20 or more (rough guess — you should look at the degree distribution for more formal work). We’ll also include the degree for those nodes so we can size them properly:
# V(rt_g)$node_label <- unname(ifelse(degree(rt_g)[V(rt_g)] > 20, names(V(rt_g)), ""))
# V(rt_g)$node_size <- unname(ifelse(degree(rt_g)[V(rt_g)] > 20, degree(rt_g), 0))
# 
# # V(rt_g)$node_label <- unname(names(V(rt_g)), "")
# # V(rt_g)$node_size <- unname(degree(rt_g), 0)
# ```
# 
# ```{r}
# # Now, we’ll create the graph. Using ..index.. for the alpha channel will help show edge weight without too much extra effort. Note the heavy customization of geom_node_label(). Thomas made it way too easy to make beautiful network graphs with ggraph:
# ggraph(rt_g, layout = 'linear', circular = TRUE) + 
#   geom_edge_arc(edge_width=0.125, aes(alpha=..index..)) +
#   geom_node_label(aes(label=node_label, size=node_size),
#                   label.size=0, fill="#ffffff66", segment.colour="springgreen",
#                   color="slateblue", repel=TRUE, family=font_rc, fontface="bold") +
#   coord_fixed() +
#   scale_size_area(trans="sqrt") +
#   labs(title="Retweet Relationships", subtitle="Most retweeted screen names labeled. Darkers edges == more retweets. Node size == larger degree") +
#   theme_graph(base_family=font_rc) +
#   theme(legend.position="none")
# ```

library(vilaweb)
library(rtweet)
library(tidyverse)
library(databrew)
library(translateR)
library(sentimentr) # https://github.com/trinker/sentimentr
require(RPostgreSQL)
require(readr)  
library(ggrepel)
require(DBI)
library(ggtern)

people <- 'globalspain'
if(file.exists('tl.RData')){
  load('tl.RData')
} else {
  # Connect to the db
  pg = DBI::dbDriver("PostgreSQL")
  con = DBI::dbConnect(pg, dbname="twitter")
  tl <- RPostgreSQL::dbGetQuery(
    con,
    paste0("SELECT * FROM twitter WHERE username='", people, "'")
  )
  save(tl, file = 'tl.RData')  
  dbDisconnect(con)
}

tl <- tl %>%
  mutate(madrid = grepl('madrid', tolower(tweet)),
         barcelona = grepl('barcelona', tolower(tweet)),
         catalonia = grepl('cataluña|catalunya|catalonia|catala', tolower(tweet)),
         andalucia = grepl('andalu', tolower(tweet)),
         prado = grepl('prado', tolower(tweet))) 
x <- tl %>%
  summarise(madrid = length(which(madrid)),
            barcelona = length(which(barcelona)),
            catalonia = length(which(catalonia)),
            anadalucia = length(which(andalucia)),
            prado = length(which(prado)))
x
y <- tl %>%
  group_by(month = as.Date(cut(date, 'month'))) %>%
  summarise(n_mad = length(which(madrid)),
            n_bcn = length(which(barcelona)),
            n_cat = length(which(catalonia)),
            n = n()) %>%
  ungroup %>%
  mutate(p_mad = n_mad / n * 100,
         p_bcn = n_bcn / n * 100,
         p_cat = n_cat / n * 100)
yg <- gather(y, key, value, n_mad:p_cat)

g1 <- ggplot(data = yg %>% filter(month < '2019-02-01',
                            key %in% c('p_bcn', 'p_mad')) %>%
                              mutate(key = ifelse(key == 'p_bcn',
                                                  'Barcelona',
                                                  'Madrid')),
       aes(x = month,
           y = value)) + 
  geom_line(aes(color = key)) +
  theme_vilaweb() +
  labs(x = 'Month',
       y = '% of tweets that month',
       title = 'Tweets mentioning "Barcelona" and "Madrid"\nby @GlobalSpain',
       subtitle = 'Monthly percentages') +
  scale_color_manual(name ='', values = c('darkorange', 'darkblue'))


g2 <- ggplot(data = yg %>% filter(month < '2019-02-01',
                            key %in% c('n_bcn', 'n_mad')) %>%
         mutate(key = ifelse(key == 'n_bcn',
                             'Barcelona',
                             'Madrid')),
       aes(x = month,
           y = value)) + 
  geom_line(aes(color = key)) +
  theme_vilaweb() +
  labs(x = 'Month',
       y = 'Number of tweets',
       # title = 'Tweets mentioning "Barcelona" and "Madrid" by @GlobalSpain',
       subtitle = 'Exact number of tweets') +
  scale_color_manual(name ='', values = c('darkorange', 'darkblue'))



ggplot(data = yg %>% filter(month < '2019-02-01',
                            key %in% c('p_cat')),
       aes(x = month,
           y = value)) + 
  geom_line(aes(color = key)) +
  theme_vilaweb() +
  labs(x = 'Month',
       y = '% of tweets that month',
       title = 'Tweets mentioning "Barcelona" and "Madrid" by @GlobalSpain',
       subtitle = 'Monthly percentages') 

# LOS GOYA
goya_bcnrt <-
  rt <- search_tweets(
    'goya', 
    n = 1000000000, 
    include_rts = T, 
    retryonratelimit = TRUE,
    geocode = "41.385,2.173,20mi"
  )
goya_madrt <-
  rt <- search_tweets(
    'goya', 
    n = 1000000000, 
    include_rts = T, 
    retryonratelimit = TRUE,
    geocode = "40.41678,-3.703,20mi"
  )
save(goya_bcnrt,
     goya_madrt,
     file = 'goya_bcnrt.RData')

presos_bcnrt <-
  rt <- search_tweets(
    'presos', 
    n = 1000000000, 
    include_rts = T, 
    retryonratelimit = TRUE,
    geocode = "41.385,2.173,20mi"
  )
presos_madrt <-
  rt <- search_tweets(
    'presos', 
    n = 1000000000, 
    include_rts = T, 
    retryonratelimit = TRUE,
    geocode = "40.41678,-3.703,20mi"
  )
save(presos_bcnrt,
     presos_madrt,
     file = 'presos_bcnrt.RData')

df <- bind_rows(goya_bcnrt %>% mutate(city = 'Barcelona',
                                      subject = 'Goya'), 
                goya_madrt %>% mutate(city = 'Madrid',
                                      subject = 'Goya'),
                presos_bcnrt %>% mutate(city = 'Barcelona',
                                      subject = 'Presos'), 
                presos_madrt %>% mutate(city = 'Madrid',
                                      subject = 'Presos'))
agg <-
  df %>%
  mutate(date = cut(created_at, 'day')) %>%
  mutate(date = as.Date(date)) %>%
  group_by(date, city, subject) %>%
  tally %>%
  ungroup


date_breaks <- sort(unique(agg$date))
date_labels <- vilaweb::make_catalan_date(date_breaks)
ggplot(data = agg,
       aes(x = date,
           y = n,
           group = subject,
           color = subject)) +
  xlim(as.Date('2019-01-29'),
       as.Date('2019-02-05')) +
  geom_line(size = 1, alpha = 0.8) +
  facet_grid(~city) +
  databrew::theme_databrew() +
  scale_color_manual(name = '',
                     values = c('blue', 'red')) +
  # geom_smooth() +
  labs(y = 'Tuits',
       x = '',
       title = 'Freqüencia de paraules "Goya" i "Presos" en tuits',
       subtitles = 'Tuits geolocalitzats a < 32 km del centre de BCN vs. MAD',
       caption = 'Dades: API REST de Twitter. Gràfic: Joe Brew | @joethebrew.') +
  theme(legend.text = element_text(size = 30),
        plot.title = element_text(size = 21),
        plot.subtitle =  element_text(size = 16),
        strip.text = element_text(size = 25),
        axis.title.y = element_text(size = 20)) 
ggsave('~/Desktop/goya_vs_presos.png')
save.image('saved.RData')
