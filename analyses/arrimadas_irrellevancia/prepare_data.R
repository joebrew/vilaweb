
# Libraries
library(vilaweb)
library(tidyverse)
library(databrew)
library(pageviews)
library(lubridate)

if('saved.RData' %in% dir()){
  load('saved.RData')
} else {
  # # Get tweets
  # system('twint -s \'("Arrimadas" OR "@InesArrimadas")\' --since 2019-05-01 --until 2019-07-21 -o data/arrimadas --csv')

  df <- read_csv('data/arrimadas/tweets.csv') %>%
    bind_rows(read_csv('data/arrimadas.csv')) %>%
    filter(!duplicated(id))
  
  # Adjust for time zone
  library(lubridate)
  df$date_time <- as.POSIXct(paste0(df$date, ' ', df$time, ' ', '+0', df$timezone))
  Sys.setenv(TZ='CET')
  

  # save(df, file = 'saved.RData')
}


agg <- df %>%
  mutate(hour = as.Date(cut(date_time, 'day'))) %>%
  # mutate(hour = as.POSIXct(cut(date_time, 'hour'))) %>%
  group_by(hour) %>% 
  summarise(n = n(),
            retweets = sum(retweets_count, na.rm = TRUE) + 1,
            likes = sum(likes_count))
left <- expand.grid(hour  = seq(min(agg$hour),
                                max(agg$hour),
                                by = 'day'))
agg <- left_join(left, agg)
agg$n[is.na(agg$n)] <- 0
agg$retweets[is.na(agg$retweets)] <- 0
agg$likes[is.na(agg$likes)] <- 0
agg$interactions <- agg$n + agg$retweets + agg$likes


# date_breaks <- data.frame(date_time = sort(unique(agg$hour)))
# date_breaks$date <- as.Date(date_breaks$date_time)
# date_breaks$hour <- as.numeric(substr(date_breaks$date_time, 12, 13))
# keep_breaks <- date_breaks %>%
#   filter(hour %in% seq(0, 24, 8)) %>%
#   dplyr::select(date_time) %>%
#   .$date_time
# strong_lines <- date_breaks %>%
#   filter(hour %in% 0) %>%
#   dplyr::select(date_time) %>%
#   .$date_time
# 
# shader <- date_breaks %>% filter(hour == 0)
# shader$end <- shader$date_time + hours(24)
