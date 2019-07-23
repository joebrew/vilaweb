
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

  data_dir <- dir('data', recursive = TRUE)
  out_list <- list()
  sub_dirs <- data_dir
  for(i in 1:length(sub_dirs)){
    
    this_dir <- sub_dirs[i]
    file_path <- paste0('data/', sub_dirs[i])
    search_string <- unlist(strsplit(file_path, '/'))[2]
    data <- read_csv(file_path) %>%
      mutate(search_string = search_string) %>%
      filter(!duplicated(id))
    out_list[[i]] <- data
  }
  
  df <- bind_rows(out_list)
  # Adjust for time zone
  library(lubridate)
  df$date_time <- as.POSIXct(paste0(df$date, ' ', df$time, ' ', '+0', df$timezone))
  Sys.setenv(TZ='CET')
  

  save(df, agg, file = 'saved.RData')
}


agg <- df %>%
  mutate(hour = as.Date(cut(date_time, 'day'))) %>%
  # mutate(hour = as.POSIXct(cut(date_time, 'hour'))) %>%
  group_by(hour, search_string) %>% 
  summarise(n = n(),
            retweets = sum(retweets_count, na.rm = TRUE) + 1,
            likes = sum(likes_count))
left <- expand.grid(hour  = seq(min(agg$hour),
                                max(agg$hour),
                                by = 'day'),
                    search_string = sort(unique(agg$search_string)))
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
