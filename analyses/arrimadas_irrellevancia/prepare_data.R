
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
  system('twint -s \'("Arrimadas" OR "@InesArrimadas")\' --since 2019-05-01 --until 2019-07-21 -o data/arrimadas --csv')

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
  df$ok <- TRUE
  df <- df %>%
    mutate(ok = ifelse(search_string == 'Comín' &
                         (!grepl('í', tweet) &
                            ! grepl('toni_comin', tweet) &
                            ! grepl('ni Com', tweet)),
                       FALSE,
                       ok))
  df <- df %>% filter(ok)
  
  # df$date_time <- with_tz(df$date_time, 'CET')
  
  
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
  save(df, agg, file = 'saved.RData')
}

make_plot <- function(ca = FALSE,
                      y = 'n'){
  if(ca){
    if(y == 'interactions'){
      the_title <- "'Interaccions' a Twitter per dia"
    } else {
      the_title <- 'Piulets per dia' 
    }
    the_labs <- labs(x = 'Dia',
                     y = 'Piulets',
                     title = the_title,
                     subtitle = '1r de maig - 3 de juliol 2019',
                     caption = 'Joe Brew | @joethebrew | www.vilaweb.cat')
    
  } else {
    if(y == 'interactions'){
      the_title <- "Twitter 'interactions' per day"
    } else {
      the_title <- 'Tweets per day'
    }
    the_labs <- labs(x = 'Day',
                     y = 'Tweets',
                     title = the_title,
                     subtitle = 'May 1 - July 3 2019',
                     caption = 'Joe Brew | @joethebrew | www.vilaweb.cat')
  }
  agg$y <- unlist(agg[,y])
  ggplot(data = agg %>% filter(hour <= '2019-07-03'),
         aes(x = hour,
             y = y,
             group = search_string,
             fill = search_string)) +
    geom_bar(stat = 'identity', width = 1) +
    facet_wrap(~search_string) +
    theme_vilaweb() +
    scale_fill_manual(name = '',
                      values = as.character(vilaweb::colors_vilaweb()[1:4])) +
    the_labs
}

make_plot()