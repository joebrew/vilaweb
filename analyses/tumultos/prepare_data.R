# Libraries
library(vilaweb)
library(tidyverse)
library(lubridate)
source('../../R/scrape.R')


# Get tweets about parlem/hablemos
if(!'twitter_data.csv' %in% dir()){
  scrape("('tumulto') OR ('tumultuoso') OR ('tumultuario') OR ('tumultos') OR ('tumultuosos') OR ('tumultuarios') OR ('tumultuós') OR ('tumultuari') OR ('tumultuaris')")
}
tl <- read_csv('twitter_data.csv')
  
month_plot <- function(tl,
                       min_date = '2016-01-01',
                       max_date = '2019-09-30'){
  # Make plot
  pd <- tl %>%
    # Generate date columns
      mutate(month = as.Date(cut(date, 'month')),
             year = as.Date(cut(date, 'year')),
             week = as.Date(cut(date, 'week'))) %>%
    filter(!duplicated(id)) %>%
    filter(date >= '2016-01-01') %>%
    group_by(date = month) %>%
    summarise(tweets = n(),
              retweets = sum(retweets_count + 1, na.rm = TRUE),
              likes = sum(likes_count, na.rm = TRUE),
              replies = sum(replies_count, na.rm = TRUE))
  pd <- pd %>%
    gather(key, value, tweets:replies)
  pd$key <- ifelse(pd$key == "likes", "M'agrada",
                   ifelse(pd$key == 'replies', 'Respostes',
                          ifelse(pd$key == 'retweets', 'Repiulets',
                                 ifelse(pd$key == 'tweets', 'Piulets', NA))))
  pd <- pd %>% filter(key == 'Repiulets') %>%
    filter(date < '2019-10-01')
  
  date_breaks <- seq(min(pd$date),
                     max(pd$date),
                     by = 'month')
  date_labels <- make_catalan_date(date_breaks)
  date_labels <- gsub('\n', ' ', date_labels)
  ggplot(data = pd,
         aes(x = date,
             y = value)) +
    geom_area(fill = colors_vilaweb()[4],
              alpha = 0.6,
              color = 'black') +
    geom_point() +
    # geom_line() +
    # geom_bar(stat = 'identity') +
    # facet_wrap(~key, scales = 'free_y') +
    theme_vilaweb() +
    labs(title = "Piulets amb les paraules 'parlem' i 'hablemos'",
         subtitle = "(Incloent 'repiulets')",
         x = 'Mes',
         y = 'Piulets',
         caption = "Consulta de cerca: ('parlem') AND ('hablamos' OR 'hablemos')\nDades: Twitter. Gràfic: @joethebrew. www.vilaweb.cat") +
    # geom_text(aes(label = value),
    #           nudge_y = 1000)
    scale_x_date(breaks = date_breaks,
                 labels = date_labels) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
}



day_plot <- function(tl,
                     min_date = '2017-09-01',
                     max_date = '2017-12-30'){
  # Make plot
  pd <- tl %>%
    # Generate date columns
    mutate(month = as.Date(cut(date, 'month')),
           year = as.Date(cut(date, 'year')),
           week = as.Date(cut(date, 'week'))) %>%
    filter(!duplicated(id)) %>%
    filter(date >= min_date,
           date <= max_date) %>%
    group_by(date) %>%
    summarise(tweets = n(),
              retweets = sum(retweets_count + 1, na.rm = TRUE),
              likes = sum(likes_count, na.rm = TRUE),
              replies = sum(replies_count, na.rm = TRUE))
  pd <- pd %>%
    gather(key, value, tweets:replies)
  pd$key <- ifelse(pd$key == "likes", "M'agrada",
                   ifelse(pd$key == 'replies', 'Respostes',
                          ifelse(pd$key == 'retweets', 'Repiulets',
                                 ifelse(pd$key == 'tweets', 'Piulets', NA))))
  pd <- pd %>% filter(key == 'Piulets') %>%
    filter(date < '2019-10-01')
  
  date_breaks <- seq(min(pd$date),
                     max(pd$date),
                     by = 'day')
  # date_labels <- make_catalan_date(date_breaks)
  date_labels <- format(date_breaks, '%d %b')
  date_labels <- gsub('\n', ' ', date_labels)
  ggplot(data = pd,
         aes(x = date,
             y = value)) +
    # geom_area(fill = colors_vilaweb()[4],
    #           alpha = 0.6,
    #           color = 'black') +
    # geom_point() +
    # geom_line() +
    geom_bar(stat = 'identity',
             fill = colors_vilaweb()[4],
             color = 'black') +
    # facet_wrap(~key, scales = 'free_y') +
    theme_vilaweb() +
    labs(title = "Piulets amb les paraules 'parlem' i 'hablemos'",
         subtitle = "(Incloent 'repiulets')",
         x = 'Dia',
         y = 'Piulets',
         caption = "Consulta de cerca: ('parlem') AND ('hablamos' OR 'hablemos')\nDades: Twitter. Gràfic: @joethebrew. www.vilaweb.cat") +
    # geom_text(aes(label = value),
    #           nudge_y = 1000)
    scale_x_date(breaks = date_breaks,
                 labels = date_labels) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
}
