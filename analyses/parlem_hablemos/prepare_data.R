# Libraries
library(vilaweb)
library(tidyverse)
library(lubridate)
source('../../R/scrape.R')


# Get tweets about parlem/hablemos
if(!'twitter_data.csv' %in% dir()){
  tl <- scrape("('parlem') AND ('hablamos' OR 'hablemos')",
               start_date = '2019-11-01')
}
tl <- read_csv('twitter_data.csv')
tl <- tl %>% filter(!duplicated(id))

# Get tweets about diálogo
if(!'dialeg.csv' %in% dir()){
  source('../../R/scrape.R')
  tl <- scrape("('diàleg' OR 'diálogo') AND ('cataluña' OR 'catalunya' OR 'catalans' OR 'catalanes' OR 'independència' OR 'independencia' OR 'independentisme' OR 'independentismo'",
               file_path = 'dialeg.csv')
}
dialeg <- read_csv('dialeg.csv') %>% filter(!duplicated(id))


# Get tweets about diálogo
# (had to run manually in terminal, issues with quotations)
if(!'dentroley.csv' %in% dir()){
  source('../../R/scrape.R')
  tl <- scrape('("diàleg" OR "diálogo") AND ("dentro de la ley" OR "dentro de la consticución" OR "dins de la llei" OR "dins la llei" OR "dintre de la llei" OR "dins de la constitució" OR "dintre de la constitució" OR "dins la constitució")',
               file_path = "dentroley.csv")
}
dentroley <- read_csv('dentroley.csv') %>% filter(!duplicated(id))


if(!'dentroleyonly.csv' %in% dir()){
  source('../../R/scrape.R')
  tl <- scrape('("dentro de la ley" OR "dentro de la consticución" OR "dins de la llei" OR "dins la llei" OR "dintre de la llei" OR "dins de la constitució" OR "dintre de la constitució" OR "dins la constitució")',
               file_path = "dentroleyonly.csv")
}
dentroleyonly <- read_csv('dentroleyonly.csv') %>% filter(!duplicated(id))

month_plot <- function(tl,
                       min_date = '2016-01-01',
                       max_date = '2019-10-31'){
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
    filter(date < '2019-11-01')
  
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
         caption = "Consulta de cerca: ('parlem') AND ('hablamos' OR 'hablemos')\nDades: Twitter. Gràfic: @joethebrew.") +
    # geom_text(aes(label = value),
    #           nudge_y = 1000)
    scale_x_date(breaks = date_breaks,
                 labels = date_labels) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
}



day_plot <- function(tl,
                     min_date = '2017-11-01',
                     max_date = '2017-11-30'){
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
    filter(date < '2019-11-01')
  
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

pd <- tl %>%
  group_by(date) %>%
  tally %>%
  filter(date >= '2017-08-01')
ggplot(data = pd,
       aes(x = date,
           y = n)) +
  geom_line() +
  ggthemes::theme_fivethirtyeight() +
  labs(title = 'Tuits con las palabras\n"parlem" i "hablamos"',
       subtitle = '(Sin contar retuits)')
