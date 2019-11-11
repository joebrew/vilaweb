# Libraries
library(vilaweb)
library(tidyverse)
library(lubridate)

# Define list of people
people <- c('j_zaragoza_', 'inesarrimadas', 'albert_rivera', 'santi_abascal', 'pablocasado_',
            'sanchezcastejon', 'miqueliceta', 'pablo_iglesias_', 'pnique', 'ortega_smith', 'cayetanaat',
            'carrizosacarlos', 'lroldansu', 'eva_granados', 'jessicaalbiach', 'carlesral', 'nataliadipp', 'alejandrotgn',
            'rogertorrent', 'lauraborras', 'josepcosta')

## Ensure they are updated in the database
# update_database(people = people)

if('tweets.RData' %in% dir()){
  load('tweets.RData')
} else {
  tl <- get_twitter_data_from_database(people = people)
  save(tl, file = 'tweets.RData')
}

# Identified torra tweets
tl$is_torra <- grepl('quimtorraipla', tolower(tl$tweet)) |
                    grepl('Torra', tl$tweet) 
tl$is_puigdemont <- grepl('puigdemont|krls', tolower(tl$tweet))
tl$torra_puigdemont <- tl$is_torra | tl$is_puigdemont
tl$is_violence <- grepl('violen', tolower(tl$tweet))

# Get by person and year
tl$year <- as.numeric(format(tl$date, '%Y'))
tl$timey <- tl$date >= '2019-10-01'
pd <- tl %>%
  filter(timey) %>%
  group_by(username) %>%
  summarise(torra = length(which(is_torra)),
            puigdemont = length(which(is_puigdemont)),
            torra_puig = length(which(torra_puigdemont)),
            tweets = n(),
            violence = length(which(is_violence))) %>%
  ungroup %>%
  mutate(p = violence / tweets * 100) %>%
  arrange(desc(violence))

if('quim.RData' %in% dir()){
  load('quim.RData')
} else {
  quim <- get_twitter_data_from_database(people = 'quimtorraipla')
  save(quim, file = 'quim.RData')
}

# Mencions de Zaragoza o Rivera
pd <- quim %>%
  mutate(is_rivera = grepl('albert_rivera', tolower(tweet)) |
           grepl('Rivera', tweet)) %>%
  mutate(is_zaragoza = grepl('j_zaragoza_', tolower(tweet)) |
           grepl('Zaragoza', tweet)) %>%
  mutate(year = as.numeric(format(date, '%Y'))) %>%
  group_by(year) %>%
  summarise(rivera = length(which(is_rivera)),
            zaragoza = length(which(is_zaragoza)),
            tweets = n()) %>%
  ungroup %>%
  mutate(p_rivera = rivera / tweets * 100,
         p_zaragoza = zaragoza / tweets * 100)
