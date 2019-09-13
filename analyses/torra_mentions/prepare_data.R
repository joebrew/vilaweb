# Libraries
library(vilaweb)
library(tidyverse)
library(lubridate)

# Define list of people
people <- c('j_zaragoza_', 'inesarrimadas', 'albert_rivera', 'santi_abascal', 'pablocasado_')

## Ensure they are updated in the database
# update_database(people = people)

# Get their tweets
# Source functions
functions_dir <- '../../R/'
functions_files <- dir(functions_dir)
for(i in 1:length(functions_files)){
  source(paste0(functions_dir, functions_files[i]))
}

if('tweets.RData' %in% dir()){
  load('tweets.RData')
} else {
  tl <- get_twitter_data_from_database(people = people)
  save(tl, file = 'tweets.RData')
}

# Identified torra tweets
tl$is_torra <- grepl('quimtorraipla', tolower(tl$tweet)) |
                    grepl('Torra', tl$tweet)

# Get by person and year
tl$year <- as.numeric(format(tl$date, '%Y'))
pd <- tl %>%
  group_by(username, year) %>%
  summarise(torra = length(which(is_torra)),
            tweets = n()) %>%
  ungroup %>%
  mutate(p = torra / tweets * 100)

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
