library(tidyverse)
library(vilaweb)

# Get people
people <- c('sanchezcastejon', 'miqueliceta', 'albert_rivera', 'inesarrimadas',
            'pablocasado_', 'cayetanaat', 'igarrigavaz', 'santi_abascal',
            'pablo_iglesias', 'jaumeasens')

# Get their tweets
tl <- get_twitter_data_from_database(people)

# Search for the word violence
searcher <- function(x){
  grepl('cataluñ|catalun|català|catalan|catalán|catalo', tolower(x))
}
tl$detected <- searcher(tl$tweet)

# Get results
pd <- tl %>%
  filter(date >= '2019-11-03',
         date <= '2019-11-17') %>%
  filter(date != '2019-11-10') %>%
  mutate(timey = ifelse(date > '2019-11-10', 'Després',
                ifelse(date < '2019-11-10', 'Abans', NA))) %>%
  group_by(timey, username) %>%
  summarise(numerator = length(which(detected)),
            denominator = n()) %>%
  ungroup %>%
  mutate(p = numerator / denominator * 100) %>%
  arrange(username, timey)
pd
