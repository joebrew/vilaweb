library(vilaweb)
library(tidyverse)

if('twitter_data.RData' %in% dir()){
  load('twitter_data.RData')
} else {
  scrape(search_string = 'hispanofobia',
                    file_path = 'hispano.csv')
  scrape(search_string = 'catalanofobia',
                    file_path = 'catalano.csv')
  hispano <- read_csv('hispano.csv') %>% mutate(key = 'Hispanofòbia') %>% filter(!duplicated(id))
  catalano <- read_csv('catalano.csv') %>% mutate(key = 'Catalanofòbia') %>% filter(!duplicated(id))
  fobia <- bind_rows(hispano, catalano)
  save(fobia, file = 'twitter_data.RData')
}
