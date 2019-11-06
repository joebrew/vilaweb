# Libraries
library(vilaweb)
library(tidyverse)
library(lubridate)

# Get twitter data about 384bis
source('../../R/scrape.R')
if(!'twitter_data.csv' %in% dir()){
 scrape(search_string = '384bis',
               file_path = 'twitter_data.csv')
}
df <- read_csv('twitter_data.csv')

df$year <- as.numeric(format(df$date, '%Y'))
df$month <- as.Date(cut(df$date, 'month'))

pd <- df %>%
  group_by(date = month) %>%
  tally
