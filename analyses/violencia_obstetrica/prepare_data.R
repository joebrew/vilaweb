# Libraries
library(vilaweb)
library(tidyverse)
library(lubridate)

# Get data on obstetric violence from twitter
if("vo.csv" %in% dir('data'){
  vo <- read_csv('data/vo.csv')
}) else {
  vo <- vilaweb::scrape(search_string = 'obstetric violence',
                        file_path = 'data/vo.csv')
}
if('vo_ex.csv' %in% dir('data')){
  vo_es <- read_csv('data/vo_es.csv')  
} else {
  vo_es <- vilaweb::scrape(search_string = 'violencia obstétrica',
                           file_path = 'data/vo_es.csv')
}



# Google trends
library(gtrendsR)