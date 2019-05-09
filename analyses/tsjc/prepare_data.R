
library(pageviews)
library(lubridate)
library(vilaweb)
library(rtweet)
library(tidyverse)
library(databrew)
library(translateR)
library(sentimentr) # https://github.com/trinker/sentimentr
require(RPostgreSQL)
require(readr)  
require(DBI)
library(webshot)

# Get newspaper headlines

if(!dir.exists('newspaper_headlines')){
  dir.create('newspaper_headlines')
}
# Get newspapers
newspapers <- c('elpais', 'abc',
                'elmundo',
                'larazon',
                'lavanguardia',
                'elperiodico')
# dates <- seq(as.Date('2017-09-15'), as.Date('2017-12-24'),by = 1)
# for(i in 1:length(newspapers)){
#   for(j in 1:length(dates)){
#     this_newspaper <- newspapers[i]
#     this_date <- dates[j]
#     formatted_date <- format(this_date, '%Y/%m/%d')
#     this_path <- 
#       paste0("http://img.kiosko.net/",
#              formatted_date,
#              "/es/", 
#              this_newspaper,
#              ".750.jpg")
#     message(this_newspaper, ' - ', this_date)
#     this_file <- paste0('newspaper_headlines/',
#                         this_date,
#                         '_',
#                         this_newspaper,
#                         '.jpg')
#     if(!file.exists(this_file)){
#       message('...Downloading')
#       download.file(url = this_path,
#                     destfile =
#                       this_file)
#       Sys.sleep(1)
#     } else {
#       message('...Skipping')
#     }
#   }
# }

add_month <- function(x){
  d <- as.Date(x, format = '%Y%m%d00')
  month(d) <- month(d) + 1
  format(d, '%Y%m%d00')
}

#' Prepend zero(s) to a number
#' 
#' Prepend one or more 0's to a number. Useful for alphabetizing facto levels named with numbers.
add_zero <- function(x, n){
  x <- as.character(x)
  adders <- n - nchar(x)
  adders <- ifelse(adders < 0, 0, adders)
  for (i in 1:length(x)){
    if(!is.na(x[i])){
      x[i] <- paste0(
        paste0(rep('0', adders[i]), collapse = ''),
        x[i],
        collapse = '')  
    } 
  }
  return(x)
}

