
library(dplyr)
library(readr)
library(RPostgreSQL)

pg = dbDriver("PostgreSQL")
con = dbConnect(pg, dbname="twitter")

df <- dbGetQuery(con, "SELECT * from twitter where username = 'miqueliceta'")

dbDisconnect(con)

x <- df %>%
  filter(date >= '2017-10-07',
         date <= '2018-12-31') %>%
  filter(grepl('viole', tolower(tweet)))
