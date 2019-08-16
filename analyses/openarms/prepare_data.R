
# Libraries
library(vilaweb)
library(tidyverse)
library(DBI)
library(stringi)

# Define the people to be examined (Spanish and Catalan socialists)
url <- 'https://docs.google.com/spreadsheets/d/12NwO0huV1Fo8MES5ZHBLZeEbxRwwP36wOleS1zkpPgc/edit?usp=sharing'
if(!'socialists.RData' %in% dir()){
  socialists <- gsheet::gsheet2tbl(url = url)
  save(socialists,
       file = 'socialists.RData')
} else {
  load('socialists.RData')
}
people <- tolower(socialists$username)
people <- people[!is.na(people)]

# # Update the database
# update_database(people = people)

if(file.exists('tl.RData')){
  load('tl.RData')
} else {
  # Connect to the db
  pg = DBI::dbDriver("PostgreSQL")
  con = DBI::dbConnect(pg, dbname="twitter")
  query = paste0("SELECT * FROM twitter where username = ANY ('{",
                 paste0('"', people, '"', collapse = ','),
                 "}')")
  tl <- RPostgreSQL::dbGetQuery(
    con,
    query
  )
  save(tl, file = 'tl.RData')  
  dbDisconnect(con)
}

tl <- tl %>% filter(date >= '2018-01-01',
                    date <= '2019-08-14')

tl$openarms <- grepl('open arms|openarms', tolower(tl$tweet))
tl$year <- as.numeric(format(tl$date, '%Y'))
tl <- tl %>% filter(!username %in% c('socialistes_cat', 'psoe'))

make_chart <- function(ca = FALSE){
  
  left <- expand.grid(username = sort(unique(tl$username)),
                      year = 2018:2019)
  right <- tl %>%
    filter(openarms) %>%
    group_by(username, year) %>%
    tally
  joined <- left_join(left, right) %>%
    mutate(n = ifelse(is.na(n), 0, n)) %>%
    left_join(socialists %>% 
                mutate(username = tolower(username)) %>%
                dplyr::select(username, full_name)) %>%
    mutate(year = as.character(year))
  label_df <- joined %>%
    group_by(year) %>%
    summarise(n = sum(n))
  zeros <- joined %>%
    group_by(full_name) %>%
    summarise(n = sum(n)) %>%
    filter(n == 0) %>%
    .$full_name
  joined <- joined %>%
    filter(!full_name %in% zeros)
  
  if(ca){
    the_labs <- labs(x = 'Any',
                     y = 'Piulets',
                     title = "Piulets esmentant 'Open Arms', 2018 vs 2019*",
                     subtitle = 'Socialistes espanyols i catalans. *Fins al 14 d\'agost de 2019',
                     caption = paste0('Els polítics següents no van esmentar Open Arms ni en 2018, ni en 2019:\n',
                                      paste0(stri_wrap(paste0(zeros, collapse = ', '),
                                                       floor(2.01 * getOption("width"))), collapse = '\n')))
  } else {
    the_labs <- labs(x = 'Year',
                     y = 'Tweets',
                     title = "Tweets mentioning 'Open Arms', 2018 vs 2019*",
                     subtitle = 'Spanish and Catalan Socialists. *Through August 14th, 2019',
                     caption = paste0('The following politicians did not mention Open Arms in either 2018 or 2019:\n',
                                      paste0(stri_wrap(paste0(zeros, collapse = ', '),
                                                       floor(2.01 * getOption("width"))), collapse = '\n')))
  }
  
  ggplot(data = joined) +
    geom_bar(stat = 'identity',
             position = position_stack(),
             aes(x = year,
                 y = n,
                 fill = full_name)) +
    geom_text(data = label_df,aes(x = year,
                                  y = n,
                                  label = n),
              nudge_y = 5,
              size = 5,
              alpha = 0.6) +
    theme_vilaweb() +
    scale_fill_manual(name = '',
                      values = colorRampPalette(RColorBrewer::brewer.pal(n = 8, name = 'Set1'))(length(unique(joined$username)))) +
    the_labs +
    theme(legend.text = element_text(size = 6.5)) +
    theme(plot.caption = element_text(size = 6))
}


make_other_chart <- function(ca = FALSE){
  
  left <- expand.grid(username = sort(unique(tl$username)),
                      year = 2018:2019)
  right <- tl %>%
    filter(openarms) %>%
    group_by(username, year) %>%
    tally
  joined <- left_join(left, right) %>%
    mutate(n = ifelse(is.na(n), 0, n)) %>%
    left_join(socialists %>% 
                mutate(username = tolower(username)) %>%
                dplyr::select(username, full_name)) %>%
    mutate(year = as.character(year))
  label_df <- joined %>%
    group_by(year) %>%
    summarise(n = sum(n))
  zeros <- joined %>%
    group_by(full_name) %>%
    summarise(n = sum(n)) %>%
    filter(n == 0) %>%
    .$full_name
  joined <- joined %>%
    filter(!full_name %in% zeros)
  
  if(ca){
    the_labs <- labs(x = 'Any',
                     y = 'Piulets',
                     title = "Piulets esmentant 'Open Arms', 2018 vs 2019*",
                     subtitle = 'Socialistes espanyols i catalans. *Fins al 14 d\'agost de 2019',
                     caption = paste0('Els polítics següents no van esmentar Open Arms ni en 2018, ni en 2019:\n',
                                      paste0(stri_wrap(paste0(zeros, collapse = ', '),
                                                       floor(2.01 * getOption("width"))), collapse = '\n')))
  } else {
    the_labs <- labs(x = 'Year',
                     y = 'Tweets',
                     title = "Tweets mentioning 'Open Arms', 2018 vs 2019*",
                     subtitle = 'Spanish and Catalan Socialists. *Through August 14th, 2019',
                     caption = paste0('The following politicians did not mention Open Arms in either 2018 or 2019:\n',
                                      paste0(stri_wrap(paste0(zeros, collapse = ', '),
                                                       floor(2.01 * getOption("width"))), collapse = '\n')))
  }
  
  ggplot(data = joined,
         aes(x = year,
             y = n,
             group = username)) +
    geom_point() +
    geom_line() +
    geom_text(aes(label = n),
              nudge_y = 5) +
    facet_wrap(~full_name) +
    the_labs +
    scale_y_continuous(name = '', breaks = c(0, 10)) +
    theme_vilaweb()  +
    theme(plot.caption = element_text(size = 6))
}