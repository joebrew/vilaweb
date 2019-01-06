
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
dates <- seq(as.Date('2017-09-15'), as.Date('2017-12-24'),by = 1)
for(i in 1:length(newspapers)){
  for(j in 1:length(dates)){
    this_newspaper <- newspapers[i]
    this_date <- dates[j]
    formatted_date <- format(this_date, '%Y/%m/%d')
    this_path <- 
      paste0("http://img.kiosko.net/",
             formatted_date,
             "/es/", 
             this_newspaper,
             ".750.jpg")
    message(this_newspaper, ' - ', this_date)
    this_file <- paste0('newspaper_headlines/',
                        this_date,
                        '_',
                        this_newspaper,
                        '.jpg')
    if(!file.exists(this_file)){
      message('...Downloading')
      download.file(url = this_path,
                    destfile =
                      this_file)
      Sys.sleep(1)
    } else {
      message('...Skipping')
    }
  }
}

add_month <- function(x){
  d <- as.Date(x, format = '%Y%m%d00')
  month(d) <- month(d) + 1
  format(d, '%Y%m%d00')
}

# Get catalan page vies
if(!'data_ca.RData' %in% dir()){
  people <- c("Carles Puigdemont",
              "Pedro Sánchez (politician)",
              "Pablo Casado",
              "Albert Rivera",
              "Oriol Junqueras",
              "Jordi Turull",
              "Jordi Cuixart",
              "Jordi Sànchez i Picanyol",
              "Josep Rull",
              "Dolors Bassa",
              "Raül Romeva",
              "Carme Forcadell",
              "Joaquim Forn",
              "Marta Rovira",
              "Antoni Comín",
              "Meritxell Serret",
              "Lluís Puig",
              "Anna Gabriel i Sabaté",
              "Clara Ponsatí",
              "Valtònyc",
              "Inés Arrimadas",
              "Miquel Iceta",
              "Xavier García Albiol",
              "Josep Borrell")
  out_list <- list()
  for(i in 1:length(people)){
    start_date <- "2017010100"
    person <- people[i]
    new_person <- person
    if(person == 'Pedro Sánchez (politician)'){new_person <- 'Pedro Sánchez Pérez-Castejón'}
    if(person == 'Carles Puigdemont'){new_person <- 'Carles Puigdemont i Casamajó'}
    if(person == 'Pablo Casado'){new_person <- 'Pablo Casado Blanco'}
    if(person == 'Albert Rivera'){new_person <- 'Albert Rivera Díaz'}
    if(person == 'Oriol Junqueras'){new_person <- 'Oriol Junqueras i Vies'}
    if(person == 'Jordi Turull'){new_person <- 'Jordi Turull i Negre'}
    if(person == 'Jordi Cuixart'){new_person <- 'Jordi Cuixart i Navarro'}
    # if(person == 'Jordi Sànchez i Picanyol'){new_person <- ''}
    if(person == 'Josep Rull'){new_person <- 'Josep Rull i Andreu'}
    if(person == 'Dolors Bassa'){new_person <- 'Dolors Bassa i Coll'}
    if(person == 'Raül Romeva'){new_person <- 'Raül Romeva i Rueda'}
    if(person == 'Carme Forcadell'){new_person <- 'Carme Forcadell i Lluís'}
    if(person == 'Joaquim Forn'){new_person <- 'Joaquim Forn i Chiariello'}
    if(person == 'Marta Rovira'){new_person <- 'Marta Rovira i Vergés'}
    if(person == 'Antoni Comín'){new_person <- 'Antoni Comín i Oliveres'}
    if(person == 'Meritxell Serret'){new_person <- 'Meritxell Serret i Aleu'}
    if(person == 'Lluís Puig'){new_person <- 'Lluís Puig i Gordi'}
    if(person == 'Anna Gabriel i Sabaté'){new_person <- 'Anna Gabriel i Sabaté'}
    if(person == 'Clara Ponsatí'){new_person <- 'Clara Ponsatí i Obiols'}
    if(person == 'Valtònyc'){new_person <- 'Valtònyc'}
    if(person == 'Inés Arrimadas'){new_person <- 'Inés Arrimadas García'}
    if(person == 'Miquel Iceta'){new_person <- 'Miquel Iceta i Llorens'}
    # if(person == 'Xavier García Albiol'){new_person <- ''}
    if(person == 'Josep Borrell'){new_person <- 'Josep Borrell i Fontelles'}
    message(person, '---------')
    res <- try(
      pv <- 
        article_pageviews(project = "ca.wikipedia",
                          article = new_person, 
                          platform = "all",
                          user_type = "all", 
                          start = start_date, 
                          end = "2018121500", 
                          reformat = TRUE)
    )
    while(class(res) == 'try-error'){
      Sys.sleep(1)
      start_date <- add_month(start_date)
      message('---New date: ', start_date)
      pv <- 
        article_pageviews(project = "ca.wikipedia",
                          article = new_person, 
                          platform = "all",
                          user_type = "all", 
                          start = start_date, 
                          end = "2018121500", 
                          reformat = TRUE)
    }
    pv$person <- person
    out_list[[i]] <- pv
  }
  pv_ca <- bind_rows(out_list)
  save(pv_ca, file = 'data_ca.RData')
} else {
  load('data_ca.RData')
}
pv_ca$date <- as.Date(pv_ca$date)

if(!'data.RData' %in% dir()){
  people <- c("Carles Puigdemont",
              "Pedro Sánchez (politician)",
              "Pablo Casado",
              "Albert Rivera",
              "Oriol Junqueras",
              "Jordi Turull",
              "Jordi Cuixart",
              "Jordi Sànchez i Picanyol",
              "Josep Rull",
              "Dolors Bassa",
              "Raül Romeva",
              "Carme Forcadell",
              "Joaquim Forn",
              "Marta Rovira",
              "Antoni Comín",
              "Meritxell Serret",
              "Lluís Puig",
              "Anna Gabriel i Sabaté",
              "Clara Ponsatí",
              "Valtònyc",
              "Inés Arrimadas",
              "Miquel Iceta",
              "Xavier García Albiol",
              "Josep Borrell")
  
  out_list <- list()
  for(i in 1:length(people)){
    start_date <- "2017010100"
    person <- people[i]
    
    new_person <- person
    if(person == 'Clara Ponsatí'){new_person <- 'Clara Ponsatí i Obiols'}
    
    message(person, '---------')
    res <- try(
      pv <- 
        article_pageviews(project = "en.wikipedia",
                          article = person, 
                          platform = "all",
                          user_type = "all", 
                          start = start_date, 
                          end = "2018121500", 
                          reformat = TRUE)
    )
    while(class(res) == 'try-error'){
      Sys.sleep(1)
      start_date <- add_month(start_date)
      message('---New date: ', start_date)
      pv <- 
        article_pageviews(project = "en.wikipedia",
                          article = person, 
                          platform = "all",
                          user_type = "all", 
                          start = start_date, 
                          end = "2018121500", 
                          reformat = TRUE)
    }
    pv$person <- person
    out_list[[i]] <- pv
  }
  pv <- bind_rows(out_list)
  save(pv, file = 'data.RData')
} else {
  load('data.RData')
}
pv$date <- as.Date(pv$date)

# Get spanish too
if(!'data_es.RData' %in% dir()){
  people <- c("Carles Puigdemont",
              "Pedro Sánchez",
              "Pablo Casado",
              "Albert Rivera",
              "Oriol Junqueras",
              "Jordi Turull",
              "Jordi Cuixart",
              "Jordi Sànchez i Picanyol",
              "Josep Rull",
              "Dolors Bassa",
              "Raül Romeva",
              "Carme Forcadell",
              "Joaquim Forn",
              "Marta Rovira",
              "Antoni Comín",
              "Meritxell Serret",
              "Lluís Puig",
              "Anna Gabriel",
              "Clara Ponsatí",
              "Valtònyc",
              "Inés Arrimadas",
              "Miquel Iceta",
              "Xavier García Albiol",
              "Josep Borrell")
  out_list <- list()
  for(i in 18:length(people)){
    start_date <- "2017010100"
    person <- people[i]
    message(person, '---------')
    res <- try(
      pv <- 
        article_pageviews(project = "es.wikipedia",
                          article = person, 
                          platform = "all",
                          user_type = "all", 
                          start = start_date, 
                          end = "2018121500", 
                          reformat = TRUE)
    )
    while(class(res) == 'try-error'){
      Sys.sleep(1)
      start_date <- add_month(start_date)
      message('---New date: ', start_date)
      pv <- 
        article_pageviews(project = "es.wikipedia",
                          article = person, 
                          platform = "all",
                          user_type = "all", 
                          start = start_date, 
                          end = "2018121500", 
                          reformat = TRUE)
    }
    pv$person <- person
    out_list[[i]] <- pv
  }
  pv_es <- bind_rows(out_list)
  # Conform names
  pv_es <- pv_es %>%
    mutate(person = ifelse(grepl('Gabriel', person), 'Anna Gabriel i Sabaté',
                           ifelse(grepl('Pedro Sánchez', person),
                                  "Pedro Sánchez (politician)", person)))
  
  save(pv_es, file = 'data_es.RData')
} else {
  load('data_es.RData')
}
pv_es$date <- as.Date(pv_es$date)
pv$date <- as.Date(pv$date)
# Combine
pv <- pv %>%
  mutate(language = 'English') %>% 
  bind_rows(pv_es %>%
              mutate(language = 'Español')) %>%
  bind_rows(pv_ca %>%
              mutate(language = 'Catalan'))

# Clean up a bit
pv <- pv %>%
  mutate(person = gsub(' (politician)', '', person, fixed = TRUE),
         person = gsub(' i Picanyol', '', person, fixed = TRUE),
         person = gsub(' i Sabaté', '', person, fixed = TRUE))

cleaner <- data_frame(
  person = c('Albert Rivera',
             'Anna Gabriel',
             'Antoni Comín',
             'Carles Puigdemont',
             'Carme Forcadell',
             'Clara Ponsatí',
             'Dolors Bassa',
             'Inés Arrimadas',
             'Joaquim Forn',
             'Jordi Cuixart',
             'Jordi Sànchez',
             'Jordi Turull',
             'Josep Borrell',
             'Josep Rull',
             'Lluís Puig',
             'Marta Rovira',
             'Meritxell Serret',
             'Miquel Iceta',
             'Oriol Junqueras',
             'Pablo Casado',
             'Pedro Sánchez',
             'Raül Romeva',
             'Valtònyc',
             'Xavier García Albiol'),
  indepe = c(
    FALSE  ,#'Albert Rivera'
    TRUE,#'Anna Gabriel'
    TRUE,#'Antoni Comín'
    TRUE,#'Carles Puigdemont'
    TRUE,#'Carme Forcadell'
    TRUE,#'Clara Ponsatí'
    TRUE,#'Dolors Bassa'
    FALSE,#'Inés Arrimadas'
    TRUE,#'Joaquim Forn'
    TRUE,#'Jordi Cuixart'
    TRUE,#'Jordi Sànchez'
    TRUE,#'Jordi Turull'
    FALSE,#'Josep Borrell'
    TRUE,#'Josep Rull'
    TRUE ,#'Lluís Puig'
    TRUE,#'Marta Rovira'
    TRUE,#'Meritxell Serret'
    FALSE,#'Miquel Iceta'
    TRUE,#'Oriol Junqueras'
    FALSE,#'Pablo Casado'
    FALSE,#'Pedro Sánchez'
    TRUE,#'Raül Romeva'
    TRUE,#'Valtònyc'
    FALSE#'Xavier García Albiol'
  ),
  exile = 
    c(
      NA,#'Albert Rivera'
      'Exile',#'Anna Gabriel'
      'Exile' ,#'Antoni Comín'
      'Exile',#'Carles Puigdemont'
      'Prison'  ,#'Carme Forcadell'
      'Exile',#'Clara Ponsatí'
      'Prison' ,#'Dolors Bassa'
      NA,#'Inés Arrimadas'
      'Prison',#'Joaquim Forn'
      'Prison',#'Jordi Cuixart'
      'Prison',#'Jordi Sànchez'
      'Prison',#'Jordi Turull'
      NA,#'Josep Borrell'
      'Prison',#'Josep Rull'
      'Exile',#'Lluís Puig'
      'Exile',#'Marta Rovira'
      'Exile',#'Meritxell Serret'
      NA,#'Miquel Iceta'
      'Prison',#'Oriol Junqueras'
      NA,#'Pablo Casado'
      NA,#'Pedro Sánchez'
      'Prison',#'Raül Romeva'
      'Exile' ,#'Valtònyc'
      NA#'Xavier García Albiol'
    )
)
pv <- left_join(pv, cleaner)

# Get wikipedia data for charlottesville organizer
if('pv_char.RData' %in% dir()){
  load('pv_char.RData')
} else {
  
start_date <- "2017010100"
char_people <- c('Jason Kessler', 'Richard B. Spencer')
out_list <- list()
for(i in 1:length(char_people)){
  person <- char_people[i]
  message(person, '---------')
  res <- try(
    pv_char <- 
      article_pageviews(project = "en.wikipedia",
                        article = person, 
                        platform = "all",
                        user_type = "all", 
                        start = start_date, 
                        end = "2018121500", 
                        reformat = TRUE)
  )
  while(class(res) == 'try-error'){
    Sys.sleep(1)
    start_date <- add_month(start_date)
    message('---New date: ', start_date)
    pv_char <- 
      article_pageviews(project = "en.wikipedia",
                        article = person, 
                        platform = "all",
                        user_type = "all", 
                        start = start_date, 
                        end = "2018121500", 
                        reformat = TRUE)
  }
  pv_char$person <- person
  out_list[[i]] <- pv_char
}


pv_char <- bind_rows(out_list)
pv_char$date <- as.Date(pv_char$date)
save(pv_char,
     file = 'pv_char.RData')
}

make_wiki_plot <- function(language = 'en',
                           since = '2018-01-01'){
  plot_data <- 
    pv %>%
    filter(date >= since) %>%
    group_by(person, language) %>%
    summarise(views = sum(views))
  
  if(language == 'ca'){
    x <- ''
    y <- 'Visites'
    title <- 'Visites de pàgines Wikipedia, 2018'
  } else {
    x <- ''
    y <- 'Visits'
    title <- 'Wikipedia page visits, 2018'
  }
  cols <- databrew::make_colors(10)[c(3,5,7)]
  ggplot(data = plot_data,
         aes(x = person,
             y = views,
             fill = language)) +
    geom_bar(stat = 'identity') +
    theme_vilaweb() +
    theme(axis.text.x = element_text(angle = 90,
                                     vjust = 0.5,
                                     hjust = 1)) +
    scale_fill_manual(name = '',
                      values = cols) +
    labs(x = x,
         y = y,
         title = title) +
    facet_wrap(~language, ncol = 3)
}
# make_wiki_plot()
# make_wiki_plot('ca')

# # Get most recent tweets from our people of interest
if(file.exists('tl.RData')){
  load('tl.RData')
} else {
  # Connect to the db
  pg = DBI::dbDriver("PostgreSQL")
  con = DBI::dbConnect(pg, dbname="twitter")
  tl <- RPostgreSQL::dbGetQuery(
    con,
    paste0("SELECT * FROM twitter")
  )
  save(tl, file = 'tl.RData')  
  dbDisconnect(con)
}

# GET TWEETS FROM POLITICIANS, SEP 20-22
if('people_tweets.RData' %in% dir()){
  load('people_tweets.RData')
  load('people_tweets_long.RData')
  load('newspaper_tweets.RData')
} else {
  
  # Keep only the 2 week period following 20 sep
  dates <- seq(as.Date('2017-09-20'),(as.Date('2017-09-20')+13), 1)
  long_dates <- seq(as.Date('2017-09-10'),(as.Date('2017-10-31')), 1)

  df <- tl %>% filter(date %in% dates)
  people <- 
    tolower(c('Santi_ABASCAL',
              'Albert_Rivera',
              'InesArrimadas',
              'sanchezcastejon',
              'pablocasado_',
              'ALevySoler',
              'miqueliceta',
              'Pablo_Iglesias_',
              'albiol_xg',
              'carrizosacarlos',
              'ciudadanoscs',
              'ciutadanscs',
              'eva_granados',
              'j_zaragoza_',
              'marianorajoy',
              'meritxell_batet',
              'miqueliceta',
              'pablocasado_',
              'ppcatalunya',
              'pnique',
              'ppopular',
              'psoe',
              'santi_abascal',
              'sanchezcastejon',
              'socialistes_cat',
              'societatcc',
              'vox_es'))
  newspapers <- c('cronicaglobal',
                  'elespanolcom', 
                  'elmundoespana',
                  'elconfidencial',
                  'okdiario',
                  'elpais_espana',
                  'lavanguardia',
                  'elperiodico')
  
  df <- df %>%
    filter(username %in% c(newspapers, people)) %>%
    mutate(is_newspaper = username %in% newspapers,
           is_person = username %in% people)
  
  
  # Go through each persons tweet on each day
  people_tweets <- df %>%
    filter(is_person) %>%
    filter(date <= '2017-09-22') %>%
    arrange(username, date)
  
  # Save df
  people_tweets_long <- tl %>%
    filter(username %in% c(newspapers, people)) %>%
    mutate(is_newspaper = username %in% newspapers,
           is_person = username %in% people) %>%
    filter(is_person) %>%
      filter(date %in% long_dates)
  save(people_tweets_long,
       file = 'people_tweets_long.RData')
  
  newspaper_tweets <- tl %>%
    filter(username %in% c(newspapers, people)) %>%
    mutate(is_newspaper = username %in% newspapers,
           is_person = username %in% people) %>%
    filter(is_newspaper) %>%
    filter(date %in% long_dates)
  save(newspaper_tweets,
       file = 'newspaper_tweets.RData')
  
  # Save for later use
  save(people_tweets,
       file = 'people_tweets.RData')
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



if(!dir.exists('screenshots')){
  dir.create('screenshots')
}
person_dates <- the_dates <- the_times <- the_timezones <- the_ids <- rep(NA, nrow(people_tweets))
done <- TRUE
for(i in 1:nrow(people_tweets)){
  message(i)
  this_person <- people_tweets$username[i]
  this_tweet <- people_tweets$tweet[i]
  this_url <- people_tweets$link[i]
  this_date <- people_tweets$date[i]
  this_id <- people_tweets$id[i]
  person_dates[i] <- paste0(this_person, ' ', this_date)
  the_dates[i] <- this_date
  the_times[i] <- as.character(people_tweets$time[i])
  the_timezones[i] <- as.character(people_tweets$timezone[i])
  the_ids[i] <- this_id
  # webshot(this_url, paste0(i, '.png'), 
  #         cliprect = 'viewport')
  file_name <- 
    paste0("screenshots/",
           add_zero(i, 5),
           ".png")
  if(!file.exists(file_name)){
    system(paste0(
      "screenshot-tweet ",
      this_url,
      " ",
      file_name
    ))
  } else {
    message('Skipping ', i, ' because file already exists.')
  }
}
files <- dir('screenshots/')

picture_df <- 
  data_frame(file = files,
             person_dates,
             the_dates,
             the_times,
             the_timezones,
             id = the_ids)
picture_df <- picture_df %>%
  arrange(person_dates, the_times) %>%
  mutate(the_dates = as.character(as.Date(the_dates, origin = '1970-01-01')))


# GET TWEETS FROM POLITICIANS, OCT 16-18
if('people_tweets2.RData' %in% dir()){
  load('people_tweets2.RData')
} else {
  load('tl.RData')
  
  # Keep only the 2 week period following 20 sep
  dates <- seq(as.Date('2017-10-16'),(as.Date('2017-10-18')), 1)
  
  df <- tl %>% filter(date %in% dates)
  people <- 
    tolower(c('Santi_ABASCAL',
              'Albert_Rivera',
              'InesArrimadas',
              'sanchezcastejon',
              'pablocasado_',
              'ALevySoler',
              'miqueliceta',
              'Pablo_Iglesias_',
              'albiol_xg',
              'carrizosacarlos',
              'ciudadanoscs',
              'ciutadanscs',
              'eva_granados',
              'j_zaragoza_',
              'marianorajoy',
              'meritxell_batet',
              'miqueliceta',
              'pablocasado_',
              'ppcatalunya',
              'pnique',
              'ppopular',
              'psoe',
              'santi_abascal',
              'sanchezcastejon',
              'socialistes_cat',
              'societatcc',
              'vox_es'))
  newspapers <- c('cronicaglobal',
                  'elespanolcom', 
                  'elmundoespana',
                  'elconfidencial',
                  'okdiario',
                  'elpais_espana',
                  'lavanguardia',
                  'elperiodico')
  df <- df %>%
    filter(username %in% c(newspapers, people)) %>%
    mutate(is_newspaper = username %in% newspapers,
           is_person = username %in% people)
  
  
  # Go through each persons tweet on each day
  people_tweets2 <- df %>%
    filter(is_person) %>%
    filter(date %in% dates) %>%
    arrange(username, date)
  
  # Save for later use
  save(people_tweets2,
       file = 'people_tweets2.RData')
}

if(!dir.exists('screenshots2')){
  dir.create('screenshots2')
}
person_dates <- the_dates <- the_times <- the_timezones <- rep(NA, nrow(people_tweets2))
done <- TRUE
for(i in 1:nrow(people_tweets2)){
  message(i)
  this_person <- people_tweets2$username[i]
  this_tweet <- people_tweets2$tweet[i]
  this_url <- people_tweets2$link[i]
  this_date <- people_tweets2$date[i]
  person_dates[i] <- paste0(this_person, ' ', this_date)
  the_dates[i] <- this_date
  the_times[i] <- as.character(people_tweets2$time[i])
  the_timezones[i] <- as.character(people_tweets2$timezone[i])
  # webshot(this_url, paste0(i, '.png'), 
  #         cliprect = 'viewport')
  file_name <- 
    paste0("screenshots2/",
           add_zero(i, 5),
           ".png")
  if(!file.exists(file_name)){
    system(paste0(
      "screenshot-tweet ",
      this_url,
      " ",
      file_name
    ))
  } else {
    message('Skipping ', i, ' because file already exists.')
  }
}
files <- dir('screenshots2/')

picture_df2 <- 
  data_frame(file = files,
             person_dates,
             the_dates,
             the_times,
             the_timezones)
picture_df2 <- picture_df2 %>%
  arrange(person_dates, the_times) %>%
  mutate(the_dates = as.character(as.Date(the_dates, origin = '1970-01-01')))

if(!'usa.RData' %in% dir()){
  load('tl.RData')
  keep <- congress$user_name
  usa <- tl %>%
    filter(username %in% keep) %>%
    filter(date >= '2017-08-01',
           date <= '2017-08-30')
  save(usa, file = 'usa.RData')
} else {
  load('usa.RData')
}


if(!'news.RData' %in% dir()){
  load('tl.RData')
  keep <- tolower(news$user_name)
  usa_news <- tl %>%
    filter(username %in% keep) %>%
    filter(date >= '2017-08-01',
           date <= '2017-08-30')
  save(usa_news, file = 'news.RData')
} else {
  load('news.RData')
}

# Get google search trends
library(gtrendsR)
if('gt.RData' %in% dir()){
  load('gt.RData')
} else {
  g_df <-
    data_frame(people = c("Jordi Cuixart",
                          'Jordi Sànchez',
                          'Jason Kessler', 
                          'Richard Spencer'),
               geo = c('ES', 'ES', 'US', 'US'))
  out_list <- list()
  for(i in 1:nrow(g_df)){
    person <- g_df$people[i]
    geo <- g_df$geo[i]
    g <- gtrends(person,geo=geo)
    interest <- g$interest_over_time
    interest$date <- as.Date(interest$date)
    
    out_list[[i]] <- interest
  }
  for(i in 1:length(out_list)){
    out <- out_list[[i]]
    out$hits[out$hits == '<1'] <- '0.5'
    out$hits <- as.numeric(out$hits)
    out_list[[i]] <- out
  }
  gt <- bind_rows(out_list)
  save(gt, file = 'gt.RData')
}

# Prepare turkish coup data
if('turkey.RData' %in% dir()){
  load('turkey.RData')
} else {
  turkey <- tl %>%
    filter(username %in% newspapers |
             username %in% tolower(news$user_name)) %>%
    filter(date >= '2016-07-01',
           date <= '2016-07-31') %>%
    mutate(is_spanish = username %in% newspapers,
           is_international = username %in% tolower(news$user_name))
  save(turkey,
       file = 'turkey.RData')
}


detect_violence <- function(x){
  grepl('violen|violèn',x)
}

newspapers <- c('cronicaglobal',
                'elespanolcom', 
                'elmundoespana',
                'elconfidencial',
                'okdiario',
                'elpais_espana',
                'lavanguardia',
                'elperiodico')
