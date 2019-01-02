
# Libraries
library(vilaweb)
library(tidyverse)
library(databrew)
library(pageviews)
library(lubridate)

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
make_wiki_plot()
make_wiki_plot('ca')
