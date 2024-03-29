
# Libraries
library(vilaweb)
library(tidyverse)
library(databrew)
library(pageviews)
library(lubridate)

# if(!dir.exists('newspaper_headlines')){
#   dir.create('newspaper_headlines')
# }
# # Get newspapers
# newspapers <- c('elpais', 'abc',
#                 'elmundo',
#                 'larazon',
#                 'lavanguardia',
#                 'elperiodico')
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
# 
# add_month <- function(x){
#   d <- as.Date(x, format = '%Y%m%d00')
#   month(d) <- month(d) + 1
#   format(d, '%Y%m%d00')
# }

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
                          end = "2018123100", 
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
                          end = "2018123100", 
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
                          article = new_person, 
                          platform = "all",
                          user_type = "all", 
                          start = start_date, 
                          end = "2018123100", 
                          reformat = TRUE)
    )
    while(class(res) == 'try-error'){
      Sys.sleep(1)
      start_date <- add_month(start_date)
      message('---New date: ', start_date)
      pv <- 
        article_pageviews(project = "en.wikipedia",
                          article = new_person, 
                          platform = "all",
                          user_type = "all", 
                          start = start_date, 
                          end = "2018123100", 
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
  for(i in 1:length(people)){
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
                          end = "2018123100", 
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
                          end = "2018123100", 
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
                           filter_language = c('English', 'Català', 'Español'),
                           since = '2017-01-01',
                           return_table = FALSE){
  plot_data <- 
    pv %>%
    filter(date >= since) %>%
    group_by(person, language) %>%
    summarise(views = sum(views)) %>%
    mutate(language = ifelse(language == 'Catalan',
                             'Català',
                             language)) %>%
    filter(language %in% filter_language)
  if(return_table){
    return(plot_data)
  }
  
  if(language == 'ca'){
    x <- ''
    y <- 'Visites'
    # title <- 'Visites de pàgines Wikipedia, 2018'
    caption <- 'Dades de Wikipedia. Gràfic de Joe Brew @joethebrew. | www.vilaweb.cat'
  } else {
    x <- ''
    y <- 'Visits'
    # title <- 'Wikipedia page visits, 2018'
    caption <- 'Data from Wikipedia. Chart by Joe Brew @joethebrew. | www.vilaweb.cat'
  }
  cols <- databrew::make_colors(10)[c(3,5,7)]
  ggplot(data = plot_data,
         aes(x = person,
             y = views,
             fill = language,
             group = language)) +
    geom_bar(stat = 'identity', position = position_dodge()) +
    theme_vilaweb() +
    theme(axis.text.x = element_text(angle = 90,
                                     vjust = 0.5,
                                     hjust = 1)) +
    scale_fill_manual(name = '',
                      values = cols) +
    labs(x = x,
         y = y,
         caption = caption)
}
# make_wiki_plot()
# make_wiki_plot('ca')

make_wiki_time_plot <- function(people = NULL,
                                language = 'en',
                                since = '2017-01-01',
                                cols = NULL,
                                alpha = 1,
                                size = 1,
                                return_table = FALSE){
  
  if(is.null(people)){
    people <- sort(unique(pv$person))
  }
  plot_data <- 
    pv %>%
    filter(person %in% people) %>%
    filter(date >= since) %>%
    mutate(month = date_truncate(date, 'month')) %>%
    group_by(month, person, language) %>%
    summarise(views = sum(views, na.rm = TRUE)) %>%
    mutate(language = ifelse(language == 'Catalan',
                             'Català',
                             language))
  if(return_table){
    return(plot_data)
  }
  
  if(language == 'ca'){
    x <- ''
    y <- 'Visites'
    # title <- 'Visites de pàgines Wikipedia, 2018'
    date_breaks <- gsub('\n', ' ', make_catalan_date(sort(unique(plot_data$month))))
    caption <- 'Dades de Wikipedia. Gràfic de Joe Brew @joethebrew. | www.vilaweb.cat'
    
  } else {
    x <- ''
    y <- 'Visits'
    # title <- 'Wikipedia page visits, 2018'
    date_breaks <- format(sort(unique(plot_data$month)), '%b %Y')
    caption <- 'Data from Wikipedia. Chart by Joe Brew. @joethebrew. | www.vilaweb.cat'
  } 
  if(is.null(cols)){
    if(length(unique(plot_data$person)) == 2){
      cols <- databrew::make_colors(10)[c(2,5)]
    } else if(length(unique(plot_data$person)) == 3){
      cols <- databrew::make_colors(10)[c(2,5,8)]
    } else {
      cols <- databrew::make_colors(length(unique(plot_data$person)))
    }
  }
  
  ggplot(data = plot_data,
         aes(x = month,
             y = views,
             color = person,
             group = person)) +
    geom_line(size = size,
              alpha = alpha) +
    theme_vilaweb() +
    theme(axis.text.x = element_text(angle = 90,
                                     vjust = 0.5,
                                     hjust = 1)) +
    scale_color_manual(name = '',
                      values = cols) +
    labs(x = x,
         y = y,
         caption = caption) +
    facet_wrap(~language, ncol = 1,
               scales = 'free_y') +
    scale_x_date(breaks = sort(unique(plot_data$month)),
                 labels = date_breaks)
}

# make_wiki_time_plot(people = c('Inés Arrimadas',
#                                'Carles Puigdemont'))

borrell_plot <- function(language = 'en'){
  if(language == 'en'){
    x <- 'Day'
    y <- 'Visits'
    title <- 'Wikipedia page views for Josep Borrell'
    caption <- 'Data from Wikipedia. Chart made lovingly by Joe Brew for @Cristobal1_one.'
  } else {
    x <- 'Dia'
    y <- 'Visites'
    title <- 'Visites de la pàgina Wikipedia de Josep Borrell'
    caption <- 'Dades de Wikipedia. Gràfic de Joe Brew @joethebrew. | www.vilaweb.cat'
  }
  josep <- pv %>% filter(person == 'Josep Borrell')
  
  ggplot(data = josep %>% filter(date >= '2018-01-01',
                                 language == 'English'),
         aes(x = date,
             y = views)) +
    # geom_point(alpha = 0.3) +
    # geom_line() +
    geom_area() +
    theme_vilaweb() +
    labs(x = x,
         y = y,
         title = title,
         # subtitle = '2018',
         caption = caption)
}

# Ratio plot
ratio_plot <-function(language = 'en',
                      since = '2017-01-01',
                      ratio = FALSE,return_table = FALSE){

  if(language == 'ca'){
    x <- ''
    y1 <- 'Llengua'
    y2 <- 'Visites'
    # title <- 'Visites de pàgines Wikipedia, 2018'
    caption <- 'Dades de Wikipedia. Gràfic de Joe Brew @joethebrew. | www.vilaweb.cat'
    if(ratio){
      y2 <- 'Ràtio'
    }
  } else {
    x <- ''
    y1 <- 'Language'
    y2 <- 'Visits'
    # title <- 'Wikipedia page visits, 2018'
    caption <- 'Data from Wikipedia. Chart by Joe Brew @joethebrew. | www.vilaweb.cat'
    if(ratio){
      y2 <- 'Ratio'
    }
  }
  
  # Who geneates most in catalan
  pd <- make_wiki_plot(since = since, return_table = T) %>%
    ungroup %>%
    group_by(person) %>%
    summarise(catala = sum(views[which(language == 'Català')]),
              espanol = sum(views[which(language == 'Español')])) %>%
    mutate(cat_esp_ratio = catala / espanol,
           esp_cat_ratio = espanol / catala) %>%
    mutate(ratio = ifelse(catala > espanol, cat_esp_ratio,
                          esp_cat_ratio * -1)) %>%
    arrange(desc(cat_esp_ratio))
  pd$person <- factor(pd$person, levels = pd$person)
  
  if(return_table){
    return(pd)
  }
  
  if(!ratio){
    ggplot(data = pd,
           aes(x = person)) +
      geom_bar(stat = 'identity', aes(y = catala),
               fill = 'darkorange') +
      geom_text(aes(y = catala + 150000,
                    label = catala),
                angle = 90,
                alpha = 0.5,
                size = 2) +
      geom_bar(stat = 'identity', aes(y = -1 * espanol),
               fill = 'darkblue') +
      geom_text(aes(y = (-1*espanol) - 190000,
                    label = espanol),
                angle = 90,
                alpha = 0.5,
                size = 2) +
      theme_vilaweb() +
      theme(axis.text.x = element_text(angle = 90,
                                       hjust = 1,
                                       vjust = 0.5)) +
      scale_y_continuous(name = y1,
                         breaks = c(-500000, 0, 500000),
                         labels = c('Español',
                                    '',
                                    'Català'),
                         sec.axis = sec_axis(name = y2, 
                                             breaks = seq(-750000, 750000, 250000), trans = ~.*1)) +
      geom_hline(yintercept = 0) +
      labs(x = x,
           caption = caption)
  } else {
    ggplot(data = pd,
           aes(x = person)) +
      geom_point(aes(y = ratio)) +
      geom_line(aes(y = ratio,
                    yend = 0,
                    xend = person)) +
      geom_text(aes(y = ratio +
                      ifelse(ratio > 0, 3, -3),
                    label = round(ratio, digits = 1)),
                # angle = 90,
                alpha = 0.5,
                size = 3) +
      theme_vilaweb() +
      theme(axis.text.x = element_text(angle = 90,
                                       hjust = 1,
                                       vjust = 0.5)) +
      scale_y_continuous(name = y1,
                         breaks = c(-10, 0, 10),
                         labels = c('Español',
                                    '',
                                    'Català'),
                         sec.axis = sec_axis(name = y2, 
                                             breaks = seq(-30, 20, 10),
                                             trans = ~.*1,
                                             labels = abs(seq(-30, 20, 10))
                                             )) +
      geom_hline(yintercept = 0) +
      labs(x = x,
           caption = caption)
  }
}

exile_plot <- function(language = 'English'){
  if(language == 'English'){
    y <- 'Visits'
    caption <- 'Data from Wikipedia. Chart by Joe Brew @joethebrew. | www.vilaweb.cat'
  } else {
    y <- 'Visites'
    caption <- 'Dades de Wikipedia. Gràfic de Joe Brew @joethebrew. | www.vilaweb.cat'
  }
  
  x <- pv %>%
    filter(language == 'English') %>%
    filter(date >= '2018-01-01') %>%
    filter(indepe) %>%
    group_by(person, exile) %>%
    summarise(n = sum(views)) %>%
    arrange(exile)
  if(language != 'English'){
    x$exile <- ifelse(x$exile == 'Exile', 'Exili',
                      'Presó')
  }
  x$person <- factor(x$person, levels = x$person)
  ggplot(data = x,
         aes(x = person,
             y = n,
             color = exile)) +
    geom_point() +
    geom_segment(aes(yend = 0,
                     xend = person,
                     group = person)) +
    theme_vilaweb() +
    theme(axis.text.x = element_text(angle = 90,
                                     hjust = 1,
                                     vjust = 0.5)) +
    labs(x = '',
         y = y,
         caption = caption) +
    scale_color_manual(name = '',
                       values = c('darkorange', 'darkblue')) +
    theme(legend.text = element_text(size = 18))
}

jp <- function(language = 'Catalan'){
  x <- pv %>%
    filter(language == 'English')%>%
    filter(person %in% c('Oriol Junqueras',
                         'Carles Puigdemont')) %>%
    mutate(before = date <= '2017-11-02') %>%
    group_by(person, before) %>%
    summarise(views = mean(views)) %>%
    group_by(person) %>%
    mutate(p = views / views[before] * 100) %>%
    gather(key, value, views:p)
  if(language == 'Catalan'){
    x <- x %>%
      mutate(before = ifelse(before, 'Abans', 'Després'))
    x$key <- ifelse(x$key == 'p', 'Estandarditzat (%)', 'Visites diaries')
    x$key <- factor(x$key, levels = c('Visites diaries', 'Estandarditzat (%)'))
    y <- 'Valor'
    caption <- 'Dades de Wikipedia. Gràfic de Joe Brew @joethebrew. | www.vilaweb.cat'
  } else {
    x <- x %>%
      mutate(before = ifelse(before, 'Before', 'After'))
    x$before <- factor(x$before, levels = c('Before', 'After'))
    x$key <- ifelse(x$key == 'p', 'Standardized (%)', 'Daily visits')
    x$key <- factor(x$key, levels = c('Daily visits', 'Standardized (%)'))
    y <- 'Value'
    caption <- 'Data from Wikipedia. Chart by Joe Brew @joethebrew. | www.vilaweb.cat'
  }
  
  ggplot(data = x,
         aes(x = before,
             y = value,
             group = person,
             color = person)) +
    geom_line() +
    geom_point() +
    facet_wrap(~key, scales = 'free_y') +
    theme_vilaweb() +
    labs(x = '',
         y = y,
         caption = caption) +
    scale_color_manual(name = '',
                       values = c('darkorange', 'darkblue')) +
    theme(legend.text = element_text(size = 18))
}

# Save a csv
write_csv(pv, 'wiki_data_2017-2018.csv')

