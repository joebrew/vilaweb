
# Libraries
library(vilaweb)
library(tidyverse)
library(databrew)
library(pageviews)
library(lubridate)

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
                          end = "2019070500", 
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
                          end = "2019070500", 
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

if(!'data_en.RData' %in% dir()){
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
    Sys.sleep(2)
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
                          end = "2019070500", 
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
                          end = "2019070500", 
                          reformat = TRUE)
    }
    pv$person <- person
    out_list[[i]] <- pv
  }
  pv <- bind_rows(out_list)
  save(pv, file = 'data_en.RData')
} else {
  load('data_en.RData')
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
    Sys.sleep(1)
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
                          end = "2019070500", 
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
                          end = "2019070500", 
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
                           people = NULL,
                           filter_language = c('English', 'Català', 'Español'),
                           since = '2017-01-01',
                           return_table = FALSE){
  
  
  if(is.null(people)){
    people <- sort(unique(pv$person))
  }
  plot_data <- 
    pv %>%
    filter(person %in% people) %>%
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
  cols <- as.character(vilaweb::colors_vilaweb()[c(1,3,5)])
  if(language == 'ca'){
    plot_data$language <-
      ifelse(plot_data$language == 'English',
             'Anglès',
             ifelse(plot_data$language == 'Español',
                    'Espanyol',
                    plot_data$language))
  } else {
    plot_data$language <-
      ifelse(plot_data$language == 'Català',
             'Catalan',
             ifelse(plot_data$language == 'Español',
                    'Spanish',
                    plot_data$language))
  }
  ggplot(data = plot_data,
         aes(x = person,
             y = views,
             fill = language,
             group = language)) +
    geom_bar(stat = 'identity', position = position_dodge()) +
    theme_vilaweb() +
    # theme(axis.text.x = element_text(angle = 90,
    #                                  vjust = 0.5,
    #                                  hjust = 1)) +
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
                                return_table = FALSE,
                                the_scales = 'free_y'){
  
  if(is.null(people)){
    people <- sort(unique(pv$person))
  }
  plot_data <- 
    pv %>%
    filter(person %in% people) %>%
    filter(date >= since) %>%
    # mutate(month = date_truncate(date, 'week')) %>%
    mutate(month = date) %>%
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
    if(length(unique(plot_data$language)) == 2){
      cols <- databrew::make_colors(10)[c(2,5)]
    } else if(length(unique(plot_data$language)) == 3){
      cols <- databrew::make_colors(10)[c(2,5,8)]
    } else {
      cols <- databrew::make_colors(length(unique(plot_data$language)))
    }
  }
  
  if(language == 'ca'){
    plot_data$language <-
      ifelse(plot_data$language == 'English',
             'Anglès',
             ifelse(plot_data$language == 'Español',
                    'Espanyol',
                    plot_data$language))
  } else {
    plot_data$language <-
      ifelse(plot_data$language == 'Català',
             'Catalan',
             ifelse(plot_data$language == 'Español',
                    'Spanish',
                    plot_data$language))
  }
  
  ggplot(data = plot_data,
         aes(x = month,
             y = views,
             color = language,
             group = language)) +
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
    facet_wrap(~person, ncol = 2,
               scales = the_scales) 
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

d21 <- function(ca = FALSE){
  require(vilaweb)
  if(ca){
    facs <- c('Favorable a\ninvestir Puigdemont',
              'No favorable a\ninvestir Puigdemont',
              'Cap posició explícita\nsobre Puigdemont')
    seats <- 'escons'
    votes <- 'vots'
    the_labs <- labs(x = '',
                     y = 'Diputats',
                     title = 'Eleccions catalanes de 2017')
  } else {
    facs <- c('In favor of Puigdemont\nas President',
              'Not in favor of Puigdemont\nas President',
              'No explicit position\non Puigdemont')
    seats <- 'seats'
    votes <- 'votes'
    the_labs <- labs(x = '',
                     y = 'MPs',
                     title = '2017 Catalan elections')
  }
  pd <- tibble(partit = c("Cs", "JxCat", "ERC", "PSC", "Comuns", "CUP", "PP", "PACMA", "Verds", "PU M+J"),
               vots = c(1109732, 948233, 935861, 606659, 326360, 195246, 185670, 38743, 10287, 577),
               escons = c(36, 34, 32, 17, 8, 4, 4, 0, 0, 0),
               favorable = c(F, T, T, F, F, T, F, NA, F, NA),
               pa = c(T, F, F, F, F, F, T, F, F, F)) %>%
    arrange(escons) %>%
    group_by(favorable) %>%
    mutate(total = sum(escons),
           total_vots = sum(vots)) %>%
    ungroup %>%
    mutate(fac = ifelse(is.na(favorable), facs[3], ifelse(favorable, facs[1], facs[2]))) %>%
    filter(!is.na(fac)) %>%
    mutate(fac = paste0(fac, ':\n', numberfy(total_vots), ' ', votes))
  pd$partit <- factor(pd$partit, levels = pd$partit)
  # pd <- pd %>% filter(escons > 0)
  cols <- c('#FFFFFF', '#2C913D', '#ABBB92', 'black', '#5DBCD2', 'purple', '#C00F19', '#9E9F97', '#5B3217', '#DF8547')
  
  
  ggplot(data = pd,
         aes(x = partit,
             y = vots,
             fill = partit)) +
    facet_wrap(~fac, scales = 'free_x') +
    geom_bar(stat = 'identity') +
    theme_vilaweb() +
    geom_text(aes(label = numberfy(vots)),
              color = 'black',
              nudge_y = 50000) +
    the_labs +
    scale_fill_manual(name = '',
                      values = cols) 
}

d21p <- function(ca = FALSE){
  require(vilaweb)
  if(ca){
    facs <- c('Favorable a investir Puigdemont',
              'No favorable a investir Puigdemont',
              'Favorable a investir Arrimadas')
    seats <- 'escons'
    the_labs <- labs(x = '',
                     y = 'Diputats',
                     title = 'Eleccions catalanes de 2017: suport pels candidats presidencials entre diputats elegits',
                     subtitle = '135 diputats en total. Majoria absoluta = 68 vots')
  } else {
    facs <- c('In favor of Puigdemont as President',
              'Not in favor of Puigdemont as President',
              'In favor of Arrimadas as President')
    seats <- 'seats'
    the_labs <- labs(x = '',
                     y = 'MPs',
                     title = '2017 Catalan elections: support for Presidential candidates among elected MPs',
                     subtitle = '135 total MPs. Absolute majority = 68 votes')
  }
  cols <- c('black', '#5DBCD2', '#C00F19', 'purple', '#DF8547')
  pd <- tibble(partit = c("Cs", "JxCat", "ERC", "PSC", "Comuns", "CUP", "PP", "PACMA", "Verds", "PU M+J"),
               vots = c(1109732, 948233, 935861, 606659, 326360, 195246, 185670, 38743, 10287, 577),
               escons = c(36, 34, 32, 17, 8, 4, 4, 0, 0, 0),
               favorable = c(F, T, T, F, F, T, F, F, F, F),
               pa = c(T, F, F, F, F, F, T, F, F, F)) %>%
    arrange(escons) %>%
    group_by(favorable) %>%
    mutate(total = sum(escons)) %>%
    ungroup %>%
    mutate(fac = ifelse(favorable, facs[1], 
                        ifelse(pa, facs[3], NA))) %>%
    filter(!is.na(fac)) %>%
    mutate(fac = paste0(fac, ':\n', total, ' ', seats))
  pd$partit <- factor(pd$partit, levels = pd$partit)
  ggplot(data = pd %>% filter(escons > 0),
         aes(x = fac,
             y = escons,
             fill = partit)) +
    geom_bar(stat = 'identity',
             position = position_stack()) +
    theme_vilaweb() +
    geom_text(aes(label = escons),
              color = 'white',
              # nudge_y = -2,
              position = position_stack(vjust = 0.5)) +
    the_labs +
    scale_fill_manual(name = '',
                      values = cols) +
    facet_wrap(~fac, scales = 'free_x') +
    theme(axis.text.x = element_text(size = 0))
}

europees <- function(ca = FALSE){
  pd <- tibble(partit = c('JxCat', 'PSC', 'ERC', 'Cs', 'Podem', 'PP', 'VOX', 'PACMA', 'CV-EC',
                          'Recortes\nCero', 'Pirates', 'Altres'),
               vots = c(987149, 766107, 733401, 298781, 292088, 178950, 68824, 48733, 11713, 6822, 4937, 42858))
  pd$partit <- factor(pd$partit, levels = pd$partit)
  if(ca){
    the_labs <- labs(title = 'Resultats 2019: elecciones europees a Catalunya',
                     x = 'Partit',
                     y = 'Vots')
  } else {
    the_labs <- labs(title = '2019 results: European elections in Catalonia',
                     x = 'Party',
                     y = 'Votes')
  }
  ggplot(data = pd,
         aes(x = partit,
             y = vots)) +
    geom_point() +
    geom_segment(aes(xend = partit,
                     yend = 0)) +
    theme_vilaweb() +
    the_labs +
    geom_text(aes(label = numberfy(vots)),
              nudge_y = 50000,
              alpha = 0.7)
}

twitter <- function(ca = FALSE){
  pd <-
    bind_rows(
      puigdemont_bcn %>% mutate(who = 'Puigdemont'),
      sanchez_bcn %>% mutate(who = 'Sánchez')
    ) %>%
    mutate(date = as.Date(created_at)) %>%
    filter(date != max(date)) %>%
    group_by(who, date) %>%
    summarise(n = n(),
              rt = sum(retweet_count),
              likes = sum(favorite_count)) %>%
    mutate(interactions = n + rt + likes)
  ggplot(data = pd,
         aes(x = date,
             y = interactions,
             color = who)) +
    geom_line()
}

