library(tidyverse)

# Define function for reading mossos data
mossos_dir <- 'data/mossos/'
mossos_files <- dir(mossos_dir)

read_mosso <- function(file_name){
  # file_name = paste0(mossos_dir, mossos_files[1])
  data <- read_delim(file_name, delim = ';', locale = locale(encoding = "latin1"))
  return(data)
}

mosso_list <- list()
for(i in 1:length(mossos_files)){
  this_file <- paste0(mossos_dir, mossos_files[i])
  data <- read_mosso(this_file)
  mosso_list[[i]] <- data
}
mossos <- bind_rows(mosso_list)
mossos$date <- as.Date(paste0(mossos$Any, '-', mossos$Mes, '-15'))

# # Ministry data
# ministry <- read_delim('data/ministry/comparacion_del_crimen_en.csv',
#                        delim = ';')[1:20,] 
# names(ministry)[3] <- 'p'
# ministry <- ministry %>%
#   mutate(p = as.numeric(gsub(',', '.', p, fixed = T)))
# ministry$date <- paste0(ministry$Año, '\nT', unlist(lapply(strsplit(ministry$Periodo, ' '), function(x){x[2]})))

# https://estadisticasdecriminalidad.ses.mir.es/jaxiPx/Datos.htm?path=/DatosBalanceAct//l0/&file=09003.px
ministry <- 
  tibble(date = c('2019\nT1',
                  '2018\nT1',
                  '2017\nT1',
                  '2018\nT2',
                  '2017\nT2',
                  '2018\nT3',
                  '2017\nT3',
                  '2018\nT4',
                  '2017\nT4'),
         n = c(49363, 43979, 36346, 92294, 76574, 144024, 120536, 194212, 165633)) %>%
  bind_rows(
    
    tibble(date = c('2016\nT1',
                    '2015\nT1',
                    '2016\nT2',
                    '2015\nT2',
                    '2016\nT3',
                    '2015\nT3',
                    '2016\nT4',
                    '2015\nT4'),
           n = c(35484,35386,72878,75848,115329, 115781,153063,153346))
    
  )
# Adjust for the yearly running totals
ministry <- ministry %>%
  mutate(year = substr(date, 1, 4)) %>%
  mutate(quarter = as.numeric(substr(date, 7,7))) %>%
  arrange(year, quarter) %>%
  mutate(l = n - dplyr::lag(n, 1)) %>%
  mutate(n = ifelse(quarter != 1, l, n))



# Functions
ministry_chart <- function(ca = FALSE){
  

  if(ca){
    the_labs <- labs(x = 'Trimestre',
                     y = 'Delictes',
                     title = 'Criminalitat a Barcelona',
                     subtitle = "Font: Ministre d'Interior d'Espanya",
                     caption = 'Gràfic de @joethebrew. Font de dades: https://estadisticasdecriminalidad.ses.mir.es\nvilaweb.cat')
  } else {
    the_labs <- labs(x = 'Trimester',
                     y = 'Crimes',
                     title = 'Criminality in Barcelona',
                     subtitle = 'Source: Spanish Ministry of the Interior',
                     caption = 'Chart by @joethebrew. Data source: https://estadisticasdecriminalidad.ses.mir.es\nvilaweb.cat')
  }
  ggplot(data = ministry,
         aes(x = date,
             y = n,
             group = 1)) +
    geom_area(alpha = 0.4,
              fill = vilaweb::colors_vilaweb()[3]) +
    theme_vilaweb() +
    geom_line() +
    geom_point() +
    the_labs
}


mossos_chart <- function(ca = FALSE, robbery = FALSE){
  
  
  if(ca){
    the_labs <- labs(x = 'Mes',
                     y = 'Delictes',
                     title = 'Criminalitat a Barcelona',
                     subtitle = "Font: Mossos d'Esquadra",
                     caption = 'Gràfic de @joethebrew. Font de dades: https://mossos.gencat.cat/ca/els_mossos_desquadra/indicadors_i_qualitat/dades_obertes/cataleg_dades_obertes/dades-delinqueencials/\nvilaweb.cat')
  } else {
    the_labs <- labs(x = 'Months',
                     y = 'Crimes',
                     title = 'Criminality in Barcelona',
                     subtitle = 'Source: Catalan police force',
                     caption = 'Chart by @joethebrew. Data source: https://mossos.gencat.cat/ca/els_mossos_desquadra/indicadors_i_qualitat/dades_obertes/cataleg_dades_obertes/dades-delinqueencials/\nvilaweb.cat')
  }
  
  pd <- mossos %>% 
    filter(`Regió Policial (RP)` == 'RP Metropolitana Barcelona')
  if(robbery){
    pd <- pd %>%
      filter(`Tipus de fet` == 'Robatori amb violència i/o intimidació')
    if(ca){
      the_labs$title <- 'Robatoris amb violència i/o intimidació a Barcelona'
    } else {
      the_labs$title <- 'Robberies with violence and/or intimidation in Barcelona'
    }
    
  }
  pd <- pd %>%
    group_by(date) %>%
    summarise(n = sum(Coneguts,na.rm=T)) %>%
    filter(date >= '2015-01-01')
  
  ggplot(data = pd,
         aes(x = date,
             y = n,
             group = 1)) +
    geom_area(alpha = 0.4,
              fill = vilaweb::colors_vilaweb()[4]) +
    theme_vilaweb() +
    geom_line() +
    geom_point() +
    the_labs +
    theme(plot.caption = element_text(size = 6))
}


xarxes_chart <- function(ca = FALSE, timey = 'month'){
  
  if(ca){
    the_labs <- labs(x = 'Mes',
                     y = 'Piulets',
                     title = 'Piulets sobre la criminalitat a Barcelona',
                     caption = 'Gràfic de @joethebrew. vilaweb.cat\nBúsqueda:"violencia" OR "violència" OR "inseguridad" OR "inseguretat" OR "delitos" OR "delictes" OR "crimen" OR "crim") AND ("Barcelona")')
  } else {
    the_labs <- labs(x = 'Month',
                     y = 'Tweets',
                     title = 'Tweets about criminality in Barcelona',
                     caption = 'Chart by @joethebrew. vilaweb.cat\nSearch:"violencia" OR "violència" OR "inseguridad" OR "inseguretat" OR "delitos" OR "delictes" OR "crimen" OR "crim") AND ("Barcelona")')
  }
  
  if(timey == 'day'){
    if(ca){
      the_labs$x <- 'Dia'
    } else {
      the_labs$x <- 'Day'
    }
  }
  
  
  pd <- df %>%
    group_by(date = as.Date(cut(date, timey))) %>%
    tally %>%
    filter(date >= '2018-01-01')
  if(timey == 'day'){
    pd <- pd %>%
      filter(date >= '2019-04-01')
  }
  ggplot(data = pd,
         aes(x = date,
             y = n,
             group = 1)) +
    geom_area(alpha = 0.4,
              fill = vilaweb::colors_vilaweb()[5]) +
    theme_vilaweb() +
    geom_line() +
    geom_point() +
    the_labs +
    theme(plot.caption = element_text(size = 6))
}

newspapers_chart <- function(ca = FALSE, timey = 'month'){
  
  if(ca){
    the_labs <- labs(x = 'Mes',
                     y = 'Piulets',
                     title = 'Piulets de diaris sobre la criminalitat a Barcelona',
                     caption = 'Gràfic de @joethebrew. vilaweb.cat\nBúsqueda:"violencia" OR "violència" OR "inseguridad" OR "inseguretat" OR "delitos" OR "delictes" OR "crimen" OR "crim") AND ("Barcelona")')
  } else {
    the_labs <- labs(x = 'Month',
                     y = 'Tweets',
                     title = 'Tweets by newspapers about criminality in Barcelona',
                     caption = 'Chart by @joethebrew. vilaweb.cat\nSearch:"violencia" OR "violència" OR "inseguridad" OR "inseguretat" OR "delitos" OR "delictes" OR "crimen" OR "crim") AND ("Barcelona")')
  }
  
  if(timey == 'day'){
    if(ca){
      the_labs$x <- 'Dia'
    } else {
      the_labs$x <- 'Day'
    }
  }
  pd <- newspapers_df
  pd$violence <-
    grepl('violen|violèn|insegurida|inseguretat|delito|delicte|crimen|crim ', tolower(pd$tweet)) &
    grepl('barcelona', tolower(pd$tweet)) &
    !grepl('1-o', tolower(pd$tweet), fixed = T) &
    !grepl('referèndum|referendum', tolower(pd$tweet)) &
    !grepl('autodetermin', tolower(pd$tweet))
  
  pd <- pd %>%
    filter(violence) %>%
    group_by(date = as.Date(cut(date, timey)),
             username) %>%
    tally
  if(timey == 'day'){
    pd <- pd %>%
      filter(date >= '2019-06-01',
             date <= '2019-08-22')
  }
  left <- expand.grid(date = seq(min(pd$date),
                            max(pd$date),
                            by = timey),
                      username = sort(unique(pd$username)))
  joined <- left_join(left, pd)
  pd <- joined %>%
    mutate(n = ifelse(is.na(n), 0, n))
  cols <- colorRampPalette(RColorBrewer::brewer.pal(n = 9, name = 'Set3'))(length(unique(pd$username)))
  g <- ggplot(data = pd,
              aes(x = date,
                  y = n)) +
    theme_vilaweb() +
    the_labs +
    theme(plot.caption = element_text(size = 6)) +
    theme(legend.text = element_text(size = 8))
  if(timey == 'day'){
    g <- g +
      geom_bar(stat = 'identity',
               aes(fill = username),
               position = position_stack(),
               width = 1) +
      scale_fill_manual(name = '',
                        values = cols)
  } else {
    g <- g +
      geom_bar(stat = 'identity',
               aes(fill = username),
               position = position_stack()) +
      scale_fill_manual(name = '',
                        values = cols)    
    # g <- g +
    #   geom_line(aes(color = username)) +
    #   scale_color_manual(name = '',
    #                      values = cols)
  }
  return(g)
}


newspapers_bar_chart <- function(ca = FALSE){
  
  if(ca){
    the_labs <- labs(x = '',
                     y = '% de tots els seus piulets',
                     title = 'Piulets de diaris sobre la criminalitat a Barcelona',
                     subtitle = 'Agost 2019',
                     caption = 'Gràfic de @joethebrew. vilaweb.cat\nBúsqueda:"violencia" OR "violència" OR "inseguridad" OR "inseguretat" OR "delitos" OR "delictes" OR "crimen" OR "crim") AND ("Barcelona")')
  } else {
    the_labs <- labs(x = '',
                     y = '% of all its tweets',
                     title = 'Tweets by newspapers about criminality in Barcelona',
                     subtitle = 'August 2019',
                     caption = 'Chart by @joethebrew. vilaweb.cat\nSearch:"violencia" OR "violència" OR "inseguridad" OR "inseguretat" OR "delitos" OR "delictes" OR "crimen" OR "crim") AND ("Barcelona")')
  }
  

  pdx <- newspapers_df %>%
    filter(date >= '2019-01-01') 
  pdx$violence <-
    grepl('violen|violèn|insegurida|inseguretat|delito|delicte|crimen|crim ', tolower(pdx$tweet)) &
    grepl('barcelona', tolower(pdx$tweet)) &
    !grepl('1-o', tolower(pdx$tweet), fixed = T) &
    !grepl('referèndum|referendum', tolower(pdx$tweet)) &
    !grepl('autodetermin', tolower(pdx$tweet))
  
  pd <- pdx %>%
    filter(date >= '2019-01-01',
           date <= '2019-08-22') %>%
    mutate(agost = ifelse(date >= '2019-08-01', T, F)) %>%
    group_by(username, agost) %>%
    summarise(n_violence = length(which(violence)),
              n = n(),
              n_days = as.numeric(max(date) - min(date))+1) %>%
    mutate(daily_violence = n_violence / n_days,
           daily_tweets = n / n_days) %>%
    mutate(daily_p = daily_violence / daily_tweets * 100)
  if(ca){
    pd$agost <- ifelse(pd$agost, 'Agost\n2019',
                       'Gen-Jul\n2019') 
  } else {
    pd$agost <- ifelse(pd$agost, 'August\n2019',
                       'Jan-Jul\n2019')
  }
  pd$agost <- factor(pd$agost,
                     levels = rev(unique(sort(pd$agost))))
  
  ggplot(data = pd,
         aes(x = agost,
             y = daily_p,
             group = username)) +
    geom_area(fill = colors_vilaweb()[2], alpha = 0.3) +
    geom_line() +
    geom_point() +
    facet_wrap(~username) +
    theme_vilaweb() +
    scale_y_continuous(breaks = seq(0,2, by = 0.5)) +
    the_labs +
    theme(strip.text = element_text(size = 10))
  
}



newspapers_chart_simple <- function(ca = FALSE, timey = 'month'){
  
  if(ca){
    the_labs <- labs(x = 'Mes',
                     y = 'Piulets',
                     title = 'Piulets de diaris sobre la criminalitat a Barcelona',
                     caption = 'Gràfic de @joethebrew. vilaweb.cat\nBúsqueda:"violencia" OR "violència" OR "inseguridad" OR "inseguretat" OR "delitos" OR "delictes" OR "crimen" OR "crim") AND ("Barcelona")')
  } else {
    the_labs <- labs(x = 'Month',
                     y = 'Tweets',
                     title = 'Tweets by newspapers about criminality in Barcelona',
                     caption = 'Chart by @joethebrew. vilaweb.cat\nSearch:"violencia" OR "violència" OR "inseguridad" OR "inseguretat" OR "delitos" OR "delictes" OR "crimen" OR "crim") AND ("Barcelona")')
  }
  
  if(timey == 'day'){
    if(ca){
      the_labs$x <- 'Dia'
    } else {
      the_labs$x <- 'Day'
    }
  }
  pd <- newspapers_df
  pd$violence <-
    grepl('violen|violèn|insegurida|inseguretat|delito|delicte|crimen|crim ', tolower(pd$tweet)) &
    grepl('barcelona', tolower(pd$tweet)) &
    !grepl('1-o', tolower(pd$tweet), fixed = T) &
    !grepl('referèndum|referendum', tolower(pd$tweet)) &
    !grepl('autodetermin', tolower(pd$tweet))
  
  pd <- pd %>%
    filter(violence) %>%
    group_by(date = as.Date(cut(date, timey))) %>%
    tally
  if(timey == 'day'){
    pd <- pd %>%
      filter(date >= '2019-07-01',
             date <= '2019-08-22')
    g <-  ggplot(data = pd,
                     aes(x = date,
                         y = n,
                         group = 1)) + geom_area(alpha = 0.4,
                    fill = vilaweb::colors_vilaweb()[6]) +
      geom_line() +
      geom_point() 
  } else {
    g <- ggplot(data = pd,
                aes(x = date,
                    y = n,
                    group = 1)) +
    geom_bar(stat = 'identity')
  }
  
  g <- g +
    theme_vilaweb() +
    the_labs +
    theme(plot.caption = element_text(size = 6)) +
    theme(legend.text = element_text(size = 8))
  return(g)
}


# Define a function for combining different datasets
combination_data <- function(){
  
  # Newspapers
  pd <- newspapers_df
  pd$violence <-
    grepl('violen|violèn|insegurida|inseguretat|delito|delicte|crimen|crim ', tolower(pd$tweet)) &
    grepl('barcelona', tolower(pd$tweet)) &
    !grepl('1-o', tolower(pd$tweet), fixed = T) &
    !grepl('referèndum|referendum', tolower(pd$tweet)) &
    !grepl('autodetermin', tolower(pd$tweet))
  
  pd_newspapers <- pd %>%
    filter(violence) %>%
    group_by(date = as.Date(cut(date, 'month'))) %>%
    tally
  
  pd_xarxes <-  df %>%
    group_by(date = as.Date(cut(date, 'month'))) %>%
    tally
  
  pd_crime  <- mossos %>% 
    filter(`Regió Policial (RP)` == 'RP Metropolitana Barcelona') %>%
    group_by(date) %>%
    summarise(n = sum(Coneguts,na.rm=T)) 
  
  pd_violent_crime  <- mossos %>% 
    filter(`Regió Policial (RP)` == 'RP Metropolitana Barcelona') %>%
    filter(`Tipus de fet` == 'Robatori amb violència i/o intimidació') %>%
    group_by(date) %>%
    summarise(n = sum(Coneguts,na.rm=T))
  
  pd <-
    bind_rows(
      pd_crime %>% mutate(type = 'Delictes'),
      pd_violent_crime %>% mutate(type = 'Robatoris violents'),
      pd_xarxes %>% mutate(type = 'Twitter'),
      pd_newspapers %>% mutate(type = 'Diaris')
    ) %>%
    filter(date >= as.Date('2018-01-01')) %>%
    mutate(year = as.numeric(format(date, '%Y'))) %>%
    mutate(date = as.Date(cut(date, 'month'))) %>%
    mutate(date = date + 14)
  
  return(pd)
}

combination_chart <- function(ca = FALSE){
  pd <- combination_data()
  if(ca){
    type_dict <- tibble(
      type = c('Delictes', 'Diaris',
               'Robatoris violents',
               'Twitter'),
      category = c('Realitat', 'Relat',
                   'Realitat', 'Relat'),
      new_type = c('Delictes (tots)',
                   'Diaris (piulets sobre la\nviolència/criminalitat a Barcelona)',
                   'Robatoris violents',
                   'Xarxes (piulets sobre la\nviolència/criminalitat a Barcelona)')
    )
    the_labs <- labs(x = 'Mes',
                     y = "Percentatge (del 'normal')",
                     title = 'Realitat vs relat: violència a Barcelona',
                     caption = "Gràfic de @joethebrew. vilaweb.cat\n'Normal'=mitjana mensual de 2018 (per delictes, robatoris violents i piulets) i els primers 3 mesos de 2019 (diaris).")
  } else {
    type_dict <- tibble(
      type = c('Delictes', 'Diaris',
               'Robatoris violents',
               'Twitter'),
      category = c('Reality', 'Narrative',
                   'Reality', 'Narrative'),
      new_type = c('Crime (all)', 'Newspaper tweets\nabout violence/criminality in Barcelona',
                   'Violent robberies',
                   'All tweets\nabout violence/criminality in Barcelona')
    )
    the_labs <- labs(x = 'Month',
                     y = "Percentage (of 'normal')",
                     title = 'Reality vs narrative: violence in Barcelona',
                     caption = "Chart by @joethebrew. vilaweb.cat\n'Normal'=average monthly rate during 2018 (for crimes, violent robberies and tweets) and first 3 months of 2019 (for newspapers).")
  }
  type_dict$new_type <- paste0(type_dict$category, ':\n', type_dict$new_type)
  
  type_dict <- type_dict %>% arrange(new_type)
  if(!ca){
    type_dict <- type_dict %>% arrange(desc(new_type))
  }
  type_dict$new_type <- factor(type_dict$new_type, 
                               levels = type_dict$new_type)
  pd <- pd %>% left_join(type_dict)
  relative <- pd %>%
    arrange(date) %>%
    group_by(type = new_type) %>%
    mutate(avg18 = mean(n[year == 2018])) %>%
    mutate(avg319 = mean(n[date >= '2019-01-01' & date <= '2019-03-30'])) %>%
    mutate(avg = ifelse(grepl('diaris', tolower(new_type)) |
                          grepl('ewspap', tolower(new_type)), avg319, avg18)) %>%
    mutate(p = n / avg * 100) %>%
    ungroup %>%
    filter(date >= '2018-07-01')
  
  ggplot(data = relative,
         aes(x = date,
             y = p))+
    # facet_grid(category~new_type,
    #            shrink = T) +
    facet_wrap(~type, dir = 'v') +
    geom_hline(yintercept = 100, lty = 2, alpha = 0.7) +
    geom_line(color = colors_vilaweb()[4]) +
    geom_point(color = colors_vilaweb()[4]) +
    theme_vilaweb() +
    the_labs +
    theme(plot.caption = element_text(size = 6))
}

proportionality_plot <- function(ca = FALSE){
  pd <- newspapers_df
  pd$violence <-
    grepl('violen|violèn|insegurida|inseguretat|delito|delicte|crimen|crim ', tolower(pd$tweet)) &
    grepl('barcelona', tolower(pd$tweet)) &
    !grepl('1-o', tolower(pd$tweet), fixed = T) &
    !grepl('referèndum|referendum', tolower(pd$tweet)) &
    !grepl('autodetermin', tolower(pd$tweet))
  
  px <- pd %>%
    mutate(august = ifelse(date >= '2019-08-01',
                           'Agost',
                           ifelse(date <= '2019-07-31' & date >= '2019-05-01', 'Maig-Juliol', NA))) %>%
    filter(!is.na(august)) %>%
    group_by(username, august, violence) %>%
    tally %>%
    group_by(username, august) %>%
    mutate(p = n / sum(n) * 100)
}


