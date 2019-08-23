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
    tally
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
    grepl('barcelona', tolower(pd$tweet))
  
  pd <- pd %>%
    filter(violence) %>%
    group_by(date = as.Date(cut(date, timey)),
             username) %>%
    tally
  if(timey == 'day'){
    pd <- pd %>%
      filter(date >= '2019-07-01')
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
    theme(plot.caption = element_text(size = 8))
  # if(timey != 'day'){
    g <- g +
      geom_bar(stat = 'identity',
               aes(fill = username),
               position = position_stack()) +
      scale_fill_manual(name = '',
                        values = cols)
  # } else {
  #   g <- g +
  #     geom_line(aes(color = username)) +
  #     scale_color_manual(name = '',
  #                        values = cols)
  # }
  return(g)
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
    grepl('barcelona', tolower(pd$tweet))
  
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
    theme(plot.caption = element_text(size = 8)) 
  return(g)
}





