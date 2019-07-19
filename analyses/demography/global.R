# Libraries
library(knitr)
library(vilaweb)
library(tidyverse)
library(ggplot2)
library(tidyr)
library(gganimate)


# Functions
mround <- function(x,base){ 
  base*round(x/base) 
} 
round_percent <- function(x) { 
  x <- x/sum(x)*100  # Standardize result
  res <- floor(x)    # Find integer bits
  rsum <- sum(res)   # Find out how much we are missing
  if(rsum<100) { 
    # Distribute points based on remainders and a random tie breaker
    o <- order(x%%1, sample(length(x)), decreasing=TRUE) 
    res[o[1:(100-rsum)]] <- res[o[1:(100-rsum)]]+1
  } 
  res 
}
numberfy <- function(x){
  gsub(',', '.', scales::comma(x), fixed = TRUE)
}


# Read in CEO data
new_ceo <- vilaweb::new_ceo

# Read in Catalan demography data
df <- read_delim('data/idescat/t8661.csv', delim = ';', skip = 5)
names(df)[1] <- 'age'
df$age <- gsub(' i més', '', df$age)
df$age <- as.numeric(df$age)
for(j in 2:ncol(df)){
  message(j)
  vals <- as.character(unlist(df[,j]))
  vals <- gsub(',', '.', vals, fixed = TRUE)
  vals <- as.numeric(vals)
  df[,j] <- vals
}
# Keep only the relevant definitions
df <- df %>% dplyr::select(age, nqx)

# Estimate the last few rows
last_few <- tibble(age = 95:99,
                   nqx = seq(0.25, 1, length = 5))
# Combine
df <- df %>% filter(age != 95) %>% 
  bind_rows(last_few)

# Create a table of definitions for the demographic data
df_definitions <- 
  tibble(variable = c('nPx', 'nDx', 'nMx', 'nqx', 'nax', 'lx',
                      'ndx', 'nLx', 'Tx', 'ex'),
         meaning = c('Mid-year population of those aged x to x+n',
                     'Number of deaths for those aged x to x+n',
                     'Age-specific death rates beween ages x to x+n',
                     'Probability of dying between ages x and x+n',
                     'Average person-years lived in the interval by those dying in the interval',
                     'Number of survivors who make it to age x',
                     'Number of people who will die between age x and x+1',
                     'Person-years lived between ages x and x+n',
                     'Person-years lived above age x',
                     'Expectation of life at age x'))

# Read in place of birth data
lloc <- read_csv('data/idescat/poblacio_per_lloc_de_naixement.csv', skip = 7)
names(lloc) <- c('year', 'Catalunya', 'x', 'y',
                 "Espanya", "Estranger" , "Total")
lloc <- lloc %>% dplyr::select(year, Catalunya, Estranger, Espanya) %>%
  mutate(year = gsub(' (p)', '', year, fixed = TRUE)) %>%
  mutate(year = as.numeric(year))
# Make a long version
lloc_long <- lloc %>% tidyr::gather(key, value, Catalunya:Espanya)

# # Basic plot of lloc (exploratory only)
old_lloc <- lloc_long
past_plot <- function(ca = FALSE){
  pd <- old_lloc %>%
    group_by(year) %>%
    mutate(p = value / sum(value) * 100) %>%
    ungroup %>%
    filter(key == 'Espanya')
  if(ca){
    the_labs <- labs(title = 'Percentatge de catalans nascuts a Espanya',
                     caption = 'Dades de l\'Institut d\'Estadística de Catalunya. Gràfic de Joe Brew | @joethebrew | www.vilaweb.cat')
  } else {
    the_labs <- labs(title = 'Percentage of Catalans born in Spain',
                     caption = 'Data from the Institut d\'Estadística de Catalunya. Chart by Joe Brew | @joethebrew | www.vilaweb.cat')
  }
  
  ggplot(data = pd,
         aes(x = year,
             y = p)) +
    geom_bar(stat = 'identity',
             width = 1,
             color = 'white',
             fill = 'darkorange') +
    theme_vilaweb() +
    the_labs +
    geom_text(aes(label = round(p, digits = 1)),
              color = 'white',
              nudge_y = -2,
              size = 2)
}



# Read in place of birth + year + age data
lloc_dir <- 'data/lloc_i_edat_i_any/'
lloc_files <- dir(lloc_dir)
lloc_list <- list()
read_lloc <- function(x){
  data <- suppressMessages(read_delim(x,
                     delim = ';',
                     skip = 5)) %>%
    dplyr::rename(age = X1) %>%
    filter(age != 'Total') %>%
    dplyr::select(age, 
                  `Catalunya. Total`,
                  `Resta d'Espanya. Total`,
                  `Estranger. Total`)
  names(data)[2:4] <- c('Catalunya', 'Espanya', 'Estranger')
  year <- unlist(suppressMessages(suppressWarnings(read_delim(x, delim = ';'))[1,1]))
  year <- as.numeric(gsub('Catalunya. ', '', as.character(unlist(year)), fixed = TRUE))
  data$year <- year
  return(data)
}
for(i in 1:length(lloc_files)){
  this_file <- paste0(lloc_dir, lloc_files[i])
  this_data <- read_lloc(this_file)
  message(i)
  message(this_data$year[1])
  lloc_list[[i]] <- this_data
}
lloc <- bind_rows(lloc_list)
lloc$age <- gsub('anys i més', 'a 99 anys', lloc$age)
split_age <- strsplit(lloc$age, ' ')
lloc$min_age <- as.numeric(unlist(lapply(split_age, function(x){x[2]})))
lloc$max_age <- as.numeric(unlist(lapply(split_age, function(x){x[4]})))

# # Plot (exploratory only)
# pd <- lloc %>% gather(key, value, Catalunya:Estranger)
# ggplot(data = pd %>% filter(year == max(year)),
#        aes(x = age,
#            y = value,
#            fill = key)) +
#   geom_bar(stat = 'identity') +
#   coord_flip()

# Expand lloc into a one year per row data frame (interpolation)
agey <- tibble(age = 0:99)
age_list <- list()
for(i in 1:nrow(lloc)){
  this_row <- lloc[i,]
  n <- this_row$max_age - this_row$min_age + 1
  new_row <- tibble(age = this_row$min_age:this_row$max_age,
                    Catalunya = this_row$Catalunya / n,
                    Espanya = this_row$Espanya / n,
                    Estranger = this_row$Estranger / n,
                    year = this_row$year)
  # # Get endpoints for smoother interpolation
  # end_points <- lloc %>%
  #   filter(year == this_row$year,
  #          max_age %in% c(this_row$max_age +5,
  #                     this_row$max_age - 5))
  # if(nrow(end_points) == 2){
  #   the_diffs <- end_points[2,2:4] + end_points[1,2:4]
  #   gaps <- the_diffs / 7
  #   vals <- -2:2
  #   for(i in 1:nrow(new_row)){
  #     old_vals <- new_row[i,2:4]
  #     new_vals <- old_vals +  (vals[i] * gaps)
  #     message('Old vals:', sum(old_vals))
  #     message('New vals: ', round(sum(new_vals)))
  #     new_row[i,2:4] <- new_vals
  #   }
  # }
  
  
  age_list[[i]] <- new_row
}
lloc <- bind_rows(age_list)
# Make long
lloc_long <- lloc %>% tidyr::gather(key, value, Catalunya:Estranger)

# # Make plot (exploratory only)
# ggplot(data = lloc_long %>% filter(year == max(year)),
#        aes(x = age,
#            y = value,
#            fill = key)) +
#   geom_area() +
#   coord_flip()

# # Animation (exploratory only)
# library(gganimate)
# ggplot(data = lloc_long,
#        aes(x = age,
#            y = value,
#            fill = key)) +
#   geom_bar(stat = 'identity') +
#   coord_flip() +
#   labs(title = 'Year: {frame_time}') +
#   transition_time(year) +
#   ease_aes('linear')

# Estimate what would happen, demographically, with Catalonia's current population
current <- lloc_long %>% filter(year == 2018)
the_list <- list()
years <- 2019:2070
for(i in 1:length(years)){
  the_year <- years[i]
  # Get last year
  if(i == 1){
    last_year <- current
  } else {
    last_year <- the_list[[i-1]]
  }
  # Join the probability of death
  this_year <- last_year %>%
    dplyr::select(-year) %>%
    left_join(df %>% dplyr::select(age, nqx)) %>%
    # replace the value with the mortality-adjusted value
    mutate(value = value - (value * nqx)) %>%
    mutate(year = the_year) %>%
    dplyr::select(-nqx) %>%
    mutate(age = age + 1) %>%
    filter(age <= 100)
  the_list[[i]] <- this_year
}
future <- bind_rows(the_list)
make_animation <- function(ca = FALSE){
  if(ca){
    legend_title <- 'LLoc de naixement'
    pd <- future
    the_labs <- labs(title = 'Any: {frame_time}',
                     x = 'Edat',
                     y = 'Persones',
                     subtitle = 'Projecció de la composició de la població catalana\n(assumint cap naixement o migració després de 2019)',
                     caption = 'Dades de llocs de naixement i mortalitat de l\'Institut d\'Estadística de Catalunya.\nProjeccions i gràfic de Joe Brew | @joethebrew | www.vilaweb.cat')
    pd$key <- factor(pd$key,
                     levels = rev(c('Catalunya',
                                    'Espanya',
                                    'Estranger')))

  } else {
    legend_title <- 'Place of birth'
    pd <- future %>%
      mutate(key = ifelse(key == 'Catalunya', 'Catalonia',
                          ifelse(key == 'Espanya', 'Spain',
                                 ifelse(key == 'Estranger', 'Abroad', key))))
    pd$key <- factor(pd$key,
                     levels = rev(c('Catalonia',
                                'Spain',
                                'Abroad')))
    the_labs <- labs(title = 'Year: {frame_time}',
                     x = 'Age',
                     y = 'People',
                     subtitle = 'Projection of the composition of Catalonia\'s population\n(assuming no births or migration after 2019)',
                     caption = 'Data on places of birth and mortality from the Institut d\'Estadística de Catalunya.\nProjections and chart by Joe Brew | @joethebrew | www.vilaweb.cat')
  }
  cols <- rev(c(colors_vilaweb()[c(5,3)], 'grey'))
  label_df <- pd %>%
    group_by(key, year) %>%
    summarise(value = sum(value, na.rm = TRUE)) %>%
    group_by(year) %>%
    mutate(p = value / sum(value) * 100) %>%
    ungroup %>%
    filter(!is.na(p)) %>%
    group_by(year) %>%
    summarise(value = paste0(legend_title, '\n',paste0(key, ': ', round(p, digits = 1), '%', collapse = '\n'), collapse = ''))
  ggplot() +
    geom_label(data = label_df,
               aes(x = 90,
                   y = 110000,
                   label = value)) +
    geom_bar(stat = 'identity', width = 1,
             data = pd,
             aes(x = age,
                 y = value,
                 fill = key)) +
    coord_flip() +
    scale_fill_manual(name = legend_title,
                      values = cols) +
    theme_vilaweb() +
  transition_time(year) +
    the_labs
}


make_plot <- function(ca = FALSE){
  if(ca){
    legend_title <- 'LLoc de naixement'
    pd <- future
    the_labs <- labs(x = 'Any',
                     y = 'Percentatge',
                     title = 'Projecció: lloc de naixement dels catalans',
                     subtitle = '(assumint cap naixement o migració després de 2019)',
                     caption = 'Dades de llocs de naixement i mortalitat de l\'Institut d\'Estadística de Catalunya.\nProjeccions i gràfic de Joe Brew | @joethebrew | www.vilaweb.cat')
    pd$key <- factor(pd$key,
                     levels = rev(c('Catalunya',
                                    'Espanya',
                                    'Estranger')))
    
  } else {
    legend_title <- 'Place of birth'
    pd <- future %>%
      mutate(key = ifelse(key == 'Catalunya', 'Catalonia',
                          ifelse(key == 'Espanya', 'Spain',
                                 ifelse(key == 'Estranger', 'Abroad', key))))
    pd$key <- factor(pd$key,
                     levels = rev(c('Catalonia',
                                    'Spain',
                                    'Abroad')))
    the_labs <- labs(title = 'Projection: place of birth of Catalans',
                     x = 'Year',
                     y = 'Percentage',
                     subtitle = '(assuming no births or migration after 2019)',
                     caption = 'Data on places of birth and mortality from the Institut d\'Estadística de Catalunya.\nProjections and chart by Joe Brew | @joethebrew | www.vilaweb.cat')
  }
  cols <- rev(c(colors_vilaweb()[c(5,3)], 'grey'))

  # Group pd
  pd <- pd %>%
    group_by(key, year) %>%
    summarise(value = sum(value, na.rm = TRUE)) %>%
    ungroup %>%
    group_by(year) %>%
    mutate(p = value / sum(value) * 100) %>%
    ungroup
  
  ggplot() +
    geom_line(data = pd,
             aes(x = year,
                 y = p,
                 color = key)) +
    scale_color_manual(name = legend_title,
                      values = cols) +
    theme_vilaweb() +
    the_labs
}

birth_plot <- function(ca = FALSE){
  if(ca){
    no_know <- 'NS/NC'
    catalunya <- 'Catalunya'
    ccaa <- 'Espanya'
    mon <- 'Estranger'
  } else {
    no_know <- 'Not sure\nNo answer'
    catalunya <- 'Catalonia'
    ccaa <- 'Spain'
    mon <- 'Abroad'
  }
  pd <- new_ceo %>%
    mutate(neixer = `Em podria dir on va néixer?`,
           indepe = `Vol que Catalunya esdevingui un Estat independent?`,
           edat = Edat) %>%
    mutate(indepe = as.character(indepe),
           neixer = as.character(neixer)) %>%
    filter(!is.na(neixer),
           !is.na(indepe),
           !neixer %in% c('No ho sap', 'No contesta')) %>%
    mutate(indepe = ifelse(indepe %in% c('No ho sap',
                                         'No contesta'),
                           no_know,
                           indepe),
           neixer = ifelse(neixer == 'Catalunya', catalunya,
                           ifelse(neixer %in% c('Altres comunitats autònomes'), ccaa,
                                  mon))) %>%
    group_by(neixer, indepe) %>%
    tally %>%
    ungroup %>%
    group_by(neixer) %>%
    mutate(p = n / sum(n) * 100) %>%
    ungroup
  
  if(ca){
    the_labs <- labs(x = 'Lloc de naixement',
                     y = 'Percentatge',
                     title = 'Independentisme per lloc de naixement',
                     caption = paste0('Dades: Baròmetre d\'Opinió Política del Centre d\'Estudios d\'Opinió.\nMostra: ',
                     numberfy(sum(pd$n)), ' residents de Catalunya amb ciutadania espanyola.\nGràfic: Joe Brew | @joethebrew | www.vilaweb.cat'))
    legend_title <- 'A favor\nde la\nindependència?'
  } else {
    the_labs <- labs(x = 'Place of birth',
                     y = 'Percentage',
                     title = 'Support for independence by\nplace of birth',
                     caption = paste0('Data: Baròmetre d\'Opinió Política of the Centre d\'Estudios d\'Opinió.\nMostra: ',
                                      numberfy(sum(pd$n)), ' residents of Catalonia with Spanish citizenship.\nChart: Joe Brew | @joethebrew | www.vilaweb.cat'))
    legend_title <- 'In favor\nof\nindependence?'
  }
  cols <- colors_vilaweb()[c(5,4)]
  cols <- c(cols[1], 'grey', cols[2])
  ggplot(data = pd,
         aes(x = neixer,
             y = p,
             fill = indepe,
             group = indepe)) +
    geom_bar(stat = 'identity',
             position = position_stack()) +
    theme_vilaweb() +
    the_labs +
    scale_fill_manual(name = legend_title,
                      values = cols) +
    geom_text(aes(label = paste0(round(p, digits = 1), '%')),
              position = position_stack(),
              vjust = 1,
              alpha = 0.6) +
    theme(legend.position = 'right') +
    theme(plot.caption = element_text(hjust = 0))
}


age_plot <- function(ca = FALSE){
  if(ca){
    no_know <- 'NS/NC'
    catalunya <- 'Catalunya'
    ccaa <- 'Espanya'
    mon <- 'Estranger'
  } else {
    no_know <- 'Not sure\nNo answer'
    catalunya <- 'Catalonia'
    ccaa <- 'Spain'
    mon <- 'Abroad'
  }
  pd <- new_ceo %>%
    mutate(neixer = `Em podria dir on va néixer?`,
           indepe = `Vol que Catalunya esdevingui un Estat independent?`,
           edat = Edat) %>%
    mutate(indepe = as.character(indepe),
           neixer = as.character(neixer)) %>%
    filter(!is.na(neixer),
           !is.na(indepe),
           !is.na(edat),
           !neixer %in% c('No ho sap', 'No contesta')) %>%
    mutate(indepe = ifelse(indepe %in% c('No ho sap',
                                         'No contesta'),
                           no_know,
                           indepe),
           neixer = ifelse(neixer == 'Catalunya', catalunya,
                           ifelse(neixer %in% c('Altres comunitats autònomes'), ccaa,
                                  mon))) %>%
    group_by(edat, indepe) %>%
    tally %>%
    ungroup %>%
    group_by(edat) %>%
    mutate(p = n / sum(n) * 100) %>%
    ungroup
  
  if(ca){
    the_labs <- labs(x = 'Edat',
                     y = 'Percentatge',
                     title = 'Independentisme per edat',
                     caption = paste0('Dades: Baròmetre d\'Opinió Política del Centre d\'Estudios d\'Opinió.\nMostra: ',
                                      numberfy(sum(pd$n)), ' residents de Catalunya amb ciutadania espanyola.\nGràfic: Joe Brew | @joethebrew | www.vilaweb.cat'))
    legend_title <- 'A favor\nde la\nindependència?'
    point_name <- 'Mida de\nmostra'
  } else {
    the_labs <- labs(x = 'Age',
                     y = 'Percentage',
                     title = 'Support for independence by age',
                     caption = paste0('Data: Baròmetre d\'Opinió Política of the Centre d\'Estudios d\'Opinió.\nMostra: ',
                                      numberfy(sum(pd$n)), ' residents of Catalonia with Spanish citizenship.\nChart: Joe Brew | @joethebrew | www.vilaweb.cat'))
    legend_title <- 'In favor\nof\nindependence?'
    point_name <- 'Sample\nsize'
  }
  cols <- colors_vilaweb()[c(5,4)]
  cols <- c(cols[1], 'grey', cols[2])
  ggplot(data = pd,
         aes(x = edat,
             y = p,
             color = indepe)) +
    geom_point(alpha = 0.6,
               aes(size = n)) +
    theme_vilaweb() +
    the_labs +
    scale_color_manual(name = legend_title,
                      values = cols) +
    theme(legend.position = 'right') +
    theme(plot.caption = element_text(hjust = 0)) +
    geom_smooth(se = FALSE) +
    xlim(18, 90) +
    scale_size_continuous(name = point_name, breaks = c(25, 100, 200))
}

age_birth_plot <- function(ca = FALSE){
  if(ca){
    no_know <- 'NS/NC'
    catalunya <- 'Catalunya'
    ccaa <- 'Espanya'
    mon <- 'Estranger'
  } else {
    no_know <- 'Not sure\nNo answer'
    catalunya <- 'Catalonia'
    ccaa <- 'Spain'
    mon <- 'Abroad'
  }
  pd <- new_ceo %>%
    mutate(neixer = `Em podria dir on va néixer?`,
           indepe = `Vol que Catalunya esdevingui un Estat independent?`,
           edat = Edat) %>%
    mutate(indepe = as.character(indepe),
           neixer = as.character(neixer)) %>%
    filter(!is.na(neixer),
           !is.na(indepe),
           !is.na(edat),
           !neixer %in% c('No ho sap', 'No contesta')) %>%
    mutate(indepe = ifelse(indepe %in% c('No ho sap',
                                         'No contesta'),
                           no_know,
                           indepe),
           neixer = ifelse(neixer == 'Catalunya', catalunya,
                           ifelse(neixer %in% c('Altres comunitats autònomes'), ccaa,
                                  mon))) %>%
    group_by(edat, indepe, neixer) %>%
    tally %>%
    ungroup %>%
    group_by(edat, neixer) %>%
    mutate(p = n / sum(n) * 100) %>%
    ungroup
  
  if(ca){
    the_labs <- labs(x = 'Edat',
                     y = 'Percentatge',
                     title = 'Independentisme per edat',
                     caption = paste0('Dades: Baròmetre d\'Opinió Política del Centre d\'Estudios d\'Opinió.\nMostra: ',
                                      numberfy(sum(pd$n)), ' residents de Catalunya amb ciutadania espanyola.\nGràfic: Joe Brew | @joethebrew | www.vilaweb.cat'))
    legend_title <- 'A favor\nde la\nindependència?'
    point_name <- 'Mida de\nmostra'
  } else {
    the_labs <- labs(x = 'Age',
                     y = 'Percentage',
                     title = 'Support for independence by age',
                     caption = paste0('Data: Baròmetre d\'Opinió Política of the Centre d\'Estudios d\'Opinió.\nMostra: ',
                                      numberfy(sum(pd$n)), ' residents of Catalonia with Spanish citizenship.\nChart: Joe Brew | @joethebrew | www.vilaweb.cat'))
    legend_title <- 'In favor\nof\nindependence?'
    point_name <- 'Sample\nsize'
  }
  cols <- colors_vilaweb()[c(5,4)]
  cols <- c(cols[1], 'grey', cols[2])
  ggplot(data = pd %>% filter(!neixer %in% c('Abroad', 'Estranger')),
         aes(x = edat,
             y = p,
             color = indepe)) +
    geom_point(alpha = 0.6,
               aes(size = n)) +
    theme_vilaweb() +
    the_labs +
    scale_color_manual(name = legend_title,
                       values = cols) +
    theme(legend.position = 'right') +
    theme(plot.caption = element_text(hjust = 0)) +
    geom_smooth(se = FALSE) +
    xlim(18, 90) +
    scale_size_continuous(name = point_name, breaks = c(25, 100, 200)) +
    facet_wrap(~neixer, ncol = 2)
}

current_plot <- function(ca = FALSE){
  pd <- current %>%
    group_by(key) %>%
    mutate(p = value / sum(value) * 100)
  if(ca){
    pd$key <- factor(pd$key,
                     levels = c('Catalunya',
                                'Estranger',
                                'Espanya'))
    the_labs <- labs(y = 'Percentatge',
                     x = 'Edat',
                     title = 'Catalans: distribució d\'edat per lloc de naixement',
                     caption = 'Dades de l\'Institut d\'Estadística de Catalunya. Gràfic de Joe Brew | @joethebrew | www.vilaweb.cat')
  } else {
    pd$key <- factor(pd$key,
                     levels = c('Catalunya',
                                'Estranger',
                                'Espanya'),
                     labels = c('Catalonia',
                                'Abroad',
                                'Spain'))
    the_labs <- labs(y = 'Percentage',
                     x = 'Age',
                     title = 'Catalans: age distribution by place of birth',
                     caption = 'Data from the Institut d\'Estadística de Catalunya. Chart by Joe Brew | @joethebrew | www.vilaweb.cat')
  }

  ggplot(data = pd,
         aes(x = age,
             y = p)) +
    geom_bar(stat = 'identity',
             width = 1,
             color = 'white') +
    facet_wrap(~key) +
    coord_flip() +
    theme_vilaweb() +
    the_labs
}

# Create model of independentism
predictions <- 
  new_ceo %>%  mutate(neixer = `Em podria dir on va néixer?`,
         indepe = `Vol que Catalunya esdevingui un Estat independent?`,
         edat = as.numeric(Edat)) %>%
  filter(`Any de realització del baròmetre` %in% c(2018, 2019)) %>%
  # # group together all the very old (not doing for now, just an option to deal with small sample size)
  # mutate(edat = ifelse(edat >= 0, 0, edat)) %>%
  mutate(indepe = as.character(indepe),
         neixer = as.character(neixer)) %>%
  filter(!is.na(neixer),
         !is.na(indepe),
         !neixer %in% c('No ho sap', 'No contesta')) %>%
  mutate(indepe = ifelse(indepe %in% c('No ho sap',
                                       'No contesta'),
                         'NS/NC',
                         indepe),
         neixer = ifelse(neixer == 'Catalunya', 'Catalunya',
                         ifelse(neixer %in% c('Altres comunitats autònomes'), 'Espanya',
                                'Estranger'))) %>%
  group_by(neixer, indepe, edat) %>%
  tally %>%
  ungroup %>%
  group_by(edat, neixer) %>%
  mutate(p = n / sum(n) * 100) %>%
  ungroup %>%
  group_by(neixer, edat) %>%
  summarise(n_si = sum(n[indepe == 'Sí']),
            n_no = sum(n[indepe == 'No']),
            n_ns = sum(n[indepe == 'NS/NC']),
            p_si = sum(n[indepe == 'Sí']) / sum(n),
            p_no = sum(n[indepe == 'No']) / sum(n),
            p_ns = sum(n[indepe == 'NS/NC']) / sum(n),
            p_si_ref = sum(n[indepe == 'Sí']) / sum(n[indepe %in% c('Sí', 'No')]),
            p_no_ref = sum(n[indepe == 'No']) / sum(n[indepe %in% c('Sí', 'No')])) %>%
  ungroup %>%
  dplyr::select(neixer, edat, p_si, p_no, p_ns, p_si_ref, p_no_ref)

# Test to see if current matches up with survey results
test <- current %>%
  dplyr::rename(neixer = key,
                edat = age) %>%
  # mutate(edat = ifelse(edat >= 0, 0, edat)) %>%
  left_join(predictions) %>%
  # remove those for whom predictions could not be made
  filter(!is.na(p_si)) %>%
  # calculate number of people
  mutate(indepes = value * p_si,
         unionistes = value * p_no,
         undecided = value * p_ns)
# x <- test %>% 
#   summarise(indepes = sum(indepes),
#             unionistes = sum(unionistes),
#             undecided = sum(undecided))
# x
# as.numeric(x) / sum(as.numeric(x))

# Now, using this, we can create projections
projections <- future %>%
  dplyr::rename(neixer = key,
                             edat = age) %>%
  # mutate(edat = ifelse(edat >= 0, 0, edat)) %>%
  left_join(predictions) %>%
  # remove those for whom predictions could not be made
  filter(!is.na(p_si)) %>%
  # calculate number of people
  mutate(indepes = value * p_si,
         unionistes = value * p_no,
         undecided = value * p_ns) %>%
  group_by(year) %>%
  summarise(indepes = sum(indepes),
            p_indepes = sum(indepes) / (sum(indepes) + sum(unionistes) + sum(undecided)) * 100,
            unionistes = sum(unionistes),
            p_unionistes = sum(unionistes) / (sum(unionistes) + sum(indepes) + sum(undecided)) * 100,
            undecided = sum(undecided),
            p_undecided = sum(undecided) / (sum(unionistes) + sum(indepes) + sum(undecided)) * 100,) %>%
  mutate(p_si_ref = p_indepes / (100 - p_undecided),
         p_no_ref = p_unionistes / (100 - p_undecided))

projection_plot <- function(ca = FALSE){
  if(ca){
    the_labs <- labs(x = 'Any',
                     y = '%',
                     title = 'Independentisme segons la projecció demogràfica',
                     subtitle = 'Efecte de la mortalitat, població de 2019',
                     caption = "Model basat en enquestes de 2018-2019:CEO. Dades poblacionals de 2019: IDES.\nEl percentage reflecteix un hipotètic referèndum (traient els 'no sap/no contesta'), edat=18+.\nJoe Brew | @joethebrew | www.vilaweb.cat")
    choices <- c('En contra', 'A favor')
    legend_title <- 'Opinió sobre la independència'
  } else {
    the_labs <- labs(x = 'Year',
                     y = '%',
                     title = 'Support for independence per demographic projection',
                     subtitle = 'Effect of mortality only, year 2019',
                     caption = "Model based on survey data from 2018-2019: CEO. Population data from 2019: IDES.\nThe percentage reflects a hypothetical referendum (without the 'unknown' responses), age=18+.\nJoe Brew | @joethebrew | www.vilaweb.cat")
    choices <- c('Opposed', 'In favor')
    legend_title <- c('View on independence')
  }
  ggplot(data = projections %>% mutate(p_si_ref = p_si_ref * 100,
                                       p_no_ref = p_no_ref * 100) %>%
           dplyr::select(year, p_si_ref, p_no_ref) %>%
           gather(key, value, p_si_ref:p_no_ref) %>%
           mutate(key = ifelse(key == 'p_no_ref',
                               choices[1],
                               choices[2])),
         aes(x = year,
             y = value)) +
    geom_line(aes(color = key),
              size = 2) +
    xlim(2018, 2080) +
    theme_vilaweb() +
    ylim(0, 100) +
    geom_hline(yintercept = 50, lty = 2) +
    the_labs +
    scale_color_manual(name = legend_title,
                       values = colors_vilaweb()[3:4])
}


# Immigracions procedents de la resta d'Espanya. Per sexe, grups d'edat i lloc de naixement.
# https://www.idescat.cat/pub/?id=mm&n=6383
#t6383.csv
ready <- function(number = 6383){
  ides_urls <- paste0('https://www.idescat.cat/pub/?id=mm&n=',
                       number,'&t=', 2005:2017,'00&f=csv')
  yearsy <- 2005:2017
  ides_list <- list()
  for(j in 1:length(ides_urls)){
    # Sys.sleep(2)
    message(j)
    ides <- read_csv(ides_urls[j], skip = 5)
    # Keep only the total and below
    ides <- ides[(which(ides$X1 == 'Total') +1): nrow(ides),]
    ides <- ides %>%
      gather(age, value, `De 0 a 14 anys`:`De 60 anys i més`) %>%
      dplyr::rename(key = X1) %>%
      mutate(key = gsub('nascuts a ', '', key)) %>%
      mutate(key = gsub("la resta d'", "", key)) %>%
      mutate(key = gsub("l'e", "E", key, fixed = TRUE))
    # Fix age
    ides$age <- gsub('anys i més', 'a 99 anys', ides$age)
    split_age <- strsplit(ides$age, ' ')
    ides$min_age <- as.numeric(unlist(lapply(split_age, function(x){x[2]})))
    ides$max_age <- as.numeric(unlist(lapply(split_age, function(x){x[4]})))
    ides$Total <- NULL
    # Expand
    migrations_list <- list()
    for(i in 1:nrow(ides)){
      this_row <- ides[i,]
      out <- tibble(age = this_row$min_age:this_row$max_age,
                    key = this_row$key)
      n <- nrow(out)
      out$value <- this_row$value / n
      migrations_list[[i]] <- out
    }
    ides <- bind_rows(migrations_list)
    ides_list[[j]] <- ides %>% mutate(year = yearsy[j])
  }
  ides <- bind_rows(ides_list) %>%
    group_by(age, key, year) %>%
    summarise(value = mean(value))
  return(ides)
}

# Get inmig #6383
# exmig 6384
# wexmig 6386
# winbmig 6389
if(!'migdata.RData' %in% dir()){
  inmig <- ready(6383)
  exmig <- ready(6384)
  wexmig <- ready(6386)
  winmig <- ready(6389)
  save(inmig, exmig, wexmig, winmig,
       file = 'migdata.RData')
} else {
  load('migdata.RData')
}

transformy <- function(x, since_year = 2005){
  x %>% filter(year >= since_year) %>%
    group_by(age, key) %>%
    summarise(value = mean(value))
} 
inmig <- transformy(inmig)
exmig <- transformy(exmig)
wexmig <- transformy(wexmig)
winmig <- transformy(winmig)
# Copmbine all migrations
migrations <-
  bind_rows(
    inmig %>% mutate(destination = 'Catalunya'),
    winmig %>% mutate(destination = 'Catalunya'),
    exmig %>% mutate(destination = 'World'),
    wexmig %>% mutate(destination = 'World')
  )
# # Add births
births <- tibble(age = 0,
                 key = c('Catalunya', 'Espanya', 'Estranger'),
                 value = c(66803, 
                           896, # 896 + 241 - 135,
                           3051))# + 938 - 945))
# changes <- bind_rows(migrations, births)
changes <- migrations
changes <- changes %>%
  # dplyr::rename(place_of_birth = key) %>%
  mutate(action = ifelse(destination == 'Catalunya',
                         'Coming',
                         'Leaving')) %>%
  dplyr::select(-destination) %>%
  group_by(key, age) %>%
  summarise(leaving = sum(value[action == 'Leaving']),
            coming = sum(value[action == 'Coming'])) %>%

  ungroup
# Make changes a percentage, rather than absolute number
changes <- left_join(changes, current) %>% dplyr::select(-year)
changes <- changes %>% 
  # mutate(
  # leaving = leaving / value,
  # coming = coming / value) %>%
  dplyr::select(-value)
# Create an adjusted future, to account for changes (migrations and births)
# Estimate what would happen, demographically, with Catalonia's current population
current <- lloc_long %>% filter(year == 2018)
the_list <- list()
years <- 2019:2070
for(i in 1:length(years)){
  the_year <- years[i]
  # Get last year
  if(i == 1){
    last_year <- current
  } else {
    last_year <- the_list[[i-1]]
  }
  # Join the probability of death
  this_year <- last_year %>%
    dplyr::select(-year) %>%
    left_join(df %>% dplyr::select(age, nqx)) %>%
    # replace the value with the mortality-adjusted value
    mutate(value = value - (value * nqx)) %>%
    mutate(year = the_year) %>%
    dplyr::select(-nqx) %>%
    mutate(age = age + 1) %>%
    filter(age <= 100)
  # Add a zero year
  this_year <- this_year %>%
    bind_rows(births %>% mutate(year = this_year$year[1]))
  this_year <- this_year %>% arrange(age)
  
  # Add the changes
  this_year <- left_join(this_year, changes)
  this_year <- this_year %>%
    mutate(value = value + coming - leaving) %>%
    # mutate(value = value - (leaving * value) + (coming * value)) %>%
    dplyr::select(-coming, -leaving)
  
  the_list[[i]] <- this_year
}
future_adj <- bind_rows(the_list)

# Use the adjusted futures to make adjusted projections
# Now, using this, we can create projections
projections_adj <- future_adj %>%
  dplyr::rename(neixer = key,
                edat = age) %>%
  # mutate(edat = ifelse(edat >= 0, 0, edat)) %>%
  left_join(predictions) %>%
  # remove those for whom predictions could not be made
  filter(!is.na(p_si)) %>%
  # calculate number of people
  mutate(indepes = value * p_si,
         unionistes = value * p_no,
         undecided = value * p_ns) %>%
  group_by(year) %>%
  summarise(indepes = sum(indepes),
            p_indepes = sum(indepes) / (sum(indepes) + sum(unionistes) + sum(undecided)) * 100,
            unionistes = sum(unionistes),
            p_unionistes = sum(unionistes) / (sum(unionistes) + sum(indepes) + sum(undecided)) * 100,
            undecided = sum(undecided),
            p_undecided = sum(undecided) / (sum(unionistes) + sum(indepes) + sum(undecided)) * 100,) %>%
  mutate(p_si_ref = p_indepes / (100 - p_undecided),
         p_no_ref = p_unionistes / (100 - p_undecided))

# Make a version with no foreigners
projections_adj_no_estranger <- future_adj %>%
  dplyr::rename(neixer = key,
                edat = age) %>%
  filter(!neixer %in% c('Estranger', 'Abroad')) %>%
  # mutate(edat = ifelse(edat >= 0, 0, edat)) %>%
  left_join(predictions) %>%
  # remove those for whom predictions could not be made
  filter(!is.na(p_si)) %>%
  # calculate number of people
  mutate(indepes = value * p_si,
         unionistes = value * p_no,
         undecided = value * p_ns) %>%
  group_by(year) %>%
  summarise(indepes = sum(indepes),
            p_indepes = sum(indepes) / (sum(indepes) + sum(unionistes) + sum(undecided)) * 100,
            unionistes = sum(unionistes),
            p_unionistes = sum(unionistes) / (sum(unionistes) + sum(indepes) + sum(undecided)) * 100,
            undecided = sum(undecided),
            p_undecided = sum(undecided) / (sum(unionistes) + sum(indepes) + sum(undecided)) * 100,) %>%
  mutate(p_si_ref = p_indepes / (100 - p_undecided),
         p_no_ref = p_unionistes / (100 - p_undecided))


make_animation_adj <- function(ca = FALSE){
  if(ca){
    legend_title <- 'LLoc de naixement'
    pd <- future_adj
    the_labs <- labs(title = 'Any: {frame_time}',
                     x = 'Edat',
                     y = 'Persones',
                     subtitle = 'Projecció de la composició de la població catalana\n(assumint naixement/migració constants)',
                     caption = 'Dades de llocs de naixement i mortalitat de l\'Institut d\'Estadística de Catalunya.\nProjeccions i gràfic de Joe Brew | @joethebrew | www.vilaweb.cat')
    pd$key <- factor(pd$key,
                     levels = rev(c('Catalunya',
                                    'Espanya',
                                    'Estranger')))
    
  } else {
    legend_title <- 'Place of birth'
    pd <- future_adj %>%
      mutate(key = ifelse(key == 'Catalunya', 'Catalonia',
                          ifelse(key == 'Espanya', 'Spain',
                                 ifelse(key == 'Estranger', 'Abroad', key))))
    pd$key <- factor(pd$key,
                     levels = rev(c('Catalonia',
                                    'Spain',
                                    'Abroad')))
    the_labs <- labs(title = 'Year: {frame_time}',
                     x = 'Age',
                     y = 'People',
                     subtitle = 'Projection of the composition of Catalonia\'s population\n(assuming constant birth and migration rates)',
                     caption = 'Data on places of birth and mortality from the Institut d\'Estadística de Catalunya.\nProjections and chart by Joe Brew | @joethebrew | www.vilaweb.cat')
  }
  cols <- rev(c(colors_vilaweb()[c(5,3)], 'grey'))
  label_df <- pd %>%
    group_by(key, year) %>%
    summarise(value = sum(value, na.rm = TRUE)) %>%
    group_by(year) %>%
    mutate(p = value / sum(value) * 100) %>%
    ungroup %>%
    filter(!is.na(p)) %>%
    group_by(year) %>%
    summarise(value = paste0(legend_title, '\n',paste0(key, ': ', round(p, digits = 1), '%', collapse = '\n'), collapse = ''))
  ggplot() +
    geom_label(data = label_df,
               aes(x = 90,
                   y = 110000,
                   label = value)) +
    geom_bar(stat = 'identity', width = 1,
             data = pd,
             aes(x = age,
                 y = value,
                 fill = key)) +
    coord_flip() +
    scale_fill_manual(name = legend_title,
                      values = cols) +
    theme_vilaweb() +
    transition_time(year) +
    the_labs
}

make_plot_adj <- function(ca = FALSE, remove_estranger = F){
  if(ca){
    legend_title <- 'LLoc de naixement'
    pd <- future_adj
    the_labs <- labs(x = 'Any',
                     y = 'Percentatge',
                     title = 'Projecció: lloc de naixement dels catalans',
                     subtitle = '(assumint migració/naixement constants)',
                     caption = 'Dades de llocs de naixement i mortalitat de l\'Institut d\'Estadística de Catalunya.\nProjeccions i gràfic de Joe Brew | @joethebrew | www.vilaweb.cat')
    pd$key <- factor(pd$key,
                     levels = rev(c('Catalunya',
                                    'Espanya',
                                    'Estranger')))
    
  } else {
    legend_title <- 'Place of birth'
    pd <- future_adj
    pd <- pd %>%
      mutate(key = ifelse(key == 'Catalunya', 'Catalonia',
                          ifelse(key == 'Espanya', 'Spain',
                                 ifelse(key == 'Estranger', 'Abroad', key))))
    pd$key <- factor(pd$key,
                     levels = rev(c('Catalonia',
                                    'Spain',
                                    'Abroad')))
    the_labs <- labs(title = 'Projection: place of birth of Catalans',
                     x = 'Year',
                     y = 'Percentage',
                     subtitle = '(assuming constant birth/migration rates)',
                     caption = 'Data on places of birth and mortality from the Institut d\'Estadística de Catalunya.\nProjections and chart by Joe Brew | @joethebrew | www.vilaweb.cat')
  }
  cols <- rev(c(colors_vilaweb()[c(5,3)], 'grey'))
  

  
  # Group pd
  pd <- pd %>%
    group_by(key, year) %>%
    summarise(value = sum(value, na.rm = TRUE)) %>%
    ungroup %>%
    group_by(year) %>%
    mutate(p = value / sum(value) * 100) %>%
    ungroup
  
  ggplot() +
    geom_line(data = pd,
              aes(x = year,
                  y = p,
                  color = key)) +
    scale_color_manual(name = legend_title,
                       values = cols) +
    theme_vilaweb() +
    the_labs
}



projection_plot_adj <- function(ca = FALSE, short = FALSE,
                                remove_estranger = FALSE){
  if(ca){
    the_labs <- labs(x = 'Any',
                     y = '%',
                     title = 'Independentisme segons la projecció demogràfica',
                     subtitle = 'Efecte de la mortalitat/migració/fertilitat',
                     caption = "Model basat en enquestes de 2018-2019:CEO. Dades poblacionals de 2019: IDES.\nEl percentage reflecteix un hipotètic referèndum (traient els 'no sap/no contesta'), edat=18+.\nJoe Brew | @joethebrew | www.vilaweb.cat")
    choices <- c('En contra', 'A favor')
    legend_title <- 'Opinió sobre la independència'
  } else {
    the_labs <- labs(x = 'Year',
                     y = '%',
                     title = 'Support for independence per demographic projection',
                     subtitle = 'Effect of mortality/migration/fertility',
                     caption = "Model based on survey data from 2018-2019: CEO. Population data from 2019: IDES.\nThe percentage reflects a hypothetical referendum (without the 'unknown' responses), age=18+.\nJoe Brew | @joethebrew | www.vilaweb.cat")
    choices <- c('Opposed', 'In favor')
    legend_title <- c('View on independence')
  }
  if(remove_estranger){
    pd <- projections_adj_no_estranger
  } else {
    pd <- projections_adj
  }
  g <- ggplot(data = pd %>% mutate(p_si_ref = p_si_ref * 100,
                                       p_no_ref = p_no_ref * 100) %>%
           dplyr::select(year, p_si_ref, p_no_ref) %>%
           gather(key, value, p_si_ref:p_no_ref) %>%
           mutate(key = ifelse(key == 'p_no_ref',
                               choices[1],
                               choices[2])),
         aes(x = year,
             y = value))
  if(short){
    g <- g +  geom_line(aes(color = key),
                        size = 1) +
      xlim(2018, 2035) +
      ylim(46, 54)
      
  } else {
    g <- g +  geom_line(aes(color = key),
                        size = 2) +
      xlim(2018, 2080) +
      geom_line(aes(color = key),
                size = 2) +
      ylim(0, 100)
  }
    g <- g +
    theme_vilaweb() +
    geom_hline(yintercept = 50, lty = 2) +
    the_labs +
    scale_color_manual(name = legend_title,
                       values = colors_vilaweb()[3:4])
    return(g)
}

# # calculations for Ramir
# # independentisme by place of birth by year
# no_know <- 'NS/NC'
# catalunya <- 'Catalunya'
# ccaa <- 'Espanya'
# mon <- 'Estranger'
# pd <- new_ceo %>%
#   mutate(neixer = `Em podria dir on va néixer?`,
#          indepe = `Vol que Catalunya esdevingui un Estat independent?`,
#          edat = Edat,
#          any = `Any de realització del baròmetre`) %>%
#   mutate(indepe = as.character(indepe),
#          neixer = as.character(neixer)) %>%
#   filter(!is.na(neixer),
#          !is.na(indepe),
#          !neixer %in% c('No ho sap', 'No contesta')) %>%
#   mutate(indepe = ifelse(indepe %in% c('No ho sap',
#                                        'No contesta'),
#                          no_know,
#                          indepe),
#          neixer = ifelse(neixer == 'Catalunya', catalunya,
#                          ifelse(neixer %in% c('Altres comunitats autònomes'), ccaa,
#                                 mon))) %>%
#   filter(indepe != 'NS/NC') %>%
#   group_by(neixer, indepe, any) %>%
#   tally %>%
#   ungroup %>%
#   group_by(neixer, any) %>%
#   mutate(p = n / sum(n) * 100) %>%
#   ungroup %>%
#   filter(neixer %in% c('Catalunya', 'Espanya'))
# 
# ggplot(data = pd,
#        aes(x = any,
#            y = p,
#            color = indepe)) +
#   geom_line() +
#   facet_wrap(~neixer)
# write_csv(pd, '~/Desktop/ramir.csv')
