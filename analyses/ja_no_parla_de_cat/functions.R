sanchez_plot <- function(language = 'en',
                         person = 'Pedro Sánchez',
                         yy = 31){
  
  people_df <- data_frame(person = c('Pedro Sánchez',
                                     'Santiago Abascal',
                                     'Pablo Casado',
                                     'Pablo Iglesias',
                                     'Albert Rivera'),
                          account = c('sanchezcastejon',
                                      'santi_abascal',
                                      'pablocasado_',
                                      'pablo_iglesias_',
                                      'albert_rivera'))
  account <- people_df$account[people_df$person == person]
  
  if(language == 'ca'){
    x = 'Setmana'
    y = 'Percentage de piulades de la setmana'
    title = paste0('Piulades de ', person, ' mencionant Catalunya (i variacions)')
    subtitle = 'Variacions: independen-tismo/tista/cia, separatis-ta/mo, catal-uña/an, secesionis-ta/mo, golp-ista/ismo'
    caption = 'www.vilaweb.cat | Piulades públiques. Dades recollides el 16 de gener 2019.\nRegex utilitzat: "separatis|independ|catal|secesionis|golp". Joe Brew | @joethebrew'
  } else {
    x = 'Week'
    y = 'Percentage of week\'s tweets'
    title = paste0('Tweets from ', person, ' mentioning Catalonia (and variations)')
    subtitle = 'Variations: independen-tismo/tista/cia, separatis-ta/mo, catal-uña/an, secesionis-ta/mo, golp-ista/ismo'
    caption = 'Public tweets. Data collected on Jan 16 2019. Joe Brew | @joethebrew\nwww.vilaweb.cat Regex used: "separatis|independ|catal|secesionis|golp".'
  }
  pd <- tl %>%
    mutate(date = vilaweb::date_truncate(date, 'week')) %>%
    mutate(ind = grepl('separatis|independ|catal|secesionis|golp|secesionis|golp', tolower(tweet))) %>%
    group_by(date,
             person = username) %>%
    summarise(n = n(),
              p = length(which(ind)) / n() * 100) %>%
    filter(date >= '2017-07-01')
  left <- data_frame(date = sort(unique(pd$date)))
  pd <- pd %>% filter(person == account)
  pd <- left_join(left, pd)
  pd$p[is.na(pd$p)] <- 0
  dl <- range(pd$date)
  dl <- date_truncate(dl, 'month')
  dl <- seq(min(dl), max(dl), by = 'month')
  ggplot(data = pd,
         aes(x = date,
             y = p)) +
    geom_point(alpha = 0.6) +
    geom_line(alpha =0.6) +
    geom_area(alpha = 0.4) +
    geom_vline(xintercept = as.Date('2018-06-01'),
               lty = 2) +
    geom_label(data = data.frame(date = as.Date('2018-06-01'),
                               p = yy,
                               label = 'Moción de\nCensura'),
               aes(label = label,
                   y = p)) +
    labs(x = x,
         y = y,
         title = title,
         subtitle = subtitle,
         caption = caption) +
    theme_vilaweb() +
    theme(plot.subtitle = element_text(size = 10),
          plot.title = element_text(size = 14)) +
  scale_x_date(name = 'Setmana',
               breaks = dl) +
  theme(axis.text.x = element_text(angle = 90))
}


combined_plot <- function(language = 'en'){
  
  people_df <- data_frame(person = c('Pedro Sánchez',
                                     'Santiago Abascal',
                                     'Pablo Casado',
                                     'Pablo Iglesias',
                                     'Albert Rivera'),
                          account = c('sanchezcastejon',
                                      'santi_abascal',
                                      'pablocasado_',
                                      'pablo_iglesias_',
                                      'albert_rivera'))
  
  if(language == 'ca'){
    x = 'Setmana'
    y = 'Percentage de piulades de la setmana'
    title = paste0('Piulades de mencionant Catalunya (i variacions)')
    subtitle = 'Variacions: independen-tismo/tista/cia, separatis-ta/mo, catal-uña/an, secesionis-ta/mo, golp-ista/ismo'
    caption = 'www.vilaweb.cat | Piulades públiques. Dades recollides el 16 de gener 2019.\nRegex utilitzat: "separatis|independ|catal|secesionis|golp". Joe Brew | @joethebrew'
  } else {
    x = 'Week'
    y = 'Percentage of week\'s tweets'
    title = paste0('Tweets from mentioning Catalonia (and variations)')
    subtitle = 'Variations: independen-tismo/tista/cia, separatis-ta/mo, catal-uña/an, secesionis-ta/mo, golp-ista/ismo'
    caption = 'Public tweets. Data collected on Jan 16 2019. Joe Brew | @joethebrew\nwww.vilaweb.cat Regex used: "separatis|independ|catal|secesionis|golp".'
  }
  pd <- tl %>%
    mutate(date = vilaweb::date_truncate(date, 'week')) %>%
    mutate(ind = grepl('separatis|independ|catal|secesionis|golp|secesionis|golp', tolower(tweet))) %>%
    group_by(date,
             person = username) %>%
    summarise(n = n(),
              p = length(which(ind)) / n() * 100) %>%
    filter(date >= '2017-07-01')
  left <- expand.grid(date = sort(unique(pd$date)),
                     person = sort(unique(pd$person)))
  pd <- left_join(left, pd)
  pd$p[is.na(pd$p)] <- 0
  dl <- range(pd$date)
  dl <- date_truncate(dl, 'month')
  dl <- seq(min(dl), max(dl), by = 'month')
  bp <- function(x,z){RColorBrewer::brewer.pal(n = 8,name =x)[z]}
  
  cols <- 
    c(bp('Oranges', 6),
      bp('Purples', 6),
      bp('Blues', 6),
      bp('Reds', 6),
      bp('Greens', 6))
  ggplot(data = pd,
         aes(x = date,
             y = p,
             color = person)) +
    # geom_point(alpha = 0.6) +
    geom_line(alpha =0.9) +
    geom_vline(xintercept = as.Date('2018-06-01'),
               lty = 2) +
    labs(x = x,
         y = y,
         title = title,
         subtitle = subtitle,
         caption = caption) +
    theme_vilaweb() +
    theme(plot.subtitle = element_text(size = 10),
          plot.title = element_text(size = 14)) +
    scale_x_date(name = 'Setmana',
                 breaks = dl) +
    theme(axis.text.x = element_text(angle = 90)) +
    scale_color_manual(name = '',
                       values = cols) +
    theme(legend.text = element_text(size = 8))
}

plot_2018 <- function(language = 'en'){
  
  people_df <- data_frame(person = c('Pedro Sánchez',
                                     'Santiago Abascal',
                                     'Pablo Casado',
                                     'Pablo Iglesias',
                                     'Albert Rivera'),
                          account = c('sanchezcastejon',
                                      'santi_abascal',
                                      'pablocasado_',
                                      'pablo_iglesias_',
                                      'albert_rivera'))
  
  if(language == 'ca'){
    x = 'Setmana'
    y = 'Percentage de piulades'
    title = paste0('Piulades de mencionant Catalunya (i variacions)')
    subtitle = 'Variacions: independen-tismo/tista/cia, separatis-ta/mo, catal-uña/an, secesionis-ta/mo, golp-ista/ismo'
    caption = 'www.vilaweb.cat | Piulades públiques. Dades recollides el 16 de gener 2019.\nRegex utilitzat: "separatis|independ|catal|secesionis|golp". Joe Brew | @joethebrew'
  } else {
    x = 'Week'
    y = 'Percentage of tweets'
    title = paste0('Tweets from mentioning Catalonia (and variations)')
    subtitle = 'Variations: independen-tismo/tista/cia, separatis-ta/mo, catal-uña/an, secesionis-ta/mo, golp-ista/ismo'
    caption = 'Public tweets. Data collected on Jan 16 2019. Joe Brew | @joethebrew\nwww.vilaweb.cat Regex used: "separatis|independ|catal|secesionis|golp".'
  }
  pd <- tl %>%
    filter(date >= '2018-01-01',
           date <= '2018-12-31') %>%
    mutate(ind = grepl('separatis|independ|catal|secesionis|golp|secesionis|golp', tolower(tweet))) %>%
    group_by(person = username) %>%
    summarise(n = n(),
              p = length(which(ind)) / n() * 100) 
  bp <- function(x,z){RColorBrewer::brewer.pal(n = 8,name =x)[z]}
  
  cols <- 
    c(bp('Oranges', 6),
      bp('Purples', 6),
      bp('Blues', 6),
      bp('Reds', 6),
      bp('Greens', 6))
  ggplot(data = pd,
         aes(x = person,
             y = p,
             fill = person)) +
    geom_bar(stat = 'identity') +
    labs(x = x,
         y = y,
         title = title,
         subtitle = subtitle,
         caption = caption) +
    theme_vilaweb() +
    theme(plot.subtitle = element_text(size = 10),
          plot.title = element_text(size = 14)) +
    scale_fill_manual(name = '',
                       values = cols) +
    theme(legend.position = 'none') +
    geom_text(aes(label = round(p, digits = 1)),
              nudge_y = 2,
              size = 5,
              alpha = 0.7)
}