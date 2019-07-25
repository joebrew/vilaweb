# Libraries
library(vilaweb)
library(tidyverse)
library(ggplot2)
library(tidyr)

# Get pew populism data
pew <- vilaweb::pew_populism

# Create a left-right axis
pew$axis <- pew$Q37

# Create a "true" left-right axis based on individual items
pew$govt_vs_ind <- as.character(pew$Q24a)
pew$business <- (pew$Q25a)

pew$axis_true <- 
  as.numeric(ifelse(pew$govt_vs_ind == '1', '1',
         ifelse(pew$govt_vs_ind == '2', '0',
                NA))) +
  as.numeric(ifelse(pew$business == '2', '1',
                    ifelse(pew$business == '1', '0',
                           NA)))

# Redo the left right axis to get 0-2 categories
pew$axis_rec <- as.numeric(ifelse(pew$axis %in% c('0','1', '2'), '0',
                       ifelse(pew$axis == '3', '1',
                              ifelse(pew$axis %in% c('4', '5', '6'), '2', NA))))

# Create the populism index
# (combination of q26a and q27a)
pew$ordinary_people <- pew$Q26a
pew$ordinary_people_grade <- pew$Q26b
pew$elected_officials <- pew$Q27a
pew$elected_officials_grade <- pew$Q27b

pew$populist <- pew$ordinary_people == 1 & pew$elected_officials == 2
pew$remove_populist <- !(pew$ordinary_people %in% 1:2 & pew$elected_officials %in% 1:2)

# Recode country
pew$country <- as_factor(pew$country)

# Get spanish political party
pew$party_spain <- as_factor(pew$Q38ES)

# Flag how the PSOE is viewed
pew$view_psoe <- pew$Q20ESb
pew$view_cs <- pew$Q20ESd

# Define function for converting country
convert_country <- function(country, ca = FALSE){
  df <- data.frame(country = country)
  joiner <- data.frame(country = c('Denmark','France','Germany','Italy','Netherlands','Spain','Sweden','United Kingdom'),
                       country_ca = c('Dinamarca','França','Alemanya','Italia','Països\nBaixos','Espanya','Suècia','Regne\nUnit'),
                       country_en = c('Denmark','France','Germany','Italy','Netherlands','Spain','Sweden','United\nKingdom'))
  df <- left_join(df, joiner, by = 'country')
  if(ca){
    out <- as.character(df$country_ca)
  } else {
    out <- as.character(df$country_en)
  }
  return(out)
}

# Make a plot of populism by country
populism_plot <- function(ca = FALSE){
  if(ca){
    the_labs <- labs(x = '',
                     y = 'Percentatge',
                     title = "Percentatge de votants que són 'populistes'",
                     caption = paste0('Dades: Pew Research Center. https://www.pewresearch.org/global/dataset/fall-2017-media-and-politics-in-western-europe-survey-data\nGràfic: Joe Brew (@joethebrew). ',
                                      vilaweb::self_cite(),
                                      '\nwww.vilaweb.cat'))
  } else {
    the_labs <- labs(x = '',
                     y = 'Percentage',
                     title = "Percentage of voters that are 'populists'",
                     caption = paste0('Data: Pew Research Center. https://www.pewresearch.org/global/dataset/fall-2017-media-and-politics-in-western-europe-survey-data\nChart: Joe Brew (@joethebrew). ',
                                      vilaweb::self_cite(),
                                      '\nwww.vilaweb.cat'))
  }
  pd <- pew %>% filter(!remove_populist) %>%
    group_by(country = convert_country(country, ca = ca)) %>%
    summarise(n = length(which(populist)),
              d = n()) %>%
    ungroup %>%
    mutate(p = n / d * 100) %>%
    arrange(desc(p)) 
  pd$country <- factor(pd$country, levels = pd$country)
  ggplot(data = pd,
         aes(x = country,
             y = p)) +
    geom_bar(stat = 'identity',
             fill = vilaweb::colors_vilaweb()[4]) +
    theme_vilaweb() +
    the_labs +
    theme(plot.caption = element_text(size = 7)) +
    geom_text(aes(label = round(p, digits = 1)),
              nudge_y = -5,
              color = 'white',
              alpha = 0.8)
}

left_right_plot <- function(ca = FALSE){
  pd <- pew %>% filter(!axis %in% c('98','99')) %>%
    group_by(country = convert_country(country, ca = ca),
             axis_rec) %>%
    tally %>%
    group_by(country) %>%
    mutate(p = n / sum(n) * 100)
  
  joiner <- data.frame(axis_rec = 0:2,
                       val_en = c('Left', 'Center', 'Right'),
                       val_ca = c('Esquerra', 'Centre', 'Dreta'))
  joiner$val_en <- factor(joiner$val_en, levels = joiner$val_en)
  joiner$val_ca <- factor(joiner$val_ca, levels = joiner$val_ca)
  pd <- left_join(pd, joiner, by = 'axis_rec')
  if(ca){
    pd$val <- pd$val_ca
    the_labs <- labs(x = 'Ideologia esquerra-dreta',
                     y = 'Percentatge',
                     title = 'Ideologia per país (autoubicació)',
                     caption = paste0('Dades: Pew Research Center. https://www.pewresearch.org/global/dataset/fall-2017-media-and-politics-in-western-europe-survey-data\nGràfic: Joe Brew (@joethebrew). ',
                            vilaweb::self_cite(),
                            '\nwww.vilaweb.cat'))
  } else {
    pd$val <- pd$val_en
    the_labs <- labs(x = 'Left-right ideology (self-positioning)',
                     y = 'Percentage',
                     title = 'Ideology by country', 
                     caption = paste0('Data: Pew Research Center. https://www.pewresearch.org/global/dataset/fall-2017-media-and-politics-in-western-europe-survey-data\nChart: Joe Brew (@joethebrew). ',
                                      vilaweb::self_cite(),
                                      '\nwww.vilaweb.cat'))
  }
  
  ggplot(data = pd,
         aes(x = val,
             y = p)) +
    geom_bar(stat = 'identity',
             fill = vilaweb::colors_vilaweb()[4]) +
    facet_wrap(~country, ncol = 4,
               scales = 'free_x') +
    geom_text(aes(label = round(p, digits = 1)),
              nudge_y = -8,
              col = 'white') +
    theme_vilaweb() +
    the_labs +
    theme(axis.text.x = element_text(size = 8),
          plot.caption = element_text(size = 7))
}

axis_ideology_plot <- function(ca = FALSE){
  pd <- pew %>% filter(!is.na(axis_rec)) %>%
    group_by(country = convert_country(country, ca = ca),
             axis_rec,
             populist) %>%
    tally %>%
    ungroup %>%
    group_by(country, axis_rec) %>%
    summarise(nn = sum(n[which(populist)]),
              d = sum(n)) %>%
    ungroup %>%
    mutate(p = nn / d * 100) %>%
    arrange(desc(p)) 
  
  joiner <- data.frame(axis_rec = 0:2,
                       val_en = c('Left', 'Center', 'Right'),
                       val_ca = c('Esquerra', 'Centre', 'Dreta'))
  joiner$val_en <- factor(joiner$val_en, levels = joiner$val_en)
  joiner$val_ca <- factor(joiner$val_ca, levels = joiner$val_ca)
  pd <- left_join(pd, joiner, by = 'axis_rec')
  if(ca){
    pd$val <- pd$val_ca
    the_labs <- labs(x = 'Ideologia esquerra-dreta',
                     y = 'Percentatge',
                     title = 'Percentatge que és \'populista\' per ideologia',
                     caption = paste0('Dades: Pew Research Center. https://www.pewresearch.org/global/dataset/fall-2017-media-and-politics-in-western-europe-survey-data\nGràfic: Joe Brew (@joethebrew). ',
                                      vilaweb::self_cite(),
                                      '\nwww.vilaweb.cat'))
  } else {
    pd$val <- pd$val_en
    the_labs <- labs(x = 'Left-right ideology (self-positioning)',
                     y = 'Percentage',
                     title = 'Percent which is \'populist\' by ideology', 
                     caption = paste0('Data: Pew Research Center. https://www.pewresearch.org/global/dataset/fall-2017-media-and-politics-in-western-europe-survey-data\nChart: Joe Brew (@joethebrew). ',
                                      vilaweb::self_cite(),
                                      '\nwww.vilaweb.cat'))
  }
  
  ggplot(data = pd,
         aes(x = val,
             y = p)) +
    geom_bar(stat = 'identity',
             fill = vilaweb::colors_vilaweb()[4]) +
    facet_wrap(~country, ncol = 4,
               scales = 'free_x') +
    geom_text(aes(label = round(p, digits = 1)),
              nudge_y = -8,
              col = 'white') +
    theme_vilaweb() +
    the_labs +
    theme(axis.text.x = element_text(size = 8),
          plot.caption = element_text(size = 7))
}

psoe_plot <- function(ca = FALSE){
  pd <- pew %>% filter(!is.na(axis_rec),
                       party_spain %in% c('Spanish Socialist Workers’ Party (PSOE)',
                                          'People’s Party (PP)',
                                          'Podemos (We Can)',
                                          'Ciudadanos (C’s)'
                                          )) %>%
    mutate(party_spain = ifelse(grepl('Social', party_spain),
                                'PSOE',
                                ifelse(grepl('People', party_spain),
                                       'PP',
                                       ifelse(grepl('Podem', party_spain),
                                              'Podemos',
                                              ifelse(grepl('Ciudada',
                                                           party_spain),
                                                           'Cs',
                                                           NA))))) %>%
    group_by(party_spain, 
             axis_rec,
             populist) %>%
    tally %>%
    ungroup %>%
    group_by(party_spain) %>%
    mutate(p = n / sum(n) * 100) %>%
    arrange(desc(p)) 
  
  joiner <- data.frame(axis_rec = 0:2,
                       val_en = c('Left', 'Center', 'Right'),
                       val_ca = c('Esquerra', 'Centre', 'Dreta'))
  joiner$val_en <- factor(joiner$val_en, levels = joiner$val_en)
  joiner$val_ca <- factor(joiner$val_ca, levels = joiner$val_ca)
  pd <- left_join(pd, joiner, by = 'axis_rec')
  if(ca){
    pd$val <- pd$val_ca
    the_labs <- labs(x = 'Ideologia esquerra-dreta',
                     y = 'Percentatge',
                     title = 'Populisme i ideologia per partit espanyol',
                     caption = paste0('Dades: Pew Research Center. https://www.pewresearch.org/global/dataset/fall-2017-media-and-politics-in-western-europe-survey-data\nGràfic: Joe Brew (@joethebrew). ',
                                      vilaweb::self_cite(),
                                      '\nwww.vilaweb.cat'))
    pd$populist <- ifelse(pd$populist, 'Populista', 'No populista')
    
  } else {
    pd$val <- pd$val_en
    the_labs <- labs(x = 'Left-right ideology (self-positioning)',
                     y = 'Percentage',
                     title = 'Populism and ideology by Spanish party', 
                     caption = paste0('Data: Pew Research Center. https://www.pewresearch.org/global/dataset/fall-2017-media-and-politics-in-western-europe-survey-data\nChart: Joe Brew (@joethebrew). ',
                                      vilaweb::self_cite(),
                                      '\nwww.vilaweb.cat'))
    pd$populist <- ifelse(pd$populist, 'Populist', 'Mainstream')
  }
  
  ggplot(data = pd,
         aes(x = val,
             y = p,
             group = populist)) +
    geom_bar(stat = 'identity',
             aes(fill = populist),
             alpha = 0.6,
             position = position_dodge(width = 0.8)) +
    facet_wrap(~party_spain, ncol = 2,
               scales = 'free_x') +
    geom_text(aes(label = round(p, digits = 1),
                  y = p + 6),
              col = 'black',
              alpha = 0.6,
              size = 3,
              position = position_dodge(width = 0.8)) +
    theme_vilaweb() +
    the_labs +
    theme(axis.text.x = element_text(size = 8),
          plot.caption = element_text(size = 7)) +
    scale_fill_manual(name = '',
                      values = rev(c('darkorange', 'black')))
}




populism_party_plot <- function(ca = FALSE){
  pd <- pew %>% filter(!is.na(axis_rec),
                       party_spain %in% c('Spanish Socialist Workers’ Party (PSOE)',
                                          'People’s Party (PP)',
                                          'Podemos (We Can)',
                                          'Ciudadanos (C’s)'
                       )) %>%
    mutate(party_spain = ifelse(grepl('Social', party_spain),
                                'PSOE',
                                ifelse(grepl('People', party_spain),
                                       'PP',
                                       ifelse(grepl('Podem', party_spain),
                                              'Podemos',
                                              ifelse(grepl('Ciudada',
                                                           party_spain),
                                                     'Cs',
                                                     NA))))) %>%
    group_by(party_spain, 
             populist) %>%
    tally %>%
    ungroup %>%
    group_by(party_spain) %>%
    mutate(p = n / sum(n) * 100) %>%
    arrange(desc(p)) %>%
    filter(populist)
  
  if(ca){
    the_labs <- labs(x = 'Partit',
                     y = 'Percentatge',
                     title = 'Populisme per partit espanyol',
                     caption = paste0('Dades: Pew Research Center. https://www.pewresearch.org/global/dataset/fall-2017-media-and-politics-in-western-europe-survey-data\nGràfic: Joe Brew (@joethebrew). ',
                                      vilaweb::self_cite(),
                                      '\nwww.vilaweb.cat'))
    pd$populist <- ifelse(pd$populist, 'Populista', 'No populista')
    
  } else {
    the_labs <- labs(x = 'Party',
                     y = 'Percentage',
                     title = 'Populism by Spanish party', 
                     caption = paste0('Data: Pew Research Center. https://www.pewresearch.org/global/dataset/fall-2017-media-and-politics-in-western-europe-survey-data\nChart: Joe Brew (@joethebrew). ',
                                      vilaweb::self_cite(),
                                      '\nwww.vilaweb.cat'))
    pd$populist <- ifelse(pd$populist, 'Populist', 'Mainstream')
  }
  
  ggplot(data = pd,
         aes(x = party_spain,
             y = p)) +
    geom_bar(stat = 'identity',
             fill = vilaweb::colors_vilaweb()[3],
             alpha = 0.6,
             position = position_dodge(width = 0.8)) +
    geom_text(aes(label = round(p, digits = 1),
                  y = p -4),
              col = 'white',
              alpha = 0.9,
              position = position_dodge(width = 0.8)) +
    theme_vilaweb() +
    the_labs +
    theme(plot.caption = element_text(size = 7)) 
}




psoe_plot2 <- function(ca = FALSE){
  pd <- pew %>% filter(!is.na(axis_rec),
                       !party_spain %in% c('Spanish Socialist Workers’ Party (PSOE)'),
                       !view_psoe %in% c('98', '99'),
                       !is.na(view_psoe)) %>%
    group_by(axis_rec,
             view_psoe) %>%
    tally %>%
    ungroup %>%
    group_by(axis_rec) %>%
    mutate(p = n / sum(n) * 100) %>%
    arrange(desc(p)) 
  
  joiner <- data.frame(axis_rec = 0:2,
                       val_en = c('Left', 'Center', 'Right'),
                       val_ca = c('Esquerra', 'Centre', 'Dreta'))
  joiner$val_en <- factor(joiner$val_en, levels = joiner$val_en)
  joiner$val_ca <- factor(joiner$val_ca, levels = joiner$val_ca)
  pd <- left_join(pd, joiner, by = 'axis_rec')
  
  pd$view_psoe <- as_factor(pd$view_psoe)
  
  if(ca){
    levs <- levels(pd$view_psoe)
    labels <- gsub('Very', 'Molt', levs)
    labels <- gsub('Somewhat favorable', 'Favorable', labels)
    labels <- gsub('Somewhat un', 'Poc ', labels)
    labels <- gsub('unfavor', 'poc favor', labels)
    pd$view_psoe <- factor(pd$view_psoe, levels = levs, 
                         labels = labels)
    
    pd$val <- pd$val_ca
    the_labs <- labs(x = 'Ideologia esquerra-dreta del enquestat',
                     y = 'Percentatge',
                     title = 'Opinió del PSOE entre votants d\'altres partits',
                     subtitle = '(Per ideologia)',
                     caption = paste0('Dades: Pew Research Center. https://www.pewresearch.org/global/dataset/fall-2017-media-and-politics-in-western-europe-survey-data\nGràfic: Joe Brew (@joethebrew). ',
                                      vilaweb::self_cite(),
                                      '\nwww.vilaweb.cat'))

  } else {
    pd$val <- pd$val_en
    the_labs <- labs(x = 'Participant\'s left-right ideology (self-positioning)',
                     y = 'Percentage',
                     subtitle = '(By ideology)',
                     title = 'Opinion of the PSOE by voters of other parties', 
                     caption = paste0('Data: Pew Research Center. https://www.pewresearch.org/global/dataset/fall-2017-media-and-politics-in-western-europe-survey-data\nChart: Joe Brew (@joethebrew). ',
                                      vilaweb::self_cite(),
                                      '\nwww.vilaweb.cat'))
  }
  
  ggplot(data = pd,
         aes(x = val,
             y = p,
             group = view_psoe)) +
    geom_bar(stat = 'identity',
             aes(fill = view_psoe),
             alpha = 0.9,
             position = position_dodge(width = 0.8)) +
    geom_text(aes(label = round(p, digits = 1),
                  y = p + 3),
              col = 'black',
              alpha = 0.6,
              size = 3,
              position = position_dodge(width = 0.8)) +
    theme_vilaweb() +
    the_labs +
    theme(legend.text = element_text(size = 8),
          plot.caption = element_text(size = 7)) +
    scale_fill_manual(name = '',
                      values = RColorBrewer::brewer.pal(n = 4, 'Spectral'))
}


pp_cs_plot <- function(ca = FALSE){
  pd <- pew %>% filter(party_spain == 'People’s Party (PP)',
                       !view_cs %in% c('98', '99'),
                       !is.na(view_cs)) %>%
    group_by(view_cs) %>%
    tally %>%
    ungroup %>%
    mutate(p = n / sum(n) * 100) %>%
    arrange(desc(p)) 
  
  pd$view_cs <- as_factor(pd$view_cs)
  
  if(ca){
    levs <- levels(pd$view_cs)
    labels <- gsub('Very', 'Molt', levs)
    labels <- gsub('Somewhat favorable', 'Favorable', labels)
    labels <- gsub('Somewhat un', 'Poc ', labels)
    labels <- gsub('unfavor', 'poc favor', labels)
    pd$view_cs <- factor(pd$view_cs, levels = levs, 
                         labels = labels)
    the_labs <- labs(x = 'Opinió',
                     y = 'Percentatge',
                     title = 'Opinió de Cs entre votants del PP',
                     caption = paste0('Dades: Pew Research Center. https://www.pewresearch.org/global/dataset/fall-2017-media-and-politics-in-western-europe-survey-data\nGràfic: Joe Brew (@joethebrew). ',
                                      vilaweb::self_cite(),
                                      '\nwww.vilaweb.cat'))
    
  } else {
    the_labs <- labs(x = 'Opinion',
                     y = 'Percentage',
                     title = 'Opinion of Cs by voters of PP', 
                     caption = paste0('Data: Pew Research Center. https://www.pewresearch.org/global/dataset/fall-2017-media-and-politics-in-western-europe-survey-data\nChart: Joe Brew (@joethebrew). ',
                                      vilaweb::self_cite(),
                                      '\nwww.vilaweb.cat'))
  }
  
  ggplot(data = pd,
         aes(x = view_cs,
             y = p,
             group = view_cs)) +
    geom_bar(stat = 'identity',
             aes(fill = view_cs),
             alpha = 0.9,
             position = position_dodge(width = 0.8)) +
    geom_text(aes(label = round(p, digits = 1),
                  y = p + 3),
              col = 'black',
              alpha = 0.6,
              size = 5,
              position = position_dodge(width = 0.8)) +
    theme_vilaweb() +
    the_labs +
    theme(legend.position = 'none',
          plot.caption = element_text(size = 7)) +
    scale_fill_manual(name = '',
                      values = RColorBrewer::brewer.pal(n = 4, 'Spectral'))
}


# # Remove those with no axis and no populist
# pew <- pew %>% filter(!axis %in% c('98','99'),
#                       ordinary_people %in% 1:2,
#                       elected_officials %in% 1:2)