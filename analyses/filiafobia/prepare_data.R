# Libraries
library(vilaweb)
library(tidyverse)
library(lubridate)
library(databrew)
library(tidylog)
options(scipen = 999)

plot_twitter <- function(ca = TRUE){
  pd <- fobia %>% filter(key == 'Hispanofòbia') %>%
    group_by(date = as.Date(cut(date, 'month'))) %>% 
    summarise(n = n(),
              rt = sum(retweets_count))
  if(ca){
    the_labs <- labs(x = 'Mes',
                     y = 'Piulets (escala logarítmica)',
                     title = "Piulets amb la paraula de 'hispanofòbia'",
                     subtitle = 'Dades de Twitter, incloent-hi repiulets',
                     caption = 'Gràfic de Joe Brew, @joethebrew')
  } else {
    the_labs <- labs(x = 'Month',
                     y = 'Piulets (log scale)',
                     title = "Tweets with the word 'Hispanophobia'",
                     subtitle = 'Data from Twitter, including retweets',
                     caption = 'Chart by Joe Brew, @joethebrew.')
  }
  ggplot(data = pd,
         aes(x = date,
             y = rt + n)) +
    geom_line() +
    # geom_path(color = 'darkorange', alpha = 0.6) +
    theme_simple() +
    scale_y_log10() +
    the_labs
}

plot_google <- function(ca = TRUE){
  # Read in google trends data (retrieved 23 jan 2020)
  google <- read_csv('multiTimeline.csv', skip = 1)
  google$date <- as.Date(paste0(google$Month, '-01'))
  # google$date <- as.Date(cut(google$date, 'year'))
  google$value <- google$`hispanofobia: (Spain)`
  if(ca){
    the_labs <- labs(x = 'Mes',
                     y = 'Percentatge (escala logarítmica)',
                     title = "Cerques de 'hispanofòbia'",
                     subtitle = 'Dades de Google Trends',
                     caption = 'Percentatge: cerques mensuals relatives al màxim. Gràfic de Joe Brew, @joethebrew')
  } else {
    the_labs <- labs(x = 'Month',
                     y = 'Percentage (log scale)',
                     title = "Searches for 'Hispanophobia'",
                     subtitle = 'Data from Google Trends',
                     caption = 'Percentage: monthly searches relative to the maximum. Chart by Joe Brew, @joethebrew.')
  }
  pd <- google %>%
    group_by(date) %>%
    summarise(value = mean(value))
  ggplot(data = pd,
         aes(x = date,
             y = value)) +
    geom_line() +
    # geom_path(color = 'darkorange', alpha = 0.6) +
    theme_simple() +
    scale_y_log10(breaks = c(0:3, 5,10,25, 50, 100)) +
    the_labs
}

# Read in the territorial survey data
pt <- vilaweb::ceo_percepcio_territorial

# Function for cleaning up the sympathy variable
simpatia_cleaner <- function(x){
  x <- as.character(x)
  x <- ifelse(x %in% c('No contesta', 'No ho sap'), NA,
              ifelse(x == 'Us cauen molt bé 10', '10',
                     ifelse(x == 'No us cauen ni bé ni malament 5', 5,
                            ifelse(x == 'Us cauen molt malament 0', '0',
                                   x))))
  x <- as.numeric(x)
  return(x)
}

pt <- pt %>%
  mutate(visits = `I d’aquestes, en concret, em podria dir quantes vegades a viatjat a Catalunya?`,
         simpatia = `Grau de simpatia cap als habitants de cadascuna de les diferents comunitats autònomes:  Catalans`,
         referendum = `Grau d'acord: S’hauria de fer un referèndum a Catalunya perquè els catalans i les catalanes decidissin quina relació volen que hi hagi entre Catalunya i l’Estat espanyol`) %>%
  mutate(simpatia = simpatia_cleaner(simpatia)) %>%
  mutate(ccaa = `Comunitat autònoma`)
  
# # Companeros de trabajo
# pt$`Amb independència que treballeu o no, quins són els tres que menys us agradaria tenir com a companys de feina o treball? Catalans`
# 
# # Grau de simpatia
# pt$`Grau de simpatia cap als habitants de cadascuna de les diferents comunitats autònomes:  Catalans`


# Define the label sets (for factor ordering)
label_df <- tibble(
  ccaa = c('Andalusia',
           'Aragó',
           'Astúries',
           'Balears',
           'Canàries',
           'Cantàbria',
           'Castella la Manxa',
           'Castella i Lleó',
           'Catalunya',
           'País Valencià',
           'Extremadura',
           'Galícia',
           'Madrid',
           'Múrcia',
           'Navarra',
           'Euskadi/País Basc',
           'La Rioja', 'Ceuta', 'Melilla'),
  key = c('Andalusos',
          'Aragonesos',
          'Asturians',
          'Balears',
          'Canaris',
          'Càntabres',
          'Castellanomanxecs',
          'Castellanolleonesos',
          'Catalans',
          'Valencians',
          'Extremenys',
          'Gallecs',
          'Madrilenys',
          'Murcians',
          'Navarresos',
          'Bascos',
          'Riojans',
          'Ceutencs',
          'Melillencs')
)
fix_levels <- function(var, from_place = FALSE, to_place = TRUE){
  if(from_place){
    x <- tibble(ccaa = var)
  } else {
    x <- tibble(key = var)
  }
  # Join with the labels
  joined <- left_join(x, label_df)
  # Get the return val
  if(to_place){
    out <- joined$ccaa
    out <- factor(out, levels = label_df$ccaa)
  } else {
    out <- joined$key
    out <- factor(out, levels = label_df$key)
  }
  return(out)
}


plot_viatges_i_simpatia <- function(ca = TRUE){
  # Viatges a Cat i favorabilitat e Cats
  pd <- pt %>%
    filter(ccaa != 'Catalunya') %>%
    filter(!is.na(visits)) %>%
    filter(!visits %in% c('No ho sap', 'No contesta')) %>%
    group_by(visits) %>%
    summarise(simpatia = mean(simpatia, na.rm = TRUE),
              n = n()) 
  pd$visits <- factor(pd$visits,
                      levels = unique(c('No, mai',
                                        levels(pd$visits))))
  pd$visits <- factor(pd$visits,
                      levels = levels(pd$visits),
                      labels = gsub(' vegad', '\nvegad', c('Mai', levels(pd$visits)[2:length(levels(pd$visits))])))
  if(!ca){
    pd$visits <- factor(pd$visits,
                        labels = c('Never', 'Just\nonce', 
                                   '2-5 times',
                                   '6-10 times',
                                   'More than\n10 times'))
    the_labs <- labs(x = 'How many times have you travelled to Catalonia?',
                     y = 'Degree of sympathy (0-10) for\nthe residents of Catalonia',
                     title = "Times travelled to Catalonia and\nfeelings about Catalans",
                     caption = "Data source: Survey 'Percepció sobre el debat territorial a Espanya' from the Centre d'Estudis d'Opinió.\nChart: @joethebrew") 
  } else {
    the_labs <- labs(x = 'Quantes vegades ha viatjat a Catalunya?',
                     y = 'Grau de simpatia (0-10) cap\nals habitants de Catalunya',
                     title = "Vegades que ha viatjat a Catalunya i\ngrau de simpatia cap als catalans",
                     caption = "Font de dades: Enquesta 'Percepció sobre el debat territorial a Espanya' del Centre d'Estudis d'Opinió.\nGràfic: @joethebrew") 
  }
  
  library(databrew)
  ggplot(data = pd,
         aes(x = visits,
             y = simpatia)) +
    # geom_bar(stat = 'identity') +
    # geom_area(alpha = 0.5, color = NA, fill = 'darkorange', group = 1) +
    geom_point(size = 10) +
    geom_segment(aes(xend = visits,
                     yend = 5),
                 alpha = 0.8,
                 # size = 2,
                 lty = 3) +
    geom_line(alpha = 0.2, group = 1) +
    geom_text(aes(label = round(simpatia, digits = 1)),
              # nudge_y = 0.2,
              # alpha = 0.8,
              color = 'white',
              size = 3.6) +
    the_labs +
    theme_simple(black = T, base_size = 16) +
    theme(plot.title = element_text(size = 22)) +
    ylim(5, 7.2)
}


plot_viatges_i_odi <- function(ca = TRUE){
  # Viatges a Cat i favorabilitat e Cats
  pd <- pt %>%
    filter(ccaa != 'Catalunya') %>%
    filter(!is.na(visits)) %>%
    filter(!visits %in% c('No ho sap', 'No contesta')) %>%
    group_by(visits) %>%
    summarise(avg = mean(simpatia, na.rm = TRUE),
              n = n(),
              n_no_na = length(which(!is.na(simpatia))),
              n0 = length(which(simpatia == 0))) %>%
    mutate(p0 = n0 / n_no_na * 100)
  pd$visits <- factor(pd$visits,
                      levels = unique(c('No, mai',
                                        levels(pd$visits))))
  pd$visits <- factor(pd$visits,
                      levels = levels(pd$visits),
                      labels = gsub(' vegad', '\nvegad', c('Mai', levels(pd$visits)[2:length(levels(pd$visits))])))
  if(!ca){
    pd$visits <- factor(pd$visits,
                        labels = c('Never', 'Just\nonce', 
                                   '2-5 times',
                                   '6-10 times',
                                   'More than\n10 times'))
    the_labs <- labs(x = 'How many times have you travelled to Catalonia?',
                     y = "Percentage who say 'I don't like them at all'",
                     title = "Extreme dislike of Catalans among Spaniards\nand times travelled to Catalonia",
                     caption = "Data source: Survey 'Percepció sobre el debat territorial a Espanya' from the Centre d'Estudis d'Opinió.\nChart: @joethebrew") 
  } else {
    the_labs <- labs(x = 'Quantes vegades ha viatjat a Catalunya?',
                     y = "Percentatge qui diu que els catalans\n'em cauen molt malament'",
                     title = "Aversió extrema dels catalans i\nvegades que ha viatjat a Catalunya",
                     caption = "Font de dades: Enquesta 'Percepció sobre el debat territorial a Espanya' del Centre d'Estudis d'Opinió.\nGràfic: @joethebrew") 
  }
  
  library(databrew)
  ggplot(data = pd,
         aes(x = visits,
             y = p0)) +
    # geom_bar(stat = 'identity') +
    # geom_area(alpha = 0.5, color = NA, fill = 'darkorange', group = 1) +
    geom_point(size = 10) +
    geom_segment(aes(xend = visits,
                     yend = 5),
                 alpha = 0.8,
                 # size = 2,
                 lty = 3) +
    geom_line(alpha = 0.2, group = 1) +
    geom_text(aes(label = round(p0, digits = 1)),
              # nudge_y = 0.2,
              # alpha = 0.8,
              color = 'white',
              size = 3.6) +
    the_labs +
    theme_simple(black = T, base_size = 16) +
    theme(plot.title = element_text(size = 22)) +
    ylim(3.5, 12)
}


plot_support_referendum <- function(ca = FALSE){
  # Viatges a Cat i suport pel referendum
  pd <- pt %>%
    filter(`Comunitat autònoma` != 'Catalunya') %>%
    mutate(visits = `I d’aquestes, en concret, em podria dir quantes vegades a viatjat a Catalunya?`) %>%
    filter(!is.na(visits)) %>%
    filter(!visits %in% c('No ho sap', 'No contesta')) %>%
    mutate(referendum = as.character(referendum)) %>%
    mutate(referendum = ifelse(referendum %in% c('Nc', 'Nhs'),
                               'NS/NC', referendum)) %>%
    group_by(visits, referendum) %>%
    tally %>%
    group_by(visits) %>%
    mutate(p = n / sum(n) * 100) %>%
    ungroup 
  pd$visits <- factor(pd$visits,
                      levels = unique(c('No, mai',
                                        levels(pd$visits))))
  pd$visits <- factor(pd$visits,
                      levels = levels(pd$visits),
                      labels = gsub(' vegad', '\nvegad', c('Mai', levels(pd$visits)[2:length(levels(pd$visits))])))
  pd$referendum <- as.character(pd$referendum)
  pd$referendum <- ifelse(pd$referendum %in% c('Nhs', 'Nc'),
                          'NS/NC', pd$referendum)
  pd$referendum <- factor(pd$referendum,
                          levels = c("Molt d’acord",
                                     "Més aviat d’acord",
                                     "NS/NC",
                                     "Ni d’acord ni en desacord",
                                     "Més aviat en desacord",
                                     "Molt en desacord"))
  pd$referendum <- factor(pd$referendum,
                          levels = rev(levels(pd$referendum)))
  
  if(!ca){
    pd$visits <- factor(pd$visits,
                        labels = c('Never', 'Just\nonce', 
                                   '2-5 times',
                                   '6-10 times',
                                   'More than\n10 times'))
    pd$referendum <- factor(pd$referendum,
                            labels = rev(c("Strongly agree",
                                       "Agree",
                                       "Don't know/No answer",
                                       "Neither agree nor disagree",
                                       "Disagree",
                                       "Strongly disagree")))
    the_labs <- labs(x = 'How many times have you travelled to Catalonia?',
                     y = 'Percentage',
                     title = "Times travelled to Catalonia\nand support for  referendum",
                     subtitle = "Agreement with phrase: 'A referendum should take place so Catalans\ncan decide the relationship they want between Catalonia and the Spanish State'",
                     caption = "Data source: survey 'Percepció sobre el debat territorial a Espanya' from the Centre d'Estudis d'Opinió.\nChart: @joethebrew")
    legend_title <- ''
  } else {
    the_labs <- 
      labs(x = 'Quantes vegades ha viatjat a Catalunya?',
           y = 'Percentatge',
           title = "Vegades que ha viatjat a Catalunya i\nsuport al referèndum",
           subtitle = "Grau d'acord amb la frase: 'S'hauria de fer un referèndum a Catalunya perquè els catalans i les\ncatalanes decidissin quina relació volen que hi hagi entre Catalunya i l'Estat espanyol'",
           caption = "Font de dades: Enquesta 'Percepció sobre el debat territorial a Espanya'. Centre d'Estudis d'Opinió.\nGràfic: @joethebrew")
    legend_title <- ""
  }
  
  
  library(databrew)
  library(RColorBrewer)
  cols <- brewer.pal(n = 6, 'Spectral')
  cols[3] <- grey(c(0.5))
  ggplot(data = pd,
         aes(x = visits,
             y = p)) +
    geom_bar(stat = 'identity',
             aes(fill = referendum),
             alpha = 0.9) +
    # geom_text(aes(label = round(simpatia, digits = 1)),
    #           # nudge_y = 0.2,
    #           # alpha = 0.8,
    #           color = 'white',
    #           size = 3.6) +
    the_labs +
    theme_simple(black = T, base_size = 16) +
    theme(plot.title = element_text(size = 22),
          plot.caption = element_text(hjust = 0),
          axis.text.x = element_text(size = 8),
          plot.subtitle = element_text(size = 10)) +
    theme(legend.position = 'right') +
    scale_fill_manual(name = legend_title,
                      values = cols) +
    geom_hline(yintercept = 50, lty = 2, alpha = 0.7)
}




# Grau de simpatia i suport pel referendum
plot_simpatia_referendum <- function(ca = TRUE){
  pd <- pt %>%
    filter(`Comunitat autònoma` != 'Catalunya') %>%
    mutate(simpatia = `Grau de simpatia cap als habitants de cadascuna de les diferents comunitats autònomes:  Catalans`) %>%
    mutate(simpatia = simpatia_cleaner(simpatia)) %>%
    mutate(referendum = as.character(referendum)) %>%
    mutate(referendum = ifelse(referendum %in% c('Nc', 'Nhs'),
                               'NS/NC', referendum)) %>%
    group_by(referendum, simpatia) %>%
    tally %>%
    group_by(simpatia) %>%
    mutate(p = n / sum(n) * 100) %>%
    ungroup
  
  pd$referendum <- as.character(pd$referendum)
  pd$referendum <- ifelse(pd$referendum %in% c('Nhs', 'Nc'),
                          'NS/NC', pd$referendum)
  pd$referendum <- factor(pd$referendum,
                          levels = c("Molt d’acord",
                                     "Més aviat d’acord",
                                     "NS/NC",
                                     "Ni d’acord ni en desacord",
                                     "Més aviat en desacord",
                                     "Molt en desacord"))
  pd$referendum <- factor(pd$referendum,
                          levels = rev(levels(pd$referendum)))
  
  if(!ca){
    pd$referendum <- factor(pd$referendum,
                            labels = c("Strongly agree",
                                       "Agree",
                                       "Don't know/No answer",
                                       "Neither agree nor disagree",
                                       "Disagree",
                                       "Strongly disagree"))
    the_labs <- labs(x = 'How much do you like Catalans (0-10)?',
                     y = 'Percentage',
                     title = "Sympathy/antipathy for Catalans\nand support for  referendum",
                     subtitle = "Agreement with phrase: 'A referendum should take place so Catalans\ncan decide the relationship they want between Catalonia and the Spanish State'",
                     caption = "Data source: survey 'Percepció sobre el debat territorial a Espanya' from the Centre d'Estudis d'Opinió.\nChart: @joethebrew")
    legend_title <- ''
  } else {
    the_labs <- labs(x = 'Grau de simpatia (escala 0 a 10)',
         y = 'Percentatge',
         title = "Grau de simpatia pels catalans i\nsuport al referèndum",
         subtitle = "Grau d'acord amb la frase: 'S'hauria de fer un referèndum a Catalunya perquè els catalans i les\ncatalanes decidissin quina relació volen que hi hagi entre Catalunya i l'Estat espanyol'",
         caption = "Font de dades: Enquesta 'Percepció sobre el debat territorial a Espanya. Centre d'Estudis d'Opinió.\nGràfic: @joethebrew") 
  }
  
  
  
  cols <- brewer.pal(n = 6, 'Spectral')
  cols[3] <- grey(c(0.5))
  ggplot(data = pd,
         aes(x = simpatia,
             y = p)) +
    geom_bar(stat = 'identity',
             aes(fill = referendum),
             alpha = 0.9) +
    # geom_text(aes(label = round(simpatia, digits = 1)),
    #           # nudge_y = 0.2,
    #           # alpha = 0.8,
    #           color = 'white',
    #           size = 3.6) +
    the_labs +
    theme_simple(black = T, base_size = 16) +
    theme(plot.title = element_text(size = 22),
          plot.caption = element_text(hjust = 0),
          axis.text.x = element_text(size = 8),
          plot.subtitle = element_text(size = 10)) +
    theme(legend.position = 'right') +
    scale_fill_manual(name = '',
                      values = cols) +
    geom_hline(yintercept = 50, lty = 2, alpha = 0.7) +
    scale_x_continuous(breaks = 0:10)
}



plot_que_es_catalunya <- function(){
  # Que és Catalunya
  pd <- pt %>%
    filter(`Comunitat autònoma` != 'Catalunya') %>%
    mutate(visits = `I d’aquestes, en concret, em podria dir quantes vegades a viatjat a Catalunya?`) %>%
    filter(!is.na(visits)) %>%
    filter(!visits %in% c('No ho sap', 'No contesta')) %>%
    mutate(what_is_cat = as.character(`I parlant de Catalunya, quin terme preferiu per a referir-vos a Catalunya?`)) %>%
    mutate(what_is_cat = ifelse(what_is_cat %in% c('No ho sap', 'No contesta', 'Altres (especificar)'), 'NS/NC/Altres', what_is_cat)) %>%
    group_by(visits, what_is_cat) %>%
    tally %>%
    group_by(visits) %>%
    mutate(p = n / sum(n) * 100) %>%
    ungroup 
  pd$visits <- factor(pd$visits,
                      levels = unique(c('No, mai',
                                        levels(pd$visits))))
  pd$visits <- factor(pd$visits,
                      levels = levels(pd$visits),
                      labels = gsub(' vegad', '\nvegad', c('Mai', levels(pd$visits)[2:length(levels(pd$visits))])))
  
  pd$what_is_cat <- factor(pd$what_is_cat,
                           levels = c("Un país",
                                      "Una nació",
                                      "Una nacionalitat",
                                      "NS/NC/Altres",
                                      "Una comunitat autònoma",
                                      "Una regió"))
  pd$what_is_cat <- factor(pd$what_is_cat, levels = rev(levels(pd$what_is_cat)))
  
  library(databrew)
  library(RColorBrewer)
  cols <- brewer.pal(n = 6, 'Spectral')
  # cols[2:3] <- 'white'
  cols[2:3] <- grey(c(0.4, 0.7))
  ggplot(data = pd,
         aes(x = visits,
             y = p)) +
    geom_bar(stat = 'identity',
             aes(fill = what_is_cat),
             alpha = 0.9) +
    # geom_text(aes(label = round(simpatia, digits = 1)),
    #           # nudge_y = 0.2,
    #           # alpha = 0.8,
    #           color = 'white',
    #           size = 3.6) +
    labs(x = 'Quantes vegades ha viatjat a Catalunya?',
         y = 'Percentatge',
         title = "Vegades que ha viatjat a Catalunya i\nopinió sobre què és Catalunya",
         subtitle = "xxxyyyzzz",
         caption = "Font de dades: Enquesta 'Percepció sobre el debat territorial a Espanya. Centre d'Estudis d'Opinió.\n. Gràfic: @joethebrew") +
    theme_simple(black = T, base_size = 16) +
    theme(plot.title = element_text(size = 22),
          plot.caption = element_text(hjust = 0),
          axis.text.x = element_text(size = 8),
          plot.subtitle = element_text(size = 10)) +
    theme(legend.position = 'right') +
    scale_fill_manual(name = "Grau d'acord",
                      values = cols) +
    geom_hline(yintercept = 50, lty = 2, alpha = 0.7)
}



# Grau de simpatia cap als habitants
plot_simpatia_matrix <- function(ca = FALSE, var = 'avg', roundy = 1, reverse_color = FALSE, text_size = 4){
  library(RColorBrewer)
  pd <- pt %>%
    mutate(weight = `Coeficients de ponderació`) %>%
    mutate(ccaa = `Comunitat autònoma`) %>%
    dplyr::select(contains('Grau de simpatia cap als habitants'),
                  contains('ccaa'),
                  weight)
  names(pd) <- gsub("Grau de simpatia cap als habitants de cadascuna de les diferents comunitats autònomes:  ", '', names(pd))
  # Make long
  pd <- pd %>% gather(key, value, Andalusos:Ceutencs)
  pd$value <- simpatia_cleaner(pd$value)
  pda <- pd
  # Now agg and get averages
  pd <- pd %>%
    filter(!ccaa %in% c('Ceuta', 'Melilla'),
           !key %in% c('Ceutencs')) %>%
    group_by(ccaa, key) %>%
    summarise(n = n(),
              n_no_na = length(which(!is.na(value))),
              avg = mean(value, na.rm = TRUE),
              avgx = weighted.mean(x = value, w = weight, na.rm = TRUE),
              p75 = quantile(value, 0.75, na.rm = TRUE),
              p25 = quantile(value, 0.25, na.rm = TRUE),
              n_zero = length(which(value == 0)),
              n1 = length(which(value <= 1)),
              n2 = length(which(value <= 2)),
              n3 = length(which(value <= 3)),
              n4 = length(which(value <=4)),
              n5 = length(which(value <= 5)),
              n6 = length(which(value <= 6)),
              n10 = length(which(value == 10))) %>%
    mutate(p0 = n_zero / n_no_na * 100,
           p1 = n1 / n_no_na * 100,
           p2 = n2 / n_no_na * 100,
           p3 = n3 / n_no_na * 100,
           p4 = n4 / n_no_na * 100,
           p5 = n5 / n_no_na * 100,
           p10 = n10 / n_no_na * 100)
  pd <- pd %>% ungroup %>%
    mutate(gent_de = fix_levels(ccaa, from_place = TRUE, to_place = TRUE),
           cap_a = fix_levels(key, from_place = FALSE, to_place = TRUE))
  
  if(ca){
    the_labs <- labs(x = 'Grau de simpatia de les persones d\'aqui',
                     y = '...cap a les persones d\'aqui',
                     title = 'Grau de simpatia cap als habitants dels\nhabitants segons comunitat autònoma',
                     caption = "Font de dades: Enquesta 'Percepció sobre el debat territorial a Espanya' del Centre d'Estudis d'Opinió.\nGràfic: @joethebrew")
  } else {
    the_labs <- labs(x = 'How people from here feel...',
                     y = '...about people from here',
                     title = "How people from one area feel\nabout people from another area",
                     caption = "Data source: Survey 'Percepció sobre el debat territorial a Espanya' from the Centre d'Estudis d'Opinió.\nChart: @joethebrew")
  }
  
  # Define the variable
  pd$y <- as.numeric(unlist(pd[,var] ))
  
  # Colors
  cols <-  colorRampPalette(brewer.pal(n = 9, name = 'Spectral'))(10)
  if(reverse_color){
    cols <- rev(cols)
  }
  
  ggplot(data = pd,
         aes(x = gent_de,
             y = cap_a)) +
    geom_point(size = 7.5,
               aes(color = y)) +
    # geom_point(size = 5,
    #            aes(#size = avg,
    #              # color = p2
    #                # color = avg
    #                )) +
    # ggthemes::theme_fivethirtyeight() +
    scale_color_gradientn(colours = cols) +
    the_labs +
    theme_simple() +
    theme(axis.text.x = element_text(angle = 90,
                                     hjust = 1)) +
    geom_text(aes(label = round(y, digits = roundy)), size = text_size) +
    theme(legend.position = 'none')
}

# Get average
get_avg <- function(where_from = 'Catalunya',
                    var = 'avg',
                    where_to = label_df$ccaa[label_df$ccaa != 'Catalunya']){
  pd <- pt %>%
    dplyr::select(contains('Grau de simpatia cap als habitants'),
                  contains('ccaa'),
                  contains('weight'))
  names(pd) <- gsub("Grau de simpatia cap als habitants de cadascuna de les diferents comunitats autònomes:  ", '', names(pd))
  # Make long
  pd <- pd %>% gather(key, value, Andalusos:Ceutencs)
  pd$value <- simpatia_cleaner(pd$value)
  # Now agg and get averages
 pd <- pd %>%
      # filter(!ccaa %in% c('Ceuta', 'Melilla'),
      #        !key %in% c('Ceutencs')) %>%
      group_by(ccaa, key) %>%
      summarise(n = n(),
                n_no_na = length(which(!is.na(value))),
                avg = mean(value, na.rm = TRUE),
                p75 = quantile(value, 0.75, na.rm = TRUE),
                p25 = quantile(value, 0.25, na.rm = TRUE),
                n_zero = length(which(value == 0)),
                n1 = length(which(value <= 1)),
                n2 = length(which(value <= 2)),
                n3 = length(which(value <= 3)),
                n4 = length(which(value <=4)),
                n5 = length(which(value <= 5)),
                n6 = length(which(value <= 6)),
                n10 = length(which(value == 10))) %>%
      mutate(p0 = n_zero / n_no_na * 100,
             p1 = n1 / n_no_na * 100,
             p2 = n2 / n_no_na * 100,
             p3 = n3 / n_no_na * 100,
             p4 = n4 / n_no_na * 100,
             p5 = n5 / n_no_na * 100,
             p10 = n10 / n_no_na * 100)
  
  pd <- pd %>%
    mutate(gent_de = fix_levels(ccaa, from_place = TRUE, to_place = TRUE),
           cap_a = fix_levels(key, from_place = FALSE, to_place = TRUE))
  
  pd$y <- as.numeric(unlist(pd[,var]))
  
    pd %>% ungroup %>%
      filter(gent_de %in% where_from,
             cap_a %in% where_to) %>%
      summarise(avg = mean(y, na.rm = T)) %>%
      .$avg
}



# For each ccaa, what do the people there think of catalans compared to what catalans think of them
plot_compare <- function(ca = TRUE){
  pd <- pt %>%
    dplyr::select(contains('Grau de simpatia cap als habitants'),
                  contains('ccaa'),
                  contains('weight'))
  names(pd) <- gsub("Grau de simpatia cap als habitants de cadascuna de les diferents comunitats autònomes:  ", '', names(pd))
  # Make long
  pd <- pd %>% gather(key, value, Andalusos:Ceutencs)
  pd$value <- simpatia_cleaner(pd$value)
  # Rename
  pd <- pd %>%
    mutate(gent_de = fix_levels(ccaa, from_place = TRUE, to_place = TRUE),
           cap_a = fix_levels(key, from_place = FALSE, to_place = TRUE)) %>%
    dplyr::select(gent_de,
                  cap_a, value) %>%
    group_by(gent_de, cap_a) %>%
    summarise(value = mean(value, na.rm = TRUE))
  
  # Keep only what people think about catalans or vice-versa
  pd <- pd %>%
    filter(gent_de == 'Catalunya' | cap_a == 'Catalunya')
  
  # Define the ccaa
  pd$cap_a <- as.character(pd$cap_a); pd$gent_de <- as.character(pd$gent_de)
  pd$ccaa <- ifelse(pd$cap_a == 'Catalunya', pd$gent_de,
                    ifelse(pd$gent_de == 'Catalunya', pd$cap_a,
                           NA))
  pd <- pd %>%
    filter(!(cap_a == 'Catalunya' & gent_de == 'Catalunya')) %>%
    filter(!ccaa %in% c('Ceuta', 'Melilla'))
  
  
  # Make a new var
  if(ca){
    levs <- c('Valoració mitjana dels habitants d\'aquesta\ncomunitat autònoma cap als catalans',
              'Valoració mitjana dels catalans cap als\nhabitants d\'aquesta comunitat autònoma')
    the_labs <- labs(x = '',
                     y = "Valoració mitjana (0-10)",
                     title = 'Grau de simpatia/antipatia entre catalans\ni espanyols de diferents comunitats autònomes')
  } else {
    levs <- c('How people from this area feel\nabout Catalans',
              'How Catalans feel about people\nfrom this area')
    the_labs <- labs(x = '',
                     y = "Degree to which they 'like' the other (0-10)",
                     title = 'Degree to which Catalans and Spaniards\nof different regions like/dislike one another')
  }
  
  pd$var <- ifelse(pd$cap_a == 'Catalunya', levs[1], levs[2])
  pd$var <- factor(pd$var, levels = levs)
  fl <- sort(unique(pd$ccaa))
  pd$ccaa <- factor(pd$ccaa, levels = ,
                    labels = gsub('Castella', 'Cas.', gsub('/País Basc','', fl)))
  
  
  ggplot(data = pd,
         aes(x = var,
             y = value)) +
    geom_line(aes(group = ccaa), alpha = 0.7) +
    geom_point(aes(color = var), size = 9) +
    facet_wrap(~ccaa) +
    theme_simple() +
    theme(axis.text.x = element_blank()) +
    scale_color_manual(name = '',
                       values = c('darkorange', 'lightblue')) +
    geom_text(aes(label = round(value, digits = 1))) +
    the_labs +
    ylim(min(pd$value) - 1,
         max(pd$value) + 1) +
    theme(legend.position = 'top')
}

# Autonomia
# Pel que fa a les relacions entre Catalunya i Espanya, creieu que Catalunya ha assolit...
# pt$`Pel que fa a les relacions entre Catalunya i la resta de l’Estat espanyol, què diríeu que és més necessari?`

