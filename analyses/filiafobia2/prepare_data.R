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
           'Extremadura',
           'Galícia',
           'Madrid',
           'Múrcia',
           'Navarra',
           'Euskadi/País Basc',
           'País Valencià',
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
          'Extremenys',
          'Gallecs',
          'Madrilenys',
          'Murcians',
          'Navarresos',
          'Bascos',
          'Valencians',
          'Riojans',
          'Ceutencs',
          'Melillencs')
)
label_df$ccaa <- factor(label_df$ccaa, levels = label_df$ccaa)
label_df$key <- factor(label_df$key, levels = label_df$key)

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






# Grau de simpatia cap als habitants
plot_simpatia_matrix <- function(ca = FALSE, var = 'avg', roundy = 1, reverse_color = FALSE, text_size = 4, only_indepes = FALSE){
  library(RColorBrewer)
  pd <- pt
  if(only_indepes){
    pd <- pd %>%
      mutate(indepe = `I, personalment, estaríeu a favor o en contra que Catalunya fos independent?`) %>%
      mutate(keep = ccaa != 'Catalunya' | indepe == 'Hi estaria a favor') %>%
      filter(keep)
  }
  pd <- pd %>%
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
  
  # Fix x-value if only indepes
  if(only_indepes){
    if(ca){
      pd$gent_de <- factor(pd$gent_de,
                        levels = levels(pd$gent_de),
                        labels = ifelse(levels(pd$gent_de) == 'Catalunya',
                                        'Catalunya\n(només independentistes)',
                                        ifelse(levels(pd$gent_de) == 'Euskadi/País Basc', 'País Basc', 
                                               ifelse(levels(pd$gent_de) == 'Castella la Manxa', 'Castella - la Manxa', levels(pd$gent_de)))))
    } else {
      pd$gent_de <- factor(pd$gent_de,
                        levels = levels(pd$gent_de),
                        labels = ifelse(levels(pd$gent_de) == 'Catalunya',
                                        'Catalunya\n(only pro-indy)',
                                        ifelse(levels(pd$gent_de) == 'Euskadi/País Basc', 'País Basc', 
                                               ifelse(levels(pd$gent_de) == 'Castella la Manxa', 'Castella - la Manxa', levels(pd$gent_de)))))
    }
    
  } else {
    pd$gent_de <- factor(pd$gent_de,
                         levels = levels(pd$gent_de),
                         labels = ifelse(levels(pd$gent_de) == 'Euskadi/País Basc',
                                         'País Basc',
                                         ifelse(levels(pd$gent_de) == 'Castella la Manxa', 
                                                'Castella - la Manxa',
                                                levels(pd$gent_de))))
  }
  
  pd$cap_a <- factor(pd$cap_a,
                     levels = levels(pd$cap_a),
                     labels = ifelse(levels(pd$cap_a) == 'Euskadi/País Basc',
                                     'País Basc',
                                     ifelse(levels(pd$cap_a) == 'Castella la Manxa', 
                                            'Castella - la Manxa',
                                            levels(pd$cap_a))))
  
  if(ca){
    the_labs <- labs(x = 'Grau de simpatia de les persones d\'aquí',
                     y = '...cap a les persones d\'aquí',
                     title = 'Grau de simpatia cap als habitants de les altres\ncomunitats autònomes, segons comunitat autònoma',
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
                                     hjust = 1,
                                     vjust = 0.5),
          plot.title = element_text(size = 13)) +
    geom_text(aes(label = round(y, digits = roundy)), size = text_size) +
    theme(legend.position = 'none')
}

# Get average
get_avg <- function(where_from = 'Catalunya',
                    var = 'avg',
                    where_to = label_df$ccaa[label_df$ccaa != 'Catalunya']){
  pd <- pt %>%
    # mutate(indepe = `I, personalment, estaríeu a favor o en contra que Catalunya fos independent?`) %>%
    # mutate(keep = ccaa != 'Catalunya' | indepe == 'Hi estaria a favor') %>%
    # filter(keep) %>%
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
plot_compare <- function(ca = TRUE, only_indepes = FALSE){
  pd <- pt
  
  if(only_indepes){
    pd <- pd %>%
      mutate(indepe = `I, personalment, estaríeu a favor o en contra que Catalunya fos independent?`) %>%
      mutate(keep = ccaa != 'Catalunya' | indepe == 'Hi estaria a favor') %>%
      filter(keep)
  }
  pd <- pd %>%
    mutate(weight = `Coeficients de ponderació`) %>%
    mutate(ccaa = `Comunitat autònoma`) %>%
    dplyr::select(contains('Grau de simpatia cap als habitants'),
                  contains('ccaa'),
                  weight)
  names(pd) <- gsub("Grau de simpatia cap als habitants de cadascuna de les diferents comunitats autònomes:  ", '', names(pd))
  # Make long
  pd <- pd %>% gather(key, value, Andalusos:Ceutencs)
  pd$value <- simpatia_cleaner(pd$value)
  # Rename
  pd <- pd %>%
    mutate(gent_de = fix_levels(ccaa, from_place = TRUE, to_place = TRUE),
           cap_a = fix_levels(key, from_place = FALSE, to_place = TRUE)) %>%
    dplyr::select(gent_de,
                  cap_a, value, weight) %>%
    group_by(gent_de, cap_a) %>%
    summarise(avg = mean(value, na.rm = TRUE),
              value_weighted = weighted.mean(value, w= weight, na.rm = TRUE)) %>%
    mutate(value = avg)
  pd$discrep <- pd$value - pd$value_weighted
  
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
    if(only_indepes){
      levs <- c('Valoració mitjana dels habitants\nd\'aquesta comunitat cap als catalans',
                'Valoració mitjana de catalans INDEPENDENTISTES\ncap als habitants d\'aquesta comunitat')
    } else {
      levs <- c('Valoració mitjana dels habitants d\'aquesta\ncomunitat cap als catalans',
                'Valoració mitjana dels catalans cap\nals habitants d\'aquesta comunitat')
    }
    the_labs <- labs(x = '',
                     y = "Valoració mitjana (0-10)",
                     title = 'Grau de simpatia entre catalans i habitants\nde les altres comunitats autònomes',
                     caption = 'Escala de 0 a 10 on 0 significa: "Em cauen molt malament" i 10 significa: "Em cauen molt bé".\nDades de l\'enquesta "Percepció sobre el debat territorial a Espanya. 2019", Centre d\'Estudis d\'Opinió.\nEl text entre parèntesis és la xifra NO ponderada. La ponderació fa servir factors demogràfics per ajustar pel biaix de selecció/mostratge.\nMés detalls sobre la ponderació a http://upceo.ceo.gencat.cat/wsceop/7368/Press%20dossier%20-952.pdf\nGràfic de Joe Brew, @joethebrew. Codi a https://github.com/joebrew/vilaweb/tree/master/analyses/filiafobia')
  } else {
    if(only_indepes){
      levs <- c('How people from this area\nfeel about Catalans',
                'How PRO-INDY Catalans feel\nabout people from this area')
    } else {
      levs <- c('How people from this area\nfeel about Catalans',
                'How Catalans feel about\npeople from this area')
    }
    the_labs <- labs(x = '',
                     y = "Degree to which they 'like' the other (0-10)",
                     title = 'Degree to which Catalans and inhabitants of\nother regions like one another',
                     caption = 'Scale from 0 to 10, where 0 means "I don\'t like them at all" and 10 means "I like them a lot".\nData from the survey "Percepció sobre el debat territorial a Espanya. 2019", Centre d\'Estudis d\'Opinió.\nText in parenthesis is the unadjusted/raw figure. Adjustment used demographic factors to generate weights to account for sampling bias.\nFull details on weighting at http://upceo.ceo.gencat.cat/wsceop/7368/Press%20dossier%20-952.pdf\nChart by Joe Brew, @joethebrew. Code at https://github.com/joebrew/vilaweb/tree/master/analyses/filiafobia')
  }
  
  pd$var <- ifelse(pd$cap_a == 'Catalunya', levs[1], levs[2])
  pd$var <- factor(pd$var, levels = levs)
  # Get the corrected levels order
  pd$ccaa <- factor(pd$ccaa,
                    levels = c('Andalusia',
                               'Aragó',
                               'Astúries',
                               'Balears',
                               'Canàries',
                               'Cantàbria',
                               'Castella i Lleó',
                               'Castella la Manxa',
                               'Extremadura',
                               'Galícia',
                               'Madrid',
                               'Múrcia',
                               'Navarra',
                               'Euskadi/País Basc',
                               'País Valencià',
                               'La Rioja'))
  fl <- levels(pd$ccaa)
  pd$ccaa <- factor(pd$ccaa, levels = fl,
                    labels = gsub('Castella', 'Cas.', gsub('Euskadi/','', fl)))
  

  ggplot(data = pd,
         aes(x = var,
             y = value_weighted)) +
    geom_line(aes(group = ccaa), alpha = 0.7) +
    geom_point(aes(color = var), size = 9) +
    facet_wrap(~ccaa, nrow = 2) +
    theme_simple() +
    theme(axis.text.x = element_blank(),
          strip.text = element_text(size = 8)) +
    scale_color_manual(name = '',
                       values = c('darkorange', 'lightblue')) +
    geom_text(aes(label = round(value_weighted, digits = 1))) +
    the_labs +
    ylim(min(pd$value_weighted) - 0.7,
         max(pd$value_weighted) + 0.5) +
    theme(legend.position = 'top',
          plot.title = element_text(size = 16),
          legend.text = element_text(size = 10),
          plot.caption = element_text(size = 8)) +
    geom_text(aes(label = paste0('(',
                                 round(value, digits = 1),
                                 ')')),
              nudge_y = -0.6,
              size = 2.5, alpha = 0.8, color = 'black')
}

# Autonomia
# Pel que fa a les relacions entre Catalunya i Espanya, creieu que Catalunya ha assolit...
# pt$`Pel que fa a les relacions entre Catalunya i la resta de l’Estat espanyol, què diríeu que és més necessari?`


# Libraries
library(vilaweb)
library(tidyverse)
library(lubridate)

# Line breaker function
line_breaker <- function(x,n = 10){
  gsub(paste0('(.{1,',n,'})(\\s|$)'), '\\1\n', x)
}

df <- dff <- tibble(
  partit = c('PP', 'PSC', 'Cs', 'JxCat', 'ERC', 'Comuns'),
  `% "d'acord" o "molt d'acord" amb la frase "Els immigrants haurien d'abandonar la seva cultura d'origen i adoptar la cultura del país al quan han arribat"` = c(40.9, 35.3, 33.3, 21.4, 19.1, 17.7),
  `Grau d'acord amb la frase "M'agrada aprendre de les altres cultures"` = c(6.8, 7.8, 7.5, 8.4, 8.5, 8.3),
  `Grau d'acord amb la frase "Cap cultura en concret no és superior a les altres"` = c(7.6, 8.4, 8.4, 8.6, 8.9, 9),
  `% que vol "posar límits estrictes al nombre d'estrangers que poden venir aquí" o "prohibir-ne l'arribada completament"` = c(50.5, 24.7, 42.4, 16.1, 13.4, 13.8),
  `% d'acord amb la frase "Amb tanta immigració, un ja no se sent com a casa"` = c(62.2, 32.2, 50.5, 32.9, 25.8, 30.7),
  `% que vol gastar més o molt més en l'acolliment de la immigració` = c(9.3, 37.5, 13, 48.1, 47.3, 56.8)
)

fonts <- font <- tibble(`% "d'acord" o "molt d'acord" amb la frase "Els immigrants haurien d'abandonar la seva cultura d'origen i adoptar la cultura del país al quan han arribat"` = c("http://upceo.ceo.gencat.cat/wsceop/7348/Dossier%20de%20premsa%20-951.pdf", '% "agree" or "very much agree" that "immigrants should abandon their culture of origin and aopt the culture of the country where they have arrived"'),
                `Grau d'acord amb la frase "M'agrada aprendre de les altres cultures"` = c("http://upceo.ceo.gencat.cat/wsceop/7348/Dossier%20de%20premsa%20-951.pdf", 'Extent of agreement with the phrase "I like learning from other cultures"'),
                `Grau d'acord amb la frase "Cap cultura en concret no és superior a les altres"` = c("http://upceo.ceo.gencat.cat/wsceop/7348/Dossier%20de%20premsa%20-951.pdf", 'Extent of agreement with the phrase "No specific culture is superior to others"'),
                `% que vol "posar límits estrictes al nombre d'estrangers que poden venir aquí" o "prohibir-ne l'arribada completament"` = c("http://upceo.ceo.gencat.cat/wsceop/7348/Dossier%20de%20premsa%20-951.pdf", '% that wants to "put strict limits on the number of foreigners that can come here" or "completely ban" their arrival'),
                `% d'acord amb la frase "Amb tanta immigració, un ja no se sent com a casa"` = c("Aggregació d'enquestes CEO, 2015 i 2018. Mostra: 3.127 residents de Catalunya amb ciutadania espanyola. Preguntes P56i i P31.", 'Extent of agreement with the phrase "With so much immigration, one no longer feels at home"'),
                `% que vol gastar més o molt més en l'acolliment de la immigració` = c('http://upceo.ceo.gencat.cat/wsceop/7428/Dossier%20de%20premsa%20-955.pdf', '% that wants to spend more on welcoming immigrants'))

pd <- df %>%
  gather(key, value, names(df)[2:length(names(df))])
pd$partit <- factor(pd$partit, levels = c('PP', 'Cs',  'PSC',  'Comuns', 'JxCat', 'ERC'))

keys <- unique(pd$key)

key_plot <- function(index = 1, ca = FALSE){
  
  df <- dff <- tibble(
    partit = c('PP', 'PSC', 'Cs', 'JxCat', 'ERC', 'Comuns'),
    `% "d'acord" o "molt d'acord" amb la frase "Els immigrants haurien d'abandonar la seva cultura d'origen i adoptar la cultura del país al quan han arribat"` = c(40.9, 35.3, 33.3, 21.4, 19.1, 17.7),
    `Grau d'acord amb la frase "M'agrada aprendre de les altres cultures"` = c(6.8, 7.8, 7.5, 8.4, 8.5, 8.3),
    `Grau d'acord amb la frase "Cap cultura en concret no és superior a les altres"` = c(7.6, 8.4, 8.4, 8.6, 8.9, 9),
    `% que vol "posar límits estrictes al nombre d'estrangers que poden venir aquí" o "prohibir-ne l'arribada completament"` = c(50.5, 24.7, 42.4, 16.1, 13.4, 13.8),
    `% d'acord amb la frase "Amb tanta immigració, un ja no se sent com a casa"` = c(62.2, 32.2, 50.5, 32.9, 25.8, 30.7),
    `% que vol gastar més o molt més en l'acolliment de la immigració` = c(9.3, 37.5, 13, 48.1, 47.3, 56.8)
  )
  
  fonts <- font <- tibble(`% "d'acord" o "molt d'acord" amb la frase "Els immigrants haurien d'abandonar la seva cultura d'origen i adoptar la cultura del país al quan han arribat"` = c("http://upceo.ceo.gencat.cat/wsceop/7348/Dossier%20de%20premsa%20-951.pdf", '% "agree" or "very much agree" that "immigrants should abandon their culture of origin and aopt the culture of the country where they have arrived"'),
                          `Grau d'acord amb la frase "M'agrada aprendre de les altres cultures"` = c("http://upceo.ceo.gencat.cat/wsceop/7348/Dossier%20de%20premsa%20-951.pdf", 'Extent of agreement with the phrase "I like learning from other cultures"'),
                          `Grau d'acord amb la frase "Cap cultura en concret no és superior a les altres"` = c("http://upceo.ceo.gencat.cat/wsceop/7348/Dossier%20de%20premsa%20-951.pdf", 'Extent of agreement with the phrase "No specific culture is superior to others"'),
                          `% que vol "posar límits estrictes al nombre d'estrangers que poden venir aquí" o "prohibir-ne l'arribada completament"` = c("http://upceo.ceo.gencat.cat/wsceop/7348/Dossier%20de%20premsa%20-951.pdf", '% that wants to "put strict limits on the number of foreigners that can come here" or "completely ban" their arrival'),
                          `% d'acord amb la frase "Amb tanta immigració, un ja no se sent com a casa"` = c("Aggregació d'enquestes CEO, 2015 i 2018. Mostra: 3.127 residents de Catalunya amb ciutadania espanyola. Preguntes P56i i P31.", 'Extent of agreement with the phrase "With so much immigration, one no longer feels at home"'),
                          `% que vol gastar més o molt més en l'acolliment de la immigració` = c('http://upceo.ceo.gencat.cat/wsceop/7428/Dossier%20de%20premsa%20-955.pdf', '% that wants to spend more on welcoming immigrants'))
  
  pd <- df %>%
    gather(key, value, names(df)[2:length(names(df))])
  pd$partit <- factor(pd$partit, levels = c('PP', 'Cs',  'PSC',  'Comuns', 'JxCat', 'ERC'))
  
  keys <- unique(pd$key)
  this_key <- keys[index]
  sub_pd <- pd %>% filter(key == this_key)
  font1 <- unlist(fonts[,this_key][1,])
  font2 <- unlist(fonts[,this_key][2,])
  
  if(ca){
    the_labs <- the_labs <- labs(x = '',
                     y = 'Percentatge/grau d\'acord',
                     title = line_breaker(this_key, n = 65),
                     caption = paste0("Font de dades: ", line_breaker(font[1, index], n = 100)))
  } else {
    the_labs <- labs(x = '',
         y = 'Percentage/Extent of agreement',
         title = line_breaker(fonts[2,index], n = 65),
         caption = paste0("Source of data: ", line_breaker(font[1,index], n = 100)))
  }
  
  ggplot(data = sub_pd %>% mutate(key = line_breaker(key, 60)),
         aes(x = partit,
             y = value,
             fill = partit,
             color = partit)) +
    geom_bar(stat = 'identity', alpha = 0.5,
             # color = 'black',
             lwd = 0.3) +
    # facet_wrap(~key, scales = 'free') +
    databrew::theme_simple() +
    scale_fill_manual(name = '',
                      values = c('blue', 'darkorange', 'darkred', 'lightblue', 'purple', 'yellow')) +
    scale_color_manual(name = '',
                       values = c('blue', 'darkorange', 'darkred', 'lightblue', 'purple', 'yellow')) +
    theme(legend.position = 'none',
          strip.text = element_text(size = 18),
          axis.text.x = element_text(size = 18),
          axis.text.y = element_text(size = 14),
          plot.caption = element_text(size = 8)) +
    geom_text(aes(label = paste0(round(value, digits = 1), ''),
                  y = (value * 0.9) - 1),
              color = 'black',
              size = 7,
              alpha = 0.7) +
    the_labs
}
# dir.create('~/Desktop/plots')
# for(i in 1:6){
#   print(key_plot(i))
#   ggsave(paste0('~/Desktop/plots/', i, '.png'))
# }
