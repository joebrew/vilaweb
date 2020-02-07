# Libraries
library(vilaweb)
library(tidyverse)
library(lubridate)
library(databrew)
library(tidylog)
options(scipen = 999)


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

# Read in the territorial survey data
pt <- vilaweb::ceo_percepcio_territorial


pt <- pt %>%
  mutate(ccaa = `Comunitat autònoma`) %>%
  mutate(visits = `I d’aquestes, en concret, em podria dir quantes vegades a viatjat a Catalunya?`,
         simpatia = `Grau de simpatia cap als habitants de cadascuna de les diferents comunitats autònomes:  Catalans`,
         referendum = `Grau d'acord: S’hauria de fer un referèndum a Catalunya perquè els catalans i les catalanes decidissin quina relació volen que hi hagi entre Catalunya i l’Estat espanyol`) %>%
  mutate(simpatia = simpatia_cleaner(simpatia)) %>%
  mutate(ccaa = `Comunitat autònoma`) %>%
  mutate(catalan_autonomy = `Pel que fa a les relacions entre Catalunya i Espanya, creieu que Catalunya ha assolit...`) %>%
  mutate(centralization = `A continuació, us presentaré algunes fórmules alternatives d’organització de l’Estat a Espanya. Digueu-me, si us plau, amb quina esteu més d’acord?`) %>%
  mutate(weight = `Coeficients de ponderació`) %>%
  mutate(indepe4 = `En tot cas, com creieu que hauria de ser aquesta relació? Creieu que Catalunya hauria de ser...`,
         indepe = `I, personalment, estaríeu a favor o en contra que Catalunya fos independent?`)

# Define function for cleaning up NS/NC
nsnc_convert <- function(x, en = FALSE){
  x <- as.character(x)
  x <- recode(x,
              'No ho sap' = 'NS/NC',
              'No contesta' = 'NS/NC')
  if(en){
    x <- rcode(x,
               'NS/NC' = "Doesn't know or no answer")
  }
  return(x)
}
  

# Line breaker function
line_breaker <- function(x,n = 10){
  gsub(paste0('(.{1,',n,'})(\\s|$)'), '\\1\n', x)
  
}
# # Companeros de trabajo
# pt$`Amb independència que treballeu o no, quins són els tres que menys us agradaria tenir com a companys de feina o treball? Catalans`



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
  fixed = c('Andalusia',
                   'Aragó',
                   'Astúries',
                   'Balears',
                   'Canàries',
                   'Cantàbria',
                   'Castella - la Manxa',
                   'Castella i Lleó',
                   'Catalunya',
                   'País Valencià',
                   'Extremadura',
                   'Galícia',
                   'Madrid',
                   'Múrcia',
                   'Navarra',
                   'País Basc',
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

plot_self_government_es <- function(ca = FALSE, agg = FALSE){
  en <- !ca
  
  autonomy_levels <- c('NS/NC',
                       'Massa autonomia',
                       "Un nivell suficient d’autonomia",
                       "Un nivell insuficient d’autonomia")
  autonomy_levels_en <- c("Doesn't know or no answer",
                          "Too much autonomy",
                          "Enough autonomy",
                          "Not enough autonomy")
  
  pd <- pt
  pd <- pd %>%
    mutate(catalan_autonomy = nsnc_convert(catalan_autonomy, en = FALSE)) %>%
    mutate(catalan_autonomy = factor(catalan_autonomy, levels = autonomy_levels,
                                     labels = line_breaker(autonomy_levels)))
  if(!ca){
    pd <- pd %>%
      mutate(catalan_autonomy = factor(catalan_autonomy, labels = line_breaker(autonomy_levels_en)))
    the_labs <- labs(x = 'Autonomous community of the survey participant',
                     y = 'Percentage',
                     subtitle = "'In regards to the relations between Catalonia and Spain, Catalonia has...'",
                     title = "Opinions on Catalonia's current level of autonomy",
                     caption = "Data source: Survey 'Percepció sobre el debat territorial a Espanya' from the Centre d'Estudis d'Opinió.\nChart: @joethebrew")
  } else {
    the_labs <- labs(x = 'Comunitat autònoma de l\'enquestat',
                     y = 'Percentatge',
                     subtitle = "'Pel que fa a les relacions entre Catalunya i Espanya, creieu que Catalunya ha assolit...'",
                     title = "Opinió sobre el grau d'autonomia de Catalunya",
                     caption = "Font de dades: Enquesta 'Percepció sobre el debat territorial a Espanya' del Centre d'Estudis d'Opinió.\nGràfic: @joethebrew")
  }
  
  cols <- colorRampPalette(RColorBrewer::brewer.pal(n = 9, name = 'Spectral'))(length(unique(pd$catalan_autonomy))-1)
  cols <- c('darkgrey', cols)
  
  if(agg){
    x = pd %>% filter(ccaa != 'Catalunya') %>%
      group_by(catalan_autonomy) %>%
      summarise(raw = n(),
                weighted = sum(weight, na.rm = TRUE)) %>%
      ungroup %>%
      mutate(denom_raw = sum(raw),
             denom_weighted = sum(weighted)) %>%
      mutate(p_raw = raw / denom_raw * 100,
             p_weighted = weighted / denom_weighted * 100)
    
    if(ca){
      the_labs$title <- paste0(the_labs$title, '\n(entre ciutadans de l\'estat espanyol, excloent-ne Catalunya)')
      the_labs$x <- 'Opinió: Catalunya ha assolit...'
    } else {
      the_labs$title <- paste0(the_labs$title, '\n(among citizens of the Spanish state, excluding Catalonia)')
      the_labs$x <- 'Opinion: Catalunya has...'
    }
    
    out <- ggplot(data = x,
           aes(x = catalan_autonomy,
               y = p_weighted,
               fill = catalan_autonomy)) +
      geom_bar(stat = 'identity', color = 'black') +
      theme_simple() +
      scale_fill_manual(name = '', values = cols) +
      the_labs +
      theme(legend.position = 'none') +
      geom_text(aes(label = paste0(round(p_weighted, digits = 1), '%')),
                nudge_y = -4, alpha = 0.6)
  } else {
    pd <- pd %>%
      left_join(label_df) %>% dplyr::select(-ccaa) %>% dplyr::mutate(ccaa = fixed) %>%
      group_by(ccaa, catalan_autonomy) %>%
      summarise(raw = n(),
                weighted = sum(weight, na.rm = TRUE)) %>%
      ungroup %>%
      group_by(ccaa) %>%
      mutate(denom_raw = sum(raw),
             denom_weighted = sum(weighted)) %>%
      mutate(p_raw = raw / denom_raw * 100,
             p_weighted = weighted / denom_weighted * 100)

    
    # Define colors
    # cols <- c('darkgrey', 'blue', 'lightblue', 'darkorange')
    
    out <- ggplot(data = pd,
           aes(x = ccaa,
               y = p_weighted,
               group = catalan_autonomy)) +
      geom_bar(stat = 'identity', aes(fill = catalan_autonomy)) +
      theme_simple() +
      scale_fill_manual(name = '', values = cols) +
      theme(#legend.position = 'none',
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
      # geom_text(aes(label = paste0(round(p_weighted, 1),'%')),
      #           nudge_y = 4, alpha = 0.7) +
      the_labs
  }
  return(out)
}


plot_self_government <- function(ca = FALSE){
  en <- !ca
  
  autonomy_levels <- c('NS/NC',
                       'Massa autonomia',
                       "Un nivell suficient d’autonomia",
                       "Un nivell insuficient d’autonomia")
  autonomy_levels_en <- c("Doesn't know or no answer",
                          "Too much autonomy",
                          "Enough autonomy",
                          "Not enough autonomy")
  
  pd <- pt
  pd <- pd %>%
    mutate(catalan_autonomy = nsnc_convert(catalan_autonomy, en = FALSE)) %>%
    mutate(catalan_autonomy = factor(catalan_autonomy, levels = autonomy_levels,
                                     labels = line_breaker(autonomy_levels)))
  if(!ca){
    pd <- pd %>%
      mutate(catalan_autonomy = factor(catalan_autonomy, labels = line_breaker(autonomy_levels_en)))
    the_labs <- labs(x = '',
                     y = 'Percentage',
                     subtitle = "'In regards to the relations between Catalonia and Spain, Catalonia has...'",
                     title = "Catalans' opinions on current level of autonomy",
                     caption = "Data source: Survey 'Percepció sobre el debat territorial a Espanya' from the Centre d'Estudis d'Opinió.\nChart: @joethebrew")
  } else {
    the_labs <- labs(x = '',
                     y = 'Percentatge',
                     subtitle = "'Pel que fa a les relacions entre Catalunya i Espanya, creieu que Catalunya ha assolit...'",
                     title = "Opinió de catalans sobre el grau d'autonomia assolit",
                     caption = "Font de dades: Enquesta 'Percepció sobre el debat territorial a Espanya' del Centre d'Estudis d'Opinió.\nGràfic: @joethebrew")
  }
  
  pd <- pd %>%
    left_join(label_df) %>% dplyr::select(-ccaa) %>% dplyr::mutate(ccaa = fixed) %>%
    group_by(ccaa, catalan_autonomy) %>%
    summarise(raw = n(),
              weighted = sum(weight, na.rm = TRUE)) %>%
    ungroup %>%
    group_by(ccaa) %>%
    mutate(denom_raw = sum(raw),
           denom_weighted = sum(weighted)) %>%
    mutate(p_raw = raw / denom_raw * 100,
           p_weighted = weighted / denom_weighted * 100)
  # Keep only Catalonia
  pd <- pd %>% filter(ccaa == "Catalunya")
  
  # Define colors
  cols <- c('darkgrey', 'blue', 'lightblue', 'darkorange')
  ggplot(data = pd,
         aes(x = catalan_autonomy,
             y = p_weighted)) +
    geom_bar(stat = 'identity', aes(fill = catalan_autonomy)) +
    theme_simple() +
    scale_fill_manual(name = '', values = cols) +
    theme(legend.position = 'none') +
    geom_text(aes(label = paste0(round(p_weighted, 1),'%')),
              nudge_y = 4, alpha = 0.7) +
    the_labs
}

#############
plot_centralism <- function(ca = FALSE){
  en <- !ca
  pd <- pt
  if(!ca){
    the_labs <- labs(x = '',
                     y = 'Percentage',
                     title = "Catalans' preferences regarding Spain's territorial organization",
                     
                     subtitle = 'Which form of organization for Spain do you agree with most?',
                     caption = "Data source: Survey 'Percepció sobre el debat territorial a Espanya' from the Centre d'Estudis d'Opinió.\nChart: @joethebrew")
  } else {
    the_labs <- labs(x = '',
                     y = 'Percentatge',
                     title = 'Preferències de catalans sobre l\'organització territorial de l\'estat espanyol',
                     
                     subtitle = "Amb quina fórmula d'organització de l'estat a Espanya esteu més d'acord?",
                     caption = "Font de dades: Enquesta 'Percepció sobre el debat territorial a Espanya' del Centre d'Estudis d'Opinió.\nGràfic: @joethebrew")
  }
  
  
  pd <- pt %>%
    mutate(centralization = recode(centralization,
                                   "Un estat amb un únic govern central sense autonomies" = 'Únic govern central sense autonomies',
                                   "Un estat en el que les comunitats autònomes tinguin menys a" = 'Menys autonomia',
                                   "Un estat de les comunitats autònomes com l’actual" = "Status quo",
                                   "Un estat en el que les comunitats autònomes tinguin més au" = "Més autonomia",
                                   "Un estat en el que es reconegui a les comunitats autònomes" = 'Possibilitat d\'independència',
                                   "No ho sap" = "NS/NC",
                                   'No contesta' = "NS/NC"))
  if(ca){
    the_levels <- c('NS/NC',
                    'Únic govern central sense autonomies',
                    'Menys autonomia',
                    'Status quo',
                    'Més autonomia',
                    "Possibilitat d'independència")
    the_labels <- c('NS/NC',
                    'Únic govern central sense autonomies',
                    'Menys autonomia per a les comunitats autònomes',
                    'Status quo',
                    'Més autonomia per a lescomunitats autònomes',
                    "Possibilitat d'independència per a lescomunitats autònomes")
  } else {
    the_levels <- c('NS/NC',
                    'Únic govern central sense autonomies',
                    'Menys autonomia',
                    'Status quo',
                    'Més autonomia',
                    "Possibilitat d'independència")
    the_labels <- c('Does not know or no answer',
                    'A central government with no autonomous communities',
                    'Less autonomy for the autonomous communities',
                    'Status quo',
                    'More autonomy for the autonomous communities',
                    "The possibility of independence for the autonomous communities")
  }
  the_labels <- line_breaker(the_labels)
  pd <- pd %>%
    # mutate(centralization = nsnc_convert(centralization)) %>%
    # mutate(centralization = line_breaker(centralization)) %>%
    mutate(centralization = factor(centralization,
                                   levels = the_levels,
                                   labels = the_labels))
  
  pd <- pd %>%
    left_join(label_df) %>% dplyr::select(-ccaa) %>% dplyr::mutate(ccaa = fixed) %>%
    group_by(ccaa, centralization) %>%
    summarise(raw = n(),
              weighted = sum(weight, na.rm = TRUE)) %>%
    ungroup %>%
    group_by(ccaa) %>%
    mutate(denom_raw = sum(raw),
           denom_weighted = sum(weighted)) %>%
    mutate(p_raw = raw / denom_raw * 100,
           p_weighted = weighted / denom_weighted * 100)
  # Keep only Catalonia
  pd <- pd %>% filter(ccaa == "Catalunya")
  
  # Define colors
  cols <- colorRampPalette(RColorBrewer::brewer.pal(n = 9, 'Spectral'))(length(unique(pd$centralization))-1)
  cols <- c('darkgrey', cols)
  ggplot(data = pd,
         aes(x = centralization,
             y = p_weighted)) +
    geom_bar(stat = 'identity', aes(fill = centralization)) +
    theme_simple() +
    scale_fill_manual(name = '', values = cols) +
    theme(legend.position = 'none') +
    geom_text(aes(label = paste0(round(p_weighted, 1),'%')),
              nudge_y = 4, alpha = 0.7) +
    the_labs
}


plot_centralism_es <- function(ca = FALSE, agg = FALSE){
  en <- !ca
  pd <- pt
  if(!ca){
    the_labs <- labs(x = '',
                     y = 'Percentage',
                     title = "Spaniards' preferences regarding Spain's territorial organization",
                     
                     subtitle = 'Which form of organization for Spain do you agree with most?\nPopulation of Spain (excluding Catalonia)',
                     caption = "Data source: Survey 'Percepció sobre el debat territorial a Espanya' from the Centre d'Estudis d'Opinió.\nChart: @joethebrew")
  } else {
    the_labs <- labs(x = '',
                     y = 'Percentatge',
                     title = 'Preferències sobre l\'organització territorial de l\'estat espanyol',
                     
                     subtitle = "Amb quina fórmula d'organització de l'estat a Espanya esteu més d'acord?\n(Població d'Espanya (excloent-ne Catalunya)",
                     caption = "Font de dades: Enquesta 'Percepció sobre el debat territorial a Espanya' del Centre d'Estudis d'Opinió.\nGràfic: @joethebrew")
  }
  
  
  pd <- pt %>%
    mutate(centralization = recode(centralization,
                                   "Un estat amb un únic govern central sense autonomies" = 'Únic govern central sense autonomies',
                                   "Un estat en el que les comunitats autònomes tinguin menys a" = 'Menys autonomia',
                                   "Un estat de les comunitats autònomes com l’actual" = "Status quo",
                                   "Un estat en el que les comunitats autònomes tinguin més au" = "Més autonomia",
                                   "Un estat en el que es reconegui a les comunitats autònomes" = 'Possibilitat d\'independència',
                                   "No ho sap" = "NS/NC",
                                   'No contesta' = "NS/NC"))
  if(ca){
    the_levels <- c('NS/NC',
                    'Únic govern central sense autonomies',
                    'Menys autonomia',
                    'Status quo',
                    'Més autonomia',
                    "Possibilitat d'independència")
    the_labels <- c('NS/NC',
                    'Únic govern central sense autonomies',
                    'Menys autonomia per a lescomunitats autònomes',
                    'Status quo',
                    'Més autonomia per a lescomunitats autònomes',
                    "Possibilitat d'independència per a lescomunitats autònomes")
  } else {
    the_levels <- c('NS/NC',
                    'Únic govern central sense autonomies',
                    'Menys autonomia',
                    'Status quo',
                    'Més autonomia',
                    "Possibilitat d'independència")
    the_labels <- c('Does not know or no answer',
                    'A central government with no autonomous communities',
                    'Less autonomy for the autonomous communities',
                    'Status quo',
                    'More autonomy for the autonomous communities',
                    "The possibility of independence for the autonomous communities")
  }
  the_labels <- line_breaker(the_labels, n = 20)
  pd <- pd %>%
    # mutate(centralization = nsnc_convert(centralization)) %>%
    # mutate(centralization = line_breaker(centralization)) %>%
    mutate(centralization = factor(centralization,
                                   levels = the_levels,
                                   labels = the_labels))
  
  if(agg){
    pd <- pd %>%
      filter(ccaa != 'Catalunya') %>%
      group_by(centralization) %>%
      summarise(raw = n(),
                weighted = sum(weight, na.rm = TRUE)) %>%
      ungroup %>%
      mutate(denom_raw = sum(raw),
             denom_weighted = sum(weighted)) %>%
      mutate(p_raw = raw / denom_raw * 100,
             p_weighted = weighted / denom_weighted * 100)
    # pd <- pd %>% filter(ccaa == "Catalunya")
    
    # Define colors
    cols <- colorRampPalette(RColorBrewer::brewer.pal(n = 9, 'Spectral'))(length(unique(pd$centralization))-1)
    cols <- c('darkgrey', cols)
    ggplot(data = pd,
           aes(x = centralization,
               y = p_weighted)) +
      geom_bar(stat = 'identity', aes(fill =centralization),
               color = 'black') +
      theme_simple() +
      scale_fill_manual(name = '', values = cols) +
      theme(legend.position = 'none') +
      theme(#legend.position = 'none',
        legend.position = 'right',
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
      geom_text(aes(label = paste0(round(p_weighted, 1),'%')),
                nudge_y = -3, alpha = 0.7) +
      the_labs
  } else {
    pd <- pd %>%
      # filter(ccaa != 'Catalunya') %>%
      left_join(label_df) %>% dplyr::select(-ccaa) %>% dplyr::mutate(ccaa = fixed) %>%
      group_by(ccaa, centralization) %>%
      summarise(raw = n(),
                weighted = sum(weight, na.rm = TRUE)) %>%
      ungroup %>%
      group_by(ccaa) %>%
      mutate(denom_raw = sum(raw),
             denom_weighted = sum(weighted)) %>%
      mutate(p_raw = raw / denom_raw * 100,
             p_weighted = weighted / denom_weighted * 100)
    # pd <- pd %>% filter(ccaa == "Catalunya")
    
    # Define colors
    cols <- colorRampPalette(RColorBrewer::brewer.pal(n = 9, 'Spectral'))(length(unique(pd$centralization))-1)
    cols <- c('darkgrey', cols)
    ggplot(data = pd,
           aes(x = ccaa,
               y = p_weighted)) +
      geom_bar(stat = 'identity', aes(fill = centralization)) +
      theme_simple() +
      scale_fill_manual(name = '', values = cols) +
      # theme(legend.position = 'none') +
      theme(#legend.position = 'none',
        legend.position = 'right',
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
      # geom_text(aes(label = paste0(round(p_weighted, 1),'%')),
      #           nudge_y = 4, alpha = 0.7) +
      the_labs
  }
  
  
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

plot_conditional <- function(ca = FALSE){
  pd = pt %>% filter(ccaa == 'Catalunya') %>%
    mutate(indepe4 = nsnc_convert(indepe4),
           indepe = nsnc_convert(indepe)) 
  pd <- pd %>%
    mutate(indepe4 = factor(indepe4, levels = c("Una regió d’espanya",
                                                "Una comunitat autònoma d’espanya",
                                                "Un estat dins una espanya federal",
                                                "Un estat independent",
                                                "NS/NC")))
  pd <- pd %>%
    mutate(indepe = recode(indepe,
                           "Hi estaria a favor" = 'Independentistes',
                           "Hi estaria en contra" = 'Unionistes')) %>%
    mutate(indepe = factor(indepe, levels = c('Independentistes', 'Unionistes')))
  
  pd <- pd %>%
    filter(!is.na(indepe)) %>%
    filter(!indepe %in% c('NS/NC')) %>%
    
    group_by(indepe4,indepe) %>%
    summarise(raw = n(),
              weighted = sum(weight, na.rm = TRUE)) %>%
    ungroup %>%
    group_by(indepe) %>%
    mutate(denom_raw = sum(raw),
           denom_weighted = sum(weighted)) %>%
    mutate(p_raw = raw / denom_raw * 100,
           p_weighted = weighted / denom_weighted * 100) 
  the_levels <- levels(pd$indepe4)
  bi_levels <- levels(pd$indepe)
  if(ca){
    the_labels <- the_levels
    bi_labels <- bi_levels
    bi_labels <- gsub('espanya', 'Espanya', bi_labels)
  } else {
    the_labels <- c('A region of Spain',
                    'An autonomous community of Spain',
                    'A state in a federal Spain',
                    'An independent state',
                    'Does not know or not sure')
    bi_labels <- c('Independentists', 'Unionists')
  }
  
  if(ca){
    the_labs <- labs(x = '',
                     y = 'Percentage',
                     title = 'Què volen que sigui Catalunya els catalans?',
                     subtitle = 'Encreuament binari vs quaternari',
                     caption = "Font de dades: Enquesta 'Percepció sobre el debat territorial a Espanya' del Centre d'Estudis d'Opinió.\nGràfic: @joethebrew")
    
  } else {
    the_labs <- labs(x = '',
                     y = 'Percentatge',
                     title = "What Catalans want Catalonia to be",
                     subtitle = "Quatrenary breakdown among 'independentists' and 'unionists'",
                     caption = "Data source: Survey 'Percepció sobre el debat territorial a Espanya' from the Centre d'Estudis d'Opinió.\nChart: @joethebrew")
  }
  
  
  the_labels <- line_breaker(the_labels)
  pd$indepe4 <- factor(pd$indepe4, levels = the_levels, labels = the_labels)
  pd$indepe <- factor(pd$indepe, levels = bi_levels, labels = bi_labels)
  
  
  ggplot(data = pd,
         aes(x = indepe4,
             y = p_weighted)) +
    geom_point() +
    geom_segment(aes(xend = indepe4,
                     yend = 0)) +
    facet_wrap(~indepe) +
    theme_simple() +
    geom_text(aes(label = paste0(round(p_weighted, digits = 2), '%')),
              nudge_y = 5, alpha = 0.7) +
    theme(#strip.background  = element_rect(fill = NA, color = 'black'),
          panel.background  = element_rect(fill = NA, color = 'black')) +
    the_labs
}

