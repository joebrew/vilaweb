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
      the_labs$title <- paste0(the_labs$title, '\n(entre ciutadans de l\'estat espanyol, excloent-hi Catalunya)')
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
                     
                     subtitle = "Amb quina fórmula d'organització de l'Estat a Espanya esteu més d'acord?",
                     caption = "Font de dades: Enquesta 'Percepció sobre el debat territorial a Espanya' del Centre d'Estudis d'Opinió.\nGràfic: @joethebrew")
  }
  
  
  pd <- pt %>%
    mutate(centralization = recode(centralization,
                                   "Un estat amb un únic govern central sense autonomies" = 'Únic govern central sense autonomies',
                                   "Un estat en el que les comunitats autònomes tinguin menys a" = 'Menys autonomia',
                                   "Un estat de les comunitats autònomes com l’actual" = "Statu quo",
                                   "Un estat en el que les comunitats autònomes tinguin més au" = "Més autonomia",
                                   "Un estat en el que es reconegui a les comunitats autònomes" = 'Possibilitat d\'independència',
                                   "No ho sap" = "NS/NC",
                                   'No contesta' = "NS/NC"))
  if(ca){
    the_levels <- c('NS/NC',
                    'Únic govern central sense autonomies',
                    'Menys autonomia',
                    'Statu quo',
                    'Més autonomia',
                    "Possibilitat d'independència")
    the_labels <- c('NS/NC',
                    'Únic govern central sense autonomies',
                    'Menys autonomia per les comunitats autònomes',
                    'Statu quo',
                    'Més autonomia per les comunitats autònomes',
                    "Possibilitat d'independència per les comunitats autònomes")
  } else {
    the_levels <- c('NS/NC',
                    'Únic govern central sense autonomies',
                    'Menys autonomia',
                    'Statu quo',
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
                     
                     subtitle = "Amb quina fórmula d'organització de l'Estat a Espanya esteu més d'acord?\n(Poblacio d'Espanya (excloent-hi Catalunya)",
                     caption = "Font de dades: Enquesta 'Percepció sobre el debat territorial a Espanya' del Centre d'Estudis d'Opinió.\nGràfic: @joethebrew")
  }
  
  
  pd <- pt %>%
    mutate(centralization = recode(centralization,
                                   "Un estat amb un únic govern central sense autonomies" = 'Únic govern central sense autonomies',
                                   "Un estat en el que les comunitats autònomes tinguin menys a" = 'Menys autonomia',
                                   "Un estat de les comunitats autònomes com l’actual" = "Statu quo",
                                   "Un estat en el que les comunitats autònomes tinguin més au" = "Més autonomia",
                                   "Un estat en el que es reconegui a les comunitats autònomes" = 'Possibilitat d\'independència',
                                   "No ho sap" = "NS/NC",
                                   'No contesta' = "NS/NC"))
  if(ca){
    the_levels <- c('NS/NC',
                    'Únic govern central sense autonomies',
                    'Menys autonomia',
                    'Statu quo',
                    'Més autonomia',
                    "Possibilitat d'independència")
    the_labels <- c('NS/NC',
                    'Únic govern central sense autonomies',
                    'Menys autonomia per les comunitats autònomes',
                    'Statu quo',
                    'Més autonomia per les comunitats autònomes',
                    "Possibilitat d'independència per les comunitats autònomes")
  } else {
    the_levels <- c('NS/NC',
                    'Únic govern central sense autonomies',
                    'Menys autonomia',
                    'Statu quo',
                    'Més autonomia',
                    "Possibilitat d'independència")
    the_labels <- c('Does not know or no answer',
                    'A central government with no autonomous communities',
                    'Less autonomy for the autonomous communities',
                    'Status quo',
                    'More autonomy for the autonomous communities',
                    "The possibility of independence for the autonomous communities")
  }
  the_labels <- line_breaker(the_labels, n = 16)
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
                     title = 'Els catalans volen que Catalunya sigui',
                     subtitle = 'Creuada binària vs quatrenària',
                     caption = "Font de dades: Enquesta 'Percepció sobre el debat territorial a Espanya' del Centre d'Estudis d'Opinió.\nGràfic: @joethebrew")
    
  } else {
    the_labs <- labs(x = '',
                     y = 'Percentatge',
                     title = "What Catalans want Catalonia to be",
                     subtitle = "Binary vs. quatrenary breakdown",
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
    levs <- c('Valoració mitjana dels habitants d\'aquesta\ncomunitat autònoma cap als catalans',
              'Valoració mitjana dels catalans cap als\nhabitants d\'aquesta comunitat autònoma')
    the_labs <- labs(x = '',
                     y = "Valoració mitjana (0-10)",
                     title = 'Grau de simpatia entre catalans i habitants d\'altres comunitats autònomes',
                     caption = 'Escala de 0 a 10 on 0 significa: "Em cauen molt malament" i 10 significa: "Em cauen molt bé".\nDades de l\'enquesta "Percepció sobre el debat territorial a Espanya. 2019", Centre d\'Estudis d\'Opinió.\nEl texte entre parèntesis és la xifra NO ponderada. La ponderació fa servir factors demogràfics per ajustar pel biaix de selecció/mostreig.\nMés detalls sobre la ponderació a http://upceo.ceo.gencat.cat/wsceop/7368/Press%20dossier%20-952.pdf\nGràfic de Joe Brew, @joethebrew. Codi a https://github.com/joebrew/vilaweb/tree/master/analyses/filiafobia')
  } else {
    levs <- c('How people from this area feel\nabout Catalans',
              'How Catalans feel about people\nfrom this area')
    the_labs <- labs(x = '',
                     y = "Degree to which they 'like' the other (0-10)",
                     title = 'Degree to which Catalans and inhabitants of other regions like one another',
                     caption = 'Scale from 0 to 10, where 0 means "I don\'t like them at all" and 10 means "I like them a lot".\nData from the survey "Percepció sobre el debat territorial a Espanya. 2019", Centre d\'Estudis d\'Opinió.\nText in parenthesis is the unadjusted/raw figure. Adjustment used demographic factors to generate weights to account for sampling bias.\nFull details on weighting at http://upceo.ceo.gencat.cat/wsceop/7368/Press%20dossier%20-952.pdf\nChart by Joe Brew, @joethebrew. Code at https://github.com/joebrew/vilaweb/tree/master/analyses/filiafobia')
  }
  
  pd$var <- ifelse(pd$cap_a == 'Catalunya', levs[1], levs[2])
  pd$var <- factor(pd$var, levels = levs)
  fl <- sort(unique(pd$ccaa))
  pd$ccaa <- factor(pd$ccaa, levels = ,
                    labels = gsub('Castella', 'Cas.', gsub('/País Basc','', fl)))
  
  
  ggplot(data = pd,
         aes(x = var,
             y = value_weighted)) +
    geom_line(aes(group = ccaa), alpha = 0.7) +
    geom_point(aes(color = var), size = 9) +
    facet_wrap(~ccaa, nrow = 2) +
    theme_simple() +
    theme(axis.text.x = element_blank(),
          strip.text = element_text(size = 10)) +
    scale_color_manual(name = '',
                       values = c('darkorange', 'lightblue')) +
    geom_text(aes(label = round(value_weighted, digits = 1))) +
    the_labs +
    ylim(min(pd$value_weighted) - 0.7,
         max(pd$value_weighted) + 0.5) +
    theme(legend.position = 'top',
          plot.title = element_text(size = 16),
          legend.text = element_text(size = 12),
          plot.caption = element_text(size = 8)) +
    geom_text(aes(label = paste0('(',
                                 round(value, digits = 1),
                                 ')')),
              nudge_y = -1.1,
              size = 2.5, alpha = 0.8, color = 'black')
}

# Autonomia
# Pel que fa a les relacions entre Catalunya i Espanya, creieu que Catalunya ha assolit...
# pt$`Pel que fa a les relacions entre Catalunya i la resta de l’Estat espanyol, què diríeu que és més necessari?`

