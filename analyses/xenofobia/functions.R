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

# # Read in data manually (until package built)
# cis_2018_09 <- haven::read_sav('../../../data-raw/cis/monthly/2018-09/3223.sav')
# cis_2018_09 <- haven::as_factor(cis_2018_09)
# cis_list <- list('2018-09' = cis_2018_09)

overall_plot <- function(language = 'en',
                         geo = c('cat', 'esp', 'all'),
                         return_table = FALSE){
  
  cis_data  <- cis_list$`2018-09`
  
  if(length(geo) != 1){
    stop('geo must be one of "cat", "esp", "all"')
  }
  
  if(geo == 'esp'){
    plot_data <- cis_data %>% filter(CCAA != 'Cataluña')  
    if(language == 'en'){subtitle <- 'Spain, not counting Catalonia'} else {subtitle <-'Espanya, sense incloure Catalunya'}
  }
  if(geo == 'cat'){
    plot_data <- cis_data %>% filter(CCAA == 'Cataluña')
    if(language == 'en'){subtitle <- 'Catalonia'} else {subtitle <-'Catalunya'}
  }
  if(geo == 'all'){
    if(language == 'en'){subtitle <- 'Spain, including Catalonia'} else {subtitle <- 'Espanya, incloent Catalonia'}
    plot_data <- cis_data 
  }
  
  
  plot_data <- plot_data %>%
    mutate(P13 = as.character(P13)) %>%
    # Combine the no answer / not sures, etc.
    mutate(P13 = ifelse(P13 == 'N.C.',
                        '(NO LEER) Está en duda, no sabe lo suficiente',
                        P13)) %>%
    group_by(es = P13) %>%
    tally %>%
    ungroup %>%
    mutate(es = as.character(es)) %>%
    mutate(y  = n / sum(n) * 100)
  right <- 
    data_frame(
      # Manually copied from the Sep 2018 CIS avance de resultados
      # y = c(7.4, 39.9, 17.5, 19.5, 8.2, 7.4),
      es = c('Muy satisfecho/a',
             'Bastante satisfecho/a',
             '(NO LEER) Regular',
             'Poco satisfecho/a',
             'Nada satisfecho/a',
             '(NO LEER) Está en duda, no sabe lo suficiente'),
      ca = factor(c('Molt satisfet',
                    'Bastant satisfet',
                    'Regular',
                    'Poc satisfet',
                    'Gens satisfet',
                    'NS/NC'),
                  levels = c('Molt satisfet',
                             'Bastant satisfet',
                             'Regular',
                             'NS/NC',
                             'Poc satisfet',
                             'Gens satisfet')),
      en = factor(c('Very satisfied',
                    'Satisfied',
                    'So-so',
                    'Not satisfied',
                    'Not at all satisfied',
                    'No answer'),
                  levels = c('Very satisfied',
                             'Satisfied',
                             'So-so',
                             'No answer',
                             'Not satisfied',
                             'Not at all satisfied')))
  plot_data <- left_join(plot_data, right, by = 'es')
  if(language == 'en'){
    title <- 'Level of satisfaction with Spanish Constitution'
    caption <- 'Data from CIS survey, September 2018.\nJoe Brew | @joethebrew.'
    plot_data$x <- plot_data$en
  } else {
    title <- 'Nivell de satisfacció amb la Constitució Espanyola'
    caption <- 'Dades del CIS, Setembre 2018.\nJoe Brew | @joethebrew.'
    plot_data$x <- plot_data$ca
  }
  
  bp <- function(x,z){RColorBrewer::brewer.pal(n = 8,name =x)[z]}
  cols <- c(bp('Blues', 8),
            bp('Blues', 5),
            bp('Greys', 3),
            bp('Greys', 6),
            bp('Oranges', 5),
            bp('Oranges', 8))
  if(return_table){
    return(plot_data)
  }
  
  plot_data <- plot_data %>% arrange(x)
  x <- plot_data$y
  names(x) <- plot_data$x
  x <- round_percent(x)
  names(x) <- factor(levels(plot_data$x))
  
  gg <- waffle(parts = x,
               rows = 10,
               colors = cols) +
    theme_vilaweb() +
    theme(legend.position = 'right') +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0)) +
    coord_equal() +
    theme(panel.grid = element_blank()) +
    theme(panel.border = element_blank()) +
    theme(panel.background = element_blank()) +
    theme(panel.spacing = unit(0, "null")) +
    theme(axis.text = element_blank()) +
    theme(axis.title.x = element_text(size = 10)) +
    theme(axis.ticks = element_blank()) +
    theme(axis.line = element_blank()) +
    theme(axis.ticks.length = unit(0, "null")) +
    theme(plot.title = element_text(size = 18)) +
    theme(plot.background = element_blank()) +
    theme(panel.spacing = unit(c(0, 0, 0, 0), "null")) +
    guides(fill=guide_legend(ncol=1)) + 
    theme(legend.text = element_text(size = 14),
          plot.subtitle = element_text(size = 16))  +
    labs(title = title,
         subtitle = subtitle,
         caption = caption) 
  
  return(gg)
  # sample size 2972
}

comparison_plot <- function(language = 'en',
                            return_table = FALSE){
  
  plot_data  <- cis_list$`2018-09`
  
  
  plot_data <- plot_data %>%
    mutate(P13 = as.character(P13)) %>%
    # Combine the no answer / not sures, etc.
    mutate(P13 = ifelse(P13 == 'N.C.',
                        '(NO LEER) Está en duda, no sabe lo suficiente',
                        P13)) %>%
    group_by(es = P13, ccaa = CCAA) %>%
    tally %>%
    ungroup %>%
    mutate(es = as.character(es)) %>%
    group_by(ccaa) %>%
    mutate(y  = n / sum(n) * 100) %>%
    ungroup 
  
  right <- 
    data_frame(
      # Manually copied from the Sep 2018 CIS avance de resultados
      # y = c(7.4, 39.9, 17.5, 19.5, 8.2, 7.4),
      es = c('Muy satisfecho/a',
             'Bastante satisfecho/a',
             '(NO LEER) Regular',
             'Poco satisfecho/a',
             'Nada satisfecho/a',
             '(NO LEER) Está en duda, no sabe lo suficiente'),
      ca = factor(c('Molt satisfet',
                    'Bastant satisfet',
                    'Regular',
                    'Poc satisfet',
                    'Gens satisfet',
                    'NS/NC'),
                  levels = c('Molt satisfet',
                             'Bastant satisfet',
                             'Regular',
                             'NS/NC',
                             'Poc satisfet',
                             'Gens satisfet')),
      en = factor(c('Very satisfied',
                    'Satisfied',
                    'So-so',
                    'Not satisfied',
                    'Not at all satisfied',
                    'No answer'),
                  levels = c('Very satisfied',
                             'Satisfied',
                             'So-so',
                             'No answer',
                             'Not satisfied',
                             'Not at all satisfied')))
  plot_data <- left_join(plot_data, right, by = 'es')
  
  
  aa <- plot_data %>%
    group_by(ccaa) %>%
    summarise(satisfied = sum(y[en %in% c('Very satisfied',
                                          'Satisfied')]),
              unsatisfied = sum(y[en %in% c('Not satisfied',
                                            'Not at all satisfied')])) %>%
    mutate(bad = unsatisfied > satisfied)
  
  x <- 'CCAA'
  y <- 'Percentage'
  if(language == 'en'){
    subtitle <- 'By CCAA (autonomous community)'
    xxx <- 'Not at all satisfied'
    title <- 'Level of satisfacation with Spanish Constitution'
    caption <- 'Data from CIS survey, September 2018.\nChart: Joe Brew | @joethebrew.'
    plot_data$x <- plot_data$en
  } else {
    subtitle <- 'Per comunitat autònoma'
    xxx <- 'Gens satisfet'
    title <- 'Nivell de satisfacció amb la Constitució Espanyola'
    caption <- 'Dades del CIS, Setembre 2018.\nElaboració del gràfic: Joe Brew | @joethebrew.'
    plot_data$x <- plot_data$ca
  }
  
  bp <- function(x,z){RColorBrewer::brewer.pal(n = 8,name =x)[z]}
  cols <- c(bp('Blues', 8),
            bp('Blues', 5),
            bp('Greys', 3),
            bp('Greys', 6),
            bp('Oranges', 5),
            bp('Oranges', 8))
  if(return_table){
    return(plot_data)
  }
  
  plot_data <- plot_data %>% 
    filter(!grepl('Ceuta', ccaa),
           !grepl('Melilla', ccaa)) %>%
    mutate(ccaa = gsub(' (Principado de)', '', ccaa, fixed = TRUE),
           ccaa = gsub(' (Comunidad de)', '', ccaa, fixed = TRUE),
           ccaa = gsub(' (Región de)', '', ccaa, fixed = TRUE),
           ccaa = gsub(' (Comunidad Foral de)', '', ccaa, fixed = TRUE),
           ccaa = gsub('unitat', '.', ccaa, fixed = TRUE))
  plot_data <- plot_data %>% group_by(ccaa) %>% mutate(rangey = max(y[x == xxx], rm.na = TRUE)) %>% arrange(desc(rangey))
  plot_data$ccaa <- factor(plot_data$ccaa,
                           levels = unique(plot_data$ccaa))
  ggplot(data = plot_data,
         aes(x = ccaa,
             y = y,
             fill = x)) +
    geom_bar(stat = 'identity',
             position = position_stack(),
             width = 0.9) +
    scale_fill_manual(name = '',
                      values = cols) +
    theme_vilaweb() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1,
                                     vjust = 0.5),
          legend.text = element_text(size = 13)) +
    theme(legend.position = 'right') +
    guides(fill=guide_legend(ncol=1)) +
    geom_hline(yintercept = 50,
               lty = 2,
               alpha = 0.5) +
    labs(x = x,
         y = y,
         title = title,
         subtitle = subtitle,
         caption = caption)
}

ideology_spain_plot <- function(language = 'en',
                                geo = 'esp',
                                return_table = FALSE){
  
  plot_data  <- cis_list$`2018-09`
  
  if(geo == 'esp'){
    plot_data <- plot_data %>%
      filter(CCAA != 'Cataluña') 
  } else if(geo == 'cat'){
    plot_data <- plot_data %>%
      filter(CCAA == 'Cataluña') 
  }
  plot_data <- plot_data %>%
    mutate(P13 = as.character(P13)) %>%
    # Combine the no answer / not sures, etc.
    mutate(P13 = ifelse(P13 == 'N.C.',
                        '(NO LEER) Está en duda, no sabe lo suficiente',
                        P13)) %>%
    mutate(axis = P23) %>%
    mutate(axis = ifelse(axis %in% c('N.S.', 'N.C.'),
                         'NS/NC',
                         ifelse(axis %in% '1 Izquierda', as.character('01'),
                                ifelse(axis %in% '10 Derecha', as.character(10),
                                       as.character(axis))))) %>%
    mutate(axis = as.numeric(axis))
  
  # Join for the axis translations
  right <- data_frame(axis = 1:10,
                      ideology_en = factor(c(rep('Left', 3), rep('Center', 4), rep('Right', 3)),
                                           levels = c('Left', 'Center', 'Right')),
                      ideology_ca = factor(c(rep('Esquerra', 3), rep('Centre', 4), rep('Dreta', 3)),
                                           levels = c('Esquerra', 'Centre', 'Dreta')))
  plot_data <- left_join(plot_data, right, by = 'axis')
  if(language == 'en'){
    plot_data$new_axis <- plot_data$ideology_en
  } else {
    plot_data$new_axis <- plot_data$ideology_ca
  }
  plot_data$axis <- plot_data$new_axis
  
  plot_data <- plot_data %>%
    group_by(es = P13, 
             # ccaa = ifelse(CCAA == 'Cataluña', 'CAT', 'ESP'),
             axis) %>%
    tally %>%
    ungroup %>%
    mutate(es = as.character(es)) %>%
    group_by(#ccaa,
      axis) %>%
    mutate(y  = n / sum(n) * 100) %>%
    ungroup 
  right <- 
    data_frame(
      # Manually copied from the Sep 2018 CIS avance de resultados
      # y = c(7.4, 39.9, 17.5, 19.5, 8.2, 7.4),
      es = c('Muy satisfecho/a',
             'Bastante satisfecho/a',
             '(NO LEER) Regular',
             'Poco satisfecho/a',
             'Nada satisfecho/a',
             '(NO LEER) Está en duda, no sabe lo suficiente'),
      ca = factor(c('Molt satisfet',
                    'Bastant satisfet',
                    'Regular',
                    'Poc satisfet',
                    'Gens satisfet',
                    'NS/NC'),
                  levels = c('Molt satisfet',
                             'Bastant satisfet',
                             'Regular',
                             'NS/NC',
                             'Poc satisfet',
                             'Gens satisfet')),
      en = factor(c('Very satisfied',
                    'Satisfied',
                    'So-so',
                    'Not satisfied',
                    'Not at all satisfied',
                    'No answer'),
                  levels = c('Very satisfied',
                             'Satisfied',
                             'So-so',
                             'No answer',
                             'Not satisfied',
                             'Not at all satisfied')))
  plot_data <- left_join(plot_data, right, by = 'es')
  
  
  
  x <- 'CCAA'
  y <- 'Percentage'
  if(language == 'en'){
    subtitle <- 'By ideology. All of Spain, not including Catalonia.'
    xxx <- 'Not at all satisfied'
    title <- 'Level of satisfacation with Spanish Constitution'
    caption <- 'Data from CIS survey, September 2018.\nChart: Joe Brew | @joethebrew.\nIdeology: 1-3; left; 4-7: center; 8-10: right.'
    plot_data$x <- plot_data$en
  } else {
    subtitle <- 'Per ideologia. Tota Espanya, sense incloure Catalunya.'
    xxx <- 'Gens satisfet'
    title <- 'Nivell de satisfacció amb la Constitució Espanyola'
    caption <- 'Dades del CIS, Setembre 2018.\nElaboració del gràfic: Joe Brew | @joethebrew.\nIdeologia: 1-3: esquerra; 4-7: centre; 8-10: dreta.'
    plot_data$x <- plot_data$ca
  }
  
  bp <- function(x,z){RColorBrewer::brewer.pal(n = 8,name =x)[z]}
  cols <- c(bp('Blues', 8),
            bp('Blues', 5),
            bp('Greys', 3),
            bp('Greys', 6),
            bp('Oranges', 5),
            bp('Oranges', 8))
  if(return_table){
    return(plot_data)
  }
  
  # Remove the ns/nc
  plot_data <- plot_data %>%
    filter(axis != 'NS/NC')
  ggplot(data = plot_data,
         aes(x = axis,
             y = y,
             fill = x)) +
    geom_bar(stat = 'identity',
             position = position_stack(),
             width = 0.9) + 
    # facet_wrap(~ccaa) +
    scale_fill_manual(name = '',
                      values = cols) +
    theme_vilaweb() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1,
                                     vjust = 0.5),
          legend.text = element_text(size = 13)) +
    theme(legend.position = 'right') +
    guides(fill=guide_legend(ncol=1)) +
    geom_hline(yintercept = 50,
               lty = 2,
               alpha = 0.5) +
    labs(x = x,
         y = y,
         title = title,
         subtitle = subtitle,
         caption = caption)
}

left_right_plot <- function(language = 'en',
                            return_table = FALSE){
  if(language == 'en'){
    
    x = 'Ideology (self-assessed)'
    y = 'Percentage'
    title = 'Support for the Spanish Constitution among Catalans'
    subtitle = 'If a referendum were to be held today on the current\nSpanish Constitution approved in 1978, as it is now, how would you vote?'
    caption = 'Data from the Baròmetre d\'Opinió Pública, 3a onada 2018.\nSample size: 1500 residents of Catalonia with Spanish citizenship. Categories codes as follows:\n ideological scale 0-10: 0-1=far left;2-3=left;4-6=center;7-8=right;9-10=far right.\nChart created by Joe Brew | @joethebrew.'
    
    new_axis <- 
      c('Far left',
        'Far left',
        'Left',
        'Left',
        'Center',
        'Center',
        'Center',
        'Right',
        'Right',
        'Far right',
        'Far right')
  } else {
    
    x = 'Ideologia autoubicada (escala 0-10)'
    y = 'Percentage'
    title = 'Suport de la constitució espanyola entre catalans'
    subtitle = 'Si es tornés a celebrar un referèndum per decidir sobre l’actual\nConstitució espanyola aprovada el 1978, tal com és ara, vostè què faria?'
    caption = 'Dades del Baròmetre d\'Opinió Pública, 3a onada 2018.\nMostra: 1500 residents de Catalunya amb ciutadania espayola. Codificació de categories:\nescala ideòlogica 0-10: 0-1=extrema esquerra;2-3=esquerra;4-6=centre;7-8=dreta;9-10=extrema dreta.\nElaboració del gràfic: Joe Brew | @joethebrew.'
    new_axis <- 
      c('Extrema esquerra',
        'Extrema esquerra',
        'Esquerra',
        'Esquerra',
        'Centre',
        'Centre',
        'Centre',
        'Dreta',
        'Dreta',
        'Extrema dreta',
        'Extrema dreta')
  }
  axis_dict <- data_frame(axis = c('Extrema esquerra',
                                   as.character(1:9),
                                   'Extrema dreta'),
                          new_axis) %>%
    mutate(new_axis = gsub(' ', '\n', new_axis))
  plot_data <- 
    vilaweb::ceo %>%
    filter(MES %in% 10:11,
           ANY == 2018) %>%
    dplyr::mutate(axis = P25,
                  constitution = P95) %>%
    filter(!axis %in% c('No ho sap', 'No contesta')) %>%
    left_join(axis_dict) %>%
    dplyr::select(-axis) %>%
    dplyr::rename(axis = new_axis) %>%
    mutate(constitution = as.character(constitution))
  
  if(language == 'en'){
    plot_data <- plot_data %>%
      mutate(constitution = 
               ifelse(constitution == 'Votaria sí', 'Yes',
                      ifelse(constitution == 'Votaria no', 'No',
                             'Not sure/no answer')))
  } else {
    plot_data <- plot_data %>%
      mutate(constitution = ifelse(constitution %in% c('Votaria sí',
                                                       'Votaria no'),
                                   constitution,
                                   'NS/NC/Nul'))
  }
  plot_data <- plot_data %>%
    group_by(axis, constitution)%>%
    summarise(n = sum(PONDERA)) %>%
    ungroup %>%
    group_by(axis) %>%
    mutate(p = n / sum(n) * 100,
           number = sum(n),
           pp = n / sum(n[!constitution %in% c('NS/NC/Nul', 'Not sure/no answer')]) * 100) %>%
    ungroup
  
  if(language == 'en'){
    plot_data$constitution <-
      factor(plot_data$constitution,
             levels = rev(c('Yes',
                            'Not sure/no answer',
                            'No')))
    plot_data$axis <- 
      factor(plot_data$axis,
             levels = c('Far\nleft',
                        'Left',
                        'Center',
                        'Right',
                        'Far\nright'))
  } else {
    plot_data$constitution <-
      factor(plot_data$constitution,
             levels = rev(c('Votaria sí',
                            'NS/NC/Nul',
                            'Votaria no')))
    plot_data$axis <- 
      factor(plot_data$axis,
             levels = c('Extrema\nesquerra',
                        'Esquerra',
                        'Centre',
                        'Dreta',
                        'Extrema\ndreta'))
  }
  
  
  bp <- function(x,z){RColorBrewer::brewer.pal(n = 8,name =x)[z]}
  cols <- rev(c(bp('Blues', 5),
                bp('Greys', 2),
                bp('Oranges', 5)))
  if(return_table){
    return(plot_data)
  }
  ggplot(data = plot_data,
         aes(x = axis,
             y = p,
             fill = constitution)) +
    geom_bar(stat = 'identity',
             # color = 'black',
             # lwd = 0.2,
             position = position_stack()) +
    theme_vilaweb() +
    labs(x = x,
         y = y,
         title = title,
         subtitle = subtitle,
         caption = caption) +
    scale_fill_manual(name = '',
                      values = cols) +
    geom_text(aes(label = round(p, digits = 1)),
              position = position_stack(),
              vjust = 1.4,
              size = 3,
              alpha = 0.6) +
    theme(plot.title = element_text(size = 16),
          plot.subtitle = element_text(size = 11),
          axis.text.x = element_text(size = 15),
          axis.text.y = element_text(size = 13),
          legend.text = element_text(size = 20),
          plot.caption = element_text(size = 10))
}

bp <- function(x = 'Spectral', n = 9){
  RColorBrewer::brewer.pal(n = 9,
                           name = x)[n]
}

referendum_plot <- function(language = 'en',
                            return_table = FALSE){
  plot_data <- vilaweb::ceo %>%
    filter(!is.na(P56K)) %>%
    mutate(P56K = as.character(P56K)) %>%
    mutate(P56K = ifelse(P56K %in% c('No ho sap', 'No contesta'),
                         'NS/NC', P56K)) %>%
    group_by(referendum = P56K) %>%
    summarise(n = sum(PONDERA)) %>%
    ungroup %>%
    mutate(p = n / sum(n) * 100)
  
  right <- 
    data_frame(
      referendum = c("Molt d'acord",
                     "D'acord",
                     "Ni d'acord ni en desacord",
                     "En desacord",
                     "Molt en desacord",
                     "NS/NC"),
      ca = c("Molt\nd'acord",
                     "D'acord",
                     "Ni d'acord\nni en desacord",
                     "En\ndesacord",
                     "Molt en\ndesacord",
                     "NS/NC"),
      en = c("Strongly\nagree",
                          "Agree",
                          "Neither agree\nnor disagree",
                          "Disagree",
                          "Strongly\ndisagree",
                          "No answer/\ndon't know")
    )
  
  plot_data <- left_join(plot_data, right)
  
  x <-''
  y <- 'Percentage'
  if(language == 'en'){
    plot_data$en <-
      factor(plot_data$en,
             levels = c('Strongly\nagree',
                        'Agree',
                        "No answer/\ndon't know",
                        "Neither agree\nnor disagree",
                        'Disagree',
                        'Strongly\ndisagree'))
    plot_data$x <- plot_data$en
    title <- 'Agreement with following phrase:'
    subtitle <- '"Catalonia does not have a right to celebrate a self-determination referendum"'
    caption <- 'Data from Barometer of Public Opinion, 2018, round 2.\nChart by Joe Brew | @joethebrew.'
  } else {
    plot_data$ca <-
      factor(plot_data$ca,
             levels = c("Molt\nd'acord",
                        "D'acord",
                        "Ni d'acord\nni en desacord",
                        'NS/NC',
                        'En\ndesacord',
                        'Molt en\ndesacord'))
    plot_data$x <- plot_data$ca
    title <- "Grau d'acord amb la frase següent"
    subtitle <- '"Catalunya no té el dret de celebrar un referèndum d’autodeterminació"'
    caption <- 'Dades del Baròmetre d\'Opinió Públic, 2018, 2 ronda.\nGràfic de Joe Brew | @joethebrew.'
  }
  if(return_table){
    return(plot_data)
  }
  
  cols <- 
    c(bp('Oranges', c(7,4)),
      bp('Greys', c(4,6)),
      bp('Blues', c(5,7)))
  ggplot(data = plot_data,
         aes(x = x,
             y = p,
             fill = x)) +
    geom_bar(stat = 'identity') +
    theme_vilaweb() +
    labs(x = x,
         y = y,
         title = title,
         subtitle = subtitle,
         caption = caption) +
    scale_fill_manual(name = '',
                      values = cols) +
    theme(legend.position = 'none') +
    geom_text(aes(label = round(p, digits = 1)),
              alpha = 0.7,
              nudge_y = -2,
              color = 'white')
}

simple_plot <- function(language = 'en',
                            return_table = FALSE){
  x <- ''
  if(language == 'en'){
    y = 'Percentage'
    title = 'Support for the Spanish Constitution among Catalans'
    subtitle = 'If a referendum were to be held today on the current\nSpanish Constitution approved in 1978, as it is now, how would you vote?'
    caption = 'Data from the Baròmetre d\'Opinió Pública, 3a onada 2018.\nSample size: 1500 residents of Catalonia with Spanish citizenship.\nChart created by Joe Brew | @joethebrew.'
    
  } else {
    
    y = 'Percentage'
    title = 'Suport de la constitució espanyola entre catalans'
    subtitle = 'Si es tornés a celebrar un referèndum per decidir sobre l’actual\nConstitució espanyola aprovada el 1978, tal com és ara, vostè què faria?'
    caption = 'Dades del Baròmetre d\'Opinió Pública, 3a onada 2018.\nMostra: 1500 residents de Catalunya amb ciutadania espayola.\nElaboració del gràfic: Joe Brew | @joethebrew.'
  }

  plot_data <- 
    vilaweb::ceo %>%
    filter(MES %in% 10:11,
           ANY == 2018) %>%
    dplyr::mutate(constitution = P95) %>%
    mutate(constitution = as.character(constitution))
  
  if(language == 'en'){
    plot_data <- plot_data %>%
      mutate(constitution = 
               ifelse(constitution == 'Votaria sí', 'Yes',
                      ifelse(constitution == 'Votaria no', 'No',
                             'Not sure/no answer')))
  } else {
    plot_data <- plot_data %>%
      mutate(constitution = ifelse(constitution %in% c('Votaria sí',
                                                       'Votaria no'),
                                   constitution,
                                   'NS/NC/Nul'))
  }
  plot_data <- plot_data %>%
    group_by(constitution)%>%
    summarise(n = sum(PONDERA)) %>%
    ungroup %>%
    mutate(p = n / sum(n) * 100,
           number = sum(n),
           pp = n / sum(n[!constitution %in% c('NS/NC/Nul', 'Not sure/no answer')]) * 100) %>%
    ungroup
  
  if(language == 'en'){
    plot_data$constitution <-
      factor(plot_data$constitution,
             levels = rev(c('Yes',
                            'Not sure/no answer',
                            'No')))
  } else {
    plot_data$constitution <-
      factor(plot_data$constitution,
             levels = rev(c('Votaria sí',
                            'NS/NC/Nul',
                            'Votaria no')))
  }
  
  
  bp <- function(x,z){RColorBrewer::brewer.pal(n = 8,name =x)[z]}
  cols <- rev(c(bp('Blues', 5),
                bp('Greys', 6),
                bp('Oranges', 5)))
  if(return_table){
    return(plot_data)
  }
  ggplot(data = plot_data,
         aes(x = constitution,
             y = p,
             fill = constitution)) +
    geom_bar(stat = 'identity',
             # color = 'black',
             # lwd = 0.2,
             position = position_stack()) +
    theme_vilaweb() +
    labs(x = x,
         y = y,
         title = title,
         subtitle = subtitle,
         caption = caption) +
    scale_fill_manual(name = '',
                      values = cols) +
    geom_text(aes(label = round(p, digits = 1)),
              position = position_stack(),
              vjust = 2,
              # size = 3,
              color = 'white',
              alpha = 0.7) +
    theme(plot.title = element_text(size = 16),
          plot.subtitle = element_text(size = 11),
          axis.text.x = element_text(size = 15),
          axis.text.y = element_text(size = 13),
          legend.text = element_text(size = 20),
          plot.caption = element_text(size = 10),
          legend.position = 'none')
}
# simple_plot()

self_determination_plot <- function(language = 'en',
                                    geo = 'cat',
                                    return_table = FALSE){
  cis_data  <- cis_list$`2018-10`
  if(geo == 'esp'){
    plot_data <- cis_data %>% filter(CCAA != 'Cataluña')  
    if(language == 'en'){subtitle <- 'Spain, not counting Catalonia'} else {subtitle <-'Espanya, sense incloure Catalunya'}
  }
  if(geo == 'cat'){
    plot_data <- cis_data %>% filter(CCAA == 'Cataluña')
    if(language == 'en'){subtitle <- 'Catalonia'} else {subtitle <-'Catalunya'}
  }
  if(geo == 'all'){
    if(language == 'en'){subtitle <- 'Spain, including Catalonia'} else {subtitle <- 'Espanya, incloent Catalonia'}
    plot_data <- cis_data 
  }
  
  right <- 
    data_frame(P26 = 
                 c('Un Estado con un único Gobierno central sin autonomías',
                   'Un Estado en el que las comunidades autónomas tengan menor autonomía que en la actualidad',
                   'Un Estado con comunidades autónomas como en la actualidad',
                   'Un Estado en el que las comunidades autónomas tengan mayor autonomía que en la actualidad',
                   'Un Estado en el que se reconociese a las comunidades autónomas la posibilidad de convertirse en Estados independientes',
                   'N.S.',
                   'N.C.'),
               en = c('Central State/\nNo autonomy',
                      'Less autonomy',
                      'Status Quo',
                      'More autonomy',
                      'Possibility of independence',
                      'No answer/\nnot sure',
                      'No answer/\nnot sure'),
               ca = c('Estat central/\nSense autonomia',
                      'Menys autonomia',
                      'Statu Quo',
                      'Mes autonomia',
                      'Possibilitat de\n independència',
                      'NS/NC',
                      'NS/NC'))
  
  plot_data <- plot_data %>%
    left_join(right)
  if(language == 'en'){
    plot_data$x <- plot_data$en
    title <- 'Preferences for territorial organization'
    caption <- 'Data from CIS survey, October 2018.\nJoe Brew | @joethebrew.'
    x <- 'Percentage'
    y <- ''
  } else {
    plot_data$x <- plot_data$ca    
    title <- 'Preferències sobre organització territorial'
    caption <- 'Dades del CIS, Octubre 2018.\nJoe Brew | @joethebrew.'
    x <- 'Percentage'
    y <- ''
  }
  plot_data <- plot_data %>%
    group_by(x) %>%
    tally %>%
    ungroup %>%
    mutate(p = n / sum(n) * 100)
  if(language == 'en'){
    plot_data$x <- factor(plot_data$x, levels =
                            c('Central State/\nNo autonomy',
                              'Less autonomy',
                              'Status Quo',
                              'No answer/\nnot sure',
                              'More autonomy',
                              'Possibility of independence'))
  } else {
    plot_data$x <- factor(plot_data$x, levels =
                            c('Estat central/\nSense autonomia',
                              'Menys autonomia',
                              'Statu Quo',
                              'NS/NC',
                              'Mes autonomia',
                              'Possibilitat de\n independència'))
  }
  
  cols <- c(bp('Blues', 8),
            bp('Blues', 5),
            bp('Greys', 5),
            bp('Greys', 7),
            bp('Oranges', 5),
            bp('Oranges', 8))
  
  plot_data <- plot_data %>%
    arrange(x)
  
  if(return_table){
    return(plot_data)
  }

  ggplot(data = plot_data,
         aes(x = x,
             y = p,
             fill = x)) +
    geom_bar(stat = 'identity') +
    geom_text(aes(label = round(p, digits = 1)),
              alpha = 0.7,
              color = 'white',
              nudge_y = -2) +
    theme_vilaweb()+
    scale_fill_manual(name = '',
                      values = cols) +
    theme(legend.position = 'none') +
    coord_flip() +
    labs(x = x,
         y = y,
         title = title,
         subtitle = subtitle,
         caption= caption)
  
}
