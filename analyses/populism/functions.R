library(vilaweb)
colors_vilaweb <- function(){
  
  rgb2hex <- function(r,g,b) rgb(r, g, b, maxColorValue = 255)
  groc <- rgb2hex(240, 214, 0)
  rosa <- rgb2hex(232,98,130)
  verd <- rgb2hex(126, 177, 23)
  turquesa <- rgb2hex(44, 175, 194)
  blau <- rgb2hex(104, 133, 189)
  lila <- rgb2hex(156, 118, 172)
  
  out <- c(groc, rosa, verd, turquesa, blau, lila)
  names(out) <- c('groc', 'rosa', 'verd', 'turquesa', 'blau', 'lila') 
  return(out)
}

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
    caption <- 'Data from CIS survey, September 2018.\nJoe Brew | @joethebrew | www.vilaweb.cat.'
    plot_data$x <- plot_data$en
  } else {
    title <- 'Nivell de satisfacció amb la Constitució Espanyola'
    caption <- 'Dades del CIS, Setembre 2018.\nJoe Brew | @joethebrew | www.vilaweb.cat.'
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
    caption <- 'Data from CIS survey, September 2018.\nChart: Joe Brew | @joethebrew | www.vilaweb.cat.'
    plot_data$x <- plot_data$en
  } else {
    subtitle <- 'Per comunitat autònoma'
    xxx <- 'Gens satisfet'
    title <- 'Nivell de satisfacció amb la Constitució Espanyola'
    caption <- 'Dades del CIS, Setembre 2018.\nElaboració del gràfic: Joe Brew | @joethebrew | www.vilaweb.cat.'
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
    caption <- 'Data from CIS survey, September 2018.\nChart: Joe Brew | @joethebrew | www.vilaweb.cat.\nIdeology: 1-3; left; 4-7: center; 8-10: right.'
    plot_data$x <- plot_data$en
  } else {
    subtitle <- 'Per ideologia. Tota Espanya, sense incloure Catalunya.'
    xxx <- 'Gens satisfet'
    title <- 'Nivell de satisfacció amb la Constitució Espanyola'
    caption <- 'Dades del CIS, Setembre 2018.\nElaboració del gràfic: Joe Brew | @joethebrew | www.vilaweb.cat.\nIdeologia: 1-3: esquerra; 4-7: centre; 8-10: dreta.'
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
    caption = 'Data from the Baròmetre d\'Opinió Pública, 3a onada 2018.\nSample size: 1500 residents of Catalonia with Spanish citizenship. Categories codes as follows:\n ideological scale 0-10: 0-1=far left;2-3=left;4-6=center;7-8=right;9-10=far right.\nChart created by Joe Brew | @joethebrew | www.vilaweb.cat.'
    
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
    caption = 'Dades del Baròmetre d\'Opinió Pública, 3a onada 2018.\nMostra: 1500 residents de Catalunya amb ciutadania espayola. Codificació de categories:\nescala ideòlogica 0-10: 0-1=extrema esquerra;2-3=esquerra;4-6=centre;7-8=dreta;9-10=extrema dreta.\nElaboració del gràfic: Joe Brew | @joethebrew | www.vilaweb.cat.'
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
    caption <- 'Data from Barometer of Public Opinion, 2018, round 2.\nChart by Joe Brew | @joethebrew | www.vilaweb.cat.'
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
    caption <- 'Dades del Baròmetre d\'Opinió Públic, 2018, 2 ronda.\nGràfic de Joe Brew | @joethebrew | www.vilaweb.cat.'
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
    caption = 'Data from the Baròmetre d\'Opinió Pública, 3a onada 2018.\nSample size: 1500 residents of Catalonia with Spanish citizenship.\nChart created by Joe Brew | @joethebrew | www.vilaweb.cat.'
    
  } else {
    
    y = 'Percentage'
    title = 'Suport de la constitució espanyola entre catalans'
    subtitle = 'Si es tornés a celebrar un referèndum per decidir sobre l’actual\nConstitució espanyola aprovada el 1978, tal com és ara, vostè què faria?'
    caption = 'Dades del Baròmetre d\'Opinió Pública, 3a onada 2018.\nMostra: 1500 residents de Catalunya amb ciutadania espayola.\nElaboració del gràfic: Joe Brew | @joethebrew | www.vilaweb.cat.'
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
    caption <- 'Data from CIS survey, October 2018.\nJoe Brew | @joethebrew | www.vilaweb.cat.'
    x <- 'Percentage'
    y <- ''
  } else {
    plot_data$x <- plot_data$ca    
    title <- 'Preferències sobre organització territorial'
    caption <- 'Dades del CIS, Octubre 2018.\nJoe Brew | @joethebrew | www.vilaweb.cat.'
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

referendum_plot_party <- function(language = 'en',
                            return_table = FALSE){
  plot_data <- vilaweb::ceo %>%
    filter(!is.na(P56K)) %>%
    mutate(P56K = as.character(P56K)) %>%
    mutate(P56K = ifelse(P56K %in% c('No ho sap', 'No contesta'),
                         'NS/NC', P56K)) %>%
    mutate(partit = P24R) %>%
    mutate(partit = as.character(partit)) %>%
    mutate(partit = ifelse(partit %in% c("Catalunya sí que es pot",
                                         "Catalunya en Comú Podem"),
                           'Comuns',
                           ifelse(partit %in% c("Junts per Catalunya",
                                                "Junts pel Sí"),
                                  "JuntsXCat/Sí",
                                  partit))) %>%
    group_by(referendum = P56K, partit) %>%
    summarise(n = sum(PONDERA)) %>%
    ungroup %>%
    group_by(partit) %>%
    mutate(p = n / sum(n) * 100) %>%
    ungroup %>%
    filter(!partit %in% c('Altres partits', 'Cap', 'No ho sap',
                          'No contesta'))
  
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
    caption <- 'Data from Barometer of Public Opinion, 2018, round 2.\nChart by Joe Brew | @joethebrew | www.vilaweb.cat.'
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
    caption <- 'Dades del Baròmetre d\'Opinió Públic, 2018, 2 ronda.\nGràfic de Joe Brew | @joethebrew | www.vilaweb.cat.'
  }
  if(return_table){
    return(plot_data)
  }
  
  cols <- 
    c(bp('Oranges', c(7,4)),
      bp('Greys', c(4,6)),
      bp('Blues', c(5,7)))
  ggplot(data = plot_data,
         aes(x = partit,
             y = p,
             fill = x)) +
    geom_bar(stat = 'identity',
             position = 'stack') +
    theme_vilaweb() +
    labs(x = x,
         y = y,
         title = title,
         subtitle = subtitle,
         caption = caption) +
    scale_fill_manual(name = '',
                      values = cols) +
    # theme(legend.position = 'none') +
    geom_hline(yintercept = 50, lty = 2, alpha = 0.6)
    # geom_text(aes(label = round(p, digits = 1)),
    #           alpha = 0.7,
    #           color = 'black',
    #           position = position_stack()) +
    # coord_flip()
}






# uk_education
uk_education <- function(ca = FALSE){
  if(!ca){
    the_labs <- labs(x = '',
                     y = 'Percent in favor of Brexit',
                     title = 'Education and support for Brexit',
                     caption = 'Data source: "Understanding the Leave Vote" by Kirby Swales.\nChart by Joe Brew. www.vilaweb.cat')
    education <- factor(c('Secondary school not completed',
                          'Secondary schoool completed',
                          'Higher education below degree',
                          'Degree'))
    education <- factor(education, 
                        levels = education,
                        labels = gsub(' ', '\n', education))
  } else {
    the_labs <- labs(x = '',
                     y = 'Percentage a favor del Brexit',
                     title = 'Nivell educatiu i suport al Brexit',
                     caption = 'Font de dades: "Understanding the Leave Vote" de Kirby Swales.\nGràfic de Joe Brew. www.vilaweb.cat')
    education <- factor(c('Escola secundaria no acabada',
                          'Escola secundaria acabada',
                          'Estudis universitaris no acabats',
                          'Grau universitari'))
    education <- factor(education, 
                        levels = education,
                        labels = gsub(' ', '\n', education))
  }
  df <- tibble(education,
               y = c(78,61, 50, 26))
  ggplot(data = df,
         aes(x = education,
             y = y)) +
    geom_bar(stat = 'identity',
             fill = as.character(colors_vilaweb()[5])) +
    theme_vilaweb() +
    the_labs +
    theme(axis.text.x = element_text(size = 13),
          plot.caption = element_text(size = 10)) +
    geom_text(aes(label = round(y, digits = 1)),
              nudge_y = -5,
              alpha = 0.8,
              size = 5,
              color = 'white')
}

# cat_education
cat_education <- function(ca = FALSE){
  
  
  education_dict <- tibble(
    C500 = c("No sap llegir i escriure",
             "Sap llegir i escriure però va anar menys de 5 anys a l'escola",
             "Va anar a l'escola 5 o mès anys però sense completar ESO, EGB o Batxillerat elemental",
             "ESO completa (Graduat escolar), EGB, Batxiller elemental o Certificat d' Escolaritat o d'Estudis Primaris",
             "Batxillerat, BUP, COU, Batxillerat superior, PREU",
             "Cicle formatiu de grau mitjà, FP1, Oficialia industrial o equivalent",
             "Cicle formatiu de grau superior, FP2, Mestratge industrial o equivalent",
             "Diplomatura, Enginyeria/arquitectura tècnica o equivalent",
             "Llicenciatura, Arquitectura, Enginyeria, Grau o equivalent (4 anys o mès)",
             "Cursos d'especialització professional, Postgrau, Màster (MIR, FIR...)",
             "Doctorat",
             "Altres estudis",
             "95",
             "No ho sap",
             "No contesta"),
    ca = c("Menys de 5 anys\na l'escola",
           "Menys de 5 anys\na l'escola",
           "5 anys a l'escola\nfins a Batxillerat/\nBUP/COU/PREU",
           "5 anys a l'escola\nfins a Batxillerat/\nBUP/COU/PREU",
           "5 anys a l'escola\nfins a Batxillerat/\nBUP/COU/PREU",
           "Grau, FP1, FP2,\nOficialia, Mestratge",
           "Grau, FP1, FP2,\nOficialia, Mestratge",
           "Llicenciatura,\nDiplomatura,\nEnginyeria tècnica",
           "Llicenciatura,\nDiplomatura,\nEnginyeria tècnica",
           "Postgrau, Màster,\nMIR, Doctorat",
           "Postgrau, Màster,\nMIR, Doctorat",
           rep(NA, 4)),
    en = c("<5 years\nof schooling",
           "<5 years\nof schooling",
           "5+ years\nof school to\nhigh school",
           "5+ years\nof school to\nhigh school",
           "5+ years\nof school to\nhigh school",
           "Post high school,\ntrade, technical\ndegree",
           "Post high school,\ntrade, technical\ndegree",
           "College or\ntechnical\nengineer degree",
           "College or\ntechnical\nengineer degree",
           "Graduate studies,\nMaster, Doctor,\nDoctorate",
           "Graduate studies,\nMaster, Doctor,\nDoctorate",
           rep(NA, 4))
  )
  education_dict$ca <- factor(education_dict$ca,
                              levels = unique(education_dict$ca))
  education_dict$en <- factor(education_dict$en,
                              levels = unique(education_dict$en))
  df <- vilaweb::ceo
  df <- df %>%
    mutate(indy = P31) %>%
    mutate(indy = as.character(indy)) %>%
    mutate(indy = ifelse(indy %in% c('No ho sap',
                                     'No contesta'),
                         'NS/NC',
                         indy)) %>%
    left_join(education_dict) %>%
    filter(!is.na(indy))
  if(ca){
    df$education <- df$ca
    legend_title <- 'Independentista'
  } else {
    df$education <- df$en
    legend_title <- 'Pro-\nindependence'
  }
  df <- df %>% filter(!is.na(education))
  pd <- df %>% group_by(education, indy) %>%
    tally %>%
    group_by(education) %>%
    mutate(p = n / sum(n) * 100) %>%
    arrange(indy)
  
  pd$indy <- factor(pd$indy,
                    levels = c('No', 'NS/NC', 'Sí'))
  if(!ca){
    pd$indy <- factor(pd$indy,
                      levels = c('No', 'NS/NC', 'Sí'),
                      labels = c('No', 'Not sure/\nno answer', 'Yes'))
  }
  
  # cols <- RColorBrewer::brewer.pal(n = 9,
  #                                  name = 'YlGnBu')
  # cols2 <- RColorBrewer::brewer.pal(n = 9,
  #                                   name = 'BrBG')
  # cols <- c(cols2[2], 
  #           grey(0.7),
  #           # grey(0.6),
  #           cols[6])
  cols <- databrew::make_colors(n = 10, categorical = TRUE)
  cols <- cols[c(2,3, 6)]
  cols <- rev(cols)
  # cols <- rev(cols)
  cols[2] <- grey(0.2)
  
  if(ca){
    the_labs <- labs(x = '',
                     y = 'Percentage',
                     title = 'Nivell educatiu i independentisme',
                     caption = paste0(
                       'Font de dades: Combinació d\'enquestes del Baròmetre d\'Opinió Política, Centre d\'Estudios d\'Opinió.\nDades recollides 2015-2018. Mostreig: ', sum(pd$n),  ' residents de Catalunya amb ciutadania espanyola.\nGràfic de Joe Brew. www.vilaweb.cat'))
  } else {
    the_labs <- labs(x = '',
                     y = 'Percentage',
                     title = 'Education and support for Catalan independence',
                     caption = paste0(
                       'Data source: Combination of surveys from the Baròmetre d\'Opinió Política, Centre d\'Estudios d\'Opinió.\nData collected 2015-2018. Sample: ', sum(pd$n),  ' residents of Catalonia with Spanish citizenship.\nChart: Joe Brew. www.vilaweb.cat'))
  }
  
  ggplot(data = pd,
         aes(x = education,
             y = p,
             fill = indy)) +
    geom_bar(stat = 'identity',
             position = position_stack()) +
    scale_fill_manual(name = legend_title,
                      values = cols) +
    theme_vilaweb() +
    theme(legend.position = 'right',
          legend.title = element_text(size = 7)) +
    theme(axis.text.x = element_text(angle = 90,
                                     hjust = 1,
                                     vjust = 0.5,
                                     size = 12),
          plot.caption  = element_text(hjust = 0,
                                       size = 9)) +
    geom_text(aes(label = round(p, digits = 1),
                  y = p),
              position = position_stack(vjust = 0.5),
              color = 'white',
              alpha = 0.7) +
    the_labs
}


# uk_immigration
uk_immigration <- function(ca = FALSE){
  if(!ca){
    the_labs <- labs(x = '',
                     y = 'Percent in favor of Brexit',
                     title = 'Lowering immigration was a main issue in the Brexit vote',                     
                     caption = 'Data source: "Understanding the Leave Vote" by Kirby Swales.\nChart by Joe Brew. www.vilaweb.cat')
    immigration <- factor(c('EU membership\nundermines\nBritain\'s\nindependence',
                            'Leaving the\nEU would\nmake\nimmigration\nlower',
                            'Leaving the\nEU would\nmake Britain\'s\neconomy\nworse'))
    immigration <- factor(immigration, 
                          levels = immigration)
  } else {
    the_labs <- labs(x = '',
                     y = 'Percentage a favor del Brexit',
                     title = 'Reduir la immigració: una des les motivacions principals del Brexit',
                     caption = 'Font de dades: "Understanding the Leave Vote" de Kirby Swales.\nGràfic de Joe Brew. www.vilaweb.cat')
    immigration <- factor(c('Adhesió a la\nUE soscava la\nindependència de\nBretanya',
                            'Marxar de la\nUE reduiria\nla immigració',
                            'Marxar de la\nUE empitjoraria la\neconomia de\nBretanya'))
    immigration <- factor(immigration, 
                          levels = immigration)
  }
  df <- tibble(immigration,
               y = c(66,66,43))
  ggplot(data = df,
         aes(x = immigration,
             y = y)) +
    geom_bar(stat = 'identity',
             fill = as.character(colors_vilaweb()[5])) +
    theme_vilaweb() +
    the_labs +
    theme(axis.text.x = element_text(size = 10),
          plot.caption = element_text(size = 10),
          plot.title = element_text(size = 16)) +
    geom_text(aes(label = round(y, digits = 1)),
              nudge_y = -5,
              alpha = 0.8,
              size = 5,
              color = 'white') 
}

cat_immigration <- function(ca = FALSE){
  pd <- vilaweb::ceo %>%
    mutate(indy = P31) %>%
    mutate(indy = as.character(indy)) %>%
    mutate(indy = ifelse(indy %in% c('No ho sap',
                                     'No contesta'),
                         'NS/NC',
                         indy)) %>%
    filter(!is.na(indy)) %>%
    filter(indy != 'NS/NC') %>%
    mutate(economy = P56I) %>%
    mutate(economy = as.character(economy)) %>%
    mutate(economy = ifelse(economy %in% c('No ho sap',
                                           'No contesta'),
                            'NS/NC',
                            economy)) %>%
    filter(!is.na(economy)) %>%
    filter(economy != 'NS/NC') %>%
    mutate(economy = 
             ifelse(economy %in% c("Molt d'acord",
                                   "D'acord"),
                    "D'acord o\nmolt d'acord",
                    ifelse(economy %in% c("En desacord",
                                          "Molt en desacord"),
                           "En desacord o\nmolt en desacord",
                           "Ni d'acord ni\nen desacord"))) %>% 
    group_by(economy, indy) %>%
    summarise(n = sum(PONDERA)) %>%
    ungroup %>%
    group_by(indy) %>%
    mutate(p = n / sum(n) * 100) %>%
    mutate(`Muestra` = n)
  
  pd$economy<- factor(pd$economy,
                      levels = c("D'acord o\nmolt d'acord",
                                 "Ni d'acord ni\nen desacord",
                                 "En desacord o\nmolt en desacord"))
  if(!ca){
    pd$economy<- factor(pd$economy,
                        levels = c("D'acord o\nmolt d'acord",
                                   "Ni d'acord ni\nen desacord",
                                   "En desacord o\nmolt en desacord"),
                        labels = c('Agree or\nstrongly agree',
                                   'Neither agree\nnor disagree',
                                   'Disagree or\nstrongly disagree'))
    pd$indy <- ifelse(pd$indy == 'No',
                      'Pro-Union Catalans',
                      'Pro-Independence Catalans')
    the_labs <- labs(x = '',
                     y = 'Percentage',
                     subtitle = "Agreement with above phrase",
                     title =  "'With so much immigration,\none no longer feels at home'",
                     caption = 'Sample: 3143 residents of Catalonia with Spanish citizenship.\nCombination of BOP/CEO surveys, 2015 and 2018. Questions P56I and P31.\nJoe Brew | www.vilaweb.cat.')
  } else {
    pd$indy <- ifelse(pd$indy == 'No',
                      'Unionistes',
                      'Independentistes')
    the_labs <- labs(x = '',
                     y = 'Percentage',
                     subtitle = "Grau d'acord amb l'afirmació",
                     title =  "'Amb tanta immigració,\nun ja no se sent com a casa'",
                     caption = 'Mostra: 3143 residents de Catalunya amb ciutadania espanyola.\nCombinació enquestes CEO. 2015 i 2018. Preguntes P56I i P31.\nJoe Brew | www.vilaweb.cat.')
  }
  n_cols <- length(unique(pd$economy))
  cols <- databrew::make_colors(n = n_cols, categorical = FALSE)
  # cols <- rev(cols)
  cols[2] <- 'darkgrey'
  
  ggplot(data = pd,
         aes(x = economy,
             y = p)) +
    geom_bar(stat = 'identity',
             position = position_dodge(width = 0.9),
             # color = 'black',
             alpha = 0.95,
             aes(fill = economy)) +
    facet_wrap(~indy) +
    theme_vilaweb() +
    # scale_fill_manual(name = '',
    #                   values = cols) +
    geom_text(aes(label = round(p, digits = 2),
                  y = p -5),
              alpha = 0.9,
              color = 'white',
              position = position_dodge(width = 0.9),
              size = 4) +
    scale_fill_manual(name = '',
                      values = as.character(colors_vilaweb()[c(1,5,3)])) +
    the_labs +
    theme(legend.position = 'none',
          plot.caption = element_text(size = 9,
                                      hjust = 0),
          axis.text.x = element_text(size = 8))
}

cat_immigration2 <- function(ca = FALSE){
  party_dict <- 
    tibble(P24 = c("PPC",
                   "CiU",
                   "ERC",
                   "PSC",
                   "ICV-EUiA",
                   "C's",
                   "Reagrupament.cat",
                   "SI",
                   "PxC",
                   "CUP",
                   "UPyD",
                   "Podemos",
                   "Barcelona en Comú",
                   "CDC",
                   "Junts pel Sí",
                   "Catalunya sí que es pot",
                   "Democràcia i Llibertat",
                   "En Comú Podem",
                   "PACMA",
                   "PDeCAT",
                   "Junts per Catalunya",
                   "Catalunya en Comú Podem",
                   "Altres partits",
                   "Cap",
                   "No ho sap",
                   "No contesta"),
           partit = c("PPC",
                      "PDCat/CiU/CDC/Junts",
                      "ERC",
                      "PSC",
                      "ICV-EUiA",
                      "C's",
                      "Reagrupament.cat",
                      "SI",
                      "PxC",
                      "CUP",
                      "UPyD",
                      "Podem(os)",
                      "Podem(os)",
                      "PDCat/CiU/CDC/Junts",
                      "PDCat/CiU/CDC/Junts",
                      "Podem(os)",
                      "Democràcia i Llibertat",
                      "Podem(os)",
                      "PACMA",
                      "PDCat/CiU/CDC/Junts",
                      "PDCat/CiU/CDC/Junts",
                      "Podem(os)",
                      "Altre/Cap/NS/NC",
                      "Altre/Cap/NS/NC",
                      "Altre/Cap/NS/NC",
                      "Altre/Cap/NS/NC"))
  
  spanish <- FALSE
  if(ca){
    catalan <- TRUE
    english <- FALSE
  } else {
    catalan <- FALSE
    english <- TRUE
  }
  
  pd <- vilaweb::ceo %>%
    left_join(party_dict) %>%
    group_by(partit) %>%
    mutate(size = n()) %>%
    filter(size >= 50) %>%
    ungroup %>%
    mutate(year = ANY) %>%
    mutate(axis = as.character(P25)) %>%
    mutate(axis = ifelse(axis == 'Extrema esquerra',
                         '1',
                         ifelse(axis == 'Extrema dreta',
                                '10',
                                as.character(axis)))) %>%
    mutate(axis = as.numeric(axis)) %>%
    filter(!partit %in% c('Altre/Cap/NS/NC',
                          'ICV-EUIA'))
  
  pd <- pd %>%
    mutate(indy = P31) %>%
    mutate(indy = as.character(indy)) %>%
    mutate(indy = ifelse(indy %in% c('No ho sap',
                                     'No contesta'),
                         'NS/NC',
                         indy)) %>%
    filter(!is.na(indy)) %>%
    filter(indy != 'NS/NC') %>%
    mutate(economy = P56I) %>%
    mutate(economy = as.character(economy)) %>%
    mutate(economy = ifelse(economy %in% c('No ho sap',
                                           'No contesta'),
                            'NS/NC',
                            economy)) %>%
    filter(!is.na(economy)) %>%
    filter(economy != 'NS/NC') %>%
    mutate(economy = 
             ifelse(economy %in% c("Molt d'acord",
                                   "D'acord"),
                    "D'acord o molt d'acord",
                    ifelse(economy %in% c("En desacord",
                                          "Molt en desacord"),
                           "En desacord o molt en desacord",
                           "Ni d'acord ni en desacord"))) %>% 
    group_by(economy, indy) %>%
    summarise(n = sum(PONDERA)) %>%
    ungroup %>%
    group_by(indy) %>%
    mutate(p = n / sum(n) * 100) %>%
    mutate(`Muestra` = n)
  
  original_levels <- c("D'acord o molt d'acord",
                       "Ni d'acord ni en desacord",
                       "En desacord o molt en desacord")
  cat_levels <- c("\nD'acord o\nmolt\nd'acord\n",
                  "\nNi d'acord\nni en\ndesacord\n",
                  "\nEn desacord\no molt en\ndesacord\n")
  en_levels <- c("\nAgree or\nstrongly agree\n",
                 "\nNeither agree\nnor disagree\n",
                 "\nDisagree or\nstrongly disagree\n")
  esp_levels <- c("\nDe acuerdo\no muy\nde acuerdo\n",
                  "\nNi de\nacuerdo ni\nen desacuerdo\n",
                  "\nEn desacuerdo\no muy\nen desacuerdo\n")
  
  if(catalan){
    the_subtitle <- "Grau d'acord amb l'afirmació"
    the_levels <- cat_levels
    the_title <- "\n'Amb tanta immigració, un ja no se sent com a casa'"
    the_caption <- 'Mostra: 3143 residents de Catalunya amb ciutadania espanyola. Dades: Combinació enquestes CEO.\n2015 i 2018. Preguntes P56I i P31. Gràfic: Joe Brew | @joethebrew.'
    indies <- c('A favor\nde la\nindependència',
                'NS/NC',
                'En contra\nde la\nindependència')
  } else if(english){
    the_levels <- en_levels
    the_subtitle <- "Extent of agreement with the phrase"
    the_title <- '\n"With so much immigration, it doesn\'t feel like home any more"'
    the_caption <- 'Sample size: 3143 residents of Catalonia with Spanish citizenship. Data: Aggregated CEO surveys,\n2015 and 2018. Questions P56I and P31. Chart: Joe Brew | @joethebrew | joe@databrew.cc\nCode for reproducibility: https://github.com/joebrew/vilaweb/tree/master/analyses/xenofobia.'
    indies <- c('In favor of\nCatalan\nindependence',
                'Not sure/no answer',
                'Against\nCatalan\nindependence')
  } else {
    the_levels <- esp_levels
    the_subtitle <- "Grado de acuerdo con la afirmación"
    the_title <- '\n"Con tanta inmigración, uno ya no se siente como en casa"'
    the_caption <- 'Muestra: 3143 residentes de Cataluña con ciudadanía española. Datos: Combinación encuestas CEO.\n2015 y 2018. Preguntas P56I y P31. Gráfico: Joe Brew | @joethebrew.'
    indies <- c('A favor\nde la\nindependencia',
                'NS/NC',
                'En contra\nde la\nindependencia')
  }
  
  pd$indy <- 
    ifelse(pd$indy == 'Sí', indies[1],
           ifelse(pd$indy == 'No', indies[3],
                  indies[2]))
  
  pd$economy<- factor(pd$economy,
                      levels = rev(original_levels),
                      labels = rev(the_levels))
  n_cols <- length(unique(pd$economy))
  cols <- databrew::make_colors(n = 10, categorical = TRUE)
  cols <- cols[c(2,3, 6)]
  cols <- rev(cols)
  # cols <- rev(cols)
  cols[2] <- grey(0.2)
  
  pd <- pd %>%
    arrange(desc(economy), p)
  pd$indy <- factor(pd$indy, levels = unique(pd$indy))
  
  ggplot(data = pd,
         aes(x = indy,
             y = p)) +
    geom_bar(stat = 'identity',
             position = 'stack',
             # position = position_dodge(width = 0.9),
             # color = 'black',
             alpha = 0.85,
             aes(fill = economy)) +
    # facet_wrap(~partit) +
    theme_vilaweb() +
    # scale_fill_manual(name = '',
    #                   values = cols) +
    geom_text(aes(label = paste0(round(p, digits = 1), '%'),
                  y = p-1),
              alpha = 0.9,
              col = 'white',
              vjust = 1,
              position = 'stack',
              # position = position_dodge(width = 0.9),
              size = 5) +
    scale_fill_manual(name = '',
                      values = cols) +
    labs(x = '',
         y = '%',
         subtitle = the_subtitle,
         title =  the_title,
         caption = the_caption) +
    theme(legend.position = 'right',
          axis.text.x = element_text(size = 12),
          plot.caption = element_text(hjust = 0,
                                      color = cols[1]))
  
}




# uk_authority
uk_authority <- function(ca = FALSE){
  if(!ca){
    the_labs <- labs(x = '',
                     y = 'Percent in favor of Brexit',
                     title = 'Authoritarianism in the "leave" vote',
                     caption = 'Data source: "Understanding the Leave Vote" by Kirby Swales.\nChart by Joe Brew. www.vilaweb.cat')
    authority <- factor(c('Authoritarian', 'Neither', 'Libertarian'))
    authority <- factor(authority, 
                        levels = authority)
  } else {
    the_labs <- labs(x = '',
                     y = 'Percentage a favor del Brexit',
                     title = 'Autoritarisme i suport al Brexit',
                     caption = 'Font de dades: "Understanding the Leave Vote" de Kirby Swales.\nGràfic de Joe Brew. www.vilaweb.cat')
    authority <- factor(c('Autoritari', 'Cap dels dos', 'Llibertari'))
    authority <- factor(authority, 
                        levels = authority)
  }
  df <- tibble(authority,
               y = c(66, 38, 18))
  ggplot(data = df,
         aes(x = authority,
             y = y)) +
    geom_bar(stat = 'identity',
             fill = as.character(colors_vilaweb()[5])) +
    theme_vilaweb() +
    the_labs +
    theme(axis.text.x = element_text(size = 13),
          plot.caption = element_text(size = 10)) +
    geom_text(aes(label = round(y, digits = 1)),
              nudge_y = -5,
              alpha = 0.8,
              size = 5,
              color = 'white')
}

# cat_authority
var_cat <- function(ca = FALSE, var = 'P56I'){
  
  pd <- vilaweb::ceo %>%
    mutate(year = ANY) %>%
    mutate(indy = P31) %>%
    mutate(indy = as.character(indy)) %>%
    mutate(indy = ifelse(indy %in% c('No ho sap',
                                     'No contesta'),
                         'NS/NC',
                         indy)) %>%
    filter(!is.na(indy)) %>%
    filter(indy != 'NS/NC') %>%
    rename(variable = var)
  pd <- pd %>%
    mutate(variable = as.character(variable)) %>%
    mutate(variable = ifelse(variable %in% c('No ho sap',
                                             'No contesta'),
                             'NS/NC',
                             variable)) %>%
    filter(!is.na(variable)) %>%
    filter(variable != 'NS/NC') %>%
    mutate(variable = as.character(variable)) %>%
    mutate(variable = 
             ifelse(variable %in% c("Molt d'acord",
                                    "D'acord"),
                    "D'acord o molt d'acord",
                    ifelse(variable %in% c("En desacord",
                                           "Molt en desacord"),
                           "En desacord o molt en desacord",
                           "Ni d'acord ni en desacord"))) %>% 
    group_by(variable, indy) %>%
    summarise(n = sum(PONDERA),
              Muestra = n()) %>%
    ungroup %>%
    group_by(indy) %>%
    mutate(p = n / sum(n) * 100) 
  
  original_levels <- c("D'acord o molt d'acord",
                       "Ni d'acord ni en desacord",
                       "En desacord o molt en desacord")
  cat_levels <- c("\nD'acord o\nmolt\nd'acord\n",
                  "\nNi d'acord\nni en\ndesacord\n",
                  "\nEn desacord\no molt en\ndesacord\n")
  en_levels <- c("\nAgree or\nstrongly agree\n",
                 "\nNeither agree\nnor disagree\n",
                 "\nDisagree or\nstrongly disagree\n")
  
  phrase_dict <- 
    tibble(var = paste0('P56',
                        c(LETTERS[1:11])),
           catalan = c("“Com menys intervingui el Govern en l’economia, millor serà pel país”",
                       "“Cal baixar els impostos, encara que això impliqui reduir\nserveis i prestacions públiques”",
                       "“El Govern hauria de prendre mesures per reduir les diferències en els nivells d’ingressos”",
                       "“Les parelles de gais i lesbianes han de poder adoptar fills en les mateixes condicions que les parelles heterosexuals”",
                       "“L’escola ha d’ensenyar als nens a obeir l’autoritat”",
                       "“La religió no hauria de tenir cap influència en la política”",
                       "“En qualsevol circumstància, la llei sempre ha de ser obeïda”",
                       "“Algú amb plenes facultats hauria de poder decidir quan vol morir”",
                       "“Amb tanta immigració, un ja no se sent com a casa”",
                       "“El creixement econòmic ha de tenir prioritat sobre la protecció del medi ambient”",
                       "“Catalunya no té el dret de celebrar un referèndum d’autodeterminació”"),
           english = c("“The less the government interferes in the economy, the better off the country will be”",
                       "“Taxes must be lowered, even though it may\nmean reducing public services”",
                       "“The government should take measures to\nreduce differenes in income”",
                       "“Gay and lesbian couples should be able to adopt children under the same conditions as heterosexual couples”",
                       "“School should teach children to obey authority”",
                       "“Religion should have no influence on politics”",
                       "“The law should always be obeyed in any circumstance”",
                       "“Someone with full abilities should be allowed to decide when (s)he wants to die”",
                       "“With so much immigration, one no longer feels at home”",
                       "“Economic growth should have priority over protection of the environment”",
                       "“Catalonia does not have a right to hold a self-determination referendum”"))
  
  if(ca){
    the_subtitle <- "Grau d'acord amb l'afirmació"
    the_levels <- cat_levels
    the_title <- phrase_dict$catalan[phrase_dict$var == var]
    the_caption <- paste0('Mostra: ',
                          sum(pd$Muestra),
                          ' residents de Catalunya amb ciutadania espanyola. Dades: Combinació enquestes CEO.\n2015 i 2018. Preguntes ',
                          var,' i P31. Gràfic: Joe Brew | @joethebrew.')
    indies <- c('A favor\nde la\nindependència',
                'NS/NC',
                'En contra\nde la\nindependència')
  } else {
    the_levels <- en_levels
    the_subtitle <- "Extent of agreement with the phrase"
    the_title <- phrase_dict$english[phrase_dict$var == var]
    the_caption <- paste0('Sample: ',
                          sum(pd$Muestra),
                          ' residents of Catalonia with Spanish citizenship. Data: Combination of CEO/BOP surveys.\n2015 and 2018. Questions ',
                          var,' and P31. Chart: Joe Brew | @joethebrew.')
    indies <- c('In favor of\nCatalan independence',
                'Not sure/no answer',
                'Against\nCatalan independence')
  }
  
  pd$indy <- 
    ifelse(pd$indy == 'Sí', indies[1],
           ifelse(pd$indy == 'No', indies[3],
                  indies[2]))
  
  pd$variable<- factor(pd$variable,
                       levels = rev(original_levels),
                       labels = rev(the_levels))
  n_cols <- length(unique(pd$variable))
  cols <- databrew::make_colors(n = 10, categorical = TRUE)
  cols <- cols[c(2,3, 6)]
  cols <- rev(cols)
  # cols <- rev(cols)
  cols[2] <- grey(0.2)
  
  pd <- pd %>%
    arrange(desc(variable), p)
  pd$indy <- factor(pd$indy, levels = rev(unique(sort(pd$indy))))
  
  ggplot(data = pd,
         aes(x = indy,
             y = p)) +
    geom_bar(stat = 'identity',
             position = 'stack',
             # position = position_dodge(width = 0.9),
             # color = 'black',
             alpha = 0.85,
             aes(fill = variable)) +
    # facet_wrap(~partit) +
    theme_vilaweb() +
    # scale_fill_manual(name = '',
    #                   values = cols) +
    geom_text(aes(label = paste0(round(p, digits = 1), '%'),
                  y = p-1),
              alpha = 0.9,
              col = 'white',
              vjust = 1,
              position = 'stack',
              # position = position_dodge(width = 0.9),
              size = 4) +
    scale_fill_manual(name = '',
                      values = cols) +
    labs(x = '',
         y = '%',
         subtitle = the_subtitle,
         title =  the_title,
         caption = the_caption) +
    theme(legend.position = 'right',
          axis.text.x = element_text(size = 12),
          plot.caption = element_text(hjust = 0,
                                      color = cols[1]),
          plot.title = element_text(size = 17))
}


# uk_age
uk_age <- function(ca = FALSE){
  if(!ca){
    the_labs <- labs(x = '',
                     y = 'Percent in favor of Brexit',
                     title = 'Age and support for Brexit',
                     caption = 'Data source: "Understanding the Leave Vote" by Kirby Swales.\nChart by Joe Brew. www.vilaweb.cat')
  } else {
    the_labs <- labs(x = '',
                     y = 'Percentage a favor del Brexit',
                     title = 'Edat i suport al Brexit',
                     caption = 'Font de dades: "Understanding the Leave Vote" de Kirby Swales.\nGràfic de Joe Brew. www.vilaweb.cat')
  }
  education <- factor(c('18-34','35-44','45-54','55-64','65+'))
  education <- factor(education, 
                      levels = education)
  df <- tibble(education,
               y = c(40,50,52,51,61))
  ggplot(data = df,
         aes(x = education,
             y = y)) +
    geom_bar(stat = 'identity',
             fill = as.character(colors_vilaweb()[5])) +
    theme_vilaweb() +
    the_labs +
    theme(axis.text.x = element_text(size = 20),
          plot.caption = element_text(size = 10)) +
    geom_text(aes(label = paste0(round(y, digits = 1),'%')),
              nudge_y = -5,
              alpha = 0.8,
              size = 5,
              color = 'white')
}


# uk_welfare
uk_welfare <- function(ca = FALSE){
  if(!ca){
    the_labs <- labs(x = '',
                     y = 'Percent in favor of Brexit',
                     title = 'Welfarism among pro-Brexit voters',
                     caption = 'Data source: "Understanding the Leave Vote" by Kirby Swales.\nChart by Joe Brew. www.vilaweb.cat')
    education <- factor(c('Anti-welfare',
                          'Neither',
                          'Pro-welfare'))
    education <- factor(education, 
                        levels = education)
  } else {
    the_labs <- labs(x = '',
                     y = 'Percentage a favor del Brexit',
                     title = 'Suport a l\'estat del benestar i suport al Brexit',
                     caption = 'Font de dades: "Understanding the Leave Vote" de Kirby Swales.\nGràfic de Joe Brew. www.vilaweb.cat')
    education <- factor(c('En contra de\nl\'estat del benestar',
                          'Cap dels dos',
                          'A favor de\nl\'estat del benestar'))
    education <- factor(education, 
                        levels = education)
  }
  df <- tibble(education,
               y = c(75,54,30))
  ggplot(data = df,
         aes(x = education,
             y = y)) +
    geom_bar(stat = 'identity',
             fill = as.character(colors_vilaweb()[5])) +
    theme_vilaweb() +
    the_labs +
    theme(axis.text.x = element_text(size = 13),
          plot.caption = element_text(size = 10)) +
    geom_text(aes(label = round(y, digits = 1)),
              nudge_y = -5,
              alpha = 0.8,
              size = 5,
              color = 'white')
}

# uk_income
uk_income <- function(ca = FALSE){
  if(!ca){
    the_labs <- labs(x = '',
                     y = 'Percent in favor of Brexit',
                     title = 'Monthly income and support for Brexit',
                     caption = 'Data source: "Understanding the Leave Vote" by Kirby Swales.\nChart by Joe Brew. www.vilaweb.cat')
  } else {
    the_labs <- labs(x = '',
                     y = 'Percentage a favor del Brexit',
                     title = 'Ingressos mensuals i suport al Brexit',
                     caption = 'Font de dades: "Understanding the Leave Vote" de Kirby Swales.\nGràfic de Joe Brew. www.vilaweb.cat')
    
  }
  education <- factor(c('< £1200',
                        '£1201-2200',
                        '£2201-3700',
                        '£3701 +'))
  education <- factor(education, 
                      levels = education)
  df <- tibble(education,
               y = c(66,57,51,38))
  ggplot(data = df,
         aes(x = education,
             y = y)) +
    geom_bar(stat = 'identity',
             fill = as.character(colors_vilaweb()[5])) +
    theme_vilaweb() +
    the_labs +
    theme(axis.text.x = element_text(size = 13),
          plot.caption = element_text(size = 10)) +
    geom_text(aes(label = round(y, digits = 1)),
              nudge_y = -5,
              alpha = 0.8,
              size = 5,
              color = 'white')
}

cat_age <- function(ca = FALSE){
  df <- vilaweb::ceo
  df <- df %>%
    mutate(indy = P31) %>%
    mutate(indy = as.character(indy)) %>%
    mutate(indy = ifelse(indy %in% c('No ho sap',
                                     'No contesta'),
                         'NS/NC',
                         indy)) %>%
    filter(!is.na(indy)) %>%
    mutate(age = EDAT) %>%
    mutate(age = ifelse(age >= 75, 75, age)) %>%
    mutate(age = ifelse(age < 10, paste0('0', age), as.character(age))) %>%
    mutate(age = substr(age, 1, 1)) %>%
    mutate(age = paste0(age, '0', '-\n', age, '9 ')) %>%
    mutate(age = ifelse(substr(age, 1, 2) == '10',
                        '<20',
                        ifelse(substr(age, 1, 2) == '70',
                               '70+',
                               age)))
  
  pd <- df %>% group_by(age, indy) %>%
    summarise(n = sum(PONDERA),
              Muestra = n()) %>%
    group_by(age) %>%
    mutate(p = n / sum(n) * 100) %>%
    arrange(indy)
  
  pd$indy <- factor(pd$indy,
                    levels = c('No', 'NS/NC', 'Sí'))
  if(!ca){
    pd$indy <- factor(pd$indy,
                      levels = c('No', 'NS/NC', 'Sí'),
                      labels = c('No', 'Not sure/\nno answer', 'Yes'))
  }
  # cols <- RColorBrewer::brewer.pal(n = 9,
  #                                  name = 'YlGnBu')
  # cols2 <- RColorBrewer::brewer.pal(n = 9,
  #                                   name = 'BrBG')
  # cols <- c(cols2[2], 
  #           grey(0.7),
  #           # grey(0.6),
  #           cols[6])
  cols <- databrew::make_colors(n = 10, categorical = TRUE)
  cols <- cols[c(2,3, 6)]
  # cols <- rev(cols)
  # cols <- rev(cols)
  cols[2] <- grey(0.2)
  if(ca){
    legend_title <- 'Independentista'
    the_labs <- labs(x = '',
                     y = 'Percentage',
                     title = 'Independentisme i edat',
                     caption = paste0('Mostra: ',
                                      sum(pd$Muestra),
                                      ' residents de Catalunya amb ciutadania espanyola.\nDades: Combinació enquestes CEO.\n2015 a 2018. Gràfic: Joe Brew | @joethebrew.'))
  } else {
    legend_title <- 'Pro-\nindependence'
    the_labs <- labs(x = '',
                     y = 'Percentage',
                     title = 'Age and support for Catalan independence',
                     caption = paste0('Sample: ',
                                      sum(pd$Muestra),
                                      ' residents of Catalonia with Spanish citizeship.\nData: Combination of CEO/BOP surveys from 2015-2018Gràfic: Joe Brew | @joethebrew.'))
  }
  ggplot(data = pd,
         aes(x = age,
             y = p,
             fill = indy)) +
    geom_bar(stat = 'identity',
             position = position_stack()) +
    theme_vilaweb() +
    scale_fill_manual(name = legend_title,
                      values = cols) +
    geom_text(aes(label = paste0(round(p, digits = 1), ''),
                  y = p),
              position = position_stack(vjust = 0.5),
              color = 'white',
              alpha = 0.7) +
    the_labs +
    theme(legend.position = 'right',
          legend.title = element_text(size = 9),
          axis.text.x = element_text(size = 15,
                                     hjust = 0.5))
  
}

cat_income <- function(ca = FALSE){
  add_zero <- function(x, n){
    x <- as.character(x)
    adders <- n - nchar(x)
    adders <- ifelse(adders < 0, 0, adders)
    for (i in 1:length(x)){
      if(!is.na(x[i])){
        x[i] <- paste0(
          paste0(rep('0', adders[i]), collapse = ''),
          x[i],
          collapse = '')  
      } 
    }
    return(x)
  }
  extract_income <- function(zzz){
    
    paste0(add_zero(as.numeric(gsub('.', '', unlist(strsplit(zzz, ' '))[c(2,4)], fixed = TRUE)), n = 4), ' ', collapse = '-\n')
  }
  extract_income <- Vectorize(extract_income)
  df <- vilaweb::ceo
  x <- df %>%
    mutate(income = as.character(C900))
  x <- x %>%
    mutate(income = ifelse(income %in% c('No contesta', 'No ho sap'), NA,
                           ifelse(income %in% c("No tè cap tipus d'ingrés", "Menys o igual a 300 \u0080"),
                                  '<300',
                                  ifelse(income == 'Més de 6.000 \u0080', '6000+', extract_income(income))))) 
  x <- x %>%  
    mutate(indy = P31) %>%
    mutate(indy = as.character(indy)) %>%
    mutate(indy = ifelse(indy %in% c('No ho sap',
                                     'No contesta'),
                         'NS/NC',
                         indy)) %>%
    filter(!is.na(income), !is.na(indy)) %>%
    filter(!grepl('NA', income)) %>%
    group_by(indy, income) %>%
    summarise(n = sum(PONDERA),
              Muestra = n()) %>%
    ungroup %>%
    group_by(income) %>%
    mutate(p = n / sum(n) * 100) %>%
    ungroup
  
  
  # cols <- RColorBrewer::brewer.pal(n = 9,
  #                                  name = 'YlGnBu')
  # cols2 <- RColorBrewer::brewer.pal(n = 9,
  #                                   name = 'BrBG')
  # cols <- c(cols2[2], 
  #           grey(0.7),
  #           # grey(0.6),
  #           cols[6])
  cols <- databrew::make_colors(n = 10, categorical = TRUE)
  cols <- cols[c(2,3, 6)]
  cols <- rev(cols)
  # cols <- rev(cols)
  cols[2] <- grey(0.2)
  if(ca){
    legend_title <- 'Independentista'
    the_labs <- labs(y = 'Percentage',
                     x = 'Ingressos mensuals familiars',
                     title = paste0('Ingressos i independentisme'),
                     caption = paste0('Mostra: ',
                                      sum(x$Muestra),
                                      ' residents de Catalunya amb ciutadania espanyola.\nDades: Combinació enquestes CEO.\n2015 a 2018. Gràfic: Joe Brew | @joethebrew.'))
  } else {
    legend_title <- 'Pro-\nindependence'
    the_labs <- labs(y = 'Percentage',
                     x = 'Family income of respondent',
                     title = paste0('Household income and support for Catalan independence'),
                     caption = paste0('Sample: ',
                                      sum(x$Muestra),
                                      ' residents of Catalonia with Spanish citizeship.\nData: Combination of CEO/BOP surveys from 2015-2018Gràfic: Joe Brew | @joethebrew.'))
  }
  
  ggplot(data = x,
         aes(x = income,
             y = p,
             fill = indy,
             group = indy)) +
    # geom_area(position = 'stack') +
    geom_bar(stat = 'identity', position = position_stack(),
             alpha = 0.9) +
    # scale_x_discrete(breaks = as.character(c(1, 3, 5, 7, 9)), labels=c('Extrema\nizquierda', 'Izquierda', 'Centro', 'Derecha', 'Extrema\nderecha'))  +
    the_labs +
    theme_vilaweb() +
    # scale_fill_manual(name = 'View on independence',
    #                   values = c('darkgrey', '#FF5733', '#1DA1F2')) +
    theme(legend.direction ="vertical",legend.position = "bottom", 
          axis.text.x = element_text(size = 6),
          plot.title = element_text(size = 16)) +
    geom_text(aes(label = round(p, digits = 1),
                  y = p),
              position = position_stack(vjust = 0.5),
              fill = NA,
              alpha = 0.8,
              color = 'white',
              size = 3) +
    scale_fill_manual(name = legend_title,
                      values = cols) +
    theme(legend.position = 'right',
          plot.caption = element_text(size = 8),
          legend.title = element_text(size = 10))
  # guides(fill=guide_legend(ncol=3,
  #                          title.position = 'top',
  #                          title.hjust = 0.5)) 
}


happy_plot <- function(ca = FALSE){
  df <- vilaweb::new_ceo
  df$happy <- df$`Fins a quin punt diria que, en línies generals, està vostè satisfet/a amb la seva vida actual?`
  df$indepe <- df$`Vol que Catalunya esdevingui un Estat independent?`
  df <- df %>%
    filter(!is.na(happy),
           !is.na(indepe))
  indepe_levels <- tibble(ca = c('Independentistes',
                                 'Unionistes',
                                 'NS/NC'),
                          en = c('Pro-independence',
                                 'Anti-independence',
                                 'Does not know\nno answer'))
  if(ca){
    indepe_levels$this <- indepe_levels$ca
  } else {
    indepe_levels$this <- indepe_levels$en
  }
  df <- df %>%
    mutate(indepe = as.character(indepe)) 
  df <- df %>%
    mutate(indepe = unlist(ifelse(indepe == 'Sí', indepe_levels$this[1],
                           ifelse(indepe == 'No', indepe_levels$this[2],
                                  indepe_levels$this[3]))))
  
  df <- df %>%
    mutate(happy = as.character(happy)) %>%
    mutate(happy = ifelse(happy == 'Molt poc satisfet', '0',
                          ifelse(happy == 'Molt satisfet', '10',
                                 happy))) %>%
    mutate(happy = as.numeric(happy)) %>%
    filter(!is.na(happy))
  
  pd <- df %>%
    group_by(indepe) %>%
    summarise(avg = mean(happy),
              n = n(),
              p5= length(which(happy >5)) / length(happy)) %>%
    gather(key, value, avg: p5)
  n <- sum(pd$value[pd$key == 'n'])
  pd <- pd %>%
    filter(key == 'avg')
  
  if(ca){
    the_labs <- labs(title = 'Satisfacció mitjana amb la seva vida personal',
                     subtitle = 'Escala de 0 a 10',
                     x = '',
                     y = 'Satisfacció mitjana (0-10)',
                     caption = paste0('Mostra: ', numberfy(n), ' residents de Catalunya amb ciutadania espanyola.\nDades del Centre d\'Estudis d\'Opinió, gràfic de Joe Brew. www.vilaweb.cat'))
  } else {
    the_labs <- labs(title = 'Average satisfaction with personal life',
                     subtitle = 'Scale from 0 to 10',
                     x = '',
                     y = 'Average satisfaction (0-10)',
                     caption = paste0('Sample size: ', numberfy(n), ' residents of Catalonia with Spanish citizenship.\nData from the Centre d\'Estudis d\'Opinió, chart by Joe Brew. www.vilaweb.cat'))
  }
  
  ggplot(data = pd) +
    # geom_point(aes(x = indepe,
    #               y = value)) +
    geom_bar(aes(x = indepe,
                   y = value),
             stat = 'identity',
             fill = as.character(colors_vilaweb()[4])) +
    theme_vilaweb() +
    # facet_wrap(~key, scales = 'free') +
    geom_text(aes(label = round(value, digits = 1),
                  x = indepe,
                  y = value),
              color = 'white',
              nudge_y = -1,
              size = 6) +
    the_labs
}
numberfy <- function(x){
  gsub(',', '.', scales::comma(x), fixed = TRUE)
}
