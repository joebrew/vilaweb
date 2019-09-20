# Libraries
library(vilaweb)
library(tidyverse)
library(ggplot2)
library(tidyr)
library(waffle)

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

# Get most recent CEO data
ceo_june_2019 <- vilaweb::ceo_june_2019

# Transform data to combine
transform_data <- function(df){
  language_dict <- tibble(input = c('Català (valencià / balear)', 'Castellà', 'Totes dues igual: català (valencià / balear) i castellà', 'Altres llengües o altres combinacions', 'Aranès', 'No ho sap', 'No contesta'),
                          output_ca = c('Català',
                                     'Castellà',
                                     'Cat+Cast',
                                     'Altres',
                                     'Català',
                                     'NS/NC',
                                     'NS/NC'),
                          output_en = c('Catalan',
                                        'Spanish',
                                        'Cat+Spa',
                                        'Others',
                                        'Catalan',
                                        'No answer',
                                        'No answer'))
  
  convert_language <- function(x, ca = TRUE){
    z <- tibble(input = x)
    joined <- left_join(z, language_dict)
    if(ca){
      as.character(joined$output_ca)
    } else {
      as.character(joined$en)
    }
  }
  
  v1 <- "Amb quina de les següents frases se sent més identificat: em sento només espanyol, més espanyol que català, tan espanyol com català, més català que espanyol o només català?"
  v2 <- 'Amb quina de les següents frases,em sento només espanyol, més espanyol que català, tan espanyol com català, més català que espanyol o només català, se sent més identificat?'
  if(v1 %in% names(df)){
    df$identificacio <- unlist(df[,v1])
  } else {
    df$identificacio <- unlist(df[,v2])
  }
  

  
  if("Per a vostè, l'etapa del franquisme ha suposat a la història de Catalunya, en conjunt, un període positiu, negatiu o va tenir coses positives i negatives?" %in% names(df)){
    df$franquisme <- df$`Per a vostè, l'etapa del franquisme ha suposat a la història de Catalunya, en conjunt, un període positiu, negatiu o va tenir coses positives i negatives?`
  } else {
    df$franquisme <- NA
  }
  if("Si es tornés a celebrar un referèndum per decidir sobre l’actual Constitució espanyola aprovada el 1978, tal com és ara, vostè què faria?" %in% names(df)){
    df$constitucio <- df$`Si es tornés a celebrar un referèndum per decidir sobre l’actual Constitució espanyola aprovada el 1978, tal com és ara, vostè què faria?`
  } else {
    df$constitucio <- NA
  }
  if("Grau de confiança en: La Monarquia espanyola" %in% names(df)){
    df$monarquia <- df$`Grau de confiança en: La Monarquia espanyola`
  } else {
    df$monarquia <- NA
  }
  
  out <- df %>%
    mutate(democracia = `Quina de les següents frases reflecteix millor la seva opinió sobre la democràcia?`) %>%
    mutate(any = `Any de realització del baròmetre`,
           mes = `Mes de realització del baròmetre`) %>%
    mutate(mes = ifelse(mes == 3 & any == 2014, 4, mes),
           mes = ifelse(mes == 10 & any == 2014, 11, mes),
           mes = ifelse(mes == 3 & any == 2015, 2, mes),
           mes = ifelse(mes == 7 & any == 2017, 6, mes),
           mes = ifelse(mes == 7 & any == 2018, 6, mes),
           mes = ifelse(mes == 11 & any == 2018, 10, mes),
           mes = ifelse(mes == 7 & any == 2019, 6, mes)) %>%
    mutate(date = as.Date(paste0(any, '-', mes, '-15'))) %>%
  mutate(avis = as.character(`Quants dels seus avis/àvies van néixer a Catalunya?`)) %>%
    mutate(avis = ifelse(avis == 'Cap', '0',
                         ifelse(avis == 'Un', '1',
                                ifelse(avis == 'Dos', '2',
                                       ifelse(avis == 'Tres', '3',
                                              ifelse(avis == 'Quatre', '4', NA)))))) %>%
    mutate(avis = as.numeric(avis)) %>%
    mutate(pare_cat = `Em podria dir el lloc de naixement del seu pare?` == 'Catalunya',
           pare_esp = `Em podria dir el lloc de naixement del seu pare?` == 'Altres comunitats autònomes',
           mare_cat = `Em podria dir el lloc de naixement de la seva mare?` == 'Catalunya',
           mare_esp = `Em podria dir el lloc de naixement de la seva mare?` == 'Altres comunitats autònomes') %>%
    mutate(pare_cat = as.numeric(pare_cat),
           pare_esp = as.numeric(pare_esp),
           mare_cat = as.numeric(mare_cat),
           mare_esp = as.numeric(mare_esp)) %>%
    mutate(llengua_primera = `Quina llengua va parlar primer vostè, a casa, quan era petit?`) %>%
    mutate(llengua_primera = convert_language(llengua_primera),
           llengua_habitual = convert_language(`Quina és la seva llengua habitual, ens referim a la llengua que parla més sovint?`),
           llengua_propia = convert_language(`Quina és la seva llengua, ens referim a quina és la llengua que vostè considera com a pròpia?`)) %>%
    mutate(indepe = `Vol que Catalunya esdevingui un Estat independent?`) %>%
    # mutate(llengua_preferiex = `Prefereix que li faci les preguntes en català o en castellà?`),
    mutate(neixer = `Em podria dir on va néixer?`,
           informat = `Es considera vostè molt, bastant, poc o gens informat/ada del que passa en política?`,
           interessat = `A vostè la política li interessa molt, bastant, poc o gens?`,
           partit = `Em podria dir per quin partit sent més simpatia?`,
           axis = `Quan es parla de política, normalment s’utilitzen les expressions esquerra i dreta,  indiqui on s’ubicaria vostè?`,
           telefon_fix = `Té telèfon fix a la seva llar?`,
           ingressos = `Quins són els ingressos familiars que entren cada mes a casa seva?`) %>%
    mutate(indepe = as.character(indepe)) %>%
    mutate(indepe = 
             ifelse(indepe %in% c('No ho sap', 'No contesta'),
                    'NS/NC', indepe)) %>%
    mutate(municipi = `Grandària del municipi`) %>%
    mutate(provincia = `Província`) %>%
    dplyr::select(
      identificacio,
      democracia,
      municipi,
      provincia,
      date,
      monarquia,
      avis,
      pare_cat, pare_esp,
      mare_cat, mare_esp,
      llengua_primera, llengua_habitual, llengua_propia, #llengua_prefereix,
      neixer,
      informat,
      interessat, 
      partit,
      axis,
      telefon_fix,
      ingressos,
      indepe, 
      franquisme,
      constitucio,
      contains('Fins a quin punt')
    ) %>%
    mutate(monarquia = as.character(monarquia)) %>%
    mutate(monarquia = ifelse(monarquia %in% c('No ho sap', 'No contesta'), NA,
                              ifelse(monarquia == 'Cap confiança', '0',
                                     ifelse(monarquia == 'Molta confiança', '10',
                                            monarquia)))) %>%
    mutate(monarquia = as.numeric(monarquia)) %>%
    mutate(pares = ifelse(pare_cat + mare_cat == 2,
                          '2 pares nascuts a Cat',
                          ifelse(pare_cat + mare_cat == 1 &
                                   pare_esp + mare_esp == 1,
                                 '1 pare nascut a Cat, l\'altre a Esp',
                                 ifelse(pare_esp + mare_esp == 2,
                                        '2 pares nascuts a Esp',
                                        'Altres combinacions')
                                 ))) #%>%
    # mutate(pares = ifelse(pare_cat + mare_cat == 1 &
    #                         pare_esp + mare_esp == 0,
    #                       '1 pare nascut a Cat, l\'altre a l\'Estranger',
    #                       ifelse(pare_cat + mare_cat == 0 &
    #                                pare_esp + mare_esp == 1,
    #                              '1 pare nascut a Esp, l\'altre a l\'Estranger', pares)))
  
  # Go through the "fins a quin punt" questions, and clean up
  fins_quins <- names(out)[grepl('Fins a quin punt està d', names(out))]
  for(j in 1:length(fins_quins)){
    this_variable_name <- fins_quins[j]
    # Detect 
    new_variable_name <- gsub("Fins a quin punt està d’acord o en desacord amb cadascuna de les següents afirmacions: ", 'fins_quin_punt_', this_variable_name)
    simple_variable_name <- gsub("Fins a quin punt està d’acord o en desacord amb cadascuna de les següents afirmacions: ", 'fins_quin_punt_simple_', this_variable_name)
    this_data <- as.character(unlist(out[,this_variable_name]))
    this_data <- ifelse(this_data %in% c('No contesta', 'No ho sap'),
                        'NS/NC',
                        this_data)
    this_simple_data <-
      ifelse(this_data %in% c("D'acord", "Molt d'acord"),
             "D'acord",
             ifelse(this_data %in% c("En desacord", "Molt en desacord"),
                    "En desacord", 
                    ifelse(this_data %in% c("Ni d'acord ni en desacord",
                                            "NS/NC"),
                           "Ni d'acord ni en\ndesacord o NS/NC",
                           this_data)))
    this_data <- factor(this_data,
                        levels = c("Molt d'acord", "D'acord",
                                   "Ni d'acord ni en desacord", "NS/NC",
                                   "En desacord", "Molt en desacord"))
    this_simple_data <- factor(this_simple_data, levels = c("D'acord",
                                                            "Ni d'acord ni en\ndesacord o NS/NC",
                                                            "En desacord"))
    out[,new_variable_name] <- this_data
    out[,simple_variable_name] <- this_simple_data
    out[,this_variable_name] <- NULL
  }
  
  # Clean up constitution
  con_dict <- data_frame(
    constitucio = c('Votaria sí',
                    'Votaria no',
                    'Votaria en blanc',
                    'Votaria nul',
                    'No votaria',
                    'No ho sap',
                    'Contesta'),
    constitucio2 = c('Votaria sí',
                     'Votaria no',
                     'Votaria nul\nen blanc\nNo votaria\nNS/NC',
                     'Votaria nul\nen blanc\nNo votaria\nNS/NC',
                     'Votaria nul\nen blanc\nNo votaria\nNS/NC',
                     'Votaria nul\nen blanc\nNo votaria\nNS/NC',
                     'Votaria nul\nen blanc\nNo votaria\nNS/NC')
  )
  out$constitucio <- as.character(out$constitucio)
  out <- out %>%
    left_join(con_dict) %>%
    dplyr::select(-constitucio) %>%
    dplyr::rename(constitucio = constitucio2)
  
  out$franquisme <- as.character(out$franquisme)
  out$franquisme <- ifelse(out$franquisme %in% c('No ho sap',
                                                 'No contesta'), 'NS/NC',
                           out$franquisme)
  
  return(out)
}

combined <- 
  transform_data(new_ceo) %>%
  bind_rows(
    transform_data(ceo_june_2019)
  )


# Read in Sep 2018 CIS data (asked about democracy)
cisy <- haven::read_sav('../../data-raw/cis/monthly/2018-09/3223.sav')
cisy <- haven::as_factor(cisy)

# Questions P15-P17 are of interest
# P15: Cuarenta años después de aprobarse nuestra constitución, ¿cree Ud. que la forma en que se llevó a cabo la transición a la democracia en España constituye un motivo de orgullo para los/as españoles/as?
# P16: En conjunto, ¿está Ud. muy satisfecho/a, bastante satisfecho/a, poco o nada satisfecho/a con la forma en que funciona la democracia en España?
# P17: Ahora vamos a hablar sobre distintos tipos de regímenes políticos. Me gustaría que me dijera con cuál de las siguientes frases está Ud. más de acuerdo.

cisy_plot <- function(var = 'P15',
                      group_cat = T,
                      ca = T){
  df <- cisy 
  df$var <- as.character(unlist(df[,var]))
  if(ca){
    the_labs <- labs(x = '',
                     y = 'Percentatge',
                     title = 'Creieu que la manera com es va dur a terme la transició a la democràcia\na Espanya és un motiu d\'orgull per als espanyols?',
                     subtitle = 'Percentatge que respon "Sí"',
                     caption = 'Dades del CIS del setembre del 2018. Percentatges calculats després de treure les respostes NS/NC.\nMida de mostra:2.653. Mida de mostra per geografia = Catalunya: 421; Espanya: 2232.') 
    if(group_cat){
      df$ccaa <- ifelse(df$CCAA == 'Cataluña', 'Catalunya', 'Espanya (sense Catalunya)')
    } else {
      df$ccaa <- df$CCAA
    }
  } else {
    the_labs <- labs(x = '',
                     y = 'Percentage',
                     title = '¿Do you believe that the way the transition to democracy was\ncarried out in Spain constitutes a source of pride for Spaniards?',
                     subtitle = "Percentage which responds 'Yes'",
                     caption = "Data from the CIS, Sep 2018. Percentages calculated after removing 'Don\'t know' and 'No answer' observations.\nSample size: 2.653. Sample size by Geography = Catalonia: 421; Spain: 2232.") 
    if(group_cat){
      df$ccaa <- ifelse(df$CCAA == 'Cataluña', 'Catalonia', 'Spain (without Catalonia)')
    } else {
      df$ccaa <- df$CCAA
    }
  }
  
  df <- df %>%
    filter(!var %in% c('N.S.', 'N.C.')) %>%
    group_by(ccaa, var) %>%
    tally %>%
    ungroup %>%
    group_by(ccaa) %>%
    mutate(p = n / sum(n) * 100)
  names(df)[2] <- 'var'
  # cols <- RColorBrewer::brewer.pal(n = length(unique(df$var)),
  #                                  'Spectral')
  df <- df %>%
    filter(var == 'Sí',
           !grepl('Melilla', ccaa),
           !grepl('Ceuta', ccaa))
  df <- df %>% arrange(p)
  df$ccaa <- gsub(' (Comunidad Foral de)', '', df$ccaa, fixed = T)
  df$ccaa <- gsub(' (Principado de)', '', df$ccaa, fixed = T)
  df$ccaa <- gsub(' (Illes)', '', df$ccaa, fixed = T)
  df$ccaa <- gsub(' (Comunidad de)', '', df$ccaa, fixed = T)
  df$ccaa <- gsub(' (Región de)', '', df$ccaa, fixed = T)
  
  
  df$ccaa <- factor(df$ccaa, levels = rev(df$ccaa))
  ggplot(data = df,
         aes(x = ccaa,
             y = p)) +
    geom_bar(stat = 'identity',
             position = position_dodge(),
             fill = 'black',
             alpha = 0.8) +
    # theme_linedraw() +
    # ggthemes::theme_fivethirtyeight() +
    theme_vilaweb() +
    # coord_flip()
    theme(axis.text.x = element_text(#angle = 90,
                                     #vjust = 0.5,
                                     #hjust = 1,
                                     size = 20)) +
    geom_text(aes(label = round(p, digits = 1)),
              nudge_y = -10,
              size = 8,
              color = 'white') +
    the_labs +
    geom_hline(yintercept = 50, lty = 2, alpha = 0.7) +
    theme(plot.title = element_text(size = 14))
}
# cisy_plot()

overall_plot <- function(language = 'en',
                         geo = c('cat', 'esp', 'all', 'compare'),
                         return_table = FALSE){
  
  cis_data  <- cis_list$`2018-09`
  
  if(length(geo) != 1){
    stop('geo must be one of "cat", "esp", "all"')
  }
  
  if(geo == 'compare'){
    if(language == 'en'){
      places <- c('Catalonia', 'Rest of Spain')
    } else {
      places <- c('Catalunya', "Resta de l'estat")
    }
    plot_data <- cis_data %>% mutate(ccaa = ifelse(CCAA == 'Cataluña', places[1], places[2])) 
    plot_data <- plot_data %>%
      mutate(P13 = as.character(P13)) %>%
      # Combine the no answer / not sures, etc.
      mutate(P13 = ifelse(P13 == 'N.C.',
                          '(NO LEER) Está en duda, no sabe lo suficiente',
                          P13)) %>%
      group_by(es = P13, ccaa) %>%
      tally %>%
      ungroup %>%
      mutate(es = as.character(es)) %>%
      group_by(ccaa) %>%
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
      plot_data$x <- plot_data$en
      the_labs <- labs(x = '',
                       y = '',
                       title = 'Satisfaction with the Spanish Constitution',
                       subtitle = 'Data from September 2018 CIS survey')
    } else {
      plot_data$x <- plot_data$ca
      the_labs <- labs(x = '',
                       y = '',
                       title = 'Satisfacció amb la constitució espanyola',
                       subtitle = 'Dades del CIS del setembre del 2018')
    }
    plot_data$x <- factor(plot_data$x, levels = levels(plot_data$x),
                          labels = gsub(' ', '\n', levels(plot_data$x)))
    bp <- function(x,z){RColorBrewer::brewer.pal(n = 8,name =x)[z]}
    cols <- c(bp('Blues', 8),
              bp('Blues', 5),
              bp('Greys', 3),
              bp('Greys', 6),
              bp('Oranges', 5),
              bp('Oranges', 8))
    ggplot(data = plot_data,
           aes(x = x,
               y = y,
               fill = x)) +
      geom_bar(stat = 'identity') +
      facet_wrap(~ccaa) +
      theme_vilaweb() +
      scale_fill_manual(name = '', values = cols) +
      theme(legend.position = 'none') +
      geom_text(aes(label = round(y, digits = 1)),
                nudge_y = 3,
                alpha = 0.6) +
      the_labs
    
  } else {
    if(geo == 'esp'){
      plot_data <- cis_data %>% filter(CCAA != 'Cataluña')  
      if(language == 'en'){subtitle <- 'Spain, not counting Catalonia'} else {subtitle <-'Estat espanyol, sense incloure-hi Catalunya'}
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
      caption <- 'Data from CIS survey, September 2018.'
      plot_data$x <- plot_data$en
    } else {
      title <- 'Nivell de satisfacció amb la constitució espanyola'
      caption <- 'Dades del CIS del setembre del 2018.'
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
    y <- 'Percentatge'
    subtitle <- 'Per comunitat autònoma'
    xxx <- 'Gens satisfet'
    title <- 'Nivell de satisfacció amb la constitució espanyola'
    caption <- 'Dades del CIS del setembre del 2018.\nElaboració del gràfic: Joe Brew | @joethebrew | www.vilaweb.cat.'
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
  
  # Translate to catala
  if(language != 'en'){
    translator <- tibble(ccaa = c('Andalucía',
                                  'Aragón',
                                  'Asturias',
                                  'Balears (Illes)',
                                  'Canarias',
                                  'Cantabria',
                                  'Castilla y León',
                                  'Castilla-La Mancha',
                                  'Cataluña',
                                  'Com. Valenciana',
                                  'Extremadura',
                                  'Galicia',
                                  'Madrid',
                                  'Murcia',
                                  'Navarra',
                                  'País Vasco',
                                  'Rioja (La)'),
                         new_ccaa = c('Andalusia',
                                      'Aragó',
                                      'Astúries',
                                      'Illes Balears',
                                      'Canàries',
                                      'Cantàbria',
                                      'Castilla i Lleó',
                                      'Castella - la Manxa',
                                      'Catalunya',
                                      'País Valencià',
                                      'Extremadura',
                                      'Galícia',
                                      'Madrid',
                                      'Múrcia',
                                      'Navarra',
                                      'País Basc',
                                      'la Rioja'))
    plot_data <- plot_data %>% left_join(translator) %>%
      dplyr::select(-ccaa) %>%
      dplyr::rename(ccaa = new_ccaa)
  }
  
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
  
  
  
  x <- 'Left-right ideology'
  y <- 'Percentage'
  if(language == 'en'){
    subtitle <- 'By ideology. All of Spain, not including Catalonia.'
    xxx <- 'Not at all satisfied'
    title <- 'Level of satisfacation with Spanish Constitution'
    caption <- 'Data from CIS survey, September 2018.\nChart: Joe Brew | @joethebrew | www.vilaweb.cat.\nIdeology: 1-3; left; 4-7: center; 8-10: right.'
    plot_data$x <- plot_data$en
  } else {
    x <- 'Ideologia esquerra-dreta'
    y <- 'Percentatge'
    subtitle <- 'Per ideologia. Tot l\'estat espanyol, sense incloure-hi Catalunya.'
    xxx <- 'Gens satisfet'
    title <- 'Nivell de satisfacció amb la constitució espanyola'
    caption <- 'Dades del CIS del setembre del 2018.\nElaboració del gràfic: Joe Brew | @joethebrew | www.vilaweb.cat.\nIdeologia: 1-3: esquerra; 4-7: centre; 8-10: dreta.'
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
             fill = x,
             group = x)) +
    geom_bar(stat = 'identity',
             position = position_dodge(width = 0.8)) + 
    geom_text(aes(label = round(y, digits = 1),
                  y = y + 3),
              alpha = 0.7,
              position = position_dodge(width = 0.8)) +
    # facet_wrap(~ccaa) +
    scale_fill_manual(name = '',
                      values = cols) +
    theme_vilaweb() +
    theme(#axis.text.x = element_text(angle = 90, hjust = 1,
                                     # vjust = 0.5),
          legend.text = element_text(size = 13)) +
    theme(legend.position = 'right') +
    guides(fill=guide_legend(ncol=1)) +
    # geom_hline(yintercept = 50,
    #            lty = 2,
    #            alpha = 0.5) +
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
    y = 'Percentatge'
    title = 'Suport a la constitució espanyola entre catalans'
    subtitle = 'Si es tornés a celebrar un referèndum per decidir sobre l’actual\nconstitució espanyola aprovada el 1978, tal com és ara, què faríeu?'
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
                bp('Greys', 5),
                bp('Oranges', 5)))
  if(return_table){
    return(plot_data)
  }
  ggplot(data = plot_data,
         aes(x = axis,
             y = p,
             fill = constitution)) +
    geom_bar(stat = 'identity',
             alpha = 0.8,
             # color = 'black',
             # lwd = 0.2,
             # position = position_stack()
             position = position_dodge(width = 0.7)
             ) +
    theme_vilaweb() +
    labs(x = x,
         y = y,
         title = title,
         subtitle = subtitle,
         caption = caption) +
    scale_fill_manual(name = '',
                      values = cols) +
    geom_text(aes(label = round(p, digits = 1)),
              position = position_dodge(width = 0.7),
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



king_plot <- function(language = 'en',
                      simple = FALSE,
                      by_time = FALSE,
                      return_table = FALSE){
  
  if(language == 'en'){
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
    # vilaweb::ceo %>%
    combined %>%
    filter(!axis %in% c('No ho sap', 'No contesta')) %>%
    filter(!is.na(monarquia)) %>%
    left_join(axis_dict) %>%
    dplyr::select(-axis) %>%
    dplyr::rename(axis = new_axis) 
  
  ss_dates <- range(plot_data$date)
  ss_size <- nrow(plot_data)
  ss_size <- numberfy(ss_size)
  
  
  if(simple){
    plot_data <- plot_data %>%
      group_by(monarquia) %>%
      tally %>%
      ungroup %>%
      mutate(p = n / sum(n) * 100)
    
    if(language == 'en'){
      
      x = 'Ideology (self-assessed)'
      y = 'Percentage'
      title = 'Support for the Spanish Monarchy among Catalans'
      subtitle = 'Average approval rating (0-10 scale)'
      caption = 'Data from the Baròmetre d\'Opinió Pública, combination of surveys 2014-2019.\nSample:12,273 residents of Catalonia with Spanish citizenship.'
      
    } else {
      
      x = 'Ideologia autoubicada (escala 0-10)'
      y = 'Percentatge'
      title = 'Suport a la monarquia espanyola entre catalans'
      subtitle = 'Grau de confiança mitjana (escala de 0-10)'
      caption = 'Dades del Baròmetre d\'Opinió Pública, combinació d\'enquestes 2014-2019.\nMostra: 12.273 residents de Catalunya amb ciutadania espayola.'
    }
    g <- ggplot(data = plot_data,
                aes(x = monarquia,
                    y = p)) +
      geom_bar(stat = 'identity') +
      scale_x_continuous(name = '',
                         breaks = 0:10) +
      theme_vilaweb() +
      labs(x = x,
           y = y,
           title = title,
           subtitle = subtitle,
           caption = caption) +
      geom_text(aes(label = paste0(round(p, digits = 1), '%')),
                size = 4,
                color = 'black',
                nudge_y = 5,
                alpha = 0.6) +
      theme(plot.title = element_text(size = 16),
            plot.subtitle = element_text(size = 11),
            axis.text.x = element_text(size = 15),
            axis.text.y = element_text(size = 13),
            legend.text = element_text(size = 20),
            plot.caption = element_text(size = 10)) 
    # End of simple
  } else {
    # Beginning of non-simple
    if(by_time){
      ca <- language != 'en'
      if(ca){
        ll <- c('Esquerra', 'Dreta')
      } else {
        ll <- c('Left', 'Right')
      }
      plot_data <- plot_data %>%
        mutate(axis = ifelse(grepl('left|esque', tolower(axis)),
                             ll[1],
                             ifelse(grepl('right|dreta', tolower(axis)), ll[2],
                                    axis))) %>%
        group_by(axis, date)%>%
        summarise(avg = mean(monarquia, na.rm = T))  
      if(language == 'en'){
        plot_data$axis <- 
          factor(plot_data$axis,
                 levels = c('Left',
                            'Center',
                            'Right'))
        plot_data$axis <- factor(plot_data$axis,
                                 levels = levels(plot_data$axis),
                                 labels = paste0("Responds on the political\n", levels(plot_data$axis)))
      } else {
        plot_data$axis <- 
          factor(plot_data$axis,
                 levels = c('Esquerra',
                            'Centre',
                            'Dreta'))
        plot_data$axis <- factor(plot_data$axis,
                                 levels = levels(plot_data$axis),
                                 labels = c("Enquestats ideològicament\nd'esquerres",
                                            "Enquestats ideològicament\ndel centre",
                                            "Enquestats ideològicament\nde dretes"))
      }
      
      
      g <- ggplot(data = plot_data, aes(x = date, y = avg)) + geom_line() + facet_wrap(~axis) +
        geom_area(fill = 'darkorange', alpha = 0.3) +
        ggthemes::theme_fivethirtyeight() +
        geom_point() +
        labs(title = 'Grau de confiança en la Monarquia, per ideologia del enquestat',
             y = 'Grau de confiança',
             subtitle = 'Cataluna, 2014-2019.',
             caption = 'Combinació d\'enquestes CEO BOP. Ideologia = autoubicació en una escala esquerra-dreta de 0-10.\n0-3: esquerres, 4-6: centre, 7-10: dretes. Grau de confiança de la Monarquia espanyola en una escala de 0 a 10.') +
        theme(strip.text = element_text(size = 14)) +
        geom_text(aes(label= round(avg, digits = 1)),
                  nudge_y = 0.2,
                  alpha = 0.3)
    } else {
      
      
      plot_data <- plot_data %>%
        group_by(axis)%>%
        summarise(avg = mean(monarquia, na.rm = T))
      if(language == 'en'){
        
        x = 'Ideology (self-assessed)'
        y = 'Approval rating'
        title = 'Support for the Spanish Monarchy among Catalans, by ideology'
        subtitle = 'Average approval rating (0-10 scale)'
        caption = 'Data from the Baròmetre d\'Opinió Pública, combination of surveys 2014-2019. 12,273 residents of Catalonia with Spanish citizenship. Categories codes as follows:\n ideological scale 0-10: 0-1=far left;2-3=left;4-6=center;7-8=right;9-10=far right.\nChart created by Joe Brew | @joethebrew | www.vilaweb.cat.'
        
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
        y = 'Grau de confiança'
        title = 'Suport a la monarquia espanyola entre catalans, per ideologia'
        subtitle = 'Grau de confiança mitjana (escala de 0-10)'
        caption = 'Dades del Baròmetre d\'Opinió Pública, combinació d\'enquestes 2014-2019\nMostra: 12.273 residents de Catalunya amb ciutadania espayola. Codificació de categories:\nescala ideòlogica 0-10: 0-1=extrema esquerra;2-3=esquerra;4-6=centre;7-8=dreta;9-10=extrema dreta.\nElaboració del gràfic: Joe Brew | @joethebrew | www.vilaweb.cat.'
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
      
      
      if(language == 'en'){
        plot_data$axis <- 
          factor(plot_data$axis,
                 levels = c('Far\nleft',
                            'Left',
                            'Center',
                            'Right',
                            'Far\nright'))
      } else {
        plot_data$axis <- 
          factor(plot_data$axis,
                 levels = c('Extrema\nesquerra',
                            'Esquerra',
                            'Centre',
                            'Dreta',
                            'Extrema\ndreta'))
      }
      
      
      if(return_table){
        return(plot_data)
      }
      g <- ggplot(data = plot_data,
                  aes(x = axis,
                      y = avg)) +
        geom_bar(stat = 'identity') +
        theme_vilaweb() +
        labs(x = x,
             y = y,
             title = title,
             subtitle = subtitle,
             caption = caption) +
        geom_text(aes(label = round(avg, digits = 1)),
                  size = 5,
                  color = 'white',
                  nudge_y = -0.3,
                  alpha = 0.6) +
        theme(plot.title = element_text(size = 16),
              plot.subtitle = element_text(size = 11),
              axis.text.x = element_text(size = 15),
              axis.text.y = element_text(size = 13),
              legend.text = element_text(size = 20),
              plot.caption = element_text(size = 10)) 
    }}
  return(g)
}


bp <- function(x = 'Spectral', n = 9){
  RColorBrewer::brewer.pal(n = 9,
                           name = x)[n]
}

deal_plot <- function(ca = FALSE,
                      by_date = TRUE){
  pd <- combined %>%
    mutate(acord = `Fins a quin punt creu vostè que és molt probable o gens probable que el Govern Espanyol acabi oferint un acord que sigui acceptable per la majoria del Parlament de Catalunya?`) %>%
    filter(!is.na(acord)) %>%
    mutate(acord = as.character(acord)) %>%
    mutate(acord = ifelse(acord %in% c('No ho sap', 'No contesta'),
                          'NS/NC', acord)) 
  
  
  pd$acord <- factor(pd$acord,
                     levels = c('Gens probable',
                                'Poc probable',
                                'NS/NC',
                                'Bastant probable',
                                'Molt probable'))
  
  if(!ca){
    pd$acord <- factor(pd$acord,
                       levels = levels(pd$acord),
                       labels = c('Not at all\nprobable',
                                  'Not probable',
                                  'Not sure/\nnoanswer',
                                  'Pretty probable',
                                  'Very probable'))
    the_labs <- labs(title = 'Perceived probability that Spanish Government will end up\noffering an agreement which is acceptable to the majority\nof the Catalan Parliament',
                     x = 'Date',
                     y = 'Percentage')
  } else {
    the_labs <- labs(title = 'Fins a quin punt creieu que és molt probable o gens\nprobable que el govern espanyol acabi oferint un acord que\nsigui acceptable per a la majoria del Parlament de Catalunya?',
                     x = 'Data',
                     y = 'Percentatge')
  }
  if(by_date){

    pd <- pd %>%
      group_by(acord, date) %>%
      tally %>%
      ungroup %>%
      group_by(date) %>%
      mutate(p = n / sum(n) * 100) %>%
      ungroup
    cols <- RColorBrewer::brewer.pal(n = length(unique(pd$acord)), 'Spectral')
    ggplot(data = pd,
           aes(x = date,
               y = p,
               fill = acord)) +
      geom_bar(stat = 'identity') +
      theme_vilaweb() +
      scale_fill_manual(name = '',
                        values = cols) +
      the_labs +
      theme(legend.position = 'right')
  } else {
    pd <- pd %>%
      group_by(acord) %>%
      tally %>%
      ungroup %>%
      mutate(p = n / sum(n) * 100)  
    
    ggplot(data = pd,
           aes(x = acord,
               y = p)) +
      geom_bar(stat = 'identity') +
      theme_vilaweb() +
      the_labs +
      geom_text(aes(label = round(p, digits = 1)),
                nudge_y = -5,
                alpha = 0.6,
                color = 'white')
  }
  
  }

franco_constitution <- function(ca = FALSE){
  
  pd <- combined %>%
    filter(!is.na(franquisme),
           franquisme != 'NS/NC') 
  
  if(ca){
    pd$franquisme <- factor(pd$franquisme, 
                            levels = c('Negatiu',
                                       'Va tenir coses positives i negatives',
                                       'Positiu'),
                            labels = c('Negatiu',
                                       'Va tenir coses\npositives i negatives',
                                       'Positiu'))
    constitution_name <- 'Com votarieu en un\nreferèndum sobre la\nconstitució de 1978'
    the_labs <- labs(x = 'Valoració del franquisme',
                     y = 'Percentatge',
                     title = 'Relació entre franquisme i suport a la constitució',
                     subtitle = 'Catalunya. Dades del CEO, ronda 3, 2018. Mida de mostra: 1.409.')
  } else {
    pd$franquisme <- factor(pd$franquisme, 
                            levels = c('Negatiu',
                                       'Va tenir coses positives i negatives',
                                       'Positiu'),
                            labels = c('Negative',
                                       'It had both good\nand bad things',
                                       'Positive'))
    constitution_name <- 'How would you vote\nin a referendum on the\n1978 Constitution?'
    the_labs <- labs(x = 'View of Francoism',
                     y = 'Percentage',
                     title = 'Association between Francoism and\nsupport for the Constitution',
                     subtitle = 'Catalonia. Data from the CEO, round 3, 2018. Sample size: 1,409.')
    pd$constitucio <- factor(pd$constitucio)
    pd$constitucio <- factor(pd$constitucio, levels = levels(pd$constitucio),
                             labels = c('I would vote no',
                                        'Not sure,\ndon\'t know,\nblank',
                                        'I would vote yes'))
  }
    pd <- pd %>%
    group_by(franquisme,
             constitucio) %>%
    tally %>%
    ungroup %>%
    group_by(franquisme) %>%
    mutate(p = n / sum(n) * 100) %>%
    ungroup
  
  bp <- function(x,z){RColorBrewer::brewer.pal(n = 8,name =x)[z]}
  cols <- rev(c(bp('Blues', 5),
                bp('Greys', 7),
                bp('Oranges', 5)))
  
  ggplot(data = pd,
         aes(x = franquisme,
             y = p,
             group = constitucio)) +
    geom_bar(stat = 'identity',
             aes(fill = constitucio),
             position = position_dodge(width = 0.8),
             alpha = 0.85) +
    theme_vilaweb() +
    scale_fill_manual(name = constitution_name,
                      values = cols) +
    theme(legend.position = 'right') +
    the_labs +
    geom_text(position = position_dodge(width = 0.8),
              aes(label = round(p, digits = 1),
                  y = p + 4),
              alpha = 0.6)
}





re_ref <- function(language = 'en',
                   return_table = FALSE){
  if(language == 'en'){
    
    x = ''
    y = 'Percentage'
    title = 'Support for the Spanish Constitution among Catalans'
    subtitle = 'If a referendum were to be held today on the current\nSpanish Constitution approved in 1978, as it is now, how would you vote?'
    caption = 'Data from the Baròmetre d\'Opinió Pública, 3a onada 2018.\nSample size: 1500 residents of Catalonia with Spanish citizenship.'
  
  } else {
    
    x = ''
    y = 'Percentatge'
    title = 'Suport a la constitució espanyola entre catalans'
    subtitle = 'Si es tornés a celebrar un referèndum per decidir sobre l’actual\nconstitució espanyola aprovada el 1978, tal com és ara, què faríeu?'
    caption = 'Dades del Baròmetre d\'Opinió Pública, 3a onada 2018.\nMostra: 1500 residents de Catalunya amb ciutadania espayola.'
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
                bp('Greys', 7),
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
              vjust = 1.4,
              size = 6,
              color = 'white',
              alpha = 0.6) +
    theme(plot.title = element_text(size = 16),
          plot.subtitle = element_text(size = 11),
          axis.text.x = element_text(size = 15),
          axis.text.y = element_text(size = 13),
          legend.text = element_text(size = 20),
          plot.caption = element_text(size = 10)) +
    theme(legend.position = 'none')
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
    y <- 'Percentatge'
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
    
    y = 'Percentatge'
    title = 'Suport a la constitució espanyola entre catalans'
    subtitle = 'Si es tornés a celebrar un referèndum per decidir sobre l’actual\nconstitució espanyola aprovada el 1978, tal com és ara, què faríeu?'
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
    if(language == 'en'){subtitle <- 'Spain, not counting Catalonia'} else {subtitle <-'Estat espanyol, sense incloure-hi Catalunya'}
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
                      'Més autonomia',
                      'Possibilitat\nd\'independència',
                      'NS/NC',
                      'NS/NC'))
  
  plot_data <- plot_data %>%
    left_join(right)
  if(language == 'en'){
    plot_data$x <- plot_data$en
    title <- 'Preferences for territorial organization'
    caption <- 'Data from CIS survey, October 2018.\nJoe Brew | @joethebrew | www.vilaweb.cat.'
    y <- 'Percentage'
    x <- 'Preference'
  } else {
    plot_data$x <- plot_data$ca    
    title <- 'Preferències sobre organització territorial'
    caption <- 'Dades del CIS de l\'octubre del 2018.\nJoe Brew | @joethebrew | www.vilaweb.cat.'
    y <- 'Percentatge'
    x <- 'Preferència'
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
                              'Més autonomia',
                              'Possibilitat\nd\'independència'))
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
    y <- 'Percentatge'
    
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

