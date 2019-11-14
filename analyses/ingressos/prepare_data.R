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
  transform_data(new_ceo) #%>%
# # (NO LONGER NECESSARY SINCE UPDATED IN NOVEMBER 2019)
  # bind_rows(
  #   transform_data(ceo_june_2019)
  # )
