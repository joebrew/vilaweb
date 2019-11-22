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


# monarquia
# identitat

# Transform data to combine
transform_data <- function(df, cosmo = FALSE){
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
  v3 <- '50. Amb quina de les següents frases se sent més identificat/ada:'
  if(v1 %in% names(df)){
    df$identificacio <- unlist(df[,v1])
  } else if(v2 %in% names(df)){
    df$identificacio <- unlist(df[,v2])
  } else if(v3 %in% names(df)) {
    df$identificacio <- unlist(df[,v3])
  } else {
    df$identificacio <- NA
  }

  # LLENGUES
  if(cosmo){
    df$llengua_primera <- df$`51b. Recorda quina llengua va parlar primer vostè, a casa, quan era petit/a?`
    df$llengua_habitual <- df$`51c. Pot indicar-me quina és la seva llengua habitual? Ens referim a la llengua que parla més sovint.`
    df$llengua_propia <- df$`51a. Pot indicar-me quina és la seva llengua? Ens referim a quina és la llengua que vostè considera com a pròpia.`
  } else {
    df$llengua_primera <- df$`Quina llengua va parlar primer vostè, a casa, quan era petit?`
    df$llengua_habitual <- df$`Quina és la seva llengua habitual, ens referim a la llengua que parla més sovint?`
    df$llengua_propia <- df$`Quina és la seva llengua, ens referim a quina és la llengua que vostè considera com a pròpia?`
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

  if("`Quina de les següents frases reflecteix millor la seva opinió sobre la democràcia?`" %in% names(df)){
    df$democracia <- df$`Quina de les següents frases reflecteix millor la seva opinió sobre la democràcia?`
  } else{
    df$democracia <- NA
  }
  if("Any de realització del baròmetre" %in% names(df)){
    df$any <- df$`Any de realització del baròmetre`
  } else {
    df$any <- 2019
  }
  if("Mes de realització del baròmetre" %in% names(df)){
    df$mes <- df$`Mes de realització del baròmetre`
  } else {
    df$mes <- 9
  }


  # indepe
  if("Vol que Catalunya esdevingui un Estat independent?" %in% names(df)){
    df$indepe <- df$`Vol que Catalunya esdevingui un Estat independent?`
  } else if("15. I, més concretament, vol que Catalunya sigui un Estat independent?" %in% names(df)){
    df$indepe <- df$`15. I, més concretament, vol que Catalunya sigui un Estat independent?`
  } else {
    df$indepe <- NA
  }

  # interest
  if("A vostè la política li interessa molt, bastant, poc o gens?" %in% names(df)){
    df$interessat <- df$`A vostè la política li interessa molt, bastant, poc o gens?`
  } else {
    df$interessat <- NA
  }

  if("Em podria dir per quin partit sent més simpatia?" %in% names(df)){
    df$partit <- df$`Em podria dir per quin partit sent més simpatia?`
  } else {
    df$partit <- NA
  }

  if("Grau de confiança en: La Monarquia espanyola" %in% names(df)){
    df$monarquia_confianca <- df$`Grau de confiança en: La Monarquia espanyola`
  } else if("10j. Posi nota al grau de confiança que li mereixen cadascuna d’elles en una escala del 0 al 10: La Monarquia espanyola" %in% names(df)){
    df$monarquia_confianca <- df$`10j. Posi nota al grau de confiança que li mereixen cadascuna d’elles en una escala del 0 al 10: La Monarquia espanyola`
  } else {
    df$monarquia_confianca <- NA
  }


  if("Quan es parla de política, normalment s’utilitzen les expressions esquerra i dreta,  indiqui on s’ubicaria vostè?" %in% names(df)){
    df$axis <- df$`Quan es parla de política, normalment s’utilitzen les expressions esquerra i dreta,  indiqui on s’ubicaria vostè?`
  } else if("11. Em pot dir on s’ubicaria vostè en una escala de 0 a 10 en la qual 0 significa extrema esquerra i 10 extrema dreta?" %in% names(df)){
    df$axis <- df$`11. Em pot dir on s’ubicaria vostè en una escala de 0 a 10 en la qual 0 significa extrema esquerra i 10 extrema dreta?`
  } else {
    df$axis <- NA
  }

  if("Quins són els ingressos familiars que entren cada mes a casa seva?" %in% names(df)){
    df$ingressos <- df$`Quins són els ingressos familiars que entren cada mes a casa seva?`
  } else if("53. Aproximadament, quins són els ingressos familiars nets que entren cada mes a casa seva?" %in% names(df)){
    df$ingressos <- df$`53. Aproximadament, quins són els ingressos familiars nets que entren cada mes a casa seva?`
  }

  if(cosmo){
    # remove numbers
    for(j in 1:ncol(df)){
      col_name <- names(df)[j]
      if(!is.na(as.numeric(substr(col_name, 1, 1))) |
         !is.na(as.numeric(substr(col_name, 2, 2)))){
        new_name <- unlist(lapply(strsplit(col_name, '. ', fixed = TRUE), function(x){x[2]}))
      } else {
        new_name <- col_name
      }
      names(df)[j] <- new_name
    }
  }
  df <- df[,!duplicated(names(df))]

  out <- df %>%
    mutate(mes = ifelse(mes == 3 & any == 2014, 4, mes),
           mes = ifelse(mes == 10 & any == 2014, 11, mes),
           mes = ifelse(mes == 3 & any == 2015, 2, mes),
           mes = ifelse(mes == 7 & any == 2017, 6, mes),
           mes = ifelse(mes == 7 & any == 2018, 6, mes),
           mes = ifelse(mes == 11 & any == 2018, 10, mes),
           mes = ifelse(mes == 7 & any == 2019, 6, mes)) %>%
    mutate(date = as.Date(paste0(any, '-', mes, '-15'))) %>%
  # mutate(avis = as.character(`Quants dels seus avis/àvies van néixer a Catalunya?`)) %>%
  #   mutate(avis = ifelse(avis == 'Cap', '0',
  #                        ifelse(avis == 'Un', '1',
  #                               ifelse(avis == 'Dos', '2',
  #                                      ifelse(avis == 'Tres', '3',
  #                                             ifelse(avis == 'Quatre', '4', NA)))))) %>%
  #   mutate(avis = as.numeric(avis)) %>%
  #   mutate(pare_cat = `Em podria dir el lloc de naixement del seu pare?` == 'Catalunya',
  #          pare_esp = `Em podria dir el lloc de naixement del seu pare?` == 'Altres comunitats autònomes',
  #          mare_cat = `Em podria dir el lloc de naixement de la seva mare?` == 'Catalunya',
  #          mare_esp = `Em podria dir el lloc de naixement de la seva mare?` == 'Altres comunitats autònomes') %>%
  #   mutate(pare_cat = as.numeric(pare_cat),
  #          pare_esp = as.numeric(pare_esp),
  #          mare_cat = as.numeric(mare_cat),
  #          mare_esp = as.numeric(mare_esp)) %>%
    mutate(llengua_primera = convert_language(llengua_primera),
           llengua_habitual = convert_language(llengua_habitual),
           llengua_propia = convert_language(llengua_propia)) %>%

    # mutate(llengua_preferiex = `Prefereix que li faci les preguntes en català o en castellà?`),
    mutate(neixer = `Em podria dir on va néixer?`,
           informat = `Es considera vostè molt, bastant, poc o gens informat/ada del que passa en política?`
           # telefon_fix = `Té telèfon fix a la seva llar?`,


           ) %>%
    mutate(neixer = as.character(neixer)) %>%
    mutate(neixer = ifelse(neixer == 'Altres comunitats autònome', 'Altres comunitats autònomes', neixer)) %>%
    mutate(indepe = as.character(indepe)) %>%
    mutate(indepe =
             ifelse(indepe %in% c('No ho sap', 'No contesta'),
                    'NS/NC', indepe)) %>%
    # mutate(municipi = `Grandària del municipi`) %>%
    mutate(provincia = `Província`) %>%
    dplyr::select(
      monarquia_confianca,
      identificacio,
      democracia,
      # municipi,
      provincia,
      date,
      monarquia,
      # avis,
      # pare_cat, pare_esp,
      # mare_cat, mare_esp,
      llengua_primera,
      llengua_habitual,
      llengua_propia, #llengua_prefereix,
      neixer,
      informat,
      interessat,
      partit,
      axis,
      # telefon_fix,
      ingressos,
      indepe,
      franquisme,

      # contains('Fins a quin punt')
      constitucio
    ) %>%
    mutate(monarquia = as.character(monarquia)) %>%
    mutate(monarquia = ifelse(monarquia %in% c('No ho sap', 'No contesta'), NA,
                              ifelse(monarquia == 'Cap confiança', '0',
                                     ifelse(monarquia == 'Molta confiança', '10',
                                            monarquia)))) %>%
    mutate(monarquia = as.numeric(monarquia)) #%>%
    # mutate(pares = ifelse(pare_cat + mare_cat == 2,
    #                       '2 pares nascuts a Cat',
    #                       ifelse(pare_cat + mare_cat == 1 &
    #                                pare_esp + mare_esp == 1,
    #                              '1 pare nascut a Cat, l\'altre a Esp',
    #                              ifelse(pare_esp + mare_esp == 2,
    #                                     '2 pares nascuts a Esp',
    #                                     'Altres combinacions')
    #                              ))) #%>%
    # mutate(pares = ifelse(pare_cat + mare_cat == 1 &
    #                         pare_esp + mare_esp == 0,
    #                       '1 pare nascut a Cat, l\'altre a l\'Estranger',
    #                       ifelse(pare_cat + mare_cat == 0 &
    #                                pare_esp + mare_esp == 1,
    #                              '1 pare nascut a Esp, l\'altre a l\'Estranger', pares)))

  # Go through the "fins a quin punt" questions, and clean up
  # fins_quins <- names(out)[grepl('Fins a quin punt està d', names(out))]
  # if(length(fins_quins) > 0){
  #   for(j in 1:length(fins_quins)){
  #     this_variable_name <- fins_quins[j]
  #     # Detect
  #     new_variable_name <- gsub("Fins a quin punt està d’acord o en desacord amb cadascuna de les següents afirmacions: ", 'fins_quin_punt_', this_variable_name)
  #     simple_variable_name <- gsub("Fins a quin punt està d’acord o en desacord amb cadascuna de les següents afirmacions: ", 'fins_quin_punt_simple_', this_variable_name)
  #     this_data <- as.character(unlist(out[,this_variable_name]))
  #     this_data <- ifelse(this_data %in% c('No contesta', 'No ho sap'),
  #                         'NS/NC',
  #                         this_data)
  #     this_simple_data <-
  #       ifelse(this_data %in% c("D'acord", "Molt d'acord"),
  #              "D'acord",
  #              ifelse(this_data %in% c("En desacord", "Molt en desacord"),
  #                     "En desacord",
  #                     ifelse(this_data %in% c("Ni d'acord ni en desacord",
  #                                             "NS/NC"),
  #                            "Ni d'acord ni en\ndesacord o NS/NC",
  #                            this_data)))
  #     this_data <- factor(this_data,
  #                         levels = c("Molt d'acord", "D'acord",
  #                                    "Ni d'acord ni en desacord", "NS/NC",
  #                                    "En desacord", "Molt en desacord"))
  #     this_simple_data <- factor(this_simple_data, levels = c("D'acord",
  #                                                             "Ni d'acord ni en\ndesacord o NS/NC",
  #                                                             "En desacord"))
  #     out[,new_variable_name] <- this_data
  #     out[,simple_variable_name] <- this_simple_data
  #     out[,this_variable_name] <- NULL
  #   }
  # }


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

  out$axis <- as.character(out$axis)
  out$axis <- ifelse(out$axis == 'Extrema esquerra', '0',
                     ifelse(out$axis == 'Extrema dreta', '10', out$axis))
  out$axis <- as.numeric(out$axis)
  out$axis_simple <- ifelse(out$axis <= 1, 'Extrema esquerra',
                            ifelse(out$axis <= 4, 'Esquerra',
                                   ifelse(out$axis <= 6, 'Centre',
                                          ifelse(out$axis <= 8, 'Dreta',
                                                 ifelse(out$axis <= 10, 'Extrema dreta', NA)))))
  out$axis_super_simple <- ifelse(out$axis <= 4, 'Esquerra',
                                   ifelse(out$axis <= 6, 'Centre',
                                          ifelse(out$axis <= 10, 'Dreta', NA)))
  out$axis_simple <- factor(out$axis_simple,
                            levels = c('Extrema esquerra',
                                       'Esquerra',
                                       'Centre',
                                       'Dreta',
                                       'Extrema dreta'))
  out$axis_super_simple <- factor(out$axis_super_simple,
                            levels = c('Esquerra',
                                       'Centre',
                                       'Dreta'))

  out$identificacio_simple <- out$identificacio
  out$identificacio_simple <-
    ifelse(grepl('Només esp', out$identificacio_simple) |
             grepl('Més esp', out$identificacio_simple), 'Més espanyol',
           ifelse(grepl('Només cata', out$identificacio_simple) |
                    grepl('Més cata', out$identificacio_simple),
                  'Més català',
                  ifelse(grepl('Tan', out$identificacio_simple),
                         'Tan espanyol com català',
                         NA)))

  out$monarquia_confianca <- as.character(out$monarquia_confianca)
  out$monarquia_confianca <- ifelse(out$monarquia_confianca == 'Cap confiança', 0,
                                    ifelse(out$monarquia_confianca == 'Molta confiança', 10,
                                           out$monarquia_confianca))
  out$monarquia_confianca <- as.numeric(out$monarquia_confianca)

  out$identificacio_simple <- factor(out$identificacio_simple,
                              levels = c('Més català', 'Tan espanyol com català',
                                         'Més espanyol'))

  return(out)
}

bop <-
  transform_data(new_ceo) #%>%
cosmo <- transform_data(cosmopolitisme, cosmo = TRUE)
maig <- transform_data(ceo_electoral_maig_2019, cosmo = TRUE)
# Omnibus doesn't have indepe question
#omnibus <- transform_data(ceo_omnibus_2019, cosmo = TRUE)

maig$ciutada <- 'Sí'
bop$ciutada <- 'Sí'
cosmo$ciutada <- ifelse(grepl('Sí', cosmopolitisme$`F3. Em podria indicar si vostè té la ciutadania espanyola, té la ciutadania espanyola i una altra o no té la ciutadania espanyola?`), 'Sí', 'No')
