# Libraries
library(vilaweb)
library(tidyverse)
library(ggplot2)
library(tidyr)

# Get most recent CEO data
ceo_june_2019 <- vilaweb::ceo_june_2019

# Get age range
get_details <- function(df){
  df %>%
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
    summarise(p50 = median(Edat),
              avg = mean(Edat),
              p75 = quantile(Edat, 0.75),
              p25 = quantile(Edat, 0.25),
              pcat = length(which(`Em podria dir on va néixer?` == 'Catalunya')) / n(),
              pesp = length(which(`Em podria dir on va néixer?` == 'Altres comunitats autònomes')) / n(),
              avis = mean(avis, na.rm = TRUE),
              avisp50 = median(avis, na.rm = TRUE),
              avisp75 = quantile(avis, 0.75, na.rm = TRUE),
              avisp25 = quantile(avis, 0.25, na.rm = TRUE),
              pares_cat = mean(pare_cat + mare_cat, na.rm = TRUE),
              pares_esp = mean(pare_esp + mare_esp, na.rm = TRUE),
              pares_catp25 = quantile(pare_cat + mare_cat, 0.25, na.rm = TRUE),
              pares_catp75 = quantile(pare_cat + mare_cat, 0.75, na.rm = TRUE),
              pares_espp25 = quantile(pare_esp + mare_esp, 0.25, na.rm = TRUE),
              pares_espp75 = quantile(pare_esp + mare_esp, 0.75, na.rm = TRUE))
}

# new_ceo %>% group_by(`Any de realització del baròmetre`) %>%
#   get_details() %>%
#   bind_rows(
#     ceo_june_2019 %>%
#       group_by(`Any de realització del baròmetre`) %>%
#       get_details()
#   ) %>%
#   View


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
  
  df %>%
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
      municipi,
      provincia,
      date,
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
      indepe
    ) %>%
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
}

combined <- 
  transform_data(new_ceo) %>%
  bind_rows(
    transform_data(ceo_june_2019)
  )
# The comined dataset contains all ceo data
make_plot <- function(ca = FALSE, var = 'llengua_propia',
                      color_var = 'indepe'){
  pd <- combined
  pd$var <- unlist(pd[,var])
  pd$color_var <- unlist(pd[,color_var])
  pd <- pd %>%
    group_by(var, date, color_var) %>%
    tally %>%
    group_by(var, date) %>%
    mutate(p = n / sum(n) * 100) %>%
    ungroup# %>%
    # filter(llengua %in% c('Castellà',
    #                       'Cat+Cast',
    #                       'Català'))
  
  ggplot(data = pd,
         aes(x = date,
             y = p,
             color = color_var)) +
    geom_point() +
    geom_line() +
    facet_wrap(~var)
}
make_plot(var = 'avis')

sample_plot <- function(ca = FALSE, var = 'avis', data = NULL){
  if(is.null(data)){
    pd <- combined
  } else {
    pd <- data
  }
  pd$var <- unlist(pd[,var])
  
  pd <- pd %>%
    filter(!is.na(var)) %>%
    filter(var != 'NS/NC') %>%
    group_by(date, var) %>%
    tally %>%
    group_by(date) %>%
    mutate(p = n / sum(n) * 100) %>%
    ungroup
  
  return(pd)
}
plot_sample_plot <- function(pd, scales = 'free_y'){
  ggplot(data = pd,
         aes(x = date,
             y = p)) +
    geom_line() +
    geom_point() +
    geom_area(alpha = 0.5) +
    facet_wrap(~var, scales = scales) +
    theme_vilaweb()
}


plot_combined <- function(ca = FALSE){
  pd <- combined %>%
    group_by(date, indepe) %>% tally %>%
    group_by(date) %>% mutate(p = n / sum(n) * 100) %>%
    ungroup %>%
    filter(date >= '2015-01-01')
  if(ca){
    the_labs <- labs(title = 'Suport per la independència de Catalunya',
                     subtitle = "Pregunta: 'Vol que Catalunya esdevingui un Estat independent?'",
                     x = 'Data',
                     y = 'Percentatge')
  } else {
    the_labs <- labs(title = 'Support for the independence of Catalonia',
                     subtitle = "Question: 'Do you want Catalonia to become an independent State?'",
                     x = 'Date',
                     y = 'Percentatge')
    pd$indepe <- as.character(pd$indepe)
    pd$indepe <- ifelse(pd$indepe == 'Sí', 'Yes',
                        ifelse(pd$indepe == 'NS/NC', 'N/A',
                               pd$indepe))
    pd$indepe <- factor(pd$indepe,
                        levels = c('No', 'N/A', 'Yes'))
    
  }
  ggplot(data = pd,
         aes(x = date,
             y = p,
             color = indepe)) +
    geom_point() +
    geom_line() +
    theme_vilaweb() +
    the_labs +
    scale_color_manual(name = '',
                       values = vilaweb::colors_vilaweb()[c(3,6,5)])
}

province_plot <- function(ca = FALSE){
  if(ca){
    the_labs <- labs(title = 'Província dels enquestats',
                     subtitle = 'Enquesta BOP del CEO',
                     x = 'Data',
                     y = 'Percentatge')
  } else {
    the_labs <- labs(title = 'Province of those surveyed',
                     subtitle = 'BOP survey of the CEO',
                     x = 'Date',
                     y = 'Percentage')
  }
  plot_sample_plot(sample_plot(var = 'provincia')) + xlim(as.Date('2016-01-01'), max(combined$date)) +
    theme_vilaweb() +
    the_labs
}

language_plot <- function(ca = FALSE){
  pd <- combined
  
  if(ca){
    the_labs <- labs(title = 'Independentisme per \'llengua pròpia\'',
                     x = 'Llengua pròpia',
                     y = 'Percentatge')
    legend_title <- 'Independentista?'
  } else {
    the_labs <- labs(title = "Support for independence by 'own language'",
                     x = 'Language considers to be \'one\'s own\'',
                     y = 'Percentage')
    legend_title <- 'Pro-independence?'
    pd$indepe <- as.character(pd$indepe)
    pd$indepe <- ifelse(pd$indepe == 'Sí', 'Yes',
                        ifelse(pd$indepe == 'NS/NC', 'N/A',
                               pd$indepe))
    pd$indepe <- factor(pd$indepe,
                        levels = c('No', 'N/A', 'Yes'))
    pd$llengua_propia <- as.character(pd$llengua_propia)
    pd$llengua_propia <- ifelse(pd$llengua_propia == 'Català',
                                'Catalan', 'Spanish')
  }
  
  pd <- pd %>%
    filter(!is.na(indepe),
           !llengua_propia %in% c('Altres',
                                  'NS/NC',
                                  'Cat+Cast')) %>%
    group_by(llengua_propia, indepe) %>%
    tally %>%
    group_by(llengua_propia) %>%
    mutate(p = n / sum(n) * 100) %>%
    ungroup
  
  ggplot(data = pd,
         aes(x = llengua_propia,
             y = p,
             fill = indepe)) +
    geom_bar(stat = 'identity',
             position = position_dodge(width = 0.7)) +
    theme_vilaweb() +
    the_labs +
    geom_text(aes(label = round(p, digits = 1),
                  y = p + 5),
              alpha = 0.6,
              position = position_dodge(width = 0.7)) +
    scale_fill_manual(name = legend_title,
                       values = vilaweb::colors_vilaweb()[c(3,6,5)])
}

identificacio_plot <- function(ca = FALSE){
  
  pd <- combined %>%
    filter(!identificacio %in% c('No ho sap', 'No contesta'))
  
  if(ca){
    the_labs <- labs(title = 'Independentisme per identitat',
                     x = 'Autoidentificació',
                     y = 'Percentatge')
    legend_title <- 'Independentista?'
    pd$identificacio <- factor(pd$identificacio,
                               levels = levels(pd$identificacio),
                               labels = gsub(' com', '\ncom', gsub(' que', '\nque', levels(pd$identificacio))))
  } else {
    the_labs <- labs(title = "Support for independence by identity",
                     x = 'Self-identification',
                     y = 'Percentage')
    legend_title <- 'Pro-independence?'
    pd$indepe <- as.character(pd$indepe)
    pd$indepe <- ifelse(pd$indepe == 'Sí', 'Yes',
                        ifelse(pd$indepe == 'NS/NC', 'N/A',
                               pd$indepe))
    pd$indepe <- factor(pd$indepe,
                        levels = c('No', 'N/A', 'Yes'))
    pd$identificacio <- as.character(pd$identificacio)
    levs <- c('Only Spanish', 'More Spanish\nthan Catalan', 'As Spanish\nas Catalan',
              'More Catalan\nthan Spanish', 'Only Catalan')
    pd <- pd %>%
      mutate(identificacio = ifelse(identificacio == 'Només espanyol/a', levs[1],
                                    ifelse(identificacio == 'Més espanyol/a que català/ana', levs[2],
                                           ifelse(identificacio == 'Tan espanyol/a com català/ana', levs[3],
                                                  ifelse(identificacio == 'Més català/ana que espanyol/a', levs[4],
                                                         ifelse(identificacio == 'Només català/ana', levs[5], NA))))))
    pd$identificacio <- factor(pd$identificacio, levels = levs)
  }
    pd <- pd %>%
    filter(!is.na(indepe)) %>%
    group_by(identificacio, indepe) %>%
    tally %>%
    group_by(identificacio) %>%
    mutate(p = n / sum(n) * 100) %>%
    ungroup
  
  ggplot(data = pd,
         aes(x = identificacio,
             y = p,
             fill = indepe)) +
    geom_bar(stat = 'identity',
             position = position_dodge(width = 0.7)) +
    theme_vilaweb() +
    the_labs +
    geom_text(aes(label = round(p, digits = 1),
                  y = p + 5),
              alpha = 0.6,
              position = position_dodge(width = 0.7)) +
    scale_fill_manual(name = legend_title,
                      values = vilaweb::colors_vilaweb()[c(3,6,5)])
}


pares_plot <- function(ca = FALSE){
  
  pd <- combined
  
  if(ca){
    the_labels <- c('2 pares nascuts a Cat',
                    "1 pare nascut a Cat, l'altre a Esp",
                    "2 pares nascuts a Esp",
                    "Altres combinacions")
    the_labs <- labs(title = 'Independentisme per lloc de naixement dels pares',
                     x = 'Lloc de naixement dels pares',
                     y = 'Percentatge')
    legend_title <- 'Independentista?'
    pd$pares <- factor(pd$pares,
                       levels = the_labels)
  
  } else {
    the_labels <- c('2 pares nascuts a Cat',
                    "1 pare nascut a Cat, l'altre a Esp",
                    "2 pares nascuts a Esp",
                    "Altres combinacions")
    en_levs <- c('Both parents\nborn in Cat',
                 '1 parent from Cat,\nthe other from Spain',
                 'Both parents\nborn in Spain',
                 'Other combinations')
    the_labs <- labs(title = "Support for independence by parents' place of birth",
                     x = 'Place of birth of parents',
                     y = 'Percentage')
    legend_title <- 'Pro-independence?'
    pd$indepe <- as.character(pd$indepe)
    pd$indepe <- ifelse(pd$indepe == 'Sí', 'Yes',
                        ifelse(pd$indepe == 'NS/NC', 'N/A',
                               pd$indepe))
    pd$indepe <- factor(pd$indepe,
                        levels = c('No', 'N/A', 'Yes'))
    pd$pares <- as.character(pd$pares)
    pd$pares <- ifelse(pd$pares == the_labels[1], en_levs[1],
                       ifelse(pd$pares == the_labels[2], en_levs[2],
                              ifelse(pd$pares == the_labels[3], en_levs[3],
                                     ifelse(pd$pares == the_labels[4], en_levs[4], NA))))
    pd$pares <- factor(pd$pares, levels = en_levs)
  }
  pd <- pd %>%
    filter(!is.na(indepe)) %>%
    group_by(pares, indepe) %>%
    tally %>%
    group_by(pares) %>%
    mutate(p = n / sum(n) * 100) %>%
    ungroup
  
  ggplot(data = pd,
         aes(x = pares,
             y = p,
             fill = indepe)) +
    geom_bar(stat = 'identity',
             position = position_dodge(width = 0.7)) +
    theme_vilaweb() +
    the_labs +
    geom_text(aes(label = round(p, digits = 1),
                  y = p + 5),
              alpha = 0.6,
              position = position_dodge(width = 0.7)) +
    scale_fill_manual(name = legend_title,
                      values = vilaweb::colors_vilaweb()[c(3,6,5)])
}

plot_identificacio <- function(ca = FALSE){
  if(ca){
    the_labs <- labs(title = 'Autoidentificació',
                     subtitle = 'Enquesta BOP del CEO',
                     x = 'Data',
                     y = 'Percentatge')
  } else {
    the_labs <- labs(title = 'Self-identification',
                     subtitle = 'BOP survey of the CEO',
                     x = 'Date',
                     y = 'Percentage')
  }
  plot_sample_plot(sample_plot(var = 'identificacio',
                               data = combined %>% filter(!identificacio %in% c('No contesta', 'No ho sap'))
  )) + xlim(as.Date('2017-10-01'), Sys.Date()) +
    the_labs
}



plot_llengua <- function(ca = FALSE){
  if(ca){
    the_labs <- labs(title = "Llengua considerada 'pròpia'",
                     subtitle = 'Enquesta BOP del CEO',
                     x = 'Data',
                     y = 'Percentatge')
  } else {
    the_labs <- labs(title = "Language considered 'one's own'",
                     subtitle = 'BOP survey of the CEO',
                     x = 'Date',
                     y = 'Percentage')
  }
  plot_sample_plot(sample_plot(var = 'llengua_propia',
                               data = combined))+ xlim(as.Date('2017-10-01'), Sys.Date()) +
    the_labs
}

parents_plot <- function(ca = FALSE){
  pd <- combined %>% filter(date >= '2018-03-01')
  
  
  if(ca){
    the_labels <- c('2 pares nascuts a Cat',
                    "1 pare nascut a Cat, l'altre a Esp",
                    "2 pares nascuts a Esp",
                    "Altres combinacions")
    the_labs <- labs(title = 'Lloc de naixement dels pares dels enquestats',
                     x = 'Data',
                     y = 'Percentatge')
    pd$pares <- factor(pd$pares,
                       levels = the_labels)
    
  } else {
    the_labels <- c('2 pares nascuts a Cat',
                    "1 pare nascut a Cat, l'altre a Esp",
                    "2 pares nascuts a Esp",
                    "Altres combinacions")
    en_levs <- c('Both parents\nborn in Cat',
                 '1 parent from Cat,\nthe other from Spain',
                 'Both parents\nborn in Spain',
                 'Other combinations')
    the_labs <- labs(title = "Survey participants' parents' place of birth",
                     x = 'Date',
                     y = 'Percentage')
    pd$pares <- as.character(pd$pares)
    pd$pares <- ifelse(pd$pares == the_labels[1], en_levs[1],
                       ifelse(pd$pares == the_labels[2], en_levs[2],
                              ifelse(pd$pares == the_labels[3], en_levs[3],
                                     ifelse(pd$pares == the_labels[4], en_levs[4], NA))))
    pd$pares <- factor(pd$pares, levels = en_levs)
  }
  
  plot_sample_plot(sample_plot(var = 'pares',
                               data = pd)) +
    the_labs +
    geom_text(aes(label = round(p, digits = 1)),
              alpha = 0.5,
              size = 3,
              nudge_y = 3)
}

make_resample <- function(var = 'pares'){
  recent <- combined %>% filter(date >= '2018-01-01') %>%
    mutate(previous4 = date <= '2019-05-01')
  recent$new_var <- unlist(recent[,var])
  # adjust population to be identical to previous
  proportions <- recent %>%
    filter(previous4) %>%
    group_by(new_var) %>%
    # group_by(pares, llengua_propia, identificacio) %>%
    summarise(indy_pro = length(which(indepe == 'Sí')),
              indy_anti = length(which(indepe == 'No')),
              n = n()) %>%
    ungroup %>%
    mutate(p = n / sum(n) * 100)
  
  out_list <- list()
  june <- recent %>% filter(date >= '2019-06-01')
  june$id <- 1:nrow(june)
  for(i in 1:nrow(proportions)){
    message(i)
    this_new_var <- proportions$new_var[i]
    n <- round(proportions$p[i] * 10000)
    these_people <- sample(june$id[june$new_var == this_new_var], size = n, replace = T)
    out_list[[i]] <- june[these_people,]
  }
  resampled <- bind_rows(out_list)
  out <- resampled %>%
    group_by(indepe) %>%
    tally %>%
    mutate(p = n / sum(n) * 100)
  return(out)
  
}

