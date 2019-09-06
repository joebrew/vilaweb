# Libraries
library(vilaweb)
library(tidyverse)
library(databrew)
library(pageviews)
library(lubridate)

# Read ceo data
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
numberfy <- function(x){
  gsub(',', '.', scales::comma(x), fixed = TRUE)
}
# # Get valoracio
transform_valoracions <- function(df){
  vars <- names(df)
  val_vars <- vars[grepl('Valoració:', vars, fixed = TRUE)]
  for(j in 1:length(val_vars)){
    this_var <- val_vars[j]
    vals <- as.numeric(as.character(unlist(df[,this_var])))
    vals <- ifelse(vals %in% 98:99, NA, vals)
    df[,this_var] <- vals
  }
  return(df)
}
transform_coneixements <- function(df){
  vars <- names(df)
  val_vars <- vars[grepl('Coneixement:', vars, fixed = TRUE)]
  for(j in 1:length(val_vars)){
    this_var <- val_vars[j]
    vals <- as.character(unlist(df[,this_var]))
    df[,this_var] <- vals
  }
  return(df)
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
  
  ref_var <- "Fins a quin punt està d’acord o en desacord amb cadascuna de les següents afirmacions: Catalunya té el dret de celebrar un referèndum d'autodeterminació"
  if(ref_var %in% names(df)){
    vals <-  unlist(df[,ref_var])
    if(!all(is.na(vals))){
      df$referendum <- vals
    }
  }
  
  ref_var <- "Fins a quin punt està d’acord o en desacord amb l’afirmació següent: “Els catalans i les catalanes tenen dret a decidir el seu futur com a país votant en un referèndum”?"
  if(ref_var %in% names(df)){
    levs <- c("Molt d'acord", "D'acord", "Ni d'acord ni en desacord", "En desacord", "Molt en desacord", "No ho sap", "No contesta")
    vals <- c(1:5, 98, 99)
    dict <- tibble(vals, referendum = levs)
    dict$referendum <- factor(dict$referendum, levels = levs)
    new_vals <- tibble(vals = unlist(df[,ref_var]))
    new_vals <- left_join(new_vals, dict)
    if(!all(is.na(new_vals$referendum))){
      df$referendum <- new_vals$referendum
    }
  } 
  
  ref_var <- "Fins a quin punt està d’acord o en desacord amb cadascuna de les següents afirmacions: Catalunya no té el dret de celebrar un referèndum d'autodeterminació"
  if(ref_var %in% names(df)){
    vals <- as.character(unlist(df[,ref_var]))
    # Reverse
    vals2 <- ifelse(vals == "D'acord", "En desacord",
                    ifelse(vals == "Molt d'acord", "Molt en desacord",
                           ifelse(vals == "En desacord", "D'acord",
                                  ifelse(vals == "Molt en desacord", "Molt d'acord", vals))))
    levs <- c("Molt d'acord", "D'acord", "Ni d'acord ni en desacord", "En desacord", "Molt en desacord", "No ho sap", "No contesta")
    vals <- factor(vals2, levels = levs)
    if(!all(is.na(vals))){
      df$referendum <- vals
    }
  }
  
  if(!'referendum' %in% names(df)){
    df$referendum <- NA
  }
  
  df <- df %>%
    mutate(partit = `Em podria dir per quin partit sent més simpatia?`) %>%
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
    mutate(age = Edat) %>%
    dplyr::select(
      age,
      partit,
      referendum,
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
      indepe,
      contains("Valoració:"),
      contains('Coneixement: ')
    ) %>%
    mutate(pares = ifelse(pare_cat + mare_cat == 2,
                          '2 pares nascuts a Cat',
                          ifelse(pare_cat + mare_cat == 1 &
                                   pare_esp + mare_esp == 1,
                                 '1 pare nascut a Cat, l\'altre a Esp',
                                 ifelse(pare_esp + mare_esp == 2,
                                        '2 pares nascuts a Esp',
                                        'Altres combinacions')
                          ))) %>%
    mutate(partit = as.character(partit)) %>%
    mutate(partit = ifelse(partit %in% c('ERC', 'PSC', 'CUP',
                                         "PPC"),
                           partit,
                           ifelse(partit %in% c('Podemos','En Comú Podem', 'Catalunya en Comú Podem', 'Barcelona en Comú', 'Catalunya sí que es pot'), 'Podem',
                                  ifelse(partit == "C's", "Cs",
                                         ifelse(partit %in% c('CiU', 'Junts pel Sí', 'CDC', 'PDeCAT', 'Junts per Catalunya'), 'JxCat/PDeCat', 'Cap o altre partit')))))

  df <- transform_valoracions(df)
  df <- transform_coneixements(df)
  return(df)
}

# Combine
bop_numbers <- sort(unique(new_ceo$`Número d'ordre del baròmetre`))
bop_list <- list()
for(i in 1:length(bop_numbers)){
  message(i)
  this_bop_number <- bop_numbers[i]
  this_bop <- new_ceo %>% filter(`Número d'ordre del baròmetre` == this_bop_number)
  out <- transform_data(this_bop)
  bop_list[[i]] <- out
}
bop <- bind_rows(bop_list)
combined <-  
  bop %>%
  bind_rows(
    transform_data(ceo_june_2019)
)

# Create an estimated birth date
combined <- 
  combined %>%
  mutate(dob = date - round((182.5 + (age * 365.25)))) %>%
  # Define the oct1 generation
  mutate(oct1 = 
           # 16 years old
           (dob <= '2002-10-01' &
         # 20 years old
         dob >= '1997-10-01'
         ))

simple_plot <- function(ca = FALSE,
                        keep_simple = FALSE){
  
  levs <- c("Molt d'acord", "D'acord", "Ni d'acord ni en desacord", "En desacord", "Molt en desacord", "No ho sap", "No contesta")
  levs_en <- c('Strongly agree',
               'Agree',
               'Neither agree\nnor disagree',
               'Disagree', 
               'Strongly disagree',
               "Don't know",
               "No answer")
  
  pd <- combined %>%
    filter(date >= '2018-01-01') %>%
    filter(!is.na(referendum)) %>%
    group_by(referendum) %>%
    tally
  
  cols <- RColorBrewer::brewer.pal(n = 5, name = 'Spectral')
  cols <- rev(cols)
  cols[3] <- 'darkgrey'
  cols <- c(cols, rep('darkgrey', 2))
  
  if(keep_simple){
    pd <- pd %>%
      filter(!referendum %in% c(levs[c(3,6,7)],
                                levs_en[c(3,6,7)]))
    cols <- cols[!(1:length(cols) %in% c(3,6,7))]
  }
  
  pd <- pd %>%
    mutate(p = n / sum(n) * 100)
  
  
  
  if(ca){
    the_labs <- labs(x = '',
                     y = 'Percentatge',
                     title = "'Catalunya té dret a celebrar\nun referèndum d'autodeterminació'",
                     subtitle = "Grau d'acord amb la frase",
                     caption = paste0('Gràfic de Joe Brew | @joethebrew | www.vilaweb.cat. Dades del CEO.\n',
                                      'Frase exacte varia per data del qüestionari, detalls complets a:\n',
                                      self_cite(),
                                      '\nMida de mostra: ',
                                      numberfy(sum(pd$n)), 
                                      ' residents de Catalunya amb ciutadania espanyola, 2018-2019.\n'))
    pd$referendum <- factor(pd$referendum,
                            levels = levs,
                            labels = gsub("Ni d'acord ni en desacord",
                                          "Ni d'acord ni\nen desacord", levs))
  } else {
    the_labs <- labs(x = '',
                     y = 'Percentage',
                     title = "'Catalonia has the right to hold\na self-determination referendum'",
                     subtitle = 'Extent of agreement with phrase',
                     caption = paste0('Chart by Joe Brew | @joethebrew | www.vilaweb.cat. Raw data from the CEO.\n',
                                      'Actual phrase varied by questionnaire date, full details at:\n',
                                      self_cite(),
                                      '\nSample size: ',
                                      numberfy(sum(pd$n)), 
                                      ' residents of Catalonia with Spanish citenship, 2018-2019.\n'))
    pd$referendum <- factor(pd$referendum,
                            levels = levs,
                            labels = levs_en)
  }
  
  
  
  ggplot(data = pd,
         aes(x = referendum,
             y = p)) +
    geom_bar(stat = 'identity',
             aes(fill = referendum)) +
    scale_fill_manual(name = '',
                      values = cols) +
    theme_vilaweb() +
    theme(legend.position = 'none') +
    the_labs +
    theme(plot.caption = element_text(size = 9)) +
    geom_text(aes(label = round(p, digits = 1)),
              nudge_y = 5,
              alpha = 0.6)
}


simple_plot_cross <- function(ca = FALSE,
                        keep_simple = TRUE,
                        cross_var = 'interessat',
                        legend_title = ''){
  
  levs <- c("Molt d'acord", "D'acord", "Ni d'acord ni en desacord", "En desacord", "Molt en desacord", "No ho sap", "No contesta")
  levs_en <- c('Strongly agree',
               'Agree',
               'Neither agree\nnor disagree',
               'Disagree', 
               'Strongly disagree',
               "Don't know",
               "No answer")
  
  var_levs <- c('Molt', 'Bastant', 'Poc', 'Gens', 'No ho sap', 'No contesta')
  var_levs_en <- c('A lot', 'A fair amount', 'Little', 'Not at all', "Don't know", 'No answer')
  
  pd <- combined %>%
    filter(date >= '2018-01-01') %>%
    filter(!is.na(referendum)) %>%
    group_by(.dots = list('referendum', cross_var)) %>%
    tally
  names(pd)[2] <- 'var'
  
  cols <- RColorBrewer::brewer.pal(n = 5, name = 'Spectral')
  cols <- rev(cols)
  cols[3] <- 'darkgrey'
  cols <- c(cols, rep('darkgrey', 2))
  
  if(keep_simple){
    pd <- pd %>%
      filter(!referendum %in% c(levs[c(3,6,7)],
                                levs_en[c(3,6,7)]))
    cols <- cols[!(1:length(cols) %in% c(3,6,7))]
  }
  
  pd <- pd %>%
    group_by(var) %>%
    mutate(p = n / sum(n) * 100)
  
  
  
  if(ca){
    the_labs <- labs(x = '',
                     y = 'Percentatge',
                     title = "'Catalunya té dret a celebrar\nun referèndum d'autodeterminació'",
                     subtitle = "Grau d'acord amb la frase",
                     caption = paste0('Gràfic de Joe Brew | @joethebrew | www.vilaweb.cat. Dades del CEO.\n',
                                      'Frase exacte varia per data del qüestionari, detalls complets a:\n',
                                      self_cite(),
                                      '\nMida de mostra: ',
                                      numberfy(sum(pd$n)), 
                                      ' residents de Catalunya amb ciutadania espanyola, 2018-2019.\n'))
    pd$referendum <- factor(pd$referendum,
                            levels = levs,
                            labels = gsub("Ni d'acord ni en desacord",
                                          "Ni d'acord ni\nen desacord", levs))
    pd$var <- factor(pd$var,
                     levels = var_levs,
                     labels = var_levs)
  } else {
    the_labs <- labs(x = '',
                     y = 'Percentage',
                     title = "'Catalonia has the right to hold\na self-determination referendum'",
                     subtitle = 'Extent of agreement with phrase',
                     caption = paste0('Chart by Joe Brew | @joethebrew | www.vilaweb.cat. Raw data from the CEO.\n',
                                      'Actual phrase varied by questionnaire date, full details at:\n',
                                      self_cite(),
                                      '\nSample size: ',
                                      numberfy(sum(pd$n)), 
                                      ' residents of Catalonia with Spanish citenship, 2018-2019.\n'))
    pd$referendum <- factor(pd$referendum,
                            levels = levs,
                            labels = levs_en)
    pd$var <- factor(pd$var,
                     levels = var_levs,
                     labels = var_levs_en)
  }
  
  pd <- pd %>% filter(!is.na(var),
                      !var %in% c('No ho sap', 'No contesta', "Don't know", 'No answer'))
  ggplot(data = pd,
         aes(x = var,
             y = p,
             group = referendum)) +
    geom_bar(stat = 'identity',
             aes(fill = referendum),
             position = position_dodge(width = 0.8)) +
    scale_fill_manual(name = legend_title,
                      values = cols) +
    theme_vilaweb() +
    # theme(legend.position = 'none') +
    the_labs +
    theme(plot.caption = element_text(size = 9),
          legend.box = 'horizontal') +
    geom_text(aes(label = round(p, digits = 1), y = p + 5),
              alpha = 0.6,
              position = position_dodge(width = 0.8)) +
    guides(fill = guide_legend(title.position = 'top', title.hjust = 0.5))
}



simple_indy_plot <- function(ca = FALSE){
  
  levs <- c("Sí", "NS/NC", "No")
  levs_en <- c("Yes", "No\nanswer", "No")
  
  pd <- combined %>%
    filter(date >= '2018-01-01') %>%
    filter(!is.na(indepe)) %>%
    group_by(indepe) %>%
    tally
  
  cols <- RColorBrewer::brewer.pal(n = 3, name = 'Spectral')
  cols <- rev(cols)
  cols[2] <- 'darkgrey'

  
  pd <- pd %>%
    mutate(p = n / sum(n) * 100)
  
  
  
  if(ca){
    the_labs <- labs(x = '',
                     y = 'Percentatge',
                     title = "'Vol que Catalunya esdevingui un Estat independent'",
                     caption = paste0('Gràfic de Joe Brew | @joethebrew | www.vilaweb.cat. Dades del CEO.\n',
                                      self_cite(),
                                      '\nMida de mostra: ',
                                      numberfy(sum(pd$n)), 
                                      ' residents de Catalunya amb ciutadania espanyola, 2018-2019.\n'))
    pd$indepe <- factor(pd$indepe, levels = levs, labels = levs)
  } else {
    the_labs <- labs(x = '',
                     y = 'Percentage',
                     title = "'Do you want Catalonia to become an independent State'",
                     caption = paste0('Chart by Joe Brew | @joethebrew | www.vilaweb.cat. Raw data from the CEO.\n',
                                      self_cite(),
                                      '\nSample size: ',
                                      numberfy(sum(pd$n)), 
                                      ' residents of Catalonia with Spanish citenship, 2018-2019.\n'))
    pd$indepe <- factor(pd$indepe, levels = levs, labels = levs_en)
  }
  
  
  
  ggplot(data = pd,
         aes(x = indepe,
             y = p)) +
    geom_bar(stat = 'identity',
             aes(fill = indepe)) +
    scale_fill_manual(name = '',
                      values = cols) +
    theme_vilaweb() +
    theme(legend.position = 'none') +
    the_labs +
    theme(plot.caption = element_text(size = 9)) +
    geom_text(aes(label = round(p, digits = 1)),
              nudge_y = 5,
              alpha = 0.6)
}



simple_indy_plot_cross <- function(ca = FALSE,
                              cross_var = 'interessat',
                              legend_title = ''){
  
  levs <- c("Sí", "NS/NC", "No")
  levs_en <- c("Yes", "No\nanswer", "No")
  
  var_levs <- c('Molt', 'Bastant', 'Poc', 'Gens', 'No ho sap', 'No contesta')
  var_levs_en <- c('A lot', 'A fair amount', 'Little', 'Not at all', "Don't know", 'No answer')
  
  pd <- combined %>%
    filter(date >= '2018-01-01') %>%
    filter(!is.na(indepe)) %>%
    group_by(.dots = list('indepe', cross_var)) %>%
    tally
  names(pd)[2] <- 'var'
  
  cols <- RColorBrewer::brewer.pal(n = 3, name = 'Spectral')
  cols <- rev(cols)
  cols[2] <- 'darkgrey'

  
  pd <- pd %>%
    group_by(var) %>%
    mutate(p = n / sum(n) * 100)
  
  
  
  if(ca){
    the_labs <- labs(x = '',
                     y = 'Percentatge',
                     title = "'Vol que Catalunya esdevingui un Estat independent'",
                     caption = paste0('Gràfic de Joe Brew | @joethebrew | www.vilaweb.cat. Dades del CEO.\n',
                                      self_cite(),
                                      '\nMida de mostra: ',
                                      numberfy(sum(pd$n)), 
                                      ' residents de Catalunya amb ciutadania espanyola, 2018-2019.\n'))
    pd$indepe <- factor(pd$indepe,
                            levels = levs,
                            labels = levs)
    pd$var <- factor(pd$var,
                     levels = var_levs,
                     labels = var_levs)
  } else {
    the_labs <- labs(x = '',
                     y = 'Percentage',
                     title = "'Do you want Catalonia to become an independent State'",
                     caption = paste0('Chart by Joe Brew | @joethebrew | www.vilaweb.cat. Raw data from the CEO.\n',
                                      self_cite(),
                                      '\nSample size: ',
                                      numberfy(sum(pd$n)), 
                                      ' residents of Catalonia with Spanish citenship, 2018-2019.\n'))
    pd$indepe <- factor(pd$indepe,
                        levels = levs,
                        labels = levs_en)
    pd$var <- factor(pd$var,
                     levels = var_levs,
                     labels = var_levs_en)
  }
  
  pd <- pd %>% filter(!is.na(var),
                      !var %in% c('No ho sap', 'No contesta', "Don't know", 'No answer'))
  ggplot(data = pd,
         aes(x = var,
             y = p,
             group = indepe)) +
    geom_bar(stat = 'identity',
             aes(fill = indepe),
             position = position_dodge(width = 0.8)) +
    scale_fill_manual(name = legend_title,
                      values = cols) +
    theme_vilaweb() +
    # theme(legend.position = 'none') +
    the_labs +
    theme(plot.caption = element_text(size = 9),
          legend.box = 'horizontal') +
    geom_text(aes(label = round(p, digits = 1), y = p + 5),
              alpha = 0.6,
              position = position_dodge(width = 0.8)) +
    guides(fill = guide_legend(title.position = 'top', title.hjust = 0.5))
}

