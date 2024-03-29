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
    vals <- ifelse(vals %in% c(98:99), NA, vals)
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
    vals <- ifelse(vals %in% c('98', '99'),
                   NA, vals)
    vals <- ifelse(vals == '1', 
                   'Coneix', 
                   ifelse(vals == '2',
                          'No coneix',
                          vals))
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
    dplyr::select(
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

coneixement_plot <- function(ca = TRUE,                           who = NULL){
  
  # Define the dataframe
  pd <- combined
  # Keep only the relevant columns
  keeps <- c('date',
             names(pd)[grepl('Coneixement: ', names(pd))])
  pd <- pd[,keeps]
  # Convert the column names
  for(j in 2:ncol(pd)){
    names(pd)[j] <- substr(names(pd)[j], 14, nchar(names(pd)[j]))
  }
  # Keep only the relevant people
  if(is.null(who)){
    who <- sort(unique(names(pd)[2:length(names(pd))]))
  }
  keeps <- c('date',
             who)
  pd <- pd[,keeps]
  
  # Make long
  long <- pd %>%
    gather(key, value,
           keeps[2]:keeps[length(keeps)])
  
  # Group by date, person, and calculate summary statistics
  pd <- long %>%
    group_by(date, key) %>%
    summarise(responded = length(which(!is.na(value))),
              queried = n(),
              coneix = length(which(value == 'Coneix')),
              no_coneix = length(which(value == 'No coneix'))) %>%
    mutate(p_coneix = coneix / responded * 100)
  
  # Plot
  ggplot(data = pd,
         aes(x = date,
             y = p_coneix,
             group = key)) +
    geom_line() +
    # geom_point() +
    facet_wrap(~key) +
    theme_vilaweb() +
    ylim(0, 100) +
    theme(axis.text.x = element_text(angle = 90,
                                     vjust = 0.5,
                                     hjust = 1),
          strip.text = element_text(size = 7))
}

valoracio_plot <- function(ca = FALSE,
                           who = NULL){
  
  # Define the dataframe
  pd <- combined
  # Keep only the relevant columns
  keeps <- c('date',
             names(pd)[grepl('Valoració: ', names(pd))])
  pd <- pd[,keeps]
  # Convert the column names
  for(j in 2:ncol(pd)){
    names(pd)[j] <- substr(names(pd)[j], 12, nchar(names(pd)[j]))
  }
  # Keep only the relevant people
  if(is.null(who)){
    who <- sort(unique(names(pd)[2:length(names(pd))]))
  }
  keeps <- c('date',
             who)
  pd <- pd[,keeps]
  
  # Make long
  long <- pd %>%
    gather(key, value,
                  keeps[2]:keeps[length(keeps)])
  
  # Group by date, person, and calculate summary statistics
  pd <- long %>%
    group_by(date, key) %>%
    summarise(responded = length(which(!is.na(value))),
              queried = n(),
              avg = mean(value, na.rm = TRUE),
              p50 = quantile(value, 0.5, na.rm = TRUE),
              p75 = quantile(value, 0.75, na.rm = TRUE),
              p25 = quantile(value, 0.25, na.rm = TRUE))
  
  # Plot
  ggplot(data = pd,
         aes(x = date,
             y = avg)) +
    geom_bar(stat = 'identity') +
    # geom_line() +
    # geom_point() +
    facet_wrap(~key) +
    theme_vilaweb() +
    ylim(0, 10) +
    theme(axis.text.x = element_text(angle = 90,
                                     vjust = 0.5,
                                     hjust = 1),
          strip.text = eelement_text(size = 6))
  
}

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

party_plot <- function(ca = FALSE,
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
    filter(!is.na(referendum)) %>%
    group_by(referendum, partit) %>%
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
    group_by(partit) %>%
    mutate(p = n / sum(n) * 100)
  
  
  if(ca){
    the_labs <- labs(x = '',
                     y = 'Percentatge',
                     title = "'Catalunya té dret a celebrar\nun referèndum d'autodeterminació'",
                     subtitle = "Grau d'acord amb la frase, per partit",
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
                     subtitle = 'Extent of agreement with phrase, by party',
                     caption = paste0('Chart by Joe Brew | @joethebrew | www.vilaweb.cat. Raw data from the CEO.\n',
                                      'Actual phrase varied by questionnaire date, full details at:\n',
                                      self_cite(),
                                      '\nSample size: ',
                                      numberfy(sum(pd$n)), 
                                      ' residents of Catalonia with Spanish citenship, 2018-2019.\n'))
    pd$referendum <- factor(pd$referendum,
                            levels = levs,
                            labels = levs_en)
    pd$partit <- gsub('Cap o altre partit', 'Other or no party', pd$partit)
  }
  
  g <- ggplot(data = pd,
              aes(x = referendum,
                  y = p)) +
    geom_bar(stat = 'identity',
             aes(fill = referendum)) +
    facet_wrap(~partit, ncol = 4, scales = 'free_x') +
    scale_fill_manual(name = '',
                      values = cols) +
    theme_vilaweb() +
    the_labs +
    theme(plot.caption = element_text(size = 9)) +
    geom_text(aes(label = round(p, digits = 1)),
              nudge_y = 5,
              size = 2.7,
              alpha = 0.6) 
  
  if(keep_simple){
    g <- g +
      theme(axis.text.x = element_text(size = 0))
  } else {
    g <- g +
      theme(legend.position = 'none') +
      theme(axis.text.x = element_text(angle = 90,
                                       hjust = 1,
                                       vjust = 0.5,
                                       size = 6))
  }
  return(g)
  
  
}

time_plot <- function(ca = FALSE){
  pd <- combined %>%
    filter(!is.na(referendum)) %>%
    filter(!referendum %in% c("Ni d'acord ni en desacord",
                              "No ho sap",
                              "No contesta")) %>%
    mutate(referendum = ifelse(grepl("d'acord|D'acord", referendum), "D'acord", "Desacord")) %>%
    group_by(date, referendum) %>%
    tally %>%
    mutate(p = n / sum(n) * 100)
  ggplot(data = pd,
         aes(x = date,
             y = p)) +
    geom_bar(stat = 'identity',
             aes(fill = referendum))
}



d21 <- function(ca = FALSE){
  require(vilaweb)
  if(ca){
    facs <- c('Favorable a\ninvestir Puigdemont',
              'No favorable a\ninvestir Puigdemont',
              'Cap posició explícita\nsobre Puigdemont')
    seats <- 'escons'
    votes <- 'vots'
    the_labs <- labs(x = '',
                     y = 'Diputats',
                     title = 'Eleccions catalanes de 2017')
  } else {
    facs <- c('In favor of Puigdemont\nas President',
              'Not in favor of Puigdemont\nas President',
              'No explicit position\non Puigdemont')
    seats <- 'seats'
    votes <- 'votes'
    the_labs <- labs(x = '',
                     y = 'MPs',
                     title = '2017 Catalan elections')
  }
  pd <- tibble(partit = c("Cs", "JxCat", "ERC", "PSC", "Comuns", "CUP", "PP", "PACMA", "Verds", "PU M+J"),
               vots = c(1109732, 948233, 935861, 606659, 326360, 195246, 185670, 38743, 10287, 577),
               escons = c(36, 34, 32, 17, 8, 4, 4, 0, 0, 0),
               favorable = c(F, T, T, F, F, T, F, NA, F, NA),
               pa = c(T, F, F, F, F, F, T, F, F, F)) %>%
    arrange(escons) %>%
    group_by(favorable) %>%
    mutate(total = sum(escons),
           total_vots = sum(vots)) %>%
    ungroup %>%
    mutate(fac = ifelse(is.na(favorable), facs[3], ifelse(favorable, facs[1], facs[2]))) %>%
    filter(!is.na(fac)) %>%
    mutate(fac = paste0(fac, ':\n', numberfy(total_vots), ' ', votes))
  pd$partit <- factor(pd$partit, levels = pd$partit)
  # pd <- pd %>% filter(escons > 0)
  cols <- c('#FFFFFF', '#2C913D', '#ABBB92', 'black', '#5DBCD2', 'purple', '#C00F19', '#9E9F97', '#5B3217', '#DF8547')
  
  
  ggplot(data = pd,
         aes(x = partit,
             y = vots,
             fill = partit)) +
    facet_wrap(~fac, scales = 'free_x') +
    geom_bar(stat = 'identity') +
    theme_vilaweb() +
    geom_text(aes(label = numberfy(vots)),
              color = 'black',
              nudge_y = 50000) +
    the_labs +
    scale_fill_manual(name = '',
                      values = cols) 
}

d21p <- function(ca = FALSE){
  require(vilaweb)
  if(ca){
    facs <- c('Favorable a investir Puigdemont',
              'No favorable a investir Puigdemont',
              'Favorable a investir Arrimadas')
    seats <- 'escons'
    the_labs <- labs(x = '',
                     y = 'Diputats',
                     title = 'Eleccions catalanes de 2017: suport pels candidats presidencials entre diputats elegits',
                     subtitle = '135 diputats en total. Majoria absoluta = 68 vots')
  } else {
    facs <- c('In favor of Puigdemont as President',
              'Not in favor of Puigdemont as President',
              'In favor of Arrimadas as President')
    seats <- 'seats'
    the_labs <- labs(x = '',
                     y = 'MPs',
                     title = '2017 Catalan elections: support for Presidential candidates among elected MPs',
                     subtitle = '135 total MPs. Absolute majority = 68 votes')
  }
  cols <- c('black', '#5DBCD2', '#C00F19', 'purple', '#DF8547')
  pd <- tibble(partit = c("Cs", "JxCat", "ERC", "PSC", "Comuns", "CUP", "PP", "PACMA", "Verds", "PU M+J"),
               vots = c(1109732, 948233, 935861, 606659, 326360, 195246, 185670, 38743, 10287, 577),
               escons = c(36, 34, 32, 17, 8, 4, 4, 0, 0, 0),
               favorable = c(F, T, T, F, F, T, F, F, F, F),
               pa = c(T, F, F, F, F, F, T, F, F, F)) %>%
    arrange(escons) %>%
    group_by(favorable) %>%
    mutate(total = sum(escons)) %>%
    ungroup %>%
    mutate(fac = ifelse(favorable, facs[1], 
                        ifelse(pa, facs[3], NA))) %>%
    filter(!is.na(fac)) %>%
    mutate(fac = paste0(fac, ':\n', total, ' ', seats))
  pd$partit <- factor(pd$partit, levels = pd$partit)
  ggplot(data = pd %>% filter(escons > 0),
         aes(x = fac,
             y = escons,
             fill = partit)) +
    geom_bar(stat = 'identity',
             position = position_stack()) +
    theme_vilaweb() +
    geom_text(aes(label = escons),
              color = 'white',
              # nudge_y = -2,
              position = position_stack(vjust = 0.5)) +
    the_labs +
    scale_fill_manual(name = '',
                      values = cols) +
    facet_wrap(~fac, scales = 'free_x') +
    theme(axis.text.x = element_text(size = 0))
}

europees <- function(ca = FALSE){
  pd <- tibble(partit = c('JxCat', 'PSC', 'ERC', 'Cs', 'Podem', 'PP', 'VOX', 'PACMA', 'CV-EC',
                          'Recortes\nCero', 'Pirates', 'Altres'),
               vots = c(987149, 766107, 733401, 298781, 292088, 178950, 68824, 48733, 11713, 6822, 4937, 42858))
  pd$partit <- factor(pd$partit, levels = pd$partit)
  if(ca){
    the_labs <- labs(title = 'Resultats 2019: elecciones europees a Catalunya',
                     x = 'Partit',
                     y = 'Vots')
  } else {
    the_labs <- labs(title = '2019 results: European elections in Catalonia',
                     x = 'Party',
                     y = 'Votes')
  }
  ggplot(data = pd,
         aes(x = partit,
             y = vots)) +
    geom_point() +
    geom_segment(aes(xend = partit,
                     yend = 0)) +
    theme_vilaweb() +
    the_labs +
    geom_text(aes(label = numberfy(vots)),
              nudge_y = 50000,
              alpha = 0.7)
}

twitter <- function(ca = FALSE){
  pd <-
    bind_rows(
      puigdemont_bcn %>% mutate(who = 'Puigdemont'),
      sanchez_bcn %>% mutate(who = 'Sánchez')
    ) %>%
    mutate(date = as.Date(created_at)) %>%
    filter(date != max(date)) %>%
    group_by(who, date) %>%
    summarise(n = n(),
              rt = sum(retweet_count),
              likes = sum(favorite_count)) %>%
    mutate(interactions = n + rt + likes)
  ggplot(data = pd,
         aes(x = date,
             y = interactions,
             color = who)) +
    geom_line()
}


