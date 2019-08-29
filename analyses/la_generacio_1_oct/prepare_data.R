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


# Plot of independentism among oct 1 generation
oct1_chart <- function(ca = FALSE,
                       add_label = FALSE,
                       label_nudge = 1,
                       group_time = FALSE,
                       cut_time = TRUE,
                       var = cut(combined$age, breaks = seq(0, 100, 10)),
                       data = combined){
  pd <- data
  pd$var <- var
    
  if(group_time){
    pd <- pd %>%
      mutate(timing = ifelse(date >= '2016-10-01' &
                               date <= '2017-10-01',
                             'Before',
                             ifelse(date > '2017-10-02' &
                                      date <= '2018-09-30',
                                    'After',
                                    NA)))
    pd$timing <- factor(pd$timing, levels = c('Before', 'After'))
    # pd <- pd %>% filter(age_group <= 70)
    # pd$age_group <- factor(pd$age_group)
    
    } else {
      if(cut_time){
        pd <- pd %>%
          filter(date >= '2016-10-01',
                 date <= '2018-10-01')
      }
      pd <- pd %>%
      mutate(timing = date)
    }
  
  
  if(ca){
    the_labs <- labs(x = '',
                     y = 'Percentatge a favor de la independència*',
                     caption = "Dades: CEO. Gràfic: Joe Brew. www.vilaweb.cat. *Exclou els que no contesten / no saben\n'Abans': les 3 enquestes fetes durant l'any anterior a l'1-O\n'Després': les 3 enquestes fetes durant l'any posterior a l'1-O")
  } else {
    the_labs <- labs(x = '',
                     y = 'Percentage in favor of independence*',
                     caption = "Data: CEO. Chart: Joe Brew. www.vilaweb.cat. *Excluding those who do not answer or do not know\n'Before': the 3 surveys carried out in the year prior to Oct 1 2017\n'After': the 3 surveys carried out in the year after Oct 1 2017")
  }
  
  pd <- pd %>%
    # mutate(timing = ifelse(date == '2017-06-15',
    #                        'Before',
    #                        ifelse(date == '2017-10-15',
    #                               'After', NA))) %>%
    filter(!is.na(indepe),
           !is.na(timing),
           age <= 90,
           indepe != 'NS/NC') %>%
    # mutate(age_group = round(age, digits = -1)) %>%
    group_by(timing,
             var,
             indepe) %>%
    tally %>%
    ungroup %>%
    group_by(timing,
             var) %>%
    mutate(p = n / sum(n) * 100) %>%
    ungroup %>%
    filter(indepe == 'Sí') %>%
    filter(var != 'NS/NC')
  if(group_time){
    if(ca){
      pd$timing <- factor(pd$timing, levels = c('Before', 'After'), labels = c('Abans de\nl\'1-O', 'Després de\nl\'1-O'))
    } else {
      pd$timing <- factor(pd$timing, levels = c('Before', 'After'), labels = c('Before\nreferendum', 'After\nreferendum'))
      
    }
  }
  
  n_cols <- length(unique(pd$var))
  cols <- colorRampPalette(RColorBrewer::brewer.pal(n = 9, 'Spectral'))(n_cols)
  if(n_cols == 3){
    cols[2] <- 'darkgreen'
  }
  if(n_cols == 5){
    cols[3] <- 'darkgrey'
  }
  
  g <- ggplot(data = pd,
         aes(x = timing,
             y = p,
             color = var,
             group = var)) +
    geom_point() +
    theme(axis.text.x = element_text(angle = 90,
                                     vjust = 0.5,
                                     hjust = 1)) +
    geom_vline(xintercept = as.Date('2017-10-01'),
               alpha = 0.5,
               lty = 2)
    if(group_time){
      g <- g + geom_line()
    } else {
      g <- g + geom_step()
    }

  g <- g +
    theme_vilaweb() +
    the_labs +
    theme(plot.caption = element_text(size = 6),
          axis.title.y = element_text(size = 10)) +
    scale_color_manual(name = '', values = cols) +
    geom_hline(yintercept = 50, lty = 2, alpha = 0.6)
  if(add_label){
    g <- g +geom_text(aes(label = round(p, digits = 1)), nudge_y = label_nudge, show.legend = FALSE)
  }
  return(g)
}
combined$dummy <- TRUE

overall_chart <- function(ca = FALSE, full = FALSE, group_time = F, cut_time = F){
  if(!ca){
    laby <- labs(title = 'Support for independence among Catalans*',
                 subtitle = ' ')
    if(!full){
      laby$subtitle <- 'The year before and the year after the Oct 1 2017 referendum'
    }
  } else {
    laby <- labs(title = 'Independentisme*')
    if(!full){
      laby$subtitle <- "L'any abans i l'any després de l'1-O"
    }
  }
  oct1_chart(var = combined$dummy, group_time = group_time, cut_time = cut_time, ca = ca) + theme(legend.position = 'none') + geom_text(aes(label = round(p, digits = 1)), nudge_y = 1) + laby
}

# oct1_chart(var = combined$provincia, group_time = F)

age_plot <- function(ca = FALSE, full = FALSE, group_time = T, cut_time = T){
  if(ca){
    ages <- c('Jove (18-30)', 'Adult (31-60)', 'Gran (61+)')
    laby <- labs(title = 'Independentisme per edat')
  } else {
    ages <- c('Young (18-30)', 'Adult (31-60)', 'Old (61+)')
    laby <- labs(title = 'Support for independence by age')
  }
  
  xd <- combined
  xd$agey <- ifelse(xd$age <= 30, ages[1], ifelse(xd$age <= 60, ages[2], ages[3]))
  xd$agey <- factor(xd$agey, levels = ages)
  oct1_chart(data = xd, var = xd$agey, group_time = group_time, cut_time = cut_time, ca = ca) +
    laby
}


age_plot2 <- function(ca = FALSE, full = FALSE, group_time = T, cut_time = T, add_label = T, label_nudge = 1){
  if(ca){
    ages <- c('Molt jove (18-22)', 'Altres')
    laby <- labs(title = 'Independentisme per edat')
  } else {
    ages <- c('Very young (18-2)', 'Others')
    laby <- labs(title = 'Support for independence by age')
  }
  
  xd <- combined
  xd$agey <- ifelse(xd$age <= 22, ages[1], ages[2])
  xd$agey <- factor(xd$agey, levels = ages)
  oct1_chart(data = xd, var = xd$agey, group_time = group_time, cut_time = cut_time, ca = ca, add_label = add_label, label_nudge = label_nudge) +
    laby
}

province_plot <- function(ca = FALSE, full = FALSE, group_time = T, cut_time = T, add_label = T, label_nudge = 1.5){
  if(ca){
    laby <- labs(title = 'Independentisme per provincia')
  } else {
    laby <- labs(title = 'Support for independence by province')
  }
  
  xd <- combined
  oct1_chart(data = xd, var = xd$provincia, group_time = group_time, cut_time = cut_time, ca = ca, add_label = add_label, label_nudge = label_nudge) +
    laby
}

avis_plot <- function(ca = FALSE, full = FALSE, group_time = T, cut_time = T, add_label = T, label_nudge = 5){
  if(ca){
    laby <- labs(title = "Independentisme per nombre d'avis\nnascuts a Catalunya")
  } else {
    laby <- labs(title = "Support for independence by number of\ngrandparents born in Catalonia")
  }
  
  xd <- combined %>% mutate(avis = factor(avis))
  oct1_chart(data = xd, var = xd$avis, group_time = group_time, cut_time = cut_time, add_label = add_label, label_nudge = label_nudge, ca = ca) +
    laby
}

language_plot <- function(ca = FALSE, full = FALSE, group_time = T, cut_time = T, add_label = T, label_nudge = 5){
  if(ca){
    laby <- labs(title = "Independentisme per 'llengua propia'")
  } else {
    laby <- labs(title = "Support for independence by 'own language'")
  }
  
  xd <- combined %>% filter(llengua_propia != 'Altres')
  oct1_chart(data = xd, var = xd$llengua_propia, group_time = group_time, cut_time = cut_time, add_label = add_label, label_nudge = label_nudge, ca = ca) +
    laby
}

party_plot <- function(ca = FALSE, full = FALSE, group_time = T, cut_time = T, add_label = F, label_nudge = 1.5){
  if(ca){
    laby <- labs(title = 'Independentisme per partit polític')
  } else {
    laby <- labs(title = 'Support for independence by political party')
  }
  
  xd <- combined
  oct1_chart(data = xd, var = xd$partit, group_time = group_time, cut_time = cut_time, ca = ca, add_label = add_label, label_nudge = label_nudge) +
    laby
}

informat_plot <- function(ca = FALSE, full = FALSE, group_time = T, cut_time = T, add_label = T, label_nudge = 1.5){
  xd <- combined %>% filter(informat != 'No ho sap')
  if(ca){
    laby <- labs(title = "Independentisme per si l'enquestat es considera\ninformat del que passa en política")
  } else {
    laby <- labs(title = 'Support for independence by whether one\nconsiders him/herself informed about\nwhat is happening in politics')
    xd$informat <- factor(xd$informat, levels = c('Molt', 'Bastant', 'Poc', 'Gens'),
                          labels = c('Very much', 'A good deal', 'A little', 'Not at all'))
  }
  
  
  oct1_chart(data = xd, var = xd$informat, group_time = group_time, cut_time = cut_time, ca = ca, add_label = add_label, label_nudge = label_nudge) +
    laby
}

interessat_plot <- function(ca = FALSE, full = FALSE, group_time = T, cut_time = T, add_label = T, label_nudge = 1.5){
  xd <- combined
  if(ca){
    laby <- labs(title = "Independentisme per si l'enquestat es considera\ninteressat en el que passa en política")
  } else {
    laby <- labs(title = 'Support for independence by whether one\nconsiders him/herself interested in\nwhat is happening in politics')
    xd$informat <- factor(xd$informat, levels = c('Molt', 'Bastant', 'Poc', 'Gens'),
                          labels = c('Very much', 'A good deal', 'A little', 'Not at all'))
  }
  
  xd <- xd %>% filter(!interessat %in% c('No ho sap', 'No contesta'))
  oct1_chart(data = xd, var = xd$interessat, group_time = group_time, cut_time = cut_time, ca = ca, add_label = add_label, label_nudge = label_nudge) +
    laby
}


axis_plot <- function(ca = FALSE, full = FALSE, group_time = T, cut_time = T, add_label = T, label_nudge = 1.5){
  xd <- combined
  xd$axis <- as.character(xd$axis)
  xd$axis <- ifelse(xd$axis == 'Extrema esquerra', '0',
                    ifelse(xd$axis == 'Extrema dreta', '10', xd$axis))
  xd$axis <- as.numeric(xd$axis)
  
  if(ca){
    axes <- c('extrema\nesquerra', 'esquerra', 'centre', 'dreta', 'extrema\ndreta')
    laby <- labs(title = "Independentisme per ideologia esquerra-dreta")
  } else {
    axes <- c('far left', 'left', 'center', 'right', 'far right')
    laby <- labs(title = 'Support for independence by left-right ideology')
  }
  xd$axis <- ifelse(xd$axis %in% 0:1, axes[1],
                    ifelse(xd$axis <= 4, axes[2],
                           ifelse(xd$axis == 5, axes[3],
                                  ifelse(xd$axis <= 8, axes[4], 
                                         ifelse(xd$axis <= 10, axes[5], NA)))))
  xd$axis <- factor(xd$axis, levels = axes)
  
  xd <- xd %>% filter(!is.na(axis))
  oct1_chart(data = xd, var = xd$axis, group_time = group_time, cut_time = cut_time, ca = ca, add_label = add_label, label_nudge = label_nudge) +
    laby
}


neixer_plot <- function(ca = FALSE, full = FALSE, group_time = T, cut_time = T, add_label = T, label_nudge = 5){
  if(ca){
    laby <- labs(title = "Independentisme per lloc de naixement")
  } else {
    laby <- labs(title = "Support for independence by place of birth")
  }
  
  xd <- combined 
  oct1_chart(data = xd, var = xd$neixer, group_time = group_time, cut_time = cut_time, add_label = add_label, label_nudge = label_nudge, ca = ca) +
    laby
}


