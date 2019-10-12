# Libraries
library(vilaweb)
library(tidyverse)
library(lubridate)

# Read in the 2019 ceo omnibus
df <- vilaweb::ceo_omnibus_2019

# Define party
df <- df %>%
  mutate(partit = `c3. I em podria dir a quin partit o coalició va votar en les darreres eleccions al Parlament de Catalunya del 21 de desembre de 2017?`)

# Define indepe and sobiranisme
df <- df %>%
  mutate(sobiranista = `15a. M’agradaria que m’indiqués el seu grau d’acord o desacord amb: S’hauria de fer un referèndum a Catalunya perquè els catalans i les catalanes decidissin quina relació volen que hi hagi entre Catalunya i Espanya`)



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

# Get most recent CEO data
ceo_june_2019 <- vilaweb::ceo_june_2019

# Get age range
get_details <- function(df){
  df %>%
    mutate(referendum = `Fins a quin punt està d’acord o en desacord amb cadascuna de les següents afirmacions: Catalunya té el dret de celebrar un referèndum d'autodeterminació`) %>%
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
  
  df %>%
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
                          ))) %>%
    mutate(partit = as.character(partit)) %>%
    mutate(partit = ifelse(partit %in% c('ERC', 'PSC', 'CUP',
                                         "PPC"),
                           partit,
                           ifelse(partit %in% c('Podemos','En Comú Podem', 'Catalunya en Comú Podem', 'Barcelona en Comú', 'Catalunya sí que es pot'), 'Podem',
                                  ifelse(partit == "C's", "Cs",
                                         ifelse(partit %in% c('CiU', 'Junts pel Sí', 'CDC', 'PDeCAT', 'Junts per Catalunya'), 'JxCat/PDeCat', 'Cap o altre partit')))))
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

pd <- combined %>%
  filter(!is.na(indepe)) %>%
  # mutate(year = as.numeric(format(date, '%Y'))) %>%
  group_by(year = date, partit, indepe) %>%
  tally %>%
  group_by(year, partit) %>%
  mutate(p = n / sum(n) * 100)

ggplot(data = pd,
       aes(x = year,
           y = p,
           color = indepe)) +
  geom_line() +
  facet_wrap(~partit) +
  geom_text(aes(label = round(p, digits = 1)),
            nudge_y = 5,
            size = 1.6)

pd <- combined %>%
  filter(!is.na(indepe)) %>%
  mutate(year = as.numeric(format(date, '%Y'))) %>%
  filter(year >= 2018) %>%
  group_by(partit, indepe) %>%
  tally %>%
  group_by(partit) %>%
  mutate(p = n / sum(n) * 100) %>%
  arrange(p) %>%
  group_by(partit) %>%
  summarise(x = paste0('No: ', round(p[indepe == 'No'], digits = 2),
                       ' | Sí: ', round(p[indepe == 'Sí'], digits = 2),
                       ' | NS/NC: ', round(p[indepe == 'NS/NC'], digits = 2)),
            y = round(p[indepe == 'No'], digits = 2))

pd <- pd %>%
  arrange(y) %>%
  dplyr::select(partit, x)

z <- paste0(pd$partit, ':: ', pd$x, collapse = NULL)
cat(paste0(z, collapse = '\n'))


pd <- combined %>%
  filter(!is.na(indepe)) %>%
  filter(indepe != 'NS/NC') %>%
  mutate(year = as.numeric(format(date, '%Y'))) %>%
  filter(year >= 2018) %>%
  group_by(indepe) %>%
  tally %>%
  mutate(p = n / sum(n) * 100) %>%
  arrange(p) %>%
  summarise(x = paste0('No: ', round(p[indepe == 'No'], digits = 2),
                       ' | Sí: ', round(p[indepe == 'Sí'], digits = 2),
                       ' | NS/NC: ', round(p[indepe == 'NS/NC'], digits = 2)),
            y = round(p[indepe == 'No'], digits = 2))

