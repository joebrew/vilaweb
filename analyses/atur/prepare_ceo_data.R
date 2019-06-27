# Libraries
library(vilaweb)
library(tidyverse)
require(readr)

# Read in Apr 2019 CEO data
df <- haven::read_sav('../../data-raw/ceo/Microdades anonimitzades -919.sav') %>%
  mutate(LLOC_NAIX = as.character(LLOC_NAIX)) %>%
  mutate(LLOC_NAIX = ifelse(LLOC_NAIX == '1', 
                            'Catalunya',
                            ifelse(LLOC_NAIX == '2',
                                   'Altres comunitats autònomes',
                                   ifelse(LLOC_NAIX == '3', 'Unió Europea',
                                          ifelse(LLOC_NAIX == '4', 'Resta del món',
                                                 NA))))) %>%
  mutate(date = '2019-03-01') %>%
  dplyr::select(date,
                P34, #indepe
                P26J, # rei
                P33, # preferencia de forma destat
                P26A, # tribunals
                P28, # partit
                contains('P26'),
                EDAT,
                EDAT_GR,
                P29,
                SEXE,
                LLOC_NAIX,
                P69,
                P78) # ingressos
df <- as_factor(df)

# Read in prior ceo data and conform it to new format
ceo <- vilaweb::ceo %>%
  mutate(franquisme = P102) %>%
  mutate(atur = C401A) %>% # atur, does not exist in new one
  mutate(C78 = C900) %>% # ingressos
  mutate(P33 = P30) %>% #format destat
  mutate(P34 = P31) %>% #indepe
  mutate(P26J = P21J) %>% #monarca
  mutate(P26A = P21A) %>% #tribunals
  mutate(P28 = P24R) %>% # partit
  mutate(EDAT_GR = GR_EDAT) %>% # grouped age
  mutate(P29 = P25) %>% # axis
  mutate(P69 = C704) %>% # llengua propia
  mutate(LLOC_NAIX = C100) %>% #lloc de naixmenet
  mutate(date = paste0(ANY, '-', MES, '-01')) %>%
  dplyr::select(date,
                atur,
                franquisme,
                P34, #indepe
                P26J, # rei
                P26A, # tribunals
                P28, # partit
                P33, # forma destat
                P26B = P21B, # partits politics
                       P26C = P21C, # el seu ajuntament
                       P26D = P21D, # govern espanyol
                       P26E = P21E, # els sindicats
                       P26F = P21F, # govern de la generalitat
                       P26G = P21G, # congres dels diputats
                       P26H = P21H, # parlament de catalunya
                       P26I = P21I, # la UE
                       P26K = P21K, # l'exercit
                       P26L = P21L, # PN i GC
                       P26M = P21M, # mossos d'esquadra
                       P26N = P21N, # esglesia catolica
                       P26O = P21O, # l ONU
                       P26P = P21P, # la banca
                       P26Q = P21Q, # mitjans de comunicacio
                       P26R = P21R, # tribunal constitucional
                       P26S = P21S, # universitats
                       P26T = P21T, # parlament europeu
                EDAT,
                EDAT_GR,
                P29,
                SEXE,
                LLOC_NAIX,
                P69)

# Combine the two
df <- bind_rows(ceo, df)

make_number <- function(x){
  x <- as.character(x)
  x <- ifelse(x == 'Cap confiança', '0',
              ifelse(x == 'Molta confiança', '10', as.character(x)))
  x <- as.numeric(x)
}
the_columns <- names(df)[grepl('P26', names(df))]
for(j in 1:length(the_columns)){
  this_column <- the_columns[j]
  df[,this_column] <- make_number(unlist(df[,this_column]))
}



# df <- vilaweb::ceo
df <- df %>%
  mutate(axis = as.character(P29)) %>%
  mutate(axis = ifelse(axis == 'Extrema esquerra', '0',
                       ifelse(axis == 'Extrema dreta', '10',
                              axis))) %>%
  mutate(axis = as.numeric(axis)) %>%
  dplyr::rename(llengua = P69) %>%
  mutate(llengua = as_factor(llengua)) %>%
  # mutate(axis = ifelse(P29 %in% 98:99,
  #                      NA, P29)) %>%
  mutate(monarquia = P26J) %>%
  mutate(SEXE = as_factor(SEXE)) %>%
  mutate(axis_simple = ifelse(axis <= 2,
                              'Extrema esquerra',
                              ifelse(axis <= 4,
                                     'Esquerra',
                                     ifelse(axis == 5,
                                            'Centre',
                                            ifelse(axis <= 7,
                                                   'Dreta',
                                                   ifelse(axis<= 10, 'Extrema dreta', axis))))))
df$axis_simple <- factor(df$axis_simple,
                         levels = c('Extrema esquerra' , 'Esquerra', 'Centre', 'Dreta', 'Extrema dreta'))
df$estat <- as.character(df$P33)
df$estat <- ifelse(df$estat %in% c('No ho sap', 'No contesta'),
                   'NS/NC',
                   df$estat)
#   mutate(axis_simple = ifelse(axis <=3, 
#                               'Esquerra',
#                               ifelse(axis <= 6,
#                                      'Centre',
#                                      'Dreta')))
# df$axis_simple <- factor(df$axis_simple,
#                          levels = c('Esquerra', 'Centre', 'Dreta'))


df$monarquia <- make_number(df$monarquia)
df$tribunals <- make_number(df$P26A)

llengua_dict <- tibble(
  llengua = c("Català (valencià / balear)",
              "Castellà",
              "Totes dues igual: català (valencià / balear) i castellà",
              "Aranès",
              "Altres llengües o altres combinacions",
              "No ho sap",
              "No contesta"),
  language = c('Català\n(o aranès)', 'Espanyol', 'Català i Espanyol', "Català\n(o aranès)", 'Altres', NA, NA))
llengua_dict$language <- factor(llengua_dict$language,
                                levels = c('Català\n(o aranès)',  'Català i Espanyol', 'Altres',  'Espanyol'))

df <- left_join(df, llengua_dict)
df <- df %>% dplyr::select(-llengua) %>%
  dplyr::rename(llengua = language)
df$llengua <- factor(df$llengua,
                     levels = c('Català\n(o aranès)',  'Català i Espanyol', 'Altres',  'Espanyol'))
# P34 is the indepenence question
# df$indy <- df$P34
indy_dict <- tibble(P34 = c('Sí', 'No', "No ho sap",
                            "No contesta"),
                    indy = c('Independentistes',
                             'Unionistes',
                             'NS/NC',
                             'NS/NC'))
df <- left_join(df, indy_dict)


# P38_R is party question
# party_dict <- tibble(P28 = c(1, 3, 4, 6, 10, 12, 20, 21, 22, 80, 93, 96, 98),
#                      partit = c('PPC','ERC', 'PSC', "C's", 'CUP', "Podemos", "PDeCAT", 'JxCat',
#                                 'CECP', rep('Altre/NS/NC', 4)))
# df <- left_join(df, party_dict)
df$P28 <- as.character(df$P28)
df$partit <-
  ifelse(df$P28 %in% c('Altres partits', 'Cap',
                       'ICV-EUiA', 'No contesta',
                       'No ho sap', 'PACMA'),
         NA,
         ifelse(df$P28 %in% c('Junts per Catalunya',
                              'Junts pel Sí',
                              'PDeCat', 'PDeCAT',
                              'CDC'),
                'PDeCat/Junts',
                ifelse(df$P28 %in% c('Catalunya en Comú Podem',
                                     'Catalunya sí que es pot',
                                     'Podemos'),
                       'Comuns',
                       df$P28)))

df$date <- as.Date(df$date)
df$date <- as.character(df$date)
df <- df %>%
  mutate(date = ifelse(date == '2014-03-01', '2014-04-01',
                       ifelse(date == '2014-10-01', '2014-11-01',
                              ifelse(date == '2015-03-01', '2015-02-01',
                                     ifelse(date == '2017-06-01',
                                            '2017-07-01',
                                            ifelse(date == '2018-06-01',
                                                   '2018-07-01',
                                                   ifelse(date == '2018-11-01', '2018-10-01', date)))))))
df$date <- as.Date(df$date)

numberfy <- function(x){
  gsub(',', '.', scales::comma(x), fixed = TRUE)
}

