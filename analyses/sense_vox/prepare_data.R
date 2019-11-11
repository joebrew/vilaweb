# Libraries
library(vilaweb)
library(tidyverse)
library(lubridate)

real <- tibble(partit = c('PSOE',
                          'PP', 
                          'Vox',
                          'Podemos',
                          'ERC',
                          'Cs',
                          'JxCat',
                          'PNV',
                          'Bildu',
                          'Más País',
                          'CUP',
                          'Otros'),
               escanos = c(120, 88, 52, 35, 13, 10, 8, 7, 5, 3,2, 7),
               votos = c(6752983, 
                         5019869,
                         3640063,
                         3097185,
                         869934,
                         1637540,
                         527375,
                         377423,
                         276519,
                         577055,
                         244754,
                         
                         sum(c(226469,
                               123981,
                         119597, 98448, 6850, 34306, 27016, 19696,
                         18206,
                         14023, 13954, 13828, 12622, 10198, 9664,
                         8925, 5952, 5816, 5399, 5290, 3241,
                         3195, 2822, 2398, 2347, 2316, 2303, 2015, 1980, 1451, 1386, 1317, 1159, 1064, 1063, 897, 866, 814, 658, 623, 608, 515, 514, 431, 269, 237, 229, 210, 144, 64, 31))),
               cat_escons = c(12,
                              2, #'PP', 
                              2, #'Vox',
                              7, #'Podemos',
                              13,#'ERC',
                              2, #'Cs',
                              8, #'JxCat',
                              0,#'PNV',
                              0, #'Bildu',
                              0, #'Más País',
                              2,# 'CUP',
                              0),
               cat_vots = c(790582, #'PSOE',
                            286302, #'PP', 
                            243026, #'Vox',
                            546733, #'Podemos',
                            869934, #'ERC',
                            216373, #'Cs',
                            527375, #'JxCat',
                            0, #'PNV',
                            0, #'Bildu',
                            41703, #'Más País',
                            244754, #'CUP',
                            sum(c(44389, 41703, 5790, 2822, 2345, 2215, 2135, 1916))),
               euskadi_escons = c(4, #'PSOE',
                                  0, #'PP', 
                                  0, #'Vox',
                                  3, #'Podemos',
                                  0,# 'ERC',
                                  0, #'Cs',
                                  0, #'JxCat',
                                  7, #'PNV',
                                  4, #'Bildu',
                                  0, #'Más País',
                                  0, #'CUP',
                                  0),
               euskadi_vots = c(225905, #'PSOE',
                                103821, #'PP', 
                                28659, #'Vox',
                                181337, #'Podemos',
                                0, #'ERC',
                                13058, #'Cs',
                                0, #'JxCat',
                                377423, #'PNV',
                                220132, #'Bildu',
                                8463, #'Más País',
                                0,# 'CUP',
                                sum(c(6920, 1707, 1256, 969, 559, 269))))

real <- real %>%
  mutate(vots_no_cat = votos - cat_vots,
         escanos_no_cat = escanos - cat_escons,
         escanos_no_euskadi = escanos - euskadi_escons) %>%
  mutate(vots_no_nacio = votos - cat_vots - euskadi_vots,
         escanos_no_nacio = escanos - cat_escons - euskadi_escons) %>%
  mutate(p_vots = votos / sum(votos) * 100,
         p_cat_vots = cat_vots / sum(cat_vots) * 100) %>%
  mutate(p_euskadi_vots = euskadi_vots / sum(euskadi_vots) * 100) %>%
  mutate(p_nacio_vots = (euskadi_vots + cat_vots) / sum(euskadi_vots + cat_vots) * 100)

pd <- real %>%
  group_by(vox = ifelse(partit == 'Vox', 'Vox', 'No Vox')) %>%
  summarise(vots_no_cat = sum(vots_no_cat),
            escanos_no_cat = sum(escanos_no_cat),
            vots_no_nacio = sum(vots_no_nacio),
            escanos_no_nacio = sum(escanos_no_nacio)) %>%
  mutate(p_vots_no_cat = vots_no_cat / sum(vots_no_cat),
         p_escanos_no_cat = escanos_no_cat / sum(escanos_no_cat),
         p_vots_no_nacio = vots_no_nacio / sum(vots_no_nacio),
         p_escanos_no_nacio = escanos_no_nacio / sum(escanos_no_nacio))

pd <- real %>%
  group_by(vox = ifelse(partit %in% c('PSOE', 'Podemos', 'Más País'), 'left', 
                        ifelse(partit %in% c('Cs', 'PP', 'Vox'), 'right', 'other'))) %>%
  summarise(vots_no_cat = sum(vots_no_cat),
            escanos_no_cat = sum(escanos_no_cat),
            vots_no_nacio = sum(vots_no_nacio),
            escanos_no_nacio = sum(escanos_no_nacio)) %>%
  mutate(p_vots_no_cat = vots_no_cat / sum(vots_no_cat),
         p_escanos_no_cat = escanos_no_cat / sum(escanos_no_cat),
         p_vots_no_nacio = vots_no_nacio / sum(vots_no_nacio),
         p_escanos_no_nacio = escanos_no_nacio / sum(escanos_no_nacio))
