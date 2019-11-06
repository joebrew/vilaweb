# Libraries
library(vilaweb)
library(tidyverse)
library(lubridate)

# Get Sanchez twitter data
tl <- get_twitter_data_from_database('miqueliceta')
tl <- tl %>% filter(!duplicated(id))
# tl$dialeg <- grepl('reformar la const|reforma consti', tolower(tl$tweet))
tl$dialeg <- grepl('plurinacional', tolower(tl$tweet))


# Get frequency
pd <- tl %>%
  group_by(date = as.Date(cut(date, 'year'))) %>%
  summarise(numerator = length(which(dialeg)),
            denominator = n()) %>%
  ungroup %>%
  mutate(p = numerator / denominator * 100)
tail(pd)



# Get iceta twitter data
tl <- get_twitter_data_from_database(c('sanchezcastejon'))
tl <- tl %>% filter(!duplicated(id))
# tl$dialeg <- grepl('reformar la const|reforma consti|federalis', tolower(tl$tweet))
tl$dialeg <- grepl('rey', tolower(tl$tweet))

# Get frequency
pd <- tl %>%
  group_by(date = as.Date(cut(date, 'year')), username) %>%
  summarise(numerator = length(which(dialeg)),
            denominator = n()) %>%
  ungroup %>%
  mutate(p = numerator / denominator * 100)
View(pd %>% arrange(username))


# Get iceta twitter data
tl <- get_twitter_data_from_database(c('miqueliceta'))
tl <- tl %>% filter(!duplicated(id))
tl$dialeg <- grepl('reforma', tolower(tl$tweet))

# tl$dialeg <- grepl('rey', tolower(tl$tweet))

# Get frequency
pd <- tl %>%
  group_by(date = as.Date(cut(date, 'year')), username) %>%
  summarise(numerator = length(which(dialeg)),
            denominator = n()) %>%
  ungroup %>%
  mutate(p = numerator / denominator * 100)
View(pd %>% arrange(username))
View(tl %>% filter(dialeg))


# Get twitter data
tl <- get_twitter_data_from_database(c('josepborrellf', 'psoe', 'sanchezcastejon', 'miqueliceta', 'socialistes_cat', 'eva_granados', 'j_zaragoza_', 'lozanoirene', 'albertrivera', 'psoe', 'sanchezcastejon', 'santi_abascal', 'pablocasado_', 'vox_es', 'populares', 'ciudadanoscs'))
tl <- tl %>% filter(!duplicated(id))
# tl$dialeg <- grepl('reformar la const|reforma consti', tolower(tl$tweet))
tl$dialeg <- grepl("diálogo|dialogar", tolower(tl$tweet))

# tl$dialeg <- grepl('rey', tolower(tl$tweet))

# Get frequency
pd <- tl %>%
  group_by(date = as.Date(cut(date, 'year')), username) %>%
  summarise(numerator = length(which(dialeg)),
            denominator = n()) %>%
  ungroup %>%
  mutate(p = numerator / denominator * 100)
View(pd %>% arrange(username))

View(tl %>% filter(dialeg, date >='2019-10-14'))



# Get twitter data
tl <- get_twitter_data_from_database(c('pablo_iglesias_', 'adacolau', 'irene_montero_', 'ahorapodemos', 'encomu_podem', 'jaumeasens', 'monederojc', 'pnique', 'g_pisarello', 'ierrejon'))
tl <- tl %>% filter(!duplicated(id))
tl$dialeg <- grepl('cataluña|catalunya', tolower(tl$tweet))
# tl$dialeg <- grepl('reformar la const|reforma consti|reforma de la consti', tolower(tl$tweet))
# tl$dialeg <- grepl('presos políticos|presos polítics|presospolíticos|presospolítics|preses polítiques|preses políticas', tolower(tl$tweet))

# tl$dialeg <- grepl('federalis', tolower(tl$tweet))

# tl$dialeg <- grepl('rey', tolower(tl$tweet))

# Get frequency
pd <- tl %>%
  group_by(date = as.Date(cut(date, 'year')), username) %>%
  summarise(numerator = length(which(dialeg)),
            denominator = n()) %>%
  ungroup %>%
  mutate(p = numerator / denominator * 100)
View(pd %>% arrange(username))



