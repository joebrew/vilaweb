# Libraries
library(vilaweb)
library(tidyverse)
library(lubridate)

# Read in the mid-2019 omnibus
omnibus <- vilaweb::ceo_omnibus_2019

# Redefine some variables
omnibus <- omnibus %>%
  mutate(sobiranisme = `15a. M’agradaria que m’indiqués el seu grau d’acord o desacord amb: S’hauria de fer un referèndum a Catalunya perquè els catalans i les catalanes decidissin quina relació volen que hi hagi entre Catalunya i Espanya`) %>%
  mutate(sobiranisme = as.character(sobiranisme)) %>%
  mutate(sobiranisme = ifelse(sobiranisme %in% c("Ni d'acord ni en desacord", "No ho sap", "No contesta"),
                              "NS/NC/Ni-ni", sobiranisme)) %>%
  mutate(sobiranisme_senzill = ifelse(sobiranisme %in% c("Molt d'acord", "Més aviat d'acord"),
                                      "Sobiranista",
                                      ifelse(sobiranisme %in% c("Més aviat en desacord", "Molt en desacord"),
                                             "No sobiranista",
                                             sobiranisme))) %>%
  dplyr::select(sobiranisme,
                neixer = `Em podria dir on va néixer?`,
                any_arribar = `c6b. I a quin any va arribar a Catalunya per quedar-se?`,
                sobiranisme_senzill,
                age = Edat,
                axis = `c1. Vostè com es defineix d'extrema esquerra, esquerra, centre-esquerra, centre, centre-dreta, dreta o extrema dreta?`,
         age_group = `Grups d'edat`,
         age_group_rec = `Grups d'edat reagrupada (recodificació grups de 5 anys)`,
         neixer = `Em podria dir on va néixer?`,
         situacio_laboral = `En quina de les següents situacions laborals es troba vostè actualment?`)

pd <- omnibus %>%
  group_by(age_group= age_group_rec, sobiranisme_senzill) %>%
  tally %>%
  group_by(age_group) %>%
  mutate(p = n / sum(n) * 100)

ggplot(data = pd,
       aes(x = age_group,
           y = p,
           color = sobiranisme_senzill,
           group = sobiranisme_senzill)) +
  geom_point(aes(size = n)) +
  geom_line()


pd <- omnibus %>%
  filter(!axis %in% c('No ho sap', 'No contesta')) %>%
  group_by(axis, sobiranisme_senzill) %>%
  tally %>%
  group_by(axis) %>%
  mutate(p = n / sum(n) * 100)

ggplot(data = pd,
       aes(x = axis,
           y = p,
           color = sobiranisme_senzill,
           group = sobiranisme_senzill)) +
  geom_point(aes(size = n)) +
  geom_line()


pd <- omnibus %>%
  filter(!any_arribar %in% c('No ho sap', 'No contesta'),
         !is.na(any_arribar)) %>%
  mutate(any_arribar = as.numeric(as.character(any_arribar))) %>%
  mutate(any_arribar = cut(any_arribar, 10)) %>%
  group_by(any_arribar, sobiranisme_senzill) %>%
  tally %>%
  group_by(any_arribar) %>%
  mutate(p = n / sum(n) * 100)

ggplot(data = pd,
       aes(x = any_arribar,
           y = p,
           color = sobiranisme_senzill,
           group = sobiranisme_senzill)) +
  geom_point(aes(size = n)) +
  geom_line()



pd <- omnibus %>%
  filter(!neixer %in% c('No ho sap', 'No contesta'),
         !is.na(neixer)) %>%
  group_by(neixer, sobiranisme_senzill) %>%
  tally %>%
  group_by(neixer) %>%
  mutate(p = n / sum(n) * 100)

ggplot(data = pd,
       aes(x = neixer,
           y = p,
           color = sobiranisme_senzill,
           group = sobiranisme_senzill)) +
  geom_point(aes(size = n)) +
  geom_line()

