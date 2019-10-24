# Libraries
library(vilaweb)
library(tidyverse)
library(lubridate)

# Define color helper
bp <- function(x = 'Spectral', n = 9){
  RColorBrewer::brewer.pal(n = 9,
                           name = x)[n]
}


# Get omnibus
omnibus <- vilaweb::ceo_omnibus_2019

# Define parties and prison question
omnibus <- omnibus %>%
  mutate(preso = `13. Considera que és just l’empresonament i l’exili dels polítics catalans?`,
         partit = `c3. I em podria dir a quin partit o coalició va votar en les darreres eleccions al Parlament de Catalunya del 21 de desembre de 2017?`) %>%
  mutate(preso = as.character(preso),
         partit = as.character(partit)) %>%
  mutate(preso = ifelse(preso %in% c('No ho sap', 'No contesta'), 'NS/NC', preso)) %>%
  mutate(partit = ifelse(partit %in% c('Nul', 'En blanco', 'En blanc', 'No ho sap', 'No contesta'),
                         'NS/NC/Altre', 
                         ifelse(partit %in% c('Catalunya en Comú Podem', 'Podemos'), 'Comuns',
                                ifelse(partit %in% c('Junts per Catalunya', 'PDeCAT'),
                                       'PDeCat/Junts',
                                       partit)))) %>%
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
                preso, 
                partit,
                neixer = `Em podria dir on va néixer?`,
                any_arribar = `c6b. I a quin any va arribar a Catalunya per quedar-se?`,
                sobiranisme_senzill,
                age = Edat,
                axis = `c1. Vostè com es defineix d'extrema esquerra, esquerra, centre-esquerra, centre, centre-dreta, dreta o extrema dreta?`,
                age_group = `Grups d'edat`,
                age_group_rec = `Grups d'edat reagrupada (recodificació grups de 5 anys)`,
                neixer = `Em podria dir on va néixer?`,
                situacio_laboral = `En quina de les següents situacions laborals es troba vostè actualment?`)


preso_overall <- function(ca = FALSE){
  if(ca){
    other <- 'Altre/NS/NC'
  } else {
    other <- 'Other/not sure'
  }
  
  pd <- omnibus %>%
    group_by(preso) %>%
    tally %>%
    mutate(p = n / sum(n) * 100) 
  if(ca){
    other <- 'Altre/NS/NC'
    the_labs <- labs(x = '',
                     y = 'Percentatge',
                     title = "Considera que és just l'empresonament i l'exili dels polítics catalans?",
                     caption = 'Dades: Òmnibus del CEO, Setembre 2019. Mostreig representatiu de 1.200 residents de Catalunya.')
  } else {
    other <- 'Other/not sure'
    the_labs <- labs(x = '',
                     y = 'Percentage',
                     title = "Do you consider the imprisonment and exile of Catalan politicians\nto be fair?",
                     caption = 'Data: CEO Omnibus, September 2019. Representative sample of 1,200 residents of Catalonia.')
    pd$preso[pd$preso == 'NS/NC'] <- 'Not sure/\nno answer'
  }
  
  
  cols <- rev(
    c(bp('Reds', c(7)),
      bp('Greys', c(6)),
      bp('Blues', c(7))))
  
  ggplot(data = pd,
         aes(x = preso,
             y = p)) +
    geom_bar(stat = 'identity',
             aes(fill = preso)) +
    theme_vilaweb() +
    geom_text(aes(label = point_to_comma(round(p, digits = 1))),
              nudge_y = -5,
              color = 'white',
              alpha = 0.7,
              size = 7) +
    the_labs +
    theme(axis.text.x = element_text(size = 15)) +
    scale_fill_manual(name = '',
                      values = cols,
                      guide = guide_legend(reverse = T)) +
    theme(legend.position = 'none') +
    theme(plot.title = element_text(size = 15, color = 'black'))
  
}

preso_by_party <- function(ca = FALSE){
  
  if(ca){
    other <- 'Altre/NS/NC'
  } else {
    other <- 'Other/not sure'
  }
  
  pd <- omnibus %>%
    mutate(partit = ifelse(partit %in% c('Altres partits', 'NS/NC/Altre'),
                           other,
                           partit)) %>%
    group_by(preso, partit) %>%
    tally %>%
    group_by(partit) %>%
    mutate(p = n / sum(n) * 100) %>%
    filter(!is.na(partit))
  if(ca){
    the_labs <- labs(x = 'Partit',
                     y = 'Percentatge',
                     subtitle = 'Per partit polític',
                     title = "Considera que és just l'empresonament i l'exili dels polítics catalans?",
                     caption = 'Dades: Òmnibus del CEO, Setembre 2019. Mostreig representatiu de 1.200 residents de Catalunya.')
    legend_title <- 'És just?'
    
  } else {
    the_labs <- labs(x = 'Party',
                     y = 'Percentage',
                     subtitle = 'By political party',
                     title = "Do you consider the imprisonment and exile of Catalan politicians\nto be fair?",
                     caption = 'Data: CEO Omnibus, September 2019. Representative sample of 1,200 residents of Catalonia.')
    pd$preso[pd$preso == 'NS/NC'] <- 'Not sure/\nno answer'
    legend_title <- 'Is\nprison\njust?'
  }
  

  
  pd$partit <- factor(pd$partit,
                      levels = c('CUP',
                                 'ERC',
                                 'PDeCat/Junts',
                                 other,
                                 'Comuns',
                                 'PSC',
                                 "C’s",
                                 "PPC"
                                 ),
                      labels = c('CUP',
                                 'ERC',
                                 'PDeCat/Junts',
                                 other,
                                 'Comuns',
                                 'PSC',
                                 "Cs",
                                 "PPC"
                      ))
  cols <- rev(
    c(bp('Reds', c(7)),
      bp('Greys', c(6)),
      bp('Blues', c(7))))
  
  ggplot(data = pd,
         aes(x = partit,
             y = p,
             group = preso,
             fill = preso)) +
    geom_bar(stat = 'identity', position = position_stack(),
             alpha = 0.8) +
    geom_text(aes(label = point_to_comma(round(p, digits = 1)),
                  y = p -1.5),
              position = position_stack(vjust = 1),
              color = 'white', alpha = 0.6) +
    the_labs +
    theme_vilaweb() +
    coord_flip() +
    scale_fill_manual(name = '',
                      values = cols,
                      guide = guide_legend(reverse = T)) +
    theme(plot.title = element_text(size = 15, color = 'black'))
}


referendum <- function(ca = FALSE){
  if(ca){
    other <- 'Altre/NS/NC'
  } else {
    other <- 'Other/not sure'
  }
  
  pd <- omnibus %>%
    group_by(sobiranisme) %>%
    tally %>%
    mutate(p = n / sum(n) * 100)
  if(ca){
    other <- 'Altre/NS/NC'
    the_labs <- labs(x = '',
                     y = 'Percentatge',
                     title = "'S’hauria de fer un referèndum a Catalunya perquè els catalans i les\ncatalanes decidissin quina relació volen que hi hagi entre Catalunya i Espanya'",
                     subtitle = "Grau d'acord amb la frase",
                     caption = 'Dades: Òmnibus del CEO, Setembre 2019. Mostreig representatiu de 1.200 residents de Catalunya.')
  } else {
    other <- 'Other/not sure'
    the_labs <- labs(x = '',
                     y = 'Percentage',
                     title = "'A referendum should take place in Catalonia so that Catalans can decide\nwhat relationship they want there to be between Catalonia and Spain'",
                     subtitle = 'Extent of agreement with above phrase',
                     caption = 'Data: CEO Omnibus, September 2019. Representative sample of 1,200 residents of Catalonia.')
  }
  ca_levels <-  c("Molt d'acord", 
                          "Més aviat d'acord",
                          "NS/NC/Ni-ni",
                          "Més aviat en desacord",
                          "Molt en desacord")
  en_levels <- c("Strongly agree", 
                 "Agree",
                 "Not sure,\nno answer",
                 "Disagree",
                 "Strongly disagree")
  if(ca){
    the_labels <- gsub('olt ', 'olt\n', gsub('aviat ', 'aviat\n', ca_levels))
  } else {
    the_labels <- en_levels
  }
  pd$sobiranisme <- factor(pd$sobiranisme, 
                           levels = ca_levels,
                           labels = the_labels)
  
  
  cols <- rev(
    c(bp('Reds', c(7,5)),
      bp('Greys', c(6)),
      bp('Blues', c(5,7))))
  
  ggplot(data = pd,
         aes(x = sobiranisme,
             y = p)) +
    geom_bar(stat = 'identity',
             aes(fill = sobiranisme)) +
    theme_vilaweb() +
    geom_text(aes(label = point_to_comma(round(p, digits = 1))),
              nudge_y = -5,
              color = 'white',
              alpha = 0.7,
              size = 7) +
    the_labs +
    theme(axis.text.x = element_text(size = 15)) +
    scale_fill_manual(name = '',
                      values = cols,
                      guide = guide_legend(reverse = T)) +
    theme(legend.position = 'none') +
    theme(plot.title = element_text(size = 13, color = 'black'))
  
}

referendum_by_party <- function(ca = FALSE){
  
  if(ca){
    other <- 'Altre/NS/NC'
  } else {
    other <- 'Other/not sure'
  }
  
  pd <- omnibus %>%
    mutate(partit = ifelse(partit %in% c('Altres partits', 'NS/NC/Altre'),
                           other,
                           partit)) %>%
    group_by(sobiranisme, partit) %>%
    tally %>%
    group_by(partit) %>%
    mutate(p = n / sum(n) * 100) %>%
    filter(!is.na(partit))
  if(ca){
    other <- 'Altre/NS/NC'
    the_labs <- labs(x = '',
                     y = 'Percentatge',
                     title = "'S’hauria de fer un referèndum a Catalunya perquè els catalans i les\ncatalanes decidissin quina relació volen que hi hagi entre Catalunya i Espanya'",
                     subtitle = "Grau d'acord amb la frase. Per partit.",
                     caption = 'Dades: Òmnibus del CEO, Setembre 2019. Mostreig representatiu de 1.200 residents de Catalunya.')
  } else {
    other <- 'Other/not sure'
    the_labs <- labs(x = '',
                     y = 'Percentage',
                     title = "'A referendum should take place in Catalonia so that Catalans can decide\nwhat relationship they want there to be between Catalonia and Spain'",
                     subtitle = 'Extent of agreement with above phrase. By party.',
                     caption = 'Data: CEO Omnibus, September 2019. Representative sample of 1,200 residents of Catalonia.')
  }
  
  ca_levels <-  c("Molt d'acord", 
                  "Més aviat d'acord",
                  "NS/NC/Ni-ni",
                  "Més aviat en desacord",
                  "Molt en desacord")
  en_levels <- c("Strongly agree", 
                 "Agree",
                 "Not sure,\nno answer",
                 "Disagree",
                 "Strongly disagree")
  if(ca){
    the_labels <- gsub('olt ', 'olt\n', gsub('aviat ', 'aviat\n', ca_levels))
  } else {
    the_labels <- en_levels
  }
  pd$sobiranisme <- factor(pd$sobiranisme, 
                           levels = ca_levels,
                           labels = the_labels)
  
  
  
  pd$partit <- factor(pd$partit,
                      levels = c('CUP',
                                 'ERC',
                                 'PDeCat/Junts',
                                 other,
                                 'Comuns',
                                 'PSC',
                                 "C’s",
                                 "PPC"
                      ),
                      labels = c('CUP',
                                 'ERC',
                                 'PDeCat/Junts',
                                 other,
                                 'Comuns',
                                 'PSC',
                                 "Cs",
                                 "PPC"
                      ))
  cols <- rev(
    c(bp('Reds', c(7,5)),
      bp('Greys', c(6)),
      bp('Blues', c(5,7))))
  
  ggplot(data = pd,
         aes(x = partit,
             y = p,
             group = sobiranisme,
             fill = sobiranisme)) +
    geom_bar(stat = 'identity', position = position_stack(),
             alpha = 0.8) +
    geom_text(aes(label = point_to_comma(round(p, digits = 1)),
                  y = p -1.5),
              position = position_stack(vjust = 1),
              color = 'white', alpha = 0.6) +
    the_labs +
    theme_vilaweb() +
    coord_flip() +
    scale_fill_manual(name = '',
                      values = cols,
                      guide = guide_legend(reverse = T)) +
    theme(plot.title = element_text(size = 11, color = 'black'))
}

referendum_by_axis <- function(ca = FALSE){
  pd <- omnibus %>%
    filter(!axis %in% c('No ho sap', 'No contesta')) %>%
    group_by(axis, sobiranisme_senzill) %>%
    tally %>%
    group_by(axis) %>%
    mutate(p = n / sum(n) * 100)
  
  if(ca){
    the_labs <- labs(x = 'Autoubicació ideològica',
                     y = 'Percentatge',
                     title = "Grau d'acord amb un referèndum d'autodeterminació per ideologia",
                     caption = 'Dades: Òmnibus del CEO, Setembre 2019. Mostreig representatiu de 1.200 residents de Catalunya.')
    pd$x <- ifelse(pd$sobiranisme_senzill == 'No sobiranista',
                   'Anti-referèndum',
                   ifelse(pd$sobiranisme_senzill == 'Sobiranista',
                          'Pro-referèndum',
                          'NS/NC/Ni-ni'))
  } else {
    pd$x <- ifelse(pd$sobiranisme_senzill == 'No sobiranista',
                   'Anti-referendum',
                   ifelse(pd$sobiranisme_senzill == 'Sobiranista',
                          'Pro-referendum',
                          'Not sure/no answer'))
    the_labs <- labs(x = 'Self-described ideology',
                     y = 'Percentage',
                     title = "Agreement with a self-determination referendum by ideology",
                     caption = 'Data: CEO Omnibus, September 2019. Representative sample of 1,200 residents of Catalonia.')
    pd$axis <- factor(pd$axis,
                      levels = levels(pd$axis),
                      labels = c('Far left', 'Left', 'Center left', 'Center',
                                 'Center right', 'Right', 'Far right', 'No ho sap', 'No contesta'))
  }
  cols <- rev(
    c(bp('Reds', c(7)),
      bp('Greys', c(6)),
      bp('Blues', c(7))))
  
  ggplot(data = pd,
         aes(x = axis,
             y = p,
             color = x,
             group = x)) +
    geom_point(size = 4, alpha = 0.7) +
    geom_line(size = 2) +
    theme_vilaweb() +
    the_labs +
    scale_color_manual(name = '',
                      values = cols) +
    theme(axis.text.x = element_text(size = 10)) +
    theme(plot.title = element_text(size = 15, color = 'black'))
}


referendum_by_birthplace <- function(ca = FALSE){
  pd <- omnibus %>%
    filter(!neixer %in% c('No ho sap', 'No contesta'),
           !is.na(neixer)) %>%
    group_by(neixer, sobiranisme_senzill) %>%
    tally %>%
    group_by(neixer) %>%
    mutate(p = n / sum(n) * 100)
  
  if(ca){
    the_labs <- labs(x = 'Lloc de naixement',
                     y = 'Percentatge',
                     title = "Grau d'acord amb un referèndum d'autodeterminació per lloc de naixement",
                     caption = 'Dades: Òmnibus del CEO, Setembre 2019. Mostreig representatiu de 1.200 residents de Catalunya.')
    pd$y <- ifelse(pd$sobiranisme_senzill == 'No sobiranista',
                   'Anti-referèndum',
                   ifelse(pd$sobiranisme_senzill == 'Sobiranista',
                          'Pro-referèndum',
                          'NS/NC/Ni-ni'))
    pd$x <- pd$neixer
    pd$x <- factor(pd$neixer,
                   levels = levels(pd$neixer),
                   labels = c('Catalunya', "Resta de l'estat espanyol", 'Estranger'))
  } else {
    pd$y <- ifelse(pd$sobiranisme_senzill == 'No sobiranista',
                   'Anti-referendum',
                   ifelse(pd$sobiranisme_senzill == 'Sobiranista',
                          'Pro-referendum',
                          'Not sure/no answer'))
    the_labs <- labs(x = 'Place of birth',
                     y = 'Percentage',
                     title = "Agreement with a self-determination referendum by birthplace",
                     caption = 'Data: CEO Omnibus, September 2019. Representative sample of 1,200 residents of Catalonia.')
  pd$x <- factor(pd$neixer,
                 levels = levels(pd$neixer),
                 labels = c('Catalonia', 'Rest of Spanish State', 'Abroad'))
  }
  cols <- rev(
    c(bp('Reds', c(7)),
      bp('Greys', c(6)),
      bp('Blues', c(7))))
  
  ggplot(data = pd,
         aes(x = x,
             y = p,
             fill = y,
             group = y)) +
    geom_bar(stat = 'identity',
             alpha = 0.8) +
    geom_text(aes(label = point_to_comma(round(p, digits = 1))),
              position = position_stack(vjust = 0.5),
              color = 'white',
              size = 5) +
    theme_vilaweb() +
    the_labs +
    scale_fill_manual(name = '',
                       values = cols) +
    theme(axis.text.x = element_text(size = 10)) +
    theme(plot.title = element_text(size = 15, color = 'black')) +
    geom_hline(yintercept = 50, lty = 2)
}


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

point_to_comma <- function(x){
  gsub('.', ',', x, fixed = T)
}


