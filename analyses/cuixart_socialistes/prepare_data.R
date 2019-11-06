# Libraries
library(vilaweb)
library(tidyverse)
library(lubridate)

# Get diputats of the parlament
if('d.RData' %in% dir()){
  load('d.RData')
} else {
  diputats <- get_google_data('parlament')
  diputats <- diputats %>% filter(!is.na(username))
  save(diputats, file = 'd.RData')
}

# Get tweets
if('tl.RData' %in% dir()){
  load('tl.RData')
} else {
  tl <- get_twitter_data_from_database(people = unique(diputats$username))
  
  # Join party data
  tl <- tl %>% left_join(diputats %>% dplyr::select(username, partit))
  save(tl, file = 'tl.RData')  
}

# Define mentions of cuixart, violence, etc.
tl$cuixart <- grepl('cuixart', tolower(tl$tweet))

# Get daily
pd <- tl %>%
  filter(date >= '2019-10-14') %>%
  group_by(cuixart, partit) %>%
  tally %>%
  ungroup %>%
  group_by(partit) %>%
  mutate(p = n / sum(n) * 100) 
left <- expand.grid(partit = sort(unique(pd$partit)),
                    cuixart = sort(unique(pd$cuixart)))
pd <- left_join(left, pd) #%>% filter(cuixart))
pd$n[is.na(pd$n)] <- 0

ggplot(data = pd,
       aes(x = partit,
           y = n,
           group = cuixart,
           fill = cuixart)) +
  geom_bar(stat = 'identity',
           position = position_dodge(width = 0.8))


# Socialists only
# Get daily
pd <- tl %>%
  filter(date >= '2019-10-14',
         partit == 'PSC') %>%
  group_by(cuixart, username) %>%
  tally %>%
  ungroup %>%
  group_by(username) %>%
  mutate(p = n / sum(n) * 100) 
left <- expand.grid(username = sort(unique(diputats$username[diputats$partit == 'PSC'])),
                    cuixart = sort(unique(pd$cuixart)))
pd <- left_join(left, pd) #%>% filter(cuixart))
pd$n[is.na(pd$n)] <- 0

pd <- left_join(pd, diputats)

ggplot(data = pd,
       aes(x = name,
           y = n)) +
  geom_bar(stat = 'identity',
           position = position_dodge(width = 0.8),
           fill = 'darkred', alpha = 0.6) +
  databrew::theme_databrew() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
  labs(x = '',
       y = 'Piulets',
       title = 'Piulets de diputats socialistes del Parlament: 14 d\'octubre - 18 d\'octubre*',
       subtitle = '(*a les 11:00 del matí, quan es va recollir aquestes dades, sense incloure repiulets)') +
  geom_text(aes(label = n),
            nudge_y = 10,
            alpha = 0.5)
